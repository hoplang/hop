use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::{self, Argument, Parameter, Type};
use crate::error_collector::ErrorCollector;
use crate::hop::ast::Ast;
use crate::hop::ast::{Attribute, ComponentDefinition};
use crate::hop::environment::Environment;
use crate::hop::type_error::TypeError;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{self, Display};

use super::ast::{AttributeValue, TypedAst, TypedAttribute, UntypedAst};
use super::module_name::ModuleName;
use super::node::{Node, TypedNode, UntypedNode};

#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub typ: Type,
    pub name: String,
    pub range: DocumentRange,
}

impl Ranged for TypeAnnotation {
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.typ)
    }
}

#[derive(Debug, Clone)]
pub struct ComponentTypeInformation {
    // Track the parameter types for the component.
    parameter_types: Option<Vec<Parameter>>,
    // Track whether the component has a slot-default.
    has_slot: bool,
}

#[derive(Debug, Clone, Default)]
pub struct ModuleTypeInformation {
    components: HashMap<String, ComponentTypeInformation>,
}

impl ModuleTypeInformation {
    fn get_parameter_types(&self, component_name: &str) -> Option<&Vec<Parameter>> {
        self.components
            .get(component_name)?
            .parameter_types
            .as_ref()
    }

    fn component_has_slot(&self, component_name: &str) -> bool {
        self.components
            .get(component_name)
            .is_some_and(|c| c.has_slot)
    }

    fn component_is_declared(&self, component_name: &str) -> bool {
        self.components.contains_key(component_name)
    }

    fn set_component_type_info(
        &mut self,
        component_name: &str,
        type_info: ComponentTypeInformation,
    ) {
        self.components
            .insert(component_name.to_string(), type_info);
    }
}

#[derive(Debug, Default)]
struct State {
    modules: HashMap<ModuleName, ModuleTypeInformation>,
}

impl State {
    fn set_type_info(
        &mut self,
        module_name: &ModuleName,
        component_name: &str,
        type_info: ComponentTypeInformation,
    ) {
        self.modules
            .entry(module_name.clone())
            .or_default()
            .set_component_type_info(component_name, type_info);
    }
}

#[derive(Default, Debug)]
pub struct TypeChecker {
    state: State,
    pub type_errors: HashMap<ModuleName, ErrorCollector<TypeError>>,
    pub type_annotations: HashMap<ModuleName, Vec<TypeAnnotation>>,
    pub typed_asts: HashMap<ModuleName, TypedAst>,
}

impl TypeChecker {
    // TODO: Return a bool here indicating whether the state for these modules
    // were changed
    pub fn typecheck(&mut self, modules: &[&UntypedAst]) {
        for module in modules {
            let type_errors = self.type_errors.entry(module.name.clone()).or_default();
            let type_annotations = self
                .type_annotations
                .entry(module.name.clone())
                .or_default();

            type_errors.clear();
            type_annotations.clear();

            let typed_ast =
                typecheck_module(module, &mut self.state, type_errors, type_annotations);
            self.typed_asts.insert(module.name.clone(), typed_ast);

            if modules.len() > 1 {
                type_errors.clear();
                for import_node in module.get_imports() {
                    let imported_module = import_node.imported_module();
                    type_errors.push(TypeError::import_cycle(
                        module.name.as_str(),
                        imported_module.as_str(),
                        &modules
                            .iter()
                            .map(|m| m.name.to_string())
                            .collect::<Vec<_>>(),
                        import_node.from_attr.value.clone(),
                    ));
                }
            }
        }
    }
}

fn typecheck_module(
    module: &UntypedAst,
    state: &mut State,
    errors: &mut ErrorCollector<TypeError>,
    annotations: &mut Vec<TypeAnnotation>,
) -> TypedAst {
    // Validate imports
    for import in module.get_imports() {
        let imported_module = import.imported_module();
        let imported_component = import.imported_component();
        let Some(module_state) = state.modules.get(imported_module) else {
            errors.push(TypeError::ImportFromUndefinedModule {
                module: imported_module.as_str().to_string(),
                range: import.from_attr.value.clone(),
            });
            continue;
        };
        if !module_state.component_is_declared(imported_component.as_str()) {
            errors.push(TypeError::UndeclaredComponent {
                module_name: import.from_attr.value.clone(),
                component_name: imported_component.clone(),
            });
        }
    }

    let mut env = Environment::new();
    let _ = env.push("hop_mode".to_string(), Type::String);

    // Build typed component definitions
    let mut typed_component_definitions = Vec::new();

    for component_def in module.get_component_definitions() {
        let ComponentDefinition {
            name: component_name,
            tag_name: name,
            params,
            children,
            has_slot,
            range,
            closing_tag_name,
        } = component_def;

        // Push parameters to environment
        if let Some((params, _)) = params {
            for param in params {
                annotations.push(TypeAnnotation {
                    range: param.var_name_range.clone(),
                    typ: param.var_type.clone(),
                    name: param.var_name.to_string(),
                });
                let _ = env.push(param.var_name.to_string(), param.var_type.clone());
            }
        }

        // Typecheck children and collect typed versions
        let typed_children: Vec<_> = children
            .iter()
            .filter_map(|child| typecheck_node(child, state, &mut env, annotations, errors))
            .collect();

        // Pop parameters from environment
        if let Some((params, _)) = params {
            // iterate in reverse to pop in right order
            for param in params.iter().rev() {
                let (_, _, accessed) = env.pop();
                if !accessed {
                    errors.push(TypeError::UnusedVariable {
                        var_name: param.var_name_range.clone(),
                    })
                }
            }
        }

        // Store type information in state
        state.set_type_info(
            &module.name,
            name.as_str(),
            ComponentTypeInformation {
                parameter_types: params.clone().map(|(params, _)| params),
                has_slot: *has_slot,
            },
        );

        // Build typed ComponentDefinition
        typed_component_definitions.push(ComponentDefinition {
            name: component_name.clone(),
            tag_name: name.clone(),
            closing_tag_name: closing_tag_name.clone(),
            params: params.clone(),
            range: range.clone(),
            children: typed_children,
            has_slot: *has_slot,
        });
    }

    // Build and return the typed AST
    Ast::new(
        module.name.clone(),
        typed_component_definitions,
        module.get_imports().to_vec(),
        module.get_records().to_vec(),
    )
}

fn typecheck_node(
    node: &UntypedNode,
    state: &State,
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut ErrorCollector<TypeError>,
) -> Option<TypedNode> {
    match node {
        Node::If {
            condition,
            children,
            range,
        } => {
            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors))
                .collect();

            let typed_condition = errors
                .ok_or_add(dop::typecheck_expr(condition, env, annotations).map_err(Into::into))?;

            let condition_type = typed_condition.as_type();
            if !condition_type.is_subtype(&Type::Bool) {
                errors.push(TypeError::ExpectedBooleanCondition {
                    found: condition_type.to_string(),
                    range: condition.range().clone(),
                })
            }

            Some(Node::If {
                condition: typed_condition,
                range: range.clone(),
                children: typed_children,
            })
        }

        Node::For {
            var_name,
            var_name_range,
            array_expr,
            children,
            range,
        } => {
            let typed_array = errors
                .ok_or_add(dop::typecheck_expr(array_expr, env, annotations).map_err(Into::into))?;
            let array_type = typed_array.as_type();
            let element_type = match &array_type {
                Type::Array(Some(inner)) => *inner.clone(),
                Type::Array(None) => {
                    errors.push(TypeError::CannotIterateEmptyArray {
                        range: array_expr.range().clone(),
                    });
                    return None;
                }
                _ => {
                    errors.push(TypeError::CannotIterateOver {
                        typ: array_type.to_string(),
                        range: array_expr.range().clone(),
                    });
                    return None;
                }
            };

            // Push the loop variable into scope
            let pushed = match env.push(var_name.to_string(), element_type.clone()) {
                Ok(_) => {
                    annotations.push(TypeAnnotation {
                        range: var_name_range.clone(),
                        typ: element_type.clone(),
                        name: var_name.to_string(),
                    });
                    true
                }
                Err(_) => {
                    errors.push(TypeError::VariableIsAlreadyDefined {
                        var: var_name.as_str().to_string(),
                        range: var_name_range.clone(),
                    });
                    false
                }
            };

            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors))
                .collect();

            if pushed {
                let (_, _, accessed) = env.pop();
                if !accessed {
                    errors.push(TypeError::UnusedVariable {
                        var_name: var_name_range.clone(),
                    })
                }
            }

            Some(Node::For {
                var_name: var_name.clone(),
                var_name_range: var_name_range.clone(),
                array_expr: typed_array,
                range: range.clone(),
                children: typed_children,
            })
        }

        Node::ComponentReference {
            tag_name,
            definition_module,
            closing_tag_name,
            args,
            children,
            range,
        } => {
            // Transform children
            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors))
                .collect();

            let module_info = match definition_module
                .as_ref()
                .and_then(|name| state.modules.get(name))
            {
                Some(module_info) => module_info,
                None => {
                    errors.push(TypeError::UndefinedComponent {
                        tag_name: tag_name.clone(),
                    });
                    return None;
                }
            };

            // Validate that content is only passed to components with slot-default
            if !children.is_empty() && !module_info.component_has_slot(tag_name.as_str()) {
                errors.push(TypeError::UndefinedSlot {
                    component: tag_name.as_str().to_string(),
                    range: tag_name.clone(),
                });
            }

            // Validate arguments and build typed versions
            let typed_args = match (module_info.get_parameter_types(tag_name.as_str()), args) {
                (None, None) => None,
                (None, Some((_, args_range))) => {
                    errors.push(TypeError::UnexpectedArguments {
                        range: args_range.clone(),
                    });
                    None
                }
                (Some(params), None) => {
                    errors.push(TypeError::missing_arguments(params, tag_name.clone()));
                    None
                }
                (Some(params), Some((args, args_range))) => {
                    let mut typed_arguments = Vec::new();
                    for param in params {
                        if !args
                            .iter()
                            .any(|a| a.var_name.as_str() == param.var_name.as_str())
                        {
                            errors.push(TypeError::MissingRequiredParameter {
                                param: param.var_name.as_str().to_string(),
                                range: args_range.clone(),
                            });
                        }
                    }

                    for arg in args {
                        let param = match params
                            .iter()
                            .find(|p| p.var_name.as_str() == arg.var_name.as_str())
                        {
                            None => {
                                errors.push(TypeError::UnexpectedArgument {
                                    arg: arg.var_name.as_str().to_string(),
                                    range: arg.var_name_range.clone(),
                                });
                                continue;
                            }
                            Some(param) => param,
                        };

                        let typed_expr = match dop::typecheck_expr(&arg.var_expr, env, annotations)
                        {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(err.into());
                                continue;
                            }
                        };
                        let arg_type = typed_expr.as_type().clone();

                        if !arg_type.is_subtype(&param.var_type) {
                            errors.push(TypeError::ArgumentIsIncompatible {
                                expected: param.var_type.clone(),
                                found: arg_type.clone(),
                                arg_name: arg.var_name_range.clone(),
                                expr_range: arg.var_expr.range().clone(),
                            });
                            continue;
                        }

                        // Build the typed argument
                        typed_arguments.push(Argument {
                            var_name: arg.var_name.clone(),
                            var_name_range: arg.var_name_range.clone(),
                            var_expr: typed_expr,
                        });

                        annotations.push(TypeAnnotation {
                            range: arg.var_expr.range().clone(),
                            typ: arg_type,
                            name: arg.var_name.to_string(),
                        });
                    }

                    Some((typed_arguments, args_range.clone()))
                }
            };

            Some(Node::ComponentReference {
                tag_name: tag_name.clone(),
                definition_module: definition_module.clone(),
                closing_tag_name: closing_tag_name.clone(),
                args: typed_args,
                range: range.clone(),
                children: typed_children,
            })
        }

        Node::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children,
            range,
        } => {
            let typed_attributes = typecheck_attributes(attributes, env, annotations, errors);

            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors))
                .collect();

            Some(Node::Html {
                tag_name: tag_name.clone(),
                closing_tag_name: closing_tag_name.clone(),
                attributes: typed_attributes,
                range: range.clone(),
                children: typed_children,
            })
        }

        Node::TextExpression { expression, range } => {
            if let Some(typed_expr) = errors
                .ok_or_add(dop::typecheck_expr(expression, env, annotations).map_err(Into::into))
            {
                let expr_type = typed_expr.as_type();
                if !expr_type.is_subtype(&Type::String) && !expr_type.is_subtype(&Type::TrustedHTML)
                {
                    errors.push(TypeError::ExpectedStringExpression {
                        found: expr_type.clone(),
                        range: range.clone(),
                    });
                }
                Some(Node::TextExpression {
                    expression: typed_expr,
                    range: range.clone(),
                })
            } else {
                None
            }
        }

        Node::Placeholder { children, range } => {
            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors))
                .collect();

            Some(Node::Placeholder {
                range: range.clone(),
                children: typed_children,
            })
        }

        Node::SlotDefinition { range } => Some(Node::SlotDefinition {
            range: range.clone(),
        }),

        Node::Text { value, range } => Some(Node::Text {
            value: value.clone(),
            range: range.clone(),
        }),

        Node::Doctype { value, range } => Some(Node::Doctype {
            value: value.clone(),
            range: range.clone(),
        }),
    }
}

fn typecheck_attributes(
    attributes: &BTreeMap<StringSpan, Attribute>,
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut ErrorCollector<TypeError>,
) -> BTreeMap<StringSpan, TypedAttribute> {
    let mut typed_attributes = BTreeMap::new();

    for (key, attr) in attributes {
        let typed_value = match &attr.value {
            Some(AttributeValue::Expressions(exprs)) => {
                let mut typed_exprs = Vec::new();
                for expr in exprs {
                    if let Some(typed_expr) = errors
                        .ok_or_add(dop::typecheck_expr(expr, env, annotations).map_err(Into::into))
                    {
                        // Check that HTML attributes are strings
                        let expr_type = typed_expr.as_type();
                        if !expr_type.is_subtype(&Type::String) {
                            errors.push(TypeError::ExpectedStringAttribute {
                                found: expr_type.to_string(),
                                range: expr.annotation().clone(),
                            });
                        }
                        typed_exprs.push(typed_expr);
                    }
                }
                if !typed_exprs.is_empty() {
                    Some(AttributeValue::Expressions(typed_exprs))
                } else {
                    None
                }
            }
            Some(AttributeValue::String(s)) => Some(AttributeValue::String(s.clone())),
            None => None,
        };

        typed_attributes.insert(
            key.clone(),
            Attribute {
                name: attr.name.clone(),
                value: typed_value,
                range: attr.range.clone(),
            },
        );
    }

    typed_attributes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::hop::module_name::ModuleName;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn check(archive_str: &str, expected: Expect) {
        let archive = Archive::from(archive_str);
        let mut error_output = Vec::new();
        let mut type_output = Vec::new();
        let error_annotator = DocumentAnnotator::new()
            .with_label("error")
            .with_lines_before(1)
            .with_location();

        let type_annotator = DocumentAnnotator::new()
            .with_lines_before(1)
            .with_location();

        let mut typechecker = TypeChecker::default();

        // Process all .hop files in the archive
        for file in archive.iter() {
            if !file.name.ends_with(".hop") {
                panic!("Got invalid file name")
            }
            let source_code = file.content.trim();
            let mut parse_errors = crate::error_collector::ErrorCollector::new();
            let tokenizer = Tokenizer::new(source_code.to_string());
            let module_name =
                ModuleName::new(file.name.trim_end_matches(".hop").to_string()).unwrap();
            let module = parse(module_name, tokenizer, &mut parse_errors);

            if !parse_errors.is_empty() {
                panic!("Got parse errors: {:#?}", parse_errors);
            }

            typechecker.typecheck(&[&module]);

            let type_errors = typechecker.type_errors.get(&module.name);
            let type_annotations = typechecker.type_annotations.get(&module.name);

            if type_errors.is_some_and(|err| !err.is_empty()) {
                error_output.push(error_annotator.annotate(
                    Some(&file.name),
                    typechecker.type_errors.get(&module.name).unwrap(),
                ));
            } else if type_annotations.is_some_and(|ann| !ann.is_empty()) {
                let formatted_errors = type_annotator.annotate(
                    Some(&file.name),
                    typechecker.type_annotations.get(&module.name).unwrap(),
                );
                type_output.push(formatted_errors);
            }
        }

        if !error_output.is_empty() {
            expected.assert_eq(error_output.join("\n").as_str());
        } else {
            expected.assert_eq(type_output.join("\n").as_str());
        }
    }

    // The typechecker allows empty files, and no types are emitted for them.
    #[test]
    fn test_empty_file() {
        check("-- main.hop --", expect![[r#""#]]);
    }

    // The parameter type of a single component without any content is `{}`.
    #[test]
    fn test_single_component_no_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                </Main>
            "#},
            expect![""],
        );
    }

    // When an undefined component is referenced, the typechecker outputs an error.
    #[test]
    fn test_undefined_component_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                	<h1>Hello,
                        <RenderBar>
                            <div></div>
                        </RenderBar>
                    </h1>
                    <RenderFoo></RenderFoo>
                </Main>
            "#},
            expect![[r#"
                error: Component RenderBar is not defined
                  --> main.hop (line 3, col 10)
                2 |     <h1>Hello,
                3 |         <RenderBar>
                  |          ^^^^^^^^^

                error: Component RenderFoo is not defined
                  --> main.hop (line 7, col 6)
                6 |     </h1>
                7 |     <RenderFoo></RenderFoo>
                  |      ^^^^^^^^^
            "#]],
        );
    }

    // When a component references itself, the typechecker outputs an error.
    #[test]
    fn test_component_references_itself() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                	<h1>Hello, <Main></Main>!</h1>
                </Main>
            "#},
            expect![[r#"
                error: Component Main is not defined
                  --> main.hop (line 2, col 14)
                1 | <Main>
                2 |     <h1>Hello, <Main></Main>!</h1>
                  |                 ^^^^
            "#]],
        );
    }

    // When a component is imported from a module that doesn't exist the typechecker outputs an error.
    // TODO: Improve error message
    #[test]
    fn test_import_from_nonexistent_module() {
        check(
            indoc! {r#"
                -- main.hop --
                import Foo from "@/other"

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module other is not defined
                  --> main.hop (line 1, col 18)
                1 | import Foo from "@/other"
                  |                  ^^^^^^^
            "#]],
        );
    }

    // When a component that doesn't exist is imported from a module that does exist the typechecker outputs an error.
    #[test]
    fn test_import_nonexistent_component_from_existing_module() {
        check(
            indoc! {r#"
                -- other.hop --
                -- main.hop --
                import Foo from "@/other"

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module other is not defined
                  --> main.hop (line 1, col 18)
                1 | import Foo from "@/other"
                  |                  ^^^^^^^
            "#]],
        );
    }

    // A component may be imported without being used.
    #[test]
    fn test_unused_import() {
        check(
            indoc! {r#"
                -- other.hop --
                <Foo>
                </Foo>

                -- main.hop --
                import Foo from "@/other"

                <Main>
                </Main>
            "#},
            expect![""],
        );
    }

    // The typechecker collects all errors rather than returning at the first error.
    #[test]
    fn test_multiple_errors_collected() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                	<h1>Hello, <RenderName></RenderName>!</h1>
                	<h1>Hello, <OtherComp></OtherComp>!</h1>
                </Main>
            "#},
            expect![[r#"
                error: Component RenderName is not defined
                  --> main.hop (line 2, col 14)
                1 | <Main>
                2 |     <h1>Hello, <RenderName></RenderName>!</h1>
                  |                 ^^^^^^^^^^

                error: Component OtherComp is not defined
                  --> main.hop (line 3, col 14)
                2 |     <h1>Hello, <RenderName></RenderName>!</h1>
                3 |     <h1>Hello, <OtherComp></OtherComp>!</h1>
                  |                 ^^^^^^^^^
            "#]],
        );
    }

    // Defining a component with the same name in two different modules is allowed.
    #[test]
    fn test_same_component_name_in_different_modules() {
        check(
            indoc! {r#"
                -- other.hop --
                <Foo>
                </Foo>

                -- main.hop --
                <Foo>
                </Foo>
            "#},
            expect![""],
        );
    }

    // When content is passed to a component without slot-default, the typechecker outputs an error.
    #[test]
    fn test_undefined_slot_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <strong>No slot here</strong>
                </Main>

                <Bar>
                    <Main>
                        This component has no slot
                    </Main>
                </Bar>
            "#},
            expect![[r#"
                error: Component Main does not have a slot-default
                  --> main.hop (line 6, col 6)
                5 | <Bar>
                6 |     <Main>
                  |      ^^^^
            "#]],
        );
    }

    // Test undefined slot with imported component.
    #[test]
    fn test_undefined_slot_with_imported_component() {
        check(
            indoc! {r#"
                -- other.hop --
                <Foo>
                    <strong>No slot here</strong>
                </Foo>
                -- main.hop --
                import Foo from "@/other"

                <Bar>
                    <Foo>
                        This component has no slot
                    </Foo>
                </Bar>
            "#},
            expect![[r#"
                error: Component Foo does not have a slot-default
                  --> main.hop (line 4, col 6)
                3 | <Bar>
                4 |     <Foo>
                  |      ^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: {foo: Array[String]}}>
                  <for {items in items.foo}>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <Main {items: {foo: Array[String]}}>
                2 |   <for {items in items.foo}>
                  |         ^^^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_array_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: Array[String]}>
                  <for {items in items}>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <Main {items: Array[String]}>
                2 |   <for {items in items}>
                  |         ^^^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_nested_loops() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: {a: Array[String], b: Array[String]}}>
                  <for {item in items.a}>
                    <for {item in items.b}>
                      <div>{item}</div>
                    </for>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Variable item is already defined
                  --> main.hop (line 3, col 11)
                2 |   <for {item in items.a}>
                3 |     <for {item in items.b}>
                  |           ^^^^
            "#]],
        );
    }

    // When an undefined variable is referenced, the typechecker outputs an error.
    #[test]
    fn test_undefined_variable_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: Array[{active: Bool}]}>
                	<for {item in params}>
                	  <if {item.active}>
                	  </if>
                	</for>
                	<if {item.active}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                error: Undefined variable: item
                  --> main.hop (line 6, col 7)
                5 |     </for>
                6 |     <if {item.active}>
                  |          ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_variable_second_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: Array[String]}>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                  <for {item in items}>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 5, col 9)
                4 |   </for>
                5 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_variable_first_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: Array[String]}>
                  <for {item in items}>
                  </for>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | <Main {items: Array[String]}>
                2 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_variable_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: Array[String]}>
                  <for {item in items}>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | <Main {items: Array[String]}>
                2 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    // When a variable is unused, the typechecker outputs an error.
    #[test]
    fn test_unused_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Bar {p: String, s: String}>
                  <div>
                  	{s}
                  </div>
                </Bar>
            "#},
            expect![[r#"
                error: Unused variable p
                  --> main.hop (line 1, col 7)
                1 | <Bar {p: String, s: String}>
                  |       ^
            "#]],
        );
    }

    // Arguments may be passed in any order to a component.
    #[test]
    fn test_component_handles_arguments_in_any_order() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {a: Bool, b: String}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </Main>
                <Foo>
                  <Main {b: "foo", a: true}/>
                </Foo>
            "#},
            expect![[r#"
                a: Bool
                  --> main.hop (line 1, col 8)
                1 | <Main {a: Bool, b: String}>
                  |        ^

                b: String
                  --> main.hop (line 1, col 17)
                1 | <Main {a: Bool, b: String}>
                  |                 ^

                b: String
                  --> main.hop (line 3, col 11)
                2 |   <if {a}>
                3 |     <div>{b}</div>
                  |           ^

                a: Bool
                  --> main.hop (line 2, col 8)
                1 | <Main {a: Bool, b: String}>
                2 |   <if {a}>
                  |        ^

                b: String
                  --> main.hop (line 7, col 13)
                6 | <Foo>
                7 |   <Main {b: "foo", a: true}/>
                  |             ^^^^^

                a: Bool
                  --> main.hop (line 7, col 23)
                6 | <Foo>
                7 |   <Main {b: "foo", a: true}/>
                  |                       ^^^^
            "#]],
        );
    }

    // When a parameter is missing the typechecker reports an error.
    #[test]
    fn test_component_missing_required_parameter_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {a: Bool, b: String}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </Main>
                <Foo>
                  <Main {b: "foo"}/>
                </Foo>
            "#},
            expect![[r#"
                error: Missing required parameter 'a'
                  --> main.hop (line 7, col 10)
                6 | <Foo>
                7 |   <Main {b: "foo"}/>
                  |          ^^^^^^^^
            "#]],
        );
    }

    // When the whole parameter expression is missing the typechecker reports an error.
    #[test]
    fn test_component_does_not_specify_required_parameters_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {a: Bool, b: String}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </Main>
                <Foo>
                  <Main />
                </Foo>
            "#},
            expect![[r#"
                error: Component requires arguments: a, b
                  --> main.hop (line 7, col 4)
                6 | <Foo>
                7 |   <Main />
                  |    ^^^^
            "#]],
        );
    }

    // When arguments are specified but the component does not expect any the typechecker reports
    // an error.
    #[test]
    fn test_component_has_no_parameters_but_arguments_were_supplied_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  hello world
                </Main>
                <Foo>
                  <Main {a: "foo"} />
                </Foo>
            "#},
            expect![[r#"
                error: Component does not accept arguments
                  --> main.hop (line 5, col 10)
                4 | <Foo>
                5 |   <Main {a: "foo"} />
                  |          ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_iterate_over_boolean_property() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: Array[{k: Bool}]}>
                	<for {item in params}>
                		<if {item.k}>
                		</if>
                	</for>
                	<for {item in params}>
                		<for {inner in item.k}>
                			<div>{inner}</div>
                		</for>
                	</for>
                </Main>
            "#},
            expect![[r#"
                error: Can not iterate over Bool
                  --> main.hop (line 7, col 18)
                 6 |     <for {item in params}>
                 7 |         <for {inner in item.k}>
                   |                        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_iterate_over_boolean_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: Bool}>
                	<if {params}>
                	</if>
                	<for {item in params}>
                		<div>{item}</div>
                	</for>
                </Main>
            "#},
            expect![[r#"
                error: Can not iterate over Bool
                  --> main.hop (line 4, col 16)
                3 |     </if>
                4 |     <for {item in params}>
                  |                   ^^^^^^
            "#]],
        );
    }

    // When successful, the typechecker identifies the parameter type of the component as well as the defined slots.
    #[test]
    fn test_successful_typechecking_with_complex_params() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {items: Array[{active: Bool, name: Bool}]}}>
                	<for {item in params.items}>
                		<if {item.active}>
                		</if>
                		<if {item.name}>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: Record[items: Array[Record[active: Bool, name: Bool]]]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {items: Array[{active: Bool, name: Bool}]}}>
                  |        ^^^^^^

                params: Record[items: Array[Record[active: Bool, name: Bool]]]
                  --> main.hop (line 2, col 16)
                1 | <Main {params: {items: Array[{active: Bool, name: Bool}]}}>
                2 |     <for {item in params.items}>
                  |                   ^^^^^^

                item: Record[active: Bool, name: Bool]
                  --> main.hop (line 2, col 8)
                1 | <Main {params: {items: Array[{active: Bool, name: Bool}]}}>
                2 |     <for {item in params.items}>
                  |           ^^^^

                item: Record[active: Bool, name: Bool]
                  --> main.hop (line 3, col 8)
                2 |     <for {item in params.items}>
                3 |         <if {item.active}>
                  |              ^^^^

                item: Record[active: Bool, name: Bool]
                  --> main.hop (line 5, col 8)
                4 |         </if>
                5 |         <if {item.name}>
                  |              ^^^^
            "#]],
        );
    }

    // A component should be able to define the <head> tag and a default slot.
    #[test]
    fn test_head_tag_with_default_slot() {
        check(
            indoc! {r#"
                -- main.hop --
                <CustomHead>
                	<head>
                		<title><slot-default/></title>
                	</head>
                </CustomHead>
            "#},
            expect![""],
        );
    }

    #[test]
    fn test_boolean_equality_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {a: String, b: Bool}}>
                  <if {(params.a == "str") == params.b}>
                    <div>Match</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: Record[a: String, b: Bool]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {a: String, b: Bool}}>
                  |        ^^^^^^

                params: Record[a: String, b: Bool]
                  --> main.hop (line 2, col 9)
                1 | <Main {params: {a: String, b: Bool}}>
                2 |   <if {(params.a == "str") == params.b}>
                  |         ^^^^^^

                params: Record[a: String, b: Bool]
                  --> main.hop (line 2, col 31)
                1 | <Main {params: {a: String, b: Bool}}>
                2 |   <if {(params.a == "str") == params.b}>
                  |                               ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {enabled: Bool, users: Array[{profile: {verified: Bool}, posts: Array[{published: Bool}]}]}}>
                	<if {params.enabled}>
                		<for {user in params.users}>
                			<if {user.profile.verified}>
                				<for {post in user.posts}>
                					<if {post.published}>
                					  published
                					</if>
                				</for>
                			</if>
                		</for>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: Record[
                  enabled: Bool,
                  users: Array[Record[
                    posts: Array[Record[published: Bool]],
                    profile: Record[verified: Bool],
                  ]],
                ]
                  --> main.hop (line 1, col 8)
                 1 | <Main {params: {enabled: Bool, users: Array[{profile: {verified: Bool}, posts: Array[{published: Bool}]}]}}>
                   |        ^^^^^^

                params: Record[
                  enabled: Bool,
                  users: Array[Record[
                    posts: Array[Record[published: Bool]],
                    profile: Record[verified: Bool],
                  ]],
                ]
                  --> main.hop (line 3, col 17)
                 2 |     <if {params.enabled}>
                 3 |         <for {user in params.users}>
                   |                       ^^^^^^

                user: Record[
                  posts: Array[Record[published: Bool]],
                  profile: Record[verified: Bool],
                ]
                  --> main.hop (line 3, col 9)
                 2 |     <if {params.enabled}>
                 3 |         <for {user in params.users}>
                   |               ^^^^

                user: Record[
                  posts: Array[Record[published: Bool]],
                  profile: Record[verified: Bool],
                ]
                  --> main.hop (line 5, col 19)
                 4 |             <if {user.profile.verified}>
                 5 |                 <for {post in user.posts}>
                   |                               ^^^^

                post: Record[published: Bool]
                  --> main.hop (line 5, col 11)
                 4 |             <if {user.profile.verified}>
                 5 |                 <for {post in user.posts}>
                   |                       ^^^^

                post: Record[published: Bool]
                  --> main.hop (line 6, col 11)
                 5 |                 <for {post in user.posts}>
                 6 |                     <if {post.published}>
                   |                          ^^^^

                user: Record[
                  posts: Array[Record[published: Bool]],
                  profile: Record[verified: Bool],
                ]
                  --> main.hop (line 4, col 9)
                 3 |         <for {user in params.users}>
                 4 |             <if {user.profile.verified}>
                   |                  ^^^^

                params: Record[
                  enabled: Bool,
                  users: Array[Record[
                    posts: Array[Record[published: Bool]],
                    profile: Record[verified: Bool],
                  ]],
                ]
                  --> main.hop (line 2, col 7)
                 1 | <Main {params: {enabled: Bool, users: Array[{profile: {verified: Bool}, posts: Array[{published: Bool}]}]}}>
                 2 |     <if {params.enabled}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_structure_2() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                	<for {section in params.sections}>
                		<if {section.header.visible}>
                			<for {item in section.items}>
                				<if {item.data.valid}>
                				</if>
                			</for>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: Record[
                  sections: Array[Record[
                    header: Record[visible: Bool],
                    items: Array[Record[data: Record[valid: Bool]]],
                  ]],
                ]
                  --> main.hop (line 1, col 8)
                 1 | <Main {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                   |        ^^^^^^

                params: Record[
                  sections: Array[Record[
                    header: Record[visible: Bool],
                    items: Array[Record[data: Record[valid: Bool]]],
                  ]],
                ]
                  --> main.hop (line 2, col 19)
                 1 | <Main {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                 2 |     <for {section in params.sections}>
                   |                      ^^^^^^

                section: Record[
                  header: Record[visible: Bool],
                  items: Array[Record[data: Record[valid: Bool]]],
                ]
                  --> main.hop (line 2, col 8)
                 1 | <Main {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                 2 |     <for {section in params.sections}>
                   |           ^^^^^^^

                section: Record[
                  header: Record[visible: Bool],
                  items: Array[Record[data: Record[valid: Bool]]],
                ]
                  --> main.hop (line 4, col 18)
                 3 |         <if {section.header.visible}>
                 4 |             <for {item in section.items}>
                   |                           ^^^^^^^

                item: Record[data: Record[valid: Bool]]
                  --> main.hop (line 4, col 10)
                 3 |         <if {section.header.visible}>
                 4 |             <for {item in section.items}>
                   |                   ^^^^

                item: Record[data: Record[valid: Bool]]
                  --> main.hop (line 5, col 10)
                 4 |             <for {item in section.items}>
                 5 |                 <if {item.data.valid}>
                   |                      ^^^^

                section: Record[
                  header: Record[visible: Bool],
                  items: Array[Record[data: Record[valid: Bool]]],
                ]
                  --> main.hop (line 3, col 8)
                 2 |     <for {section in params.sections}>
                 3 |         <if {section.header.visible}>
                   |              ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {i: {j: {k: {l: Bool}}}}}>
                	<if {params.i.j.k.l}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: Record[i: Record[j: Record[k: Record[l: Bool]]]]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {i: {j: {k: {l: Bool}}}}}>
                  |        ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]]]]
                  --> main.hop (line 2, col 7)
                1 | <Main {params: {i: {j: {k: {l: Bool}}}}}>
                2 |     <if {params.i.j.k.l}>
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_very_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {app: {ui: {theme: {dark: Bool}}, api: {endpoints: {users: {enabled: Bool}}}, database: {connection: {ssl: Bool}}}}}>
                	<if {params.app.ui.theme.dark}>
                	</if>
                	<if {params.app.api.endpoints.users.enabled}>
                	</if>
                	<if {params.app.database.connection.ssl}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: Record[
                  app: Record[
                    api: Record[
                      endpoints: Record[users: Record[enabled: Bool]],
                    ],
                    database: Record[connection: Record[ssl: Bool]],
                    ui: Record[theme: Record[dark: Bool]],
                  ],
                ]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {app: {ui: {theme: {dark: Bool}}, api: {endpoints: {users: {enabled: Bool}}}, database: {connection: {ssl: Bool}}}}}>
                  |        ^^^^^^

                params: Record[
                  app: Record[
                    api: Record[
                      endpoints: Record[users: Record[enabled: Bool]],
                    ],
                    database: Record[connection: Record[ssl: Bool]],
                    ui: Record[theme: Record[dark: Bool]],
                  ],
                ]
                  --> main.hop (line 2, col 7)
                1 | <Main {params: {app: {ui: {theme: {dark: Bool}}, api: {endpoints: {users: {enabled: Bool}}}, database: {connection: {ssl: Bool}}}}}>
                2 |     <if {params.app.ui.theme.dark}>
                  |          ^^^^^^

                params: Record[
                  app: Record[
                    api: Record[
                      endpoints: Record[users: Record[enabled: Bool]],
                    ],
                    database: Record[connection: Record[ssl: Bool]],
                    ui: Record[theme: Record[dark: Bool]],
                  ],
                ]
                  --> main.hop (line 4, col 7)
                3 |     </if>
                4 |     <if {params.app.api.endpoints.users.enabled}>
                  |          ^^^^^^

                params: Record[
                  app: Record[
                    api: Record[
                      endpoints: Record[users: Record[enabled: Bool]],
                    ],
                    database: Record[connection: Record[ssl: Bool]],
                    ui: Record[theme: Record[dark: Bool]],
                  ],
                ]
                  --> main.hop (line 6, col 7)
                5 |     </if>
                6 |     <if {params.app.database.connection.ssl}>
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_with_script_and_style() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <script>
                        console.log("test");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </Main>
            "#},
            expect![""],
        );
    }

    #[test]
    fn test_component_with_data_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {data: {message: String}}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </Main>
            "#},
            expect![[r#"
                data: Record[message: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {data: {message: String}}>
                  |        ^^^^

                data: Record[message: String]
                  --> main.hop (line 3, col 9)
                2 |     <h1>Hello World</h1>
                3 |     <p>{data.message}</p>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_string_comparisons() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                  <if {params.user.name == params.other_user.name}>
                    <div>Same name</div>
                  </if>
                  <if {(params.data.x == params.data.y)}>
                    <div>Parentheses work</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                  |        ^^^^^^

                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 2, col 8)
                1 | <Main {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                2 |   <if {params.user.name == params.other_user.name}>
                  |        ^^^^^^

                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 2, col 28)
                1 | <Main {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                2 |   <if {params.user.name == params.other_user.name}>
                  |                            ^^^^^^

                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 5, col 9)
                4 |   </if>
                5 |   <if {(params.data.x == params.data.y)}>
                  |         ^^^^^^

                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 5, col 26)
                4 |   </if>
                5 |   <if {(params.data.x == params.data.y)}>
                  |                          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_simple_string_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {x: String, y: String}}>
                  <if {params.x == params.y}>
                    <div>Values are equal</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: Record[x: String, y: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {x: String, y: String}}>
                  |        ^^^^^^

                params: Record[x: String, y: String]
                  --> main.hop (line 2, col 8)
                1 | <Main {params: {x: String, y: String}}>
                2 |   <if {params.x == params.y}>
                  |        ^^^^^^

                params: Record[x: String, y: String]
                  --> main.hop (line 2, col 20)
                1 | <Main {params: {x: String, y: String}}>
                2 |   <if {params.x == params.y}>
                  |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_nested_array_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {foo: {bar: Array[Bool]}}}>
                	<for {j in params.foo.bar}>
                		<if {j}>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: Record[foo: Record[bar: Array[Bool]]]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {foo: {bar: Array[Bool]}}}>
                  |        ^^^^^^

                params: Record[foo: Record[bar: Array[Bool]]]
                  --> main.hop (line 2, col 13)
                1 | <Main {params: {foo: {bar: Array[Bool]}}}>
                2 |     <for {j in params.foo.bar}>
                  |                ^^^^^^

                j: Bool
                  --> main.hop (line 2, col 8)
                1 | <Main {params: {foo: {bar: Array[Bool]}}}>
                2 |     <for {j in params.foo.bar}>
                  |           ^

                j: Bool
                  --> main.hop (line 3, col 8)
                2 |     <for {j in params.foo.bar}>
                3 |         <if {j}>
                  |              ^
            "#]],
        );
    }

    #[test]
    fn test_multiple_loops_same_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: Array[{a: Bool, b: Bool}]}>
                	<for {j in params}>
                		<if {j.a}>
                		</if>
                	</for>
                	<for {j in params}>
                		<if {j.b}>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: Array[Record[a: Bool, b: Bool]]
                  --> main.hop (line 1, col 8)
                 1 | <Main {params: Array[{a: Bool, b: Bool}]}>
                   |        ^^^^^^

                params: Array[Record[a: Bool, b: Bool]]
                  --> main.hop (line 2, col 13)
                 1 | <Main {params: Array[{a: Bool, b: Bool}]}>
                 2 |     <for {j in params}>
                   |                ^^^^^^

                j: Record[a: Bool, b: Bool]
                  --> main.hop (line 2, col 8)
                 1 | <Main {params: Array[{a: Bool, b: Bool}]}>
                 2 |     <for {j in params}>
                   |           ^

                j: Record[a: Bool, b: Bool]
                  --> main.hop (line 3, col 8)
                 2 |     <for {j in params}>
                 3 |         <if {j.a}>
                   |              ^

                params: Array[Record[a: Bool, b: Bool]]
                  --> main.hop (line 6, col 13)
                 5 |     </for>
                 6 |     <for {j in params}>
                   |                ^^^^^^

                j: Record[a: Bool, b: Bool]
                  --> main.hop (line 6, col 8)
                 5 |     </for>
                 6 |     <for {j in params}>
                   |           ^

                j: Record[a: Bool, b: Bool]
                  --> main.hop (line 7, col 8)
                 6 |     <for {j in params}>
                 7 |         <if {j.b}>
                   |              ^
            "#]],
        );
    }

    #[test]
    fn test_nested_array_iteration() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {i: Array[Array[Bool]]}>
                	<for {j in i}>
                		<for {k in j}>
                			<if {k}>
                			</if>
                		</for>
                	</for>
                </Main>
            "#},
            expect![[r#"
                i: Array[Array[Bool]]
                  --> main.hop (line 1, col 8)
                1 | <Main {i: Array[Array[Bool]]}>
                  |        ^

                i: Array[Array[Bool]]
                  --> main.hop (line 2, col 13)
                1 | <Main {i: Array[Array[Bool]]}>
                2 |     <for {j in i}>
                  |                ^

                j: Array[Bool]
                  --> main.hop (line 2, col 8)
                1 | <Main {i: Array[Array[Bool]]}>
                2 |     <for {j in i}>
                  |           ^

                j: Array[Bool]
                  --> main.hop (line 3, col 14)
                2 |     <for {j in i}>
                3 |         <for {k in j}>
                  |                    ^

                k: Bool
                  --> main.hop (line 3, col 9)
                2 |     <for {j in i}>
                3 |         <for {k in j}>
                  |               ^

                k: Bool
                  --> main.hop (line 4, col 9)
                3 |         <for {k in j}>
                4 |             <if {k}>
                  |                  ^
            "#]],
        );
    }

    #[test]
    fn test_simple_array_iteration() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {i: Array[Bool]}>
                	<for {j in i}>
                		<if {j}>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                i: Array[Bool]
                  --> main.hop (line 1, col 8)
                1 | <Main {i: Array[Bool]}>
                  |        ^

                i: Array[Bool]
                  --> main.hop (line 2, col 13)
                1 | <Main {i: Array[Bool]}>
                2 |     <for {j in i}>
                  |                ^

                j: Bool
                  --> main.hop (line 2, col 8)
                1 | <Main {i: Array[Bool]}>
                2 |     <for {j in i}>
                  |           ^

                j: Bool
                  --> main.hop (line 3, col 8)
                2 |     <for {j in i}>
                3 |         <if {j}>
                  |              ^
            "#]],
        );
    }

    #[test]
    fn test_component_import_with_parameters() {
        check(
            indoc! {r#"
                -- utils.hop --
                <ButtonComp {text: String}>
                  <div>{text}</div>
                </ButtonComp>

                -- main.hop --
                import ButtonComp from "@/utils"

                <Main {label: String}>
                  <ButtonComp {text: label}/>
                </Main>
            "#},
            expect![[r#"
                text: String
                  --> utils.hop (line 1, col 14)
                1 | <ButtonComp {text: String}>
                  |              ^^^^

                text: String
                  --> utils.hop (line 2, col 9)
                1 | <ButtonComp {text: String}>
                2 |   <div>{text}</div>
                  |         ^^^^

                label: String
                  --> main.hop (line 3, col 8)
                2 | 
                3 | <Main {label: String}>
                  |        ^^^^^

                label: String
                  --> main.hop (line 4, col 22)
                3 | <Main {label: String}>
                4 |   <ButtonComp {text: label}/>
                  |                      ^^^^^

                text: String
                  --> main.hop (line 4, col 22)
                3 | <Main {label: String}>
                4 |   <ButtonComp {text: label}/>
                  |                      ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_multi_module_component_chain() {
        check(
            indoc! {r#"
                -- bar.hop --
                <WidgetComp {config: {enabled: Bool, title: String}}>
                  <if {config.enabled}>
                    <div>{config.title}</div>
                  </if>
                </WidgetComp>

                -- foo.hop --
                import WidgetComp from "@/bar"

                <PanelComp {data: {items: Array[{enabled: Bool, title: String}]}}>
                  <for {item in data.items}>
                    <WidgetComp {config: item}/>
                  </for>
                </PanelComp>

                -- main.hop --
                import PanelComp from "@/foo"

                <Main {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                  <PanelComp {data: settings.dashboard}/>
                </Main>
            "#},
            expect![[r#"
                config: Record[enabled: Bool, title: String]
                  --> bar.hop (line 1, col 14)
                1 | <WidgetComp {config: {enabled: Bool, title: String}}>
                  |              ^^^^^^

                config: Record[enabled: Bool, title: String]
                  --> bar.hop (line 3, col 11)
                2 |   <if {config.enabled}>
                3 |     <div>{config.title}</div>
                  |           ^^^^^^

                config: Record[enabled: Bool, title: String]
                  --> bar.hop (line 2, col 8)
                1 | <WidgetComp {config: {enabled: Bool, title: String}}>
                2 |   <if {config.enabled}>
                  |        ^^^^^^

                data: Record[items: Array[Record[enabled: Bool, title: String]]]
                  --> foo.hop (line 3, col 13)
                2 | 
                3 | <PanelComp {data: {items: Array[{enabled: Bool, title: String}]}}>
                  |             ^^^^

                data: Record[items: Array[Record[enabled: Bool, title: String]]]
                  --> foo.hop (line 4, col 17)
                3 | <PanelComp {data: {items: Array[{enabled: Bool, title: String}]}}>
                4 |   <for {item in data.items}>
                  |                 ^^^^

                item: Record[enabled: Bool, title: String]
                  --> foo.hop (line 4, col 9)
                3 | <PanelComp {data: {items: Array[{enabled: Bool, title: String}]}}>
                4 |   <for {item in data.items}>
                  |         ^^^^

                item: Record[enabled: Bool, title: String]
                  --> foo.hop (line 5, col 26)
                4 |   <for {item in data.items}>
                5 |     <WidgetComp {config: item}/>
                  |                          ^^^^

                config: Record[enabled: Bool, title: String]
                  --> foo.hop (line 5, col 26)
                4 |   <for {item in data.items}>
                5 |     <WidgetComp {config: item}/>
                  |                          ^^^^

                settings: Record[
                  dashboard: Record[
                    items: Array[Record[enabled: Bool, title: String]],
                  ],
                ]
                  --> main.hop (line 3, col 8)
                2 | 
                3 | <Main {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                  |        ^^^^^^^^

                settings: Record[
                  dashboard: Record[
                    items: Array[Record[enabled: Bool, title: String]],
                  ],
                ]
                  --> main.hop (line 4, col 21)
                3 | <Main {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                4 |   <PanelComp {data: settings.dashboard}/>
                  |                     ^^^^^^^^

                data: Record[items: Array[Record[enabled: Bool, title: String]]]
                  --> main.hop (line 4, col 21)
                3 | <Main {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                4 |   <PanelComp {data: settings.dashboard}/>
                  |                     ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_simple_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: String}>
                	<div>{params}</div>
                </Main>
            "#},
            expect![[r#"
                params: String
                  --> main.hop (line 1, col 8)
                1 | <Main {params: String}>
                  |        ^^^^^^

                params: String
                  --> main.hop (line 2, col 8)
                1 | <Main {params: String}>
                2 |     <div>{params}</div>
                  |           ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_data_structure() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {config: {debug: Bool}, data: Array[{id: Bool, attributes: Array[Bool]}]}}>
                	<if {params.config.debug}>
                	</if>
                	<for {item in params.data}>
                		<if {item.id}>
                		</if>
                		<for {attr in item.attributes}>
                			<if {attr}>
                			</if>
                		</for>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: Record[
                  config: Record[debug: Bool],
                  data: Array[Record[attributes: Array[Bool], id: Bool]],
                ]
                  --> main.hop (line 1, col 8)
                 1 | <Main {params: {config: {debug: Bool}, data: Array[{id: Bool, attributes: Array[Bool]}]}}>
                   |        ^^^^^^

                params: Record[
                  config: Record[debug: Bool],
                  data: Array[Record[attributes: Array[Bool], id: Bool]],
                ]
                  --> main.hop (line 2, col 7)
                 1 | <Main {params: {config: {debug: Bool}, data: Array[{id: Bool, attributes: Array[Bool]}]}}>
                 2 |     <if {params.config.debug}>
                   |          ^^^^^^

                params: Record[
                  config: Record[debug: Bool],
                  data: Array[Record[attributes: Array[Bool], id: Bool]],
                ]
                  --> main.hop (line 4, col 16)
                 3 |     </if>
                 4 |     <for {item in params.data}>
                   |                   ^^^^^^

                item: Record[attributes: Array[Bool], id: Bool]
                  --> main.hop (line 4, col 8)
                 3 |     </if>
                 4 |     <for {item in params.data}>
                   |           ^^^^

                item: Record[attributes: Array[Bool], id: Bool]
                  --> main.hop (line 5, col 8)
                 4 |     <for {item in params.data}>
                 5 |         <if {item.id}>
                   |              ^^^^

                item: Record[attributes: Array[Bool], id: Bool]
                  --> main.hop (line 7, col 17)
                 6 |         </if>
                 7 |         <for {attr in item.attributes}>
                   |                       ^^^^

                attr: Bool
                  --> main.hop (line 7, col 9)
                 6 |         </if>
                 7 |         <for {attr in item.attributes}>
                   |               ^^^^

                attr: Bool
                  --> main.hop (line 8, col 9)
                 7 |         <for {attr in item.attributes}>
                 8 |             <if {attr}>
                   |                  ^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_chain_with_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <Step3Comp {settings: {enabled: Bool}}>
                	<if {settings.enabled}>
                	</if>
                </Step3Comp>

                <Step2Comp {config: {settings: {enabled: Bool}}}>
                	<Step3Comp {settings: config.settings}/>
                </Step2Comp>

                <Step1Comp {data: {config: {settings: {enabled: Bool}}}}>
                	<Step2Comp {config: data.config}/>
                </Step1Comp>

                <Main {params: {config: {settings: {enabled: Bool}}}}>
                	<Step1Comp {data: params}/>
                </Main>
            "#},
            expect![[r#"
                settings: Record[enabled: Bool]
                  --> main.hop (line 1, col 13)
                 1 | <Step3Comp {settings: {enabled: Bool}}>
                   |             ^^^^^^^^

                settings: Record[enabled: Bool]
                  --> main.hop (line 2, col 7)
                 1 | <Step3Comp {settings: {enabled: Bool}}>
                 2 |     <if {settings.enabled}>
                   |          ^^^^^^^^

                config: Record[settings: Record[enabled: Bool]]
                  --> main.hop (line 6, col 13)
                 5 | 
                 6 | <Step2Comp {config: {settings: {enabled: Bool}}}>
                   |             ^^^^^^

                config: Record[settings: Record[enabled: Bool]]
                  --> main.hop (line 7, col 24)
                 6 | <Step2Comp {config: {settings: {enabled: Bool}}}>
                 7 |     <Step3Comp {settings: config.settings}/>
                   |                           ^^^^^^

                settings: Record[enabled: Bool]
                  --> main.hop (line 7, col 24)
                 6 | <Step2Comp {config: {settings: {enabled: Bool}}}>
                 7 |     <Step3Comp {settings: config.settings}/>
                   |                           ^^^^^^^^^^^^^^^

                data: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 10, col 13)
                 9 | 
                10 | <Step1Comp {data: {config: {settings: {enabled: Bool}}}}>
                   |             ^^^^

                data: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 11, col 22)
                10 | <Step1Comp {data: {config: {settings: {enabled: Bool}}}}>
                11 |     <Step2Comp {config: data.config}/>
                   |                         ^^^^

                config: Record[settings: Record[enabled: Bool]]
                  --> main.hop (line 11, col 22)
                10 | <Step1Comp {data: {config: {settings: {enabled: Bool}}}}>
                11 |     <Step2Comp {config: data.config}/>
                   |                         ^^^^^^^^^^^

                params: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 14, col 8)
                13 | 
                14 | <Main {params: {config: {settings: {enabled: Bool}}}}>
                   |        ^^^^^^

                params: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 15, col 20)
                14 | <Main {params: {config: {settings: {enabled: Bool}}}}>
                15 |     <Step1Comp {data: params}/>
                   |                       ^^^^^^

                data: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 15, col 20)
                14 | <Main {params: {config: {settings: {enabled: Bool}}}}>
                15 |     <Step1Comp {data: params}/>
                   |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_three_level_component_hierarchy() {
        check(
            indoc! {r#"
                -- main.hop --
                <MainCard {item: {title: String, active: Bool, status: String}}>
                  <div>{item.title}
                  </div>
                  <if {item.active}>
                    <span>{item.status}
                    </span>
                  </if>
                </MainCard>

                <MainList {items: Array[{title: String, active: Bool, status: String}]}>
                  <for {item in items}>
                    <MainCard {item: item}/>
                  </for>
                </MainList>

                <Main {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                  <MainList {items: data.items}/>
                </Main>
            "#},
            expect![[r#"
                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 1, col 12)
                 1 | <MainCard {item: {title: String, active: Bool, status: String}}>
                   |            ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 2, col 9)
                 1 | <MainCard {item: {title: String, active: Bool, status: String}}>
                 2 |   <div>{item.title}
                   |         ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 5, col 12)
                 4 |   <if {item.active}>
                 5 |     <span>{item.status}
                   |            ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 4, col 8)
                 3 |   </div>
                 4 |   <if {item.active}>
                   |        ^^^^

                items: Array[Record[active: Bool, status: String, title: String]]
                  --> main.hop (line 10, col 12)
                 9 | 
                10 | <MainList {items: Array[{title: String, active: Bool, status: String}]}>
                   |            ^^^^^

                items: Array[Record[active: Bool, status: String, title: String]]
                  --> main.hop (line 11, col 17)
                10 | <MainList {items: Array[{title: String, active: Bool, status: String}]}>
                11 |   <for {item in items}>
                   |                 ^^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 11, col 9)
                10 | <MainList {items: Array[{title: String, active: Bool, status: String}]}>
                11 |   <for {item in items}>
                   |         ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 12, col 22)
                11 |   <for {item in items}>
                12 |     <MainCard {item: item}/>
                   |                      ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 12, col 22)
                11 |   <for {item in items}>
                12 |     <MainCard {item: item}/>
                   |                      ^^^^

                data: Record[
                  items: Array[Record[
                    active: Bool,
                    status: String,
                    title: String,
                  ]],
                ]
                  --> main.hop (line 16, col 8)
                15 | 
                16 | <Main {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                   |        ^^^^

                data: Record[
                  items: Array[Record[
                    active: Bool,
                    status: String,
                    title: String,
                  ]],
                ]
                  --> main.hop (line 17, col 21)
                16 | <Main {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                17 |   <MainList {items: data.items}/>
                   |                     ^^^^

                items: Array[Record[active: Bool, status: String, title: String]]
                  --> main.hop (line 17, col 21)
                16 | <Main {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                17 |   <MainList {items: data.items}/>
                   |                     ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_object_property_name_collision() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                	<if {params.i.j.k.l}>
                		<if {params.i.k}>
                		</if>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                  |        ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 3, col 8)
                2 |     <if {params.i.j.k.l}>
                3 |         <if {params.i.k}>
                  |              ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 2, col 7)
                1 | <Main {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                2 |     <if {params.i.j.k.l}>
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_object_property_separate_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                	<if {params.i.j.k.l}>
                	</if>
                	<if {params.i.k}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                  |        ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 2, col 7)
                1 | <Main {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                2 |     <if {params.i.j.k.l}>
                  |          ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 4, col 7)
                3 |     </if>
                4 |     <if {params.i.k}>
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_multiple_array_properties() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
                	<for {tag in params.tags}>
                		<if {tag}>
                		</if>
                	</for>
                	<for {category in params.categories}>
                		<if {category}>
                		</if>
                	</for>
                	<if {params.metadata.title}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: Record[
                  categories: Array[Bool],
                  metadata: Record[title: Bool],
                  tags: Array[Bool],
                ]
                  --> main.hop (line 1, col 8)
                 1 | <Main {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
                   |        ^^^^^^

                params: Record[
                  categories: Array[Bool],
                  metadata: Record[title: Bool],
                  tags: Array[Bool],
                ]
                  --> main.hop (line 2, col 15)
                 1 | <Main {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
                 2 |     <for {tag in params.tags}>
                   |                  ^^^^^^

                tag: Bool
                  --> main.hop (line 2, col 8)
                 1 | <Main {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
                 2 |     <for {tag in params.tags}>
                   |           ^^^

                tag: Bool
                  --> main.hop (line 3, col 8)
                 2 |     <for {tag in params.tags}>
                 3 |         <if {tag}>
                   |              ^^^

                params: Record[
                  categories: Array[Bool],
                  metadata: Record[title: Bool],
                  tags: Array[Bool],
                ]
                  --> main.hop (line 6, col 20)
                 5 |     </for>
                 6 |     <for {category in params.categories}>
                   |                       ^^^^^^

                category: Bool
                  --> main.hop (line 6, col 8)
                 5 |     </for>
                 6 |     <for {category in params.categories}>
                   |           ^^^^^^^^

                category: Bool
                  --> main.hop (line 7, col 8)
                 6 |     <for {category in params.categories}>
                 7 |         <if {category}>
                   |              ^^^^^^^^

                params: Record[
                  categories: Array[Bool],
                  metadata: Record[title: Bool],
                  tags: Array[Bool],
                ]
                  --> main.hop (line 10, col 7)
                 9 |     </for>
                10 |     <if {params.metadata.title}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_simple_component_chain() {
        check(
            indoc! {r#"
                -- main.hop --
                <MainBar {p: Bool}>
                  <if {p}>
                  </if>
                </MainBar>

                <MainFoo {p: Bool}>
                  <MainBar {p: p}/>
                </MainFoo>

                <Main {i: Bool}>
                  <MainFoo {p: i}/>
                </Main>
            "#},
            expect![[r#"
                p: Bool
                  --> main.hop (line 1, col 11)
                 1 | <MainBar {p: Bool}>
                   |           ^

                p: Bool
                  --> main.hop (line 2, col 8)
                 1 | <MainBar {p: Bool}>
                 2 |   <if {p}>
                   |        ^

                p: Bool
                  --> main.hop (line 6, col 11)
                 5 | 
                 6 | <MainFoo {p: Bool}>
                   |           ^

                p: Bool
                  --> main.hop (line 7, col 16)
                 6 | <MainFoo {p: Bool}>
                 7 |   <MainBar {p: p}/>
                   |                ^

                p: Bool
                  --> main.hop (line 7, col 16)
                 6 | <MainFoo {p: Bool}>
                 7 |   <MainBar {p: p}/>
                   |                ^

                i: Bool
                  --> main.hop (line 10, col 8)
                 9 | 
                10 | <Main {i: Bool}>
                   |        ^

                i: Bool
                  --> main.hop (line 11, col 16)
                10 | <Main {i: Bool}>
                11 |   <MainFoo {p: i}/>
                   |                ^

                p: Bool
                  --> main.hop (line 11, col 16)
                10 | <Main {i: Bool}>
                11 |   <MainFoo {p: i}/>
                   |                ^
            "#]],
        );
    }

    #[test]
    fn test_workflow_execution_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                <ExecuteStep {step: {condition: Bool}}>
                	<if {step.condition}>
                	</if>
                </ExecuteStep>

                <ExecuteWorkflow {workflow: {enabled: Bool, steps: Array[{condition: Bool}]}}>
                	<if {workflow.enabled}>
                		<for {step in workflow.steps}>
                			<ExecuteStep {step: step}/>
                		</for>
                	</if>
                </ExecuteWorkflow>

                <Main {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                	<for {workflow in params.workflows}>
                		<ExecuteWorkflow {workflow: workflow}/>
                	</for>
                </Main>
            "#},
            expect![[r#"
                step: Record[condition: Bool]
                  --> main.hop (line 1, col 15)
                 1 | <ExecuteStep {step: {condition: Bool}}>
                   |               ^^^^

                step: Record[condition: Bool]
                  --> main.hop (line 2, col 7)
                 1 | <ExecuteStep {step: {condition: Bool}}>
                 2 |     <if {step.condition}>
                   |          ^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 6, col 19)
                 5 | 
                 6 | <ExecuteWorkflow {workflow: {enabled: Bool, steps: Array[{condition: Bool}]}}>
                   |                   ^^^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 8, col 17)
                 7 |     <if {workflow.enabled}>
                 8 |         <for {step in workflow.steps}>
                   |                       ^^^^^^^^

                step: Record[condition: Bool]
                  --> main.hop (line 8, col 9)
                 7 |     <if {workflow.enabled}>
                 8 |         <for {step in workflow.steps}>
                   |               ^^^^

                step: Record[condition: Bool]
                  --> main.hop (line 9, col 24)
                 8 |         <for {step in workflow.steps}>
                 9 |             <ExecuteStep {step: step}/>
                   |                                 ^^^^

                step: Record[condition: Bool]
                  --> main.hop (line 9, col 24)
                 8 |         <for {step in workflow.steps}>
                 9 |             <ExecuteStep {step: step}/>
                   |                                 ^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 7, col 7)
                 6 | <ExecuteWorkflow {workflow: {enabled: Bool, steps: Array[{condition: Bool}]}}>
                 7 |     <if {workflow.enabled}>
                   |          ^^^^^^^^

                params: Record[
                  workflows: Array[Record[
                    enabled: Bool,
                    steps: Array[Record[condition: Bool]],
                  ]],
                ]
                  --> main.hop (line 14, col 8)
                13 | 
                14 | <Main {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                   |        ^^^^^^

                params: Record[
                  workflows: Array[Record[
                    enabled: Bool,
                    steps: Array[Record[condition: Bool]],
                  ]],
                ]
                  --> main.hop (line 15, col 20)
                14 | <Main {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                15 |     <for {workflow in params.workflows}>
                   |                       ^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 15, col 8)
                14 | <Main {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                15 |     <for {workflow in params.workflows}>
                   |           ^^^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 16, col 31)
                15 |     <for {workflow in params.workflows}>
                16 |         <ExecuteWorkflow {workflow: workflow}/>
                   |                                     ^^^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 16, col 31)
                15 |     <for {workflow in params.workflows}>
                16 |         <ExecuteWorkflow {workflow: workflow}/>
                   |                                     ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_two_component_chain() {
        check(
            indoc! {r#"
                -- main.hop --
                <Foo {p: Bool}>
                  <if {p}>
                  </if>
                </Foo>

                <Main {i: Bool}>
                  <Foo {p: i}/>
                </Main>
            "#},
            expect![[r#"
                p: Bool
                  --> main.hop (line 1, col 7)
                1 | <Foo {p: Bool}>
                  |       ^

                p: Bool
                  --> main.hop (line 2, col 8)
                1 | <Foo {p: Bool}>
                2 |   <if {p}>
                  |        ^

                i: Bool
                  --> main.hop (line 6, col 8)
                5 | 
                6 | <Main {i: Bool}>
                  |        ^

                i: Bool
                  --> main.hop (line 7, col 12)
                6 | <Main {i: Bool}>
                7 |   <Foo {p: i}/>
                  |            ^

                p: Bool
                  --> main.hop (line 7, col 12)
                6 | <Main {i: Bool}>
                7 |   <Foo {p: i}/>
                  |            ^
            "#]],
        );
    }

    #[test]
    fn test_process_item_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                <ProcessItem {
                    item: {
                        children: Array[{visible: Bool}],
                        status: {active: Bool}
                    }
                }>
                	<if {item.status.active}>
                	</if>
                	<for {child in item.children}>
                		<if {child.visible}>
                		</if>
                	</for>
                </ProcessItem>

                <Main {params: {
                    items: Array[{children: Array[{visible: Bool}],
                    status: {active: Bool}}],
                }}>
                	<for {item in params.items}>
                		<ProcessItem {item: item}/>
                	</for>
                </Main>
            "#},
            expect![[r#"
                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 2, col 5)
                 1 | <ProcessItem {
                 2 |     item: {
                   |     ^^^^

                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 7, col 7)
                 6 | }>
                 7 |     <if {item.status.active}>
                   |          ^^^^

                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 9, col 17)
                 8 |     </if>
                 9 |     <for {child in item.children}>
                   |                    ^^^^

                child: Record[visible: Bool]
                  --> main.hop (line 9, col 8)
                 8 |     </if>
                 9 |     <for {child in item.children}>
                   |           ^^^^^

                child: Record[visible: Bool]
                  --> main.hop (line 10, col 8)
                 9 |     <for {child in item.children}>
                10 |         <if {child.visible}>
                   |              ^^^^^

                params: Record[
                  items: Array[Record[
                    children: Array[Record[visible: Bool]],
                    status: Record[active: Bool],
                  ]],
                ]
                  --> main.hop (line 15, col 8)
                14 | 
                15 | <Main {params: {
                   |        ^^^^^^

                params: Record[
                  items: Array[Record[
                    children: Array[Record[visible: Bool]],
                    status: Record[active: Bool],
                  ]],
                ]
                  --> main.hop (line 19, col 16)
                18 | }}>
                19 |     <for {item in params.items}>
                   |                   ^^^^^^

                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 19, col 8)
                18 | }}>
                19 |     <for {item in params.items}>
                   |           ^^^^

                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 20, col 23)
                19 |     <for {item in params.items}>
                20 |         <ProcessItem {item: item}/>
                   |                             ^^^^

                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 20, col 23)
                19 |     <for {item in params.items}>
                20 |         <ProcessItem {item: item}/>
                   |                             ^^^^
            "#]],
        );
    }

    #[test]
    fn test_expr_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {user: {url: String, theme: String}}>
                  <a href={user.url} class={user.theme}>Link</a>
                </Main>
            "#},
            expect![[r#"
                user: Record[theme: String, url: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {user: {url: String, theme: String}}>
                  |        ^^^^

                user: Record[theme: String, url: String]
                  --> main.hop (line 2, col 29)
                1 | <Main {user: {url: String, theme: String}}>
                2 |   <a href={user.url} class={user.theme}>Link</a>
                  |                             ^^^^

                user: Record[theme: String, url: String]
                  --> main.hop (line 2, col 12)
                1 | <Main {user: {url: String, theme: String}}>
                2 |   <a href={user.url} class={user.theme}>Link</a>
                  |            ^^^^
            "#]],
        );
    }

    #[test]
    fn test_slot_usage() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <strong>
                        <slot-default/>
                    </strong>
                </Main>

                <Bar>
                    <Main>
                        Here's the content for the default slot
                    </Main>
                </Bar>
            "#},
            expect![""],
        );
    }

    #[test]
    fn test_simple_text_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {data: {message: String}}>
                  <div>{data.message}
                  </div>
                </Main>
            "#},
            expect![[r#"
                data: Record[message: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {data: {message: String}}>
                  |        ^^^^

                data: Record[message: String]
                  --> main.hop (line 2, col 9)
                1 | <Main {data: {message: String}}>
                2 |   <div>{data.message}
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn test_string_comparison_conditional() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {role: String}}>
                  <if {params.role == "admin"}>
                    <div>Admin</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: Record[role: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {role: String}}>
                  |        ^^^^^^

                params: Record[role: String]
                  --> main.hop (line 2, col 8)
                1 | <Main {params: {role: String}}>
                2 |   <if {params.role == "admin"}>
                  |        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_triple_nested_array() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: Array[Array[Array[Bool]]]}>
                	<for {level1 in params}>
                		<for {level2 in level1}>
                			<for {level3 in level2}>
                				<if {level3}>
                				</if>
                			</for>
                		</for>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: Array[Array[Array[Bool]]]
                  --> main.hop (line 1, col 8)
                 1 | <Main {params: Array[Array[Array[Bool]]]}>
                   |        ^^^^^^

                params: Array[Array[Array[Bool]]]
                  --> main.hop (line 2, col 18)
                 1 | <Main {params: Array[Array[Array[Bool]]]}>
                 2 |     <for {level1 in params}>
                   |                     ^^^^^^

                level1: Array[Array[Bool]]
                  --> main.hop (line 2, col 8)
                 1 | <Main {params: Array[Array[Array[Bool]]]}>
                 2 |     <for {level1 in params}>
                   |           ^^^^^^

                level1: Array[Array[Bool]]
                  --> main.hop (line 3, col 19)
                 2 |     <for {level1 in params}>
                 3 |         <for {level2 in level1}>
                   |                         ^^^^^^

                level2: Array[Bool]
                  --> main.hop (line 3, col 9)
                 2 |     <for {level1 in params}>
                 3 |         <for {level2 in level1}>
                   |               ^^^^^^

                level2: Array[Bool]
                  --> main.hop (line 4, col 20)
                 3 |         <for {level2 in level1}>
                 4 |             <for {level3 in level2}>
                   |                             ^^^^^^

                level3: Bool
                  --> main.hop (line 4, col 10)
                 3 |         <for {level2 in level1}>
                 4 |             <for {level3 in level2}>
                   |                   ^^^^^^

                level3: Bool
                  --> main.hop (line 5, col 10)
                 4 |             <for {level3 in level2}>
                 5 |                 <if {level3}>
                   |                      ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_array_used_as_object_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: Array[String]}>
                	<for {x in params}>
                		{x}
                	</for>
                	<for {y in params.foo}>
                		{y}
                	</for>
                </Main>
            "#},
            expect![[r#"
                error: Array[String] can not be used as an object
                  --> main.hop (line 5, col 13)
                4 |     </for>
                5 |     <for {y in params.foo}>
                  |                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_hop_x_raw_simple() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {data: {message: String}}>
                	<hop-x-raw>foo bar</hop-x-raw>
                	<div>{data.message}</div>
                </Main>
            "#},
            expect![[r#"
                data: Record[message: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {data: {message: String}}>
                  |        ^^^^

                data: Record[message: String]
                  --> main.hop (line 3, col 8)
                2 |     <hop-x-raw>foo bar</hop-x-raw>
                3 |     <div>{data.message}</div>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn test_hop_x_raw_with_html() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                	<hop-x-raw>
                		<div>some html</div>
                		<p>more content</p>
                	</hop-x-raw>
                </Main>
            "#},
            expect![""],
        );
    }

    // Content inside <hop-x-raw> tags are not parsed nor typechecked.
    #[test]
    fn test_hop_x_raw_not_typechecked() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                	<hop-x-raw>
                		<UndefinedComponent {nonexistent.field}>
                			<AnotherUndefined {also.nonexistent} />
                		</UndefinedComponent>
                	</hop-x-raw>
                </Main>
            "#},
            expect![""],
        );
    }

    // Test <if> tag with Bool expression.
    #[test]
    fn test_if_with_boolean_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {user: {is_active: Bool}}>
                  <if {user.is_active}>
                    <div>User is active</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                user: Record[is_active: Bool]
                  --> main.hop (line 1, col 8)
                1 | <Main {user: {is_active: Bool}}>
                  |        ^^^^

                user: Record[is_active: Bool]
                  --> main.hop (line 2, col 8)
                1 | <Main {user: {is_active: Bool}}>
                2 |   <if {user.is_active}>
                  |        ^^^^
            "#]],
        );
    }

    // Test <if> tag with comparison expression.
    #[test]
    fn test_if_with_comparison_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {data: {status: String}}>
                  <if {data.status == "approved"}>
                    <div>Status is approved</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                data: Record[status: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {data: {status: String}}>
                  |        ^^^^

                data: Record[status: String]
                  --> main.hop (line 2, col 8)
                1 | <Main {data: {status: String}}>
                2 |   <if {data.status == "approved"}>
                  |        ^^^^
            "#]],
        );
    }

    // Comparison only works when the types of each operand are equal.
    #[test]
    fn test_comparison_type_mismatch_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <if {1 == "approved"}>
                    <div>Status is approved</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                error: Can not compare Int to String
                  --> main.hop (line 2, col 8)
                1 | <Main>
                2 |   <if {1 == "approved"}>
                  |        ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Test <if> tag with nested conditionals.
    #[test]
    fn test_if_with_nested_conditionals() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {config: {enabled: Bool, debug: Bool}}>
                  <if {config.enabled}>
                    <div>Feature enabled</div>
                    <if {config.debug}>
                      <div>Debug mode on</div>
                    </if>
                  </if>
                </Main>
            "#},
            expect![[r#"
                config: Record[debug: Bool, enabled: Bool]
                  --> main.hop (line 1, col 8)
                1 | <Main {config: {enabled: Bool, debug: Bool}}>
                  |        ^^^^^^

                config: Record[debug: Bool, enabled: Bool]
                  --> main.hop (line 4, col 10)
                3 |     <div>Feature enabled</div>
                4 |     <if {config.debug}>
                  |          ^^^^^^

                config: Record[debug: Bool, enabled: Bool]
                  --> main.hop (line 2, col 8)
                1 | <Main {config: {enabled: Bool, debug: Bool}}>
                2 |   <if {config.enabled}>
                  |        ^^^^^^
            "#]],
        );
    }

    // When an argument type does not match a parameter type the typechecker
    // reports an error.
    #[test]
    fn test_component_argument_type_mismatch() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {config: {debug: Bool}}>
                  <if {config.debug}>
                    <div>Debug mode on</div>
                  </if>
                </Main>
                <Foo>
                  <Main {config: 1}/>
                </Foo>
            "#},
            expect![[r#"
                error: Argument 'config' of type Int is incompatible with expected type Record[debug: Bool]
                  --> main.hop (line 7, col 18)
                6 | <Foo>
                7 |   <Main {config: 1}/>
                  |                  ^
            "#]],
        );
    }

    // Test hop_mode global variable is available and has String type.
    #[test]
    fn test_hop_mode_global_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <div>{hop_mode}</div>
                </Main>
            "#},
            expect![[r#"
                hop_mode: String
                  --> main.hop (line 2, col 9)
                1 | <Main>
                2 |   <div>{hop_mode}</div>
                  |         ^^^^^^^^
            "#]],
        );
    }

    // Test hop_mode can be used in conditions.
    #[test]
    fn test_hop_mode_in_conditions() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <if {hop_mode == "dev"}>
                    <div>Development mode</div>
                  </if>
                  <if {hop_mode == "build"}>
                    <div>Build mode</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                hop_mode: String
                  --> main.hop (line 2, col 8)
                1 | <Main>
                2 |   <if {hop_mode == "dev"}>
                  |        ^^^^^^^^

                hop_mode: String
                  --> main.hop (line 5, col 8)
                4 |   </if>
                5 |   <if {hop_mode == "build"}>
                  |        ^^^^^^^^
            "#]],
        );
    }

    // Test type inference works when comparing variables to hop_mode.
    #[test]
    fn test_hop_mode_type_inference() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {mode: String}}>
                  <if {params.mode == hop_mode}>
                    <div>Mode matches</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: Record[mode: String]
                  --> main.hop (line 1, col 8)
                1 | <Main {params: {mode: String}}>
                  |        ^^^^^^

                params: Record[mode: String]
                  --> main.hop (line 2, col 8)
                1 | <Main {params: {mode: String}}>
                2 |   <if {params.mode == hop_mode}>
                  |        ^^^^^^

                hop_mode: String
                  --> main.hop (line 2, col 23)
                1 | <Main {params: {mode: String}}>
                2 |   <if {params.mode == hop_mode}>
                  |                       ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_object_equality_and_string_equality() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: {foo: String}}>
                  <if {params.foo == "foo"}>
                    eq 1
                  </if>
                  <if {params == params}>
                    eq 2
                  </if>
                </Main>
            "#},
            expect![[r#"
                error: Type Record[foo: String] is not comparable
                  --> main.hop (line 5, col 8)
                4 |   </if>
                5 |   <if {params == params}>
                  |        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_nested_data_structure_access() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {data: Array[{title: String, items: Array[String]}]}>
                	<for {section in data}>
                		<h1>{section.title}</h1>
                		<for {item in section.items}>
                			<div>{item}</div>
                		</for>
                	</for>
                </Main>
            "#},
            expect![[r#"
                data: Array[Record[items: Array[String], title: String]]
                  --> main.hop (line 1, col 8)
                1 | <Main {data: Array[{title: String, items: Array[String]}]}>
                  |        ^^^^

                data: Array[Record[items: Array[String], title: String]]
                  --> main.hop (line 2, col 19)
                1 | <Main {data: Array[{title: String, items: Array[String]}]}>
                2 |     <for {section in data}>
                  |                      ^^^^

                section: Record[items: Array[String], title: String]
                  --> main.hop (line 2, col 8)
                1 | <Main {data: Array[{title: String, items: Array[String]}]}>
                2 |     <for {section in data}>
                  |           ^^^^^^^

                section: Record[items: Array[String], title: String]
                  --> main.hop (line 3, col 8)
                2 |     <for {section in data}>
                3 |         <h1>{section.title}</h1>
                  |              ^^^^^^^

                section: Record[items: Array[String], title: String]
                  --> main.hop (line 4, col 17)
                3 |         <h1>{section.title}</h1>
                4 |         <for {item in section.items}>
                  |                       ^^^^^^^

                item: String
                  --> main.hop (line 4, col 9)
                3 |         <h1>{section.title}</h1>
                4 |         <for {item in section.items}>
                  |               ^^^^

                item: String
                  --> main.hop (line 5, col 10)
                4 |         <for {item in section.items}>
                5 |             <div>{item}</div>
                  |                   ^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_float() {
        check(
            indoc! {r#"
                -- main.hop --
                <StringComp {message: String}>
                	<div>{message}</div>
                </StringComp>
                <Main>
                	<StringComp {message: 42}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'message' of type Int is incompatible with expected type String
                  --> main.hop (line 5, col 24)
                4 | <Main>
                5 |     <StringComp {message: 42}/>
                  |                           ^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_object() {
        check(
            indoc! {r#"
                -- main.hop --
                <UserComp {user: {name: String, age: String}}>
                	<div>{user.name} ({user.age})</div>
                </UserComp>
                <Main>
                	<UserComp {user: "invalid"}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type String is incompatible with expected type Record[age: String, name: String]
                  --> main.hop (line 5, col 19)
                4 | <Main>
                5 |     <UserComp {user: "invalid"}/>
                  |                      ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_correct_object_passing() {
        check(
            indoc! {r#"
                -- main.hop --
                <UserComp {user: {name: String, active: String}}>
                	<div>{user.name}: {user.active}</div>
                </UserComp>
                <Main {data: {profile: {name: String, active: String}}}>
                	<UserComp {user: data.profile}/>
                </Main>
            "#},
            expect![[r#"
                user: Record[active: String, name: String]
                  --> main.hop (line 1, col 12)
                1 | <UserComp {user: {name: String, active: String}}>
                  |            ^^^^

                user: Record[active: String, name: String]
                  --> main.hop (line 2, col 8)
                1 | <UserComp {user: {name: String, active: String}}>
                2 |     <div>{user.name}: {user.active}</div>
                  |           ^^^^

                user: Record[active: String, name: String]
                  --> main.hop (line 2, col 21)
                1 | <UserComp {user: {name: String, active: String}}>
                2 |     <div>{user.name}: {user.active}</div>
                  |                        ^^^^

                data: Record[profile: Record[active: String, name: String]]
                  --> main.hop (line 4, col 8)
                3 | </UserComp>
                4 | <Main {data: {profile: {name: String, active: String}}}>
                  |        ^^^^

                data: Record[profile: Record[active: String, name: String]]
                  --> main.hop (line 5, col 19)
                4 | <Main {data: {profile: {name: String, active: String}}}>
                5 |     <UserComp {user: data.profile}/>
                  |                      ^^^^

                user: Record[active: String, name: String]
                  --> main.hop (line 5, col 19)
                4 | <Main {data: {profile: {name: String, active: String}}}>
                5 |     <UserComp {user: data.profile}/>
                  |                      ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_string() {
        check(
            indoc! {r#"
                -- main.hop --
                <NewComp {user: {name: String}}>
                	<div>{user.name}</div>
                </NewComp>
                <Main>
                	<NewComp {user: "invalid"}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type String is incompatible with expected type Record[name: String]
                  --> main.hop (line 5, col 18)
                4 | <Main>
                5 |     <NewComp {user: "invalid"}/>
                  |                     ^^^^^^^^^
            "#]],
        );
    }

    // Component with explicit object parameter type.
    #[test]
    fn test_explicit_object_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <UserComp {user: {name: String}}>
                	<div>{user.name}</div>
                </UserComp>
            "#},
            expect![[r#"
                user: Record[name: String]
                  --> main.hop (line 1, col 12)
                1 | <UserComp {user: {name: String}}>
                  |            ^^^^

                user: Record[name: String]
                  --> main.hop (line 2, col 8)
                1 | <UserComp {user: {name: String}}>
                2 |     <div>{user.name}</div>
                  |           ^^^^
            "#]],
        );
    }

    // Component with explicit array parameter type.
    #[test]
    fn test_explicit_array_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <ListComp {items: Array[String]}>
                	<for {item in items}>
                		<div>{item}</div>
                	</for>
                </ListComp>
            "#},
            expect![[r#"
                items: Array[String]
                  --> main.hop (line 1, col 12)
                1 | <ListComp {items: Array[String]}>
                  |            ^^^^^

                items: Array[String]
                  --> main.hop (line 2, col 16)
                1 | <ListComp {items: Array[String]}>
                2 |     <for {item in items}>
                  |                   ^^^^^

                item: String
                  --> main.hop (line 2, col 8)
                1 | <ListComp {items: Array[String]}>
                2 |     <for {item in items}>
                  |           ^^^^

                item: String
                  --> main.hop (line 3, col 9)
                2 |     <for {item in items}>
                3 |         <div>{item}</div>
                  |               ^^^^
            "#]],
        );
    }

    // Component with explicit Bool parameter type.
    #[test]
    fn test_explicit_boolean_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <ToggleComp {enabled: Bool}>
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                </ToggleComp>
            "#},
            expect![[r#"
                enabled: Bool
                  --> main.hop (line 1, col 14)
                1 | <ToggleComp {enabled: Bool}>
                  |              ^^^^^^^

                enabled: Bool
                  --> main.hop (line 2, col 7)
                1 | <ToggleComp {enabled: Bool}>
                2 |     <if {enabled}>
                  |          ^^^^^^^
            "#]],
        );
    }

    // Component with explicit Float parameter type.
    #[test]
    fn test_explicit_float_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <CounterComp {count: Float}>
                	<if {count == 0}>
                		<div>Zero</div>
                	</if>
                </CounterComp>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> main.hop (line 2, col 7)
                1 | <CounterComp {count: Float}>
                2 |     <if {count == 0}>
                  |          ^^^^^^^^^^
            "#]],
        );
    }

    // Component with nested object parameter type.
    #[test]
    fn test_nested_object_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <ProfileComp {profile: {user: {name: String, age: Float}}}>
                	<div>{profile.user.name}</div>
                	<if {profile.user.age == 25}>
                		<div>Quarter century</div>
                	</if>
                </ProfileComp>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> main.hop (line 3, col 7)
                2 |     <div>{profile.user.name}</div>
                3 |     <if {profile.user.age == 25}>
                  |          ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Component with nested array parameter type.
    #[test]
    fn test_nested_array_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <MatrixComp {matrix: Array[Array[Float]]}>
                	<for {row in matrix}>
                		<for {cell in row}>
                			<if {cell == 1}>
                				<span>One</span>
                			</if>
                		</for>
                	</for>
                </MatrixComp>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> main.hop (line 4, col 9)
                3 |         <for {cell in row}>
                4 |             <if {cell == 1}>
                  |                  ^^^^^^^^^
            "#]],
        );
    }

    // Component with complex object with multiple properties.
    #[test]
    fn test_complex_object_multiple_properties() {
        check(
            indoc! {r#"
                -- main.hop --
                <CardComp {data: {
                    title: String,
                    content: String,
                    tags: Array[String],
                    metadata: {author: String, published: Bool},
                }}>
                	<h1>{data.title}</h1>
                	<p>{data.content}</p>
                	<div>{data.metadata.author}</div>
                	<if {data.metadata.published}>
                		<span>Published</span>
                	</if>
                	<for {tag in data.tags}>
                		<span>{tag}</span>
                	</for>
                </CardComp>
            "#},
            expect![[r#"
                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 1, col 12)
                 1 | <CardComp {data: {
                   |            ^^^^

                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 7, col 7)
                 6 | }}>
                 7 |     <h1>{data.title}</h1>
                   |          ^^^^

                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 8, col 6)
                 7 |     <h1>{data.title}</h1>
                 8 |     <p>{data.content}</p>
                   |         ^^^^

                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 9, col 8)
                 8 |     <p>{data.content}</p>
                 9 |     <div>{data.metadata.author}</div>
                   |           ^^^^

                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 10, col 7)
                 9 |     <div>{data.metadata.author}</div>
                10 |     <if {data.metadata.published}>
                   |          ^^^^

                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 13, col 15)
                12 |     </if>
                13 |     <for {tag in data.tags}>
                   |                  ^^^^

                tag: String
                  --> main.hop (line 13, col 8)
                12 |     </if>
                13 |     <for {tag in data.tags}>
                   |           ^^^

                tag: String
                  --> main.hop (line 14, col 10)
                13 |     <for {tag in data.tags}>
                14 |         <span>{tag}</span>
                   |                ^^^
            "#]],
        );
    }

    // Error when passing wrong type to Bool parameter.
    #[test]
    fn test_wrong_type_to_boolean_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <ToggleComp {enabled: Bool}>
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                </ToggleComp>
                <Main>
                	<ToggleComp {enabled: "not a boolean"}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'enabled' of type String is incompatible with expected type Bool
                  --> main.hop (line 7, col 24)
                6 | <Main>
                7 |     <ToggleComp {enabled: "not a boolean"}/>
                  |                           ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Multi-file component with complex parameter types.
    #[test]
    fn test_multi_file_complex_parameters() {
        check(
            indoc! {r#"
                -- item-display.hop --
                <ItemDisplay {item: {id: Float, name: String, active: Bool}}>
                	<div>{item.name}</div>
                	<if {item.active}>
                		<span>Active item</span>
                	</if>
                	<if {item.id == 1}>
                		<span>First item</span>
                	</if>
                </ItemDisplay>
                -- data-list.hop --
                import ItemDisplay from "@/item-display"
                <DataList {items: Array[{id: Float, name: String, active: Bool}]}>
                	<for {item in items}>
                		<ItemDisplay {item: item}/>
                	</for>
                </DataList>
                -- main.hop --
                import DataList from "@/data-list"
                <Main {items: Array[{id: Float, name: String, active: Bool}]}>
                	<DataList {items: items}/>
                </Main>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> item-display.hop (line 6, col 7)
                5 |     </if>
                6 |     <if {item.id == 1}>
                  |          ^^^^^^^^^^^^
            "#]],
        );
    }

    // Structural subtyping should work: an object with properties a and b should be compatible with a component that only needs property a.
    #[test]
    fn test_structural_subtyping_success() {
        check(
            indoc! {r#"
                -- main.hop --
                <NeedsA {data: {a: String}}>
                	<div>{data.a}</div>
                </NeedsA>

                <Main {params: {data: {a: String, b: String}}}>
                	<NeedsA {data: params.data}/>
                </Main>
            "#},
            expect![[r#"
                data: Record[a: String]
                  --> main.hop (line 1, col 10)
                1 | <NeedsA {data: {a: String}}>
                  |          ^^^^

                data: Record[a: String]
                  --> main.hop (line 2, col 8)
                1 | <NeedsA {data: {a: String}}>
                2 |     <div>{data.a}</div>
                  |           ^^^^

                params: Record[data: Record[a: String, b: String]]
                  --> main.hop (line 5, col 8)
                4 | 
                5 | <Main {params: {data: {a: String, b: String}}}>
                  |        ^^^^^^

                params: Record[data: Record[a: String, b: String]]
                  --> main.hop (line 6, col 17)
                5 | <Main {params: {data: {a: String, b: String}}}>
                6 |     <NeedsA {data: params.data}/>
                  |                    ^^^^^^

                data: Record[a: String, b: String]
                  --> main.hop (line 6, col 17)
                5 | <Main {params: {data: {a: String, b: String}}}>
                6 |     <NeedsA {data: params.data}/>
                  |                    ^^^^^^^^^^^
            "#]],
        );
    }

    // Typechecker should print an error if an object lacks required properties.
    #[test]
    fn test_structural_subtyping_missing_property() {
        check(
            indoc! {r#"
                -- main.hop --
                <NeedsA {data: {a: String}}>
                	<div>{data.a}</div>
                </NeedsA>

                <Main {params: {data: {b: String}}}>
                	<NeedsA {data: params.data}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'data' of type Record[b: String] is incompatible with expected type Record[a: String]
                  --> main.hop (line 6, col 17)
                5 | <Main {params: {data: {b: String}}}>
                6 |     <NeedsA {data: params.data}/>
                  |                    ^^^^^^^^^^^
            "#]],
        );
    }

    // Using a condition that is not a Bool should produce an error
    #[test]
    fn test_if_condition_must_be_boolean() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <if {"str"}>
                      is str?
                    </if>
                </Main>
            "#},
            expect![[r#"
                error: Expected boolean condition, got String
                  --> main.hop (line 2, col 10)
                1 | <Main>
                2 |     <if {"str"}>
                  |          ^^^^^
            "#]],
        );
    }

    // Passing an argument that is not in the parameter list should produce an error
    #[test]
    fn test_unexpected_argument_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {a: String}>
                  {a}
                </Main>
                <Foo>
                    <Main {a: "", b: 1}/>
                </Foo>
            "#},
            expect![[r#"
                error: Unexpected argument 'b'
                  --> main.hop (line 5, col 19)
                4 | <Foo>
                5 |     <Main {a: "", b: 1}/>
                  |                   ^
            "#]],
        );
    }

    // Type errors in argument list should be reported
    #[test]
    fn test_type_errors_in_argument_list() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {a: String, b: String}>
                  {a} {b}
                </Main>
                <Foo>
                    <Main {a: 1 == "", b: 1 == ""}/>
                </Foo>
            "#},
            expect![[r#"
                error: Can not compare Int to String
                  --> main.hop (line 5, col 15)
                4 | <Foo>
                5 |     <Main {a: 1 == "", b: 1 == ""}/>
                  |               ^^^^^^^

                error: Can not compare Int to String
                  --> main.hop (line 5, col 27)
                4 | <Foo>
                5 |     <Main {a: 1 == "", b: 1 == ""}/>
                  |                           ^^^^^^^
            "#]],
        );
    }

    // Trying to iterate over an empty array should produce an error
    #[test]
    fn test_iterate_over_empty_array_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <for {x in []}>
                      ?
                    </for>
                </Main>
            "#},
            expect![[r#"
                error: Cannot iterate over an empty array with unknown element type
                  --> main.hop (line 2, col 16)
                1 | <Main>
                2 |     <for {x in []}>
                  |                ^^
            "#]],
        );
    }

    // Trying to render a non-string should produce an error
    #[test]
    fn test_render_non_string_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    {false}
                </Main>
            "#},
            expect![[r#"
                error: Expected string for text expression, got Bool
                  --> main.hop (line 2, col 5)
                1 | <Main>
                2 |     {false}
                  |     ^^^^^^^
            "#]],
        );
    }

    // Test that non-entrypoint components with the same name are allowed
    #[test]
    fn test_same_name_non_entrypoint_allowed() {
        check(
            indoc! {r#"
                -- module1.hop --
                <CardComp>
                    <div>Module 1 card</div>
                </CardComp>
                
                -- module2.hop --
                <CardComp>
                    <div>Module 2 card</div>
                </CardComp>
            "#},
            expect![""],
        );
    }
}
