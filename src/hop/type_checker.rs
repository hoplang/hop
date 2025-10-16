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
    // Track whether this component is an entrypoint.
    is_entrypoint: bool,
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

    /// Check if an entrypoint with the given name already exists in any module.
    /// Returns the module name if a duplicate is found.
    fn find_duplicate_entrypoint(
        &self,
        component_name: &str,
        current_module: &ModuleName,
    ) -> Option<ModuleName> {
        for (module_name, module_info) in &self.modules {
            if module_name == current_module {
                continue;
            }

            if let Some(component_info) = module_info.components.get(component_name) {
                if component_info.is_entrypoint {
                    return Some(module_name.clone());
                }
            }
        }
        None
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
            tag_name: name,
            params,
            children,
            has_slot,
            is_entrypoint,
            as_attr,
            attributes,
            range,
            closing_tag_name,
        } = component_def;
        // Check for duplicate entrypoints before processing
        if *is_entrypoint {
            if let Some(previous_module) =
                state.find_duplicate_entrypoint(name.as_str(), &module.name)
            {
                errors.push(TypeError::DuplicateEntrypoint {
                    component: name.as_str().to_string(),
                    module: module.name.as_str().to_string(),
                    previous_module: previous_module.as_str().to_string(),
                    range: name.clone(),
                });
            }
        }

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

        // Typecheck attributes
        let typed_attributes = typecheck_attributes(attributes, &mut env, annotations, errors);

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
                is_entrypoint: *is_entrypoint,
            },
        );

        // Build typed ComponentDefinition
        typed_component_definitions.push(ComponentDefinition {
            tag_name: name.clone(),
            closing_tag_name: closing_tag_name.clone(),
            params: params.clone(),
            as_attr: as_attr.clone(),
            attributes: typed_attributes,
            range: range.clone(),
            children: typed_children,
            is_entrypoint: *is_entrypoint,
            has_slot: *has_slot,
        });
    }

    // Build and return the typed AST
    Ast::new(
        module.name.clone(),
        typed_component_definitions,
        module.get_imports().to_vec(),
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
            attributes,
            children,
            range,
        } => {
            // Transform attributes
            let typed_attributes = typecheck_attributes(attributes, env, annotations, errors);

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
                attributes: typed_attributes,
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
            Some(AttributeValue::Expression(expr)) => {
                errors
                    .ok_or_add(dop::typecheck_expr(expr, env, annotations).map_err(Into::into))
                    .map(|typed_expr| {
                        // Check that HTML attributes are strings
                        let expr_type = typed_expr.as_type();
                        if !expr_type.is_subtype(&Type::String) {
                            errors.push(TypeError::ExpectedStringAttribute {
                                found: expr_type.to_string(),
                                range: expr.annotation().clone(),
                            });
                        }
                        AttributeValue::Expression(typed_expr)
                    })
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
                <main-comp>
                </main-comp>
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
                <main-comp>
                	<h1>Hello,
                        <render-bar>
                            <div></div>
                        </render-bar>
                    </h1>
                    <render-foo></render-foo>
                </main-comp>
            "#},
            expect![[r#"
                error: Component render-bar is not defined
                  --> main.hop (line 3, col 10)
                2 |     <h1>Hello,
                3 |         <render-bar>
                  |          ^^^^^^^^^^

                error: Component render-foo is not defined
                  --> main.hop (line 7, col 6)
                6 |     </h1>
                7 |     <render-foo></render-foo>
                  |      ^^^^^^^^^^
            "#]],
        );
    }

    // When a component references itself, the typechecker outputs an error.
    #[test]
    fn test_component_references_itself() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                	<h1>Hello, <main-comp></main-comp>!</h1>
                </main-comp>
            "#},
            expect![[r#"
                error: Component main-comp is not defined
                  --> main.hop (line 2, col 14)
                1 | <main-comp>
                2 |     <h1>Hello, <main-comp></main-comp>!</h1>
                  |                 ^^^^^^^^^
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
                <import component="foo-comp" from="@/other">

                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Module other is not defined
                  --> main.hop (line 1, col 36)
                1 | <import component="foo-comp" from="@/other">
                  |                                    ^^^^^^^
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
                <import component="foo-comp" from="@/other">

                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Module other is not defined
                  --> main.hop (line 1, col 36)
                1 | <import component="foo-comp" from="@/other">
                  |                                    ^^^^^^^
            "#]],
        );
    }

    // A component may be imported without being used.
    #[test]
    fn test_unused_import() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                </foo-comp>

                -- main.hop --
                <import component="foo-comp" from="@/other">

                <main-comp>
                </main-comp>
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
                <main-comp>
                	<h1>Hello, <render-name></render-name>!</h1>
                	<h1>Hello, <other-comp></other-comp>!</h1>
                </main-comp>
            "#},
            expect![[r#"
                error: Component render-name is not defined
                  --> main.hop (line 2, col 14)
                1 | <main-comp>
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                  |                 ^^^^^^^^^^^

                error: Component other-comp is not defined
                  --> main.hop (line 3, col 14)
                2 |     <h1>Hello, <render-name></render-name>!</h1>
                3 |     <h1>Hello, <other-comp></other-comp>!</h1>
                  |                 ^^^^^^^^^^
            "#]],
        );
    }

    // Defining a component with the same name in two different modules is allowed.
    #[test]
    fn test_same_component_name_in_different_modules() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                </foo-comp>

                -- main.hop --
                <foo-comp>
                </foo-comp>
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
                <main-comp>
                    <strong>No slot here</strong>
                </main-comp>

                <bar-comp>
                    <main-comp>
                        This component has no slot
                    </main-comp>
                </bar-comp>
            "#},
            expect![[r#"
                error: Component main-comp does not have a slot-default
                  --> main.hop (line 6, col 6)
                5 | <bar-comp>
                6 |     <main-comp>
                  |      ^^^^^^^^^
            "#]],
        );
    }

    // Test undefined slot with imported component.
    #[test]
    fn test_undefined_slot_with_imported_component() {
        check(
            indoc! {r#"
                -- other.hop --
                <foo-comp>
                    <strong>No slot here</strong>
                </foo-comp>
                -- main.hop --
                <import component="foo-comp" from="@/other">

                <bar-comp>
                    <foo-comp>
                        This component has no slot
                    </foo-comp>
                </bar-comp>
            "#},
            expect![[r#"
                error: Component foo-comp does not have a slot-default
                  --> main.hop (line 4, col 6)
                3 | <bar-comp>
                4 |     <foo-comp>
                  |      ^^^^^^^^
            "#]],
        );
    }

    // When a variable shadows another variable, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_param() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {items: {foo: Array[String]}}>
                  <for {items in items.foo}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: {foo: Array[String]}}>
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
                <main-comp {items: Array[String]}>
                  <for {items in items}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: Array[String]}>
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
                <main-comp {items: {a: Array[String], b: Array[String]}}>
                  <for {item in items.a}>
                    <for {item in items.b}>
                      <div>{item}</div>
                    </for>
                  </for>
                </main-comp>
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
                <main-comp {params: Array[{active: Bool}]}>
                	<for {item in params}>
                	  <if {item.active}>
                	  </if>
                	</for>
                	<if {item.active}>
                	</if>
                </main-comp>
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
                <main-comp {items: Array[String]}>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                  <for {item in items}>
                  </for>
                </main-comp>
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
                <main-comp {items: Array[String]}>
                  <for {item in items}>
                  </for>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: Array[String]}>
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
                <main-comp {items: Array[String]}>
                  <for {item in items}>
                  </for>
                </main-comp>
            "#},
            expect![[r#"
                error: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | <main-comp {items: Array[String]}>
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
                <bar-comp {p: String, s: String}>
                  <div>
                  	{s}
                  </div>
                </bar-comp>
            "#},
            expect![[r#"
                error: Unused variable p
                  --> main.hop (line 1, col 12)
                1 | <bar-comp {p: String, s: String}>
                  |            ^
            "#]],
        );
    }

    // Arguments may be passed in any order to a component.
    #[test]
    fn test_component_handles_arguments_in_any_order() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: Bool, b: String}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {b: "foo", a: true}/>
                </foo-comp>
            "#},
            expect![[r#"
                a: Bool
                  --> main.hop (line 1, col 13)
                1 | <main-comp {a: Bool, b: String}>
                  |             ^

                b: String
                  --> main.hop (line 1, col 22)
                1 | <main-comp {a: Bool, b: String}>
                  |                      ^

                b: String
                  --> main.hop (line 3, col 11)
                2 |   <if {a}>
                3 |     <div>{b}</div>
                  |           ^

                a: Bool
                  --> main.hop (line 2, col 8)
                1 | <main-comp {a: Bool, b: String}>
                2 |   <if {a}>
                  |        ^

                b: String
                  --> main.hop (line 7, col 18)
                6 | <foo-comp>
                7 |   <main-comp {b: "foo", a: true}/>
                  |                  ^^^^^

                a: Bool
                  --> main.hop (line 7, col 28)
                6 | <foo-comp>
                7 |   <main-comp {b: "foo", a: true}/>
                  |                            ^^^^
            "#]],
        );
    }

    // When a parameter is missing the typechecker reports an error.
    #[test]
    fn test_component_missing_required_parameter_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: Bool, b: String}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {b: "foo"}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Missing required parameter 'a'
                  --> main.hop (line 7, col 15)
                6 | <foo-comp>
                7 |   <main-comp {b: "foo"}/>
                  |               ^^^^^^^^
            "#]],
        );
    }

    // When the whole parameter expression is missing the typechecker reports an error.
    #[test]
    fn test_component_does_not_specify_required_parameters_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: Bool, b: String}>
                  <if {a}>
                    <div>{b}</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp />
                </foo-comp>
            "#},
            expect![[r#"
                error: Component requires arguments: a, b
                  --> main.hop (line 7, col 4)
                6 | <foo-comp>
                7 |   <main-comp />
                  |    ^^^^^^^^^
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
                <main-comp>
                  hello world
                </main-comp>
                <foo-comp>
                  <main-comp {a: "foo"} />
                </foo-comp>
            "#},
            expect![[r#"
                error: Component does not accept arguments
                  --> main.hop (line 5, col 15)
                4 | <foo-comp>
                5 |   <main-comp {a: "foo"} />
                  |               ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_iterate_over_boolean_property() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: Array[{k: Bool}]}>
                	<for {item in params}>
                		<if {item.k}>
                		</if>
                	</for>
                	<for {item in params}>
                		<for {inner in item.k}>
                			<div>{inner}</div>
                		</for>
                	</for>
                </main-comp>
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
                <main-comp {params: Bool}>
                	<if {params}>
                	</if>
                	<for {item in params}>
                		<div>{item}</div>
                	</for>
                </main-comp>
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
                <main-comp {params: {items: Array[{active: Bool, name: Bool}]}}>
                	<for {item in params.items}>
                		<if {item.active}>
                		</if>
                		<if {item.name}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[items: Array[Record[active: Bool, name: Bool]]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {items: Array[{active: Bool, name: Bool}]}}>
                  |             ^^^^^^

                params: Record[items: Array[Record[active: Bool, name: Bool]]]
                  --> main.hop (line 2, col 16)
                1 | <main-comp {params: {items: Array[{active: Bool, name: Bool}]}}>
                2 |     <for {item in params.items}>
                  |                   ^^^^^^

                item: Record[active: Bool, name: Bool]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: {items: Array[{active: Bool, name: Bool}]}}>
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
                <custom-head>
                	<head>
                		<title><slot-default/></title>
                	</head>
                </custom-head>
            "#},
            expect![""],
        );
    }

    #[test]
    fn test_boolean_equality_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {a: String, b: Bool}}>
                  <if {(params.a == "str") == params.b}>
                    <div>Match</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[a: String, b: Bool]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {a: String, b: Bool}}>
                  |             ^^^^^^

                params: Record[a: String, b: Bool]
                  --> main.hop (line 2, col 9)
                1 | <main-comp {params: {a: String, b: Bool}}>
                2 |   <if {(params.a == "str") == params.b}>
                  |         ^^^^^^

                params: Record[a: String, b: Bool]
                  --> main.hop (line 2, col 31)
                1 | <main-comp {params: {a: String, b: Bool}}>
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
                <main-comp {params: {enabled: Bool, users: Array[{profile: {verified: Bool}, posts: Array[{published: Bool}]}]}}>
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
                </main-comp>
            "#},
            expect![[r#"
                params: Record[
                  enabled: Bool,
                  users: Array[Record[
                    posts: Array[Record[published: Bool]],
                    profile: Record[verified: Bool],
                  ]],
                ]
                  --> main.hop (line 1, col 13)
                 1 | <main-comp {params: {enabled: Bool, users: Array[{profile: {verified: Bool}, posts: Array[{published: Bool}]}]}}>
                   |             ^^^^^^

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
                 1 | <main-comp {params: {enabled: Bool, users: Array[{profile: {verified: Bool}, posts: Array[{published: Bool}]}]}}>
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
                <main-comp {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                	<for {section in params.sections}>
                		<if {section.header.visible}>
                			<for {item in section.items}>
                				<if {item.data.valid}>
                				</if>
                			</for>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[
                  sections: Array[Record[
                    header: Record[visible: Bool],
                    items: Array[Record[data: Record[valid: Bool]]],
                  ]],
                ]
                  --> main.hop (line 1, col 13)
                 1 | <main-comp {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                   |             ^^^^^^

                params: Record[
                  sections: Array[Record[
                    header: Record[visible: Bool],
                    items: Array[Record[data: Record[valid: Bool]]],
                  ]],
                ]
                  --> main.hop (line 2, col 19)
                 1 | <main-comp {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
                 2 |     <for {section in params.sections}>
                   |                      ^^^^^^

                section: Record[
                  header: Record[visible: Bool],
                  items: Array[Record[data: Record[valid: Bool]]],
                ]
                  --> main.hop (line 2, col 8)
                 1 | <main-comp {params: {sections: Array[{header: {visible: Bool}, items: Array[{data: {valid: Bool}}]}]}}>
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
                <main-comp {params: {i: {j: {k: {l: Bool}}}}}>
                	<if {params.i.j.k.l}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[i: Record[j: Record[k: Record[l: Bool]]]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {i: {j: {k: {l: Bool}}}}}>
                  |             ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]]]]
                  --> main.hop (line 2, col 7)
                1 | <main-comp {params: {i: {j: {k: {l: Bool}}}}}>
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
                <main-comp {params: {app: {ui: {theme: {dark: Bool}}, api: {endpoints: {users: {enabled: Bool}}}, database: {connection: {ssl: Bool}}}}}>
                	<if {params.app.ui.theme.dark}>
                	</if>
                	<if {params.app.api.endpoints.users.enabled}>
                	</if>
                	<if {params.app.database.connection.ssl}>
                	</if>
                </main-comp>
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
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {app: {ui: {theme: {dark: Bool}}, api: {endpoints: {users: {enabled: Bool}}}, database: {connection: {ssl: Bool}}}}}>
                  |             ^^^^^^

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
                1 | <main-comp {params: {app: {ui: {theme: {dark: Bool}}, api: {endpoints: {users: {enabled: Bool}}}, database: {connection: {ssl: Bool}}}}}>
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
    fn test_component_with_entrypoint_script_and_style() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp entrypoint>
                    <script>
                        console.log("test");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </main-comp>
            "#},
            expect![""],
        );
    }

    #[test]
    fn test_component_with_entrypoint_and_data() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp entrypoint {data: {message: String}}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </main-comp>
            "#},
            expect![[r#"
                data: Record[message: String]
                  --> main.hop (line 1, col 24)
                1 | <main-comp entrypoint {data: {message: String}}>
                  |                        ^^^^

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
                <main-comp {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                  <if {params.user.name == params.other_user.name}>
                    <div>Same name</div>
                  </if>
                  <if {(params.data.x == params.data.y)}>
                    <div>Parentheses work</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                  |             ^^^^^^

                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
                2 |   <if {params.user.name == params.other_user.name}>
                  |        ^^^^^^

                params: Record[
                  data: Record[x: String, y: String],
                  other_user: Record[name: String],
                  user: Record[name: String],
                ]
                  --> main.hop (line 2, col 28)
                1 | <main-comp {params: {user: {name: String}, other_user: {name: String}, data: {x: String, y: String}}}>
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
                <main-comp {params: {x: String, y: String}}>
                  <if {params.x == params.y}>
                    <div>Values are equal</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[x: String, y: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {x: String, y: String}}>
                  |             ^^^^^^

                params: Record[x: String, y: String]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: {x: String, y: String}}>
                2 |   <if {params.x == params.y}>
                  |        ^^^^^^

                params: Record[x: String, y: String]
                  --> main.hop (line 2, col 20)
                1 | <main-comp {params: {x: String, y: String}}>
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
                <main-comp {params: {foo: {bar: Array[Bool]}}}>
                	<for {j in params.foo.bar}>
                		<if {j}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[foo: Record[bar: Array[Bool]]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {foo: {bar: Array[Bool]}}}>
                  |             ^^^^^^

                params: Record[foo: Record[bar: Array[Bool]]]
                  --> main.hop (line 2, col 13)
                1 | <main-comp {params: {foo: {bar: Array[Bool]}}}>
                2 |     <for {j in params.foo.bar}>
                  |                ^^^^^^

                j: Bool
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: {foo: {bar: Array[Bool]}}}>
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
                <main-comp {params: Array[{a: Bool, b: Bool}]}>
                	<for {j in params}>
                		<if {j.a}>
                		</if>
                	</for>
                	<for {j in params}>
                		<if {j.b}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                params: Array[Record[a: Bool, b: Bool]]
                  --> main.hop (line 1, col 13)
                 1 | <main-comp {params: Array[{a: Bool, b: Bool}]}>
                   |             ^^^^^^

                params: Array[Record[a: Bool, b: Bool]]
                  --> main.hop (line 2, col 13)
                 1 | <main-comp {params: Array[{a: Bool, b: Bool}]}>
                 2 |     <for {j in params}>
                   |                ^^^^^^

                j: Record[a: Bool, b: Bool]
                  --> main.hop (line 2, col 8)
                 1 | <main-comp {params: Array[{a: Bool, b: Bool}]}>
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
                <main-comp {i: Array[Array[Bool]]}>
                	<for {j in i}>
                		<for {k in j}>
                			<if {k}>
                			</if>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                i: Array[Array[Bool]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {i: Array[Array[Bool]]}>
                  |             ^

                i: Array[Array[Bool]]
                  --> main.hop (line 2, col 13)
                1 | <main-comp {i: Array[Array[Bool]]}>
                2 |     <for {j in i}>
                  |                ^

                j: Array[Bool]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {i: Array[Array[Bool]]}>
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
                <main-comp {i: Array[Bool]}>
                	<for {j in i}>
                		<if {j}>
                		</if>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                i: Array[Bool]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {i: Array[Bool]}>
                  |             ^

                i: Array[Bool]
                  --> main.hop (line 2, col 13)
                1 | <main-comp {i: Array[Bool]}>
                2 |     <for {j in i}>
                  |                ^

                j: Bool
                  --> main.hop (line 2, col 8)
                1 | <main-comp {i: Array[Bool]}>
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
                <button-comp {text: String}>
                  <div>{text}</div>
                </button-comp>

                -- main.hop --
                <import component="button-comp" from="@/utils">

                <main-comp {label: String}>
                  <button-comp {text: label}/>
                </main-comp>
            "#},
            expect![[r#"
                text: String
                  --> utils.hop (line 1, col 15)
                1 | <button-comp {text: String}>
                  |               ^^^^

                text: String
                  --> utils.hop (line 2, col 9)
                1 | <button-comp {text: String}>
                2 |   <div>{text}</div>
                  |         ^^^^

                label: String
                  --> main.hop (line 3, col 13)
                2 | 
                3 | <main-comp {label: String}>
                  |             ^^^^^

                label: String
                  --> main.hop (line 4, col 23)
                3 | <main-comp {label: String}>
                4 |   <button-comp {text: label}/>
                  |                       ^^^^^

                text: String
                  --> main.hop (line 4, col 23)
                3 | <main-comp {label: String}>
                4 |   <button-comp {text: label}/>
                  |                       ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_multi_module_component_chain() {
        check(
            indoc! {r#"
                -- bar.hop --
                <widget-comp {config: {enabled: Bool, title: String}}>
                  <if {config.enabled}>
                    <div>{config.title}</div>
                  </if>
                </widget-comp>

                -- foo.hop --
                <import component="widget-comp" from="@/bar">

                <panel-comp {data: {items: Array[{enabled: Bool, title: String}]}}>
                  <for {item in data.items}>
                    <widget-comp {config: item}/>
                  </for>
                </panel-comp>

                -- main.hop --
                <import component="panel-comp" from="@/foo">

                <main-comp {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                  <panel-comp {data: settings.dashboard}/>
                </main-comp>
            "#},
            expect![[r#"
                config: Record[enabled: Bool, title: String]
                  --> bar.hop (line 1, col 15)
                1 | <widget-comp {config: {enabled: Bool, title: String}}>
                  |               ^^^^^^

                config: Record[enabled: Bool, title: String]
                  --> bar.hop (line 3, col 11)
                2 |   <if {config.enabled}>
                3 |     <div>{config.title}</div>
                  |           ^^^^^^

                config: Record[enabled: Bool, title: String]
                  --> bar.hop (line 2, col 8)
                1 | <widget-comp {config: {enabled: Bool, title: String}}>
                2 |   <if {config.enabled}>
                  |        ^^^^^^

                data: Record[items: Array[Record[enabled: Bool, title: String]]]
                  --> foo.hop (line 3, col 14)
                2 | 
                3 | <panel-comp {data: {items: Array[{enabled: Bool, title: String}]}}>
                  |              ^^^^

                data: Record[items: Array[Record[enabled: Bool, title: String]]]
                  --> foo.hop (line 4, col 17)
                3 | <panel-comp {data: {items: Array[{enabled: Bool, title: String}]}}>
                4 |   <for {item in data.items}>
                  |                 ^^^^

                item: Record[enabled: Bool, title: String]
                  --> foo.hop (line 4, col 9)
                3 | <panel-comp {data: {items: Array[{enabled: Bool, title: String}]}}>
                4 |   <for {item in data.items}>
                  |         ^^^^

                item: Record[enabled: Bool, title: String]
                  --> foo.hop (line 5, col 27)
                4 |   <for {item in data.items}>
                5 |     <widget-comp {config: item}/>
                  |                           ^^^^

                config: Record[enabled: Bool, title: String]
                  --> foo.hop (line 5, col 27)
                4 |   <for {item in data.items}>
                5 |     <widget-comp {config: item}/>
                  |                           ^^^^

                settings: Record[
                  dashboard: Record[
                    items: Array[Record[enabled: Bool, title: String]],
                  ],
                ]
                  --> main.hop (line 3, col 13)
                2 | 
                3 | <main-comp {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                  |             ^^^^^^^^

                settings: Record[
                  dashboard: Record[
                    items: Array[Record[enabled: Bool, title: String]],
                  ],
                ]
                  --> main.hop (line 4, col 22)
                3 | <main-comp {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                4 |   <panel-comp {data: settings.dashboard}/>
                  |                      ^^^^^^^^

                data: Record[items: Array[Record[enabled: Bool, title: String]]]
                  --> main.hop (line 4, col 22)
                3 | <main-comp {settings: {dashboard: {items: Array[{enabled: Bool, title: String}]}}}>
                4 |   <panel-comp {data: settings.dashboard}/>
                  |                      ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_simple_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: String}>
                	<div>{params}</div>
                </main-comp>
            "#},
            expect![[r#"
                params: String
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: String}>
                  |             ^^^^^^

                params: String
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: String}>
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
                <main-comp {params: {config: {debug: Bool}, data: Array[{id: Bool, attributes: Array[Bool]}]}}>
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
                </main-comp>
            "#},
            expect![[r#"
                params: Record[
                  config: Record[debug: Bool],
                  data: Array[Record[attributes: Array[Bool], id: Bool]],
                ]
                  --> main.hop (line 1, col 13)
                 1 | <main-comp {params: {config: {debug: Bool}, data: Array[{id: Bool, attributes: Array[Bool]}]}}>
                   |             ^^^^^^

                params: Record[
                  config: Record[debug: Bool],
                  data: Array[Record[attributes: Array[Bool], id: Bool]],
                ]
                  --> main.hop (line 2, col 7)
                 1 | <main-comp {params: {config: {debug: Bool}, data: Array[{id: Bool, attributes: Array[Bool]}]}}>
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
                <step3-comp {settings: {enabled: Bool}}>
                	<if {settings.enabled}>
                	</if>
                </step3-comp>

                <step2-comp {config: {settings: {enabled: Bool}}}>
                	<step3-comp {settings: config.settings}/>
                </step2-comp>

                <step1-comp {data: {config: {settings: {enabled: Bool}}}}>
                	<step2-comp {config: data.config}/>
                </step1-comp>

                <main-comp {params: {config: {settings: {enabled: Bool}}}}>
                	<step1-comp {data: params}/>
                </main-comp>
            "#},
            expect![[r#"
                settings: Record[enabled: Bool]
                  --> main.hop (line 1, col 14)
                 1 | <step3-comp {settings: {enabled: Bool}}>
                   |              ^^^^^^^^

                settings: Record[enabled: Bool]
                  --> main.hop (line 2, col 7)
                 1 | <step3-comp {settings: {enabled: Bool}}>
                 2 |     <if {settings.enabled}>
                   |          ^^^^^^^^

                config: Record[settings: Record[enabled: Bool]]
                  --> main.hop (line 6, col 14)
                 5 | 
                 6 | <step2-comp {config: {settings: {enabled: Bool}}}>
                   |              ^^^^^^

                config: Record[settings: Record[enabled: Bool]]
                  --> main.hop (line 7, col 25)
                 6 | <step2-comp {config: {settings: {enabled: Bool}}}>
                 7 |     <step3-comp {settings: config.settings}/>
                   |                            ^^^^^^

                settings: Record[enabled: Bool]
                  --> main.hop (line 7, col 25)
                 6 | <step2-comp {config: {settings: {enabled: Bool}}}>
                 7 |     <step3-comp {settings: config.settings}/>
                   |                            ^^^^^^^^^^^^^^^

                data: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 10, col 14)
                 9 | 
                10 | <step1-comp {data: {config: {settings: {enabled: Bool}}}}>
                   |              ^^^^

                data: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 11, col 23)
                10 | <step1-comp {data: {config: {settings: {enabled: Bool}}}}>
                11 |     <step2-comp {config: data.config}/>
                   |                          ^^^^

                config: Record[settings: Record[enabled: Bool]]
                  --> main.hop (line 11, col 23)
                10 | <step1-comp {data: {config: {settings: {enabled: Bool}}}}>
                11 |     <step2-comp {config: data.config}/>
                   |                          ^^^^^^^^^^^

                params: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 14, col 13)
                13 | 
                14 | <main-comp {params: {config: {settings: {enabled: Bool}}}}>
                   |             ^^^^^^

                params: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 15, col 21)
                14 | <main-comp {params: {config: {settings: {enabled: Bool}}}}>
                15 |     <step1-comp {data: params}/>
                   |                        ^^^^^^

                data: Record[config: Record[settings: Record[enabled: Bool]]]
                  --> main.hop (line 15, col 21)
                14 | <main-comp {params: {config: {settings: {enabled: Bool}}}}>
                15 |     <step1-comp {data: params}/>
                   |                        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_three_level_component_hierarchy() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-card {item: {title: String, active: Bool, status: String}}>
                  <div>{item.title}
                  </div>
                  <if {item.active}>
                    <span>{item.status}
                    </span>
                  </if>
                </main-card>

                <main-list {items: Array[{title: String, active: Bool, status: String}]}>
                  <for {item in items}>
                    <main-card {item: item}/>
                  </for>
                </main-list>

                <main-comp {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                  <main-list {items: data.items}/>
                </main-comp>
            "#},
            expect![[r#"
                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 1, col 13)
                 1 | <main-card {item: {title: String, active: Bool, status: String}}>
                   |             ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 2, col 9)
                 1 | <main-card {item: {title: String, active: Bool, status: String}}>
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
                  --> main.hop (line 10, col 13)
                 9 | 
                10 | <main-list {items: Array[{title: String, active: Bool, status: String}]}>
                   |             ^^^^^

                items: Array[Record[active: Bool, status: String, title: String]]
                  --> main.hop (line 11, col 17)
                10 | <main-list {items: Array[{title: String, active: Bool, status: String}]}>
                11 |   <for {item in items}>
                   |                 ^^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 11, col 9)
                10 | <main-list {items: Array[{title: String, active: Bool, status: String}]}>
                11 |   <for {item in items}>
                   |         ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 12, col 23)
                11 |   <for {item in items}>
                12 |     <main-card {item: item}/>
                   |                       ^^^^

                item: Record[active: Bool, status: String, title: String]
                  --> main.hop (line 12, col 23)
                11 |   <for {item in items}>
                12 |     <main-card {item: item}/>
                   |                       ^^^^

                data: Record[
                  items: Array[Record[
                    active: Bool,
                    status: String,
                    title: String,
                  ]],
                ]
                  --> main.hop (line 16, col 13)
                15 | 
                16 | <main-comp {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                   |             ^^^^

                data: Record[
                  items: Array[Record[
                    active: Bool,
                    status: String,
                    title: String,
                  ]],
                ]
                  --> main.hop (line 17, col 22)
                16 | <main-comp {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                17 |   <main-list {items: data.items}/>
                   |                      ^^^^

                items: Array[Record[active: Bool, status: String, title: String]]
                  --> main.hop (line 17, col 22)
                16 | <main-comp {data: {items: Array[{title: String, active: Bool, status: String}]}}>
                17 |   <main-list {items: data.items}/>
                   |                      ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_object_property_name_collision() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                	<if {params.i.j.k.l}>
                		<if {params.i.k}>
                		</if>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                  |             ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 3, col 8)
                2 |     <if {params.i.j.k.l}>
                3 |         <if {params.i.k}>
                  |              ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 2, col 7)
                1 | <main-comp {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
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
                <main-comp {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                	<if {params.i.j.k.l}>
                	</if>
                	<if {params.i.k}>
                	</if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
                  |             ^^^^^^

                params: Record[i: Record[j: Record[k: Record[l: Bool]], k: Bool]]
                  --> main.hop (line 2, col 7)
                1 | <main-comp {params: {i: {j: {k: {l: Bool}}, k: Bool}}}>
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
                <main-comp {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
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
                </main-comp>
            "#},
            expect![[r#"
                params: Record[
                  categories: Array[Bool],
                  metadata: Record[title: Bool],
                  tags: Array[Bool],
                ]
                  --> main.hop (line 1, col 13)
                 1 | <main-comp {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
                   |             ^^^^^^

                params: Record[
                  categories: Array[Bool],
                  metadata: Record[title: Bool],
                  tags: Array[Bool],
                ]
                  --> main.hop (line 2, col 15)
                 1 | <main-comp {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
                 2 |     <for {tag in params.tags}>
                   |                  ^^^^^^

                tag: Bool
                  --> main.hop (line 2, col 8)
                 1 | <main-comp {params: {tags: Array[Bool], categories: Array[Bool], metadata: {title: Bool}}}>
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
                <main-bar {p: Bool}>
                  <if {p}>
                  </if>
                </main-bar>

                <main-foo {p: Bool}>
                  <main-bar {p: p}/>
                </main-foo>

                <main-comp {i: Bool}>
                  <main-foo {p: i}/>
                </main-comp>
            "#},
            expect![[r#"
                p: Bool
                  --> main.hop (line 1, col 12)
                 1 | <main-bar {p: Bool}>
                   |            ^

                p: Bool
                  --> main.hop (line 2, col 8)
                 1 | <main-bar {p: Bool}>
                 2 |   <if {p}>
                   |        ^

                p: Bool
                  --> main.hop (line 6, col 12)
                 5 | 
                 6 | <main-foo {p: Bool}>
                   |            ^

                p: Bool
                  --> main.hop (line 7, col 17)
                 6 | <main-foo {p: Bool}>
                 7 |   <main-bar {p: p}/>
                   |                 ^

                p: Bool
                  --> main.hop (line 7, col 17)
                 6 | <main-foo {p: Bool}>
                 7 |   <main-bar {p: p}/>
                   |                 ^

                i: Bool
                  --> main.hop (line 10, col 13)
                 9 | 
                10 | <main-comp {i: Bool}>
                   |             ^

                i: Bool
                  --> main.hop (line 11, col 17)
                10 | <main-comp {i: Bool}>
                11 |   <main-foo {p: i}/>
                   |                 ^

                p: Bool
                  --> main.hop (line 11, col 17)
                10 | <main-comp {i: Bool}>
                11 |   <main-foo {p: i}/>
                   |                 ^
            "#]],
        );
    }

    #[test]
    fn test_workflow_execution_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                <execute-step {step: {condition: Bool}}>
                	<if {step.condition}>
                	</if>
                </execute-step>

                <execute-workflow {workflow: {enabled: Bool, steps: Array[{condition: Bool}]}}>
                	<if {workflow.enabled}>
                		<for {step in workflow.steps}>
                			<execute-step {step: step}/>
                		</for>
                	</if>
                </execute-workflow>

                <main-comp {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                	<for {workflow in params.workflows}>
                		<execute-workflow {workflow: workflow}/>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                step: Record[condition: Bool]
                  --> main.hop (line 1, col 16)
                 1 | <execute-step {step: {condition: Bool}}>
                   |                ^^^^

                step: Record[condition: Bool]
                  --> main.hop (line 2, col 7)
                 1 | <execute-step {step: {condition: Bool}}>
                 2 |     <if {step.condition}>
                   |          ^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 6, col 20)
                 5 | 
                 6 | <execute-workflow {workflow: {enabled: Bool, steps: Array[{condition: Bool}]}}>
                   |                    ^^^^^^^^

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
                  --> main.hop (line 9, col 25)
                 8 |         <for {step in workflow.steps}>
                 9 |             <execute-step {step: step}/>
                   |                                  ^^^^

                step: Record[condition: Bool]
                  --> main.hop (line 9, col 25)
                 8 |         <for {step in workflow.steps}>
                 9 |             <execute-step {step: step}/>
                   |                                  ^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 7, col 7)
                 6 | <execute-workflow {workflow: {enabled: Bool, steps: Array[{condition: Bool}]}}>
                 7 |     <if {workflow.enabled}>
                   |          ^^^^^^^^

                params: Record[
                  workflows: Array[Record[
                    enabled: Bool,
                    steps: Array[Record[condition: Bool]],
                  ]],
                ]
                  --> main.hop (line 14, col 13)
                13 | 
                14 | <main-comp {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                   |             ^^^^^^

                params: Record[
                  workflows: Array[Record[
                    enabled: Bool,
                    steps: Array[Record[condition: Bool]],
                  ]],
                ]
                  --> main.hop (line 15, col 20)
                14 | <main-comp {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                15 |     <for {workflow in params.workflows}>
                   |                       ^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 15, col 8)
                14 | <main-comp {params: {workflows: Array[{enabled: Bool, steps: Array[{condition: Bool}]}]}}>
                15 |     <for {workflow in params.workflows}>
                   |           ^^^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 16, col 32)
                15 |     <for {workflow in params.workflows}>
                16 |         <execute-workflow {workflow: workflow}/>
                   |                                      ^^^^^^^^

                workflow: Record[enabled: Bool, steps: Array[Record[condition: Bool]]]
                  --> main.hop (line 16, col 32)
                15 |     <for {workflow in params.workflows}>
                16 |         <execute-workflow {workflow: workflow}/>
                   |                                      ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_two_component_chain() {
        check(
            indoc! {r#"
                -- main.hop --
                <foo-comp {p: Bool}>
                  <if {p}>
                  </if>
                </foo-comp>

                <main-comp {i: Bool}>
                  <foo-comp {p: i}/>
                </main-comp>
            "#},
            expect![[r#"
                p: Bool
                  --> main.hop (line 1, col 12)
                1 | <foo-comp {p: Bool}>
                  |            ^

                p: Bool
                  --> main.hop (line 2, col 8)
                1 | <foo-comp {p: Bool}>
                2 |   <if {p}>
                  |        ^

                i: Bool
                  --> main.hop (line 6, col 13)
                5 | 
                6 | <main-comp {i: Bool}>
                  |             ^

                i: Bool
                  --> main.hop (line 7, col 17)
                6 | <main-comp {i: Bool}>
                7 |   <foo-comp {p: i}/>
                  |                 ^

                p: Bool
                  --> main.hop (line 7, col 17)
                6 | <main-comp {i: Bool}>
                7 |   <foo-comp {p: i}/>
                  |                 ^
            "#]],
        );
    }

    #[test]
    fn test_process_item_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                <process-item {
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
                </process-item>

                <main-comp {params: {
                    items: Array[{children: Array[{visible: Bool}],
                    status: {active: Bool}}],
                }}>
                	<for {item in params.items}>
                		<process-item {item: item}/>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 2, col 5)
                 1 | <process-item {
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
                  --> main.hop (line 15, col 13)
                14 | 
                15 | <main-comp {params: {
                   |             ^^^^^^

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
                  --> main.hop (line 20, col 24)
                19 |     <for {item in params.items}>
                20 |         <process-item {item: item}/>
                   |                              ^^^^

                item: Record[
                  children: Array[Record[visible: Bool]],
                  status: Record[active: Bool],
                ]
                  --> main.hop (line 20, col 24)
                19 |     <for {item in params.items}>
                20 |         <process-item {item: item}/>
                   |                              ^^^^
            "#]],
        );
    }

    #[test]
    fn test_expr_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: {url: String, theme: String}}>
                  <a href={user.url} class={user.theme}>Link</a>
                </main-comp>
            "#},
            expect![[r#"
                user: Record[theme: String, url: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {user: {url: String, theme: String}}>
                  |             ^^^^

                user: Record[theme: String, url: String]
                  --> main.hop (line 2, col 29)
                1 | <main-comp {user: {url: String, theme: String}}>
                2 |   <a href={user.url} class={user.theme}>Link</a>
                  |                             ^^^^

                user: Record[theme: String, url: String]
                  --> main.hop (line 2, col 12)
                1 | <main-comp {user: {url: String, theme: String}}>
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
                <main-comp>
                    <strong>
                        <slot-default/>
                    </strong>
                </main-comp>

                <bar-comp>
                    <main-comp>
                        Here's the content for the default slot
                    </main-comp>
                </bar-comp>
            "#},
            expect![""],
        );
    }

    #[test]
    fn test_simple_text_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {data: {message: String}}>
                  <div>{data.message}
                  </div>
                </main-comp>
            "#},
            expect![[r#"
                data: Record[message: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {data: {message: String}}>
                  |             ^^^^

                data: Record[message: String]
                  --> main.hop (line 2, col 9)
                1 | <main-comp {data: {message: String}}>
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
                <main-comp {params: {role: String}}>
                  <if {params.role == "admin"}>
                    <div>Admin</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[role: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {role: String}}>
                  |             ^^^^^^

                params: Record[role: String]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: {role: String}}>
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
                <main-comp {params: Array[Array[Array[Bool]]]}>
                	<for {level1 in params}>
                		<for {level2 in level1}>
                			<for {level3 in level2}>
                				<if {level3}>
                				</if>
                			</for>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                params: Array[Array[Array[Bool]]]
                  --> main.hop (line 1, col 13)
                 1 | <main-comp {params: Array[Array[Array[Bool]]]}>
                   |             ^^^^^^

                params: Array[Array[Array[Bool]]]
                  --> main.hop (line 2, col 18)
                 1 | <main-comp {params: Array[Array[Array[Bool]]]}>
                 2 |     <for {level1 in params}>
                   |                     ^^^^^^

                level1: Array[Array[Bool]]
                  --> main.hop (line 2, col 8)
                 1 | <main-comp {params: Array[Array[Array[Bool]]]}>
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
                <main-comp {params: Array[String]}>
                	<for {x in params}>
                		{x}
                	</for>
                	<for {y in params.foo}>
                		{y}
                	</for>
                </main-comp>
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
                <main-comp {data: {message: String}}>
                	<hop-x-raw>foo bar</hop-x-raw>
                	<div>{data.message}</div>
                </main-comp>
            "#},
            expect![[r#"
                data: Record[message: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {data: {message: String}}>
                  |             ^^^^

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
                <main-comp>
                	<hop-x-raw>
                		<div>some html</div>
                		<p>more content</p>
                	</hop-x-raw>
                </main-comp>
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
                <main-comp>
                	<hop-x-raw>
                		<undefined-component {nonexistent.field}>
                			<another-undefined {also.nonexistent} />
                		</undefined-component>
                	</hop-x-raw>
                </main-comp>
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
                <main-comp {user: {is_active: Bool}}>
                  <if {user.is_active}>
                    <div>User is active</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                user: Record[is_active: Bool]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {user: {is_active: Bool}}>
                  |             ^^^^

                user: Record[is_active: Bool]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {user: {is_active: Bool}}>
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
                <main-comp {data: {status: String}}>
                  <if {data.status == "approved"}>
                    <div>Status is approved</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                data: Record[status: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {data: {status: String}}>
                  |             ^^^^

                data: Record[status: String]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {data: {status: String}}>
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
                <main-comp>
                  <if {1 == "approved"}>
                    <div>Status is approved</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                error: Can not compare Int to String
                  --> main.hop (line 2, col 8)
                1 | <main-comp>
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
                <main-comp {config: {enabled: Bool, debug: Bool}}>
                  <if {config.enabled}>
                    <div>Feature enabled</div>
                    <if {config.debug}>
                      <div>Debug mode on</div>
                    </if>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                config: Record[debug: Bool, enabled: Bool]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {config: {enabled: Bool, debug: Bool}}>
                  |             ^^^^^^

                config: Record[debug: Bool, enabled: Bool]
                  --> main.hop (line 4, col 10)
                3 |     <div>Feature enabled</div>
                4 |     <if {config.debug}>
                  |          ^^^^^^

                config: Record[debug: Bool, enabled: Bool]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {config: {enabled: Bool, debug: Bool}}>
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
                <main-comp {config: {debug: Bool}}>
                  <if {config.debug}>
                    <div>Debug mode on</div>
                  </if>
                </main-comp>
                <foo-comp>
                  <main-comp {config: 1}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Argument 'config' of type Int is incompatible with expected type Record[debug: Bool]
                  --> main.hop (line 7, col 23)
                6 | <foo-comp>
                7 |   <main-comp {config: 1}/>
                  |                       ^
            "#]],
        );
    }

    // Test hop_mode global variable is available and has String type.
    #[test]
    fn test_hop_mode_global_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div>{hop_mode}</div>
                </main-comp>
            "#},
            expect![[r#"
                hop_mode: String
                  --> main.hop (line 2, col 9)
                1 | <main-comp>
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
                <main-comp>
                  <if {hop_mode == "dev"}>
                    <div>Development mode</div>
                  </if>
                  <if {hop_mode == "build"}>
                    <div>Build mode</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                hop_mode: String
                  --> main.hop (line 2, col 8)
                1 | <main-comp>
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
                <main-comp {params: {mode: String}}>
                  <if {params.mode == hop_mode}>
                    <div>Mode matches</div>
                  </if>
                </main-comp>
            "#},
            expect![[r#"
                params: Record[mode: String]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {params: {mode: String}}>
                  |             ^^^^^^

                params: Record[mode: String]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {params: {mode: String}}>
                2 |   <if {params.mode == hop_mode}>
                  |        ^^^^^^

                hop_mode: String
                  --> main.hop (line 2, col 23)
                1 | <main-comp {params: {mode: String}}>
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
                <main-comp {params: {foo: String}}>
                  <if {params.foo == "foo"}>
                    eq 1
                  </if>
                  <if {params == params}>
                    eq 2
                  </if>
                </main-comp>
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
                <main-comp {data: Array[{title: String, items: Array[String]}]}>
                	<for {section in data}>
                		<h1>{section.title}</h1>
                		<for {item in section.items}>
                			<div>{item}</div>
                		</for>
                	</for>
                </main-comp>
            "#},
            expect![[r#"
                data: Array[Record[items: Array[String], title: String]]
                  --> main.hop (line 1, col 13)
                1 | <main-comp {data: Array[{title: String, items: Array[String]}]}>
                  |             ^^^^

                data: Array[Record[items: Array[String], title: String]]
                  --> main.hop (line 2, col 19)
                1 | <main-comp {data: Array[{title: String, items: Array[String]}]}>
                2 |     <for {section in data}>
                  |                      ^^^^

                section: Record[items: Array[String], title: String]
                  --> main.hop (line 2, col 8)
                1 | <main-comp {data: Array[{title: String, items: Array[String]}]}>
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
                <string-comp {message: String}>
                	<div>{message}</div>
                </string-comp>
                <main-comp>
                	<string-comp {message: 42}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'message' of type Int is incompatible with expected type String
                  --> main.hop (line 5, col 25)
                4 | <main-comp>
                5 |     <string-comp {message: 42}/>
                  |                            ^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_object() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: {name: String, age: String}}>
                	<div>{user.name} ({user.age})</div>
                </user-comp>
                <main-comp>
                	<user-comp {user: "invalid"}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'user' of type String is incompatible with expected type Record[age: String, name: String]
                  --> main.hop (line 5, col 20)
                4 | <main-comp>
                5 |     <user-comp {user: "invalid"}/>
                  |                       ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_correct_object_passing() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: {name: String, active: String}}>
                	<div>{user.name}: {user.active}</div>
                </user-comp>
                <main-comp {data: {profile: {name: String, active: String}}}>
                	<user-comp {user: data.profile}/>
                </main-comp>
            "#},
            expect![[r#"
                user: Record[active: String, name: String]
                  --> main.hop (line 1, col 13)
                1 | <user-comp {user: {name: String, active: String}}>
                  |             ^^^^

                user: Record[active: String, name: String]
                  --> main.hop (line 2, col 8)
                1 | <user-comp {user: {name: String, active: String}}>
                2 |     <div>{user.name}: {user.active}</div>
                  |           ^^^^

                user: Record[active: String, name: String]
                  --> main.hop (line 2, col 21)
                1 | <user-comp {user: {name: String, active: String}}>
                2 |     <div>{user.name}: {user.active}</div>
                  |                        ^^^^

                data: Record[profile: Record[active: String, name: String]]
                  --> main.hop (line 4, col 13)
                3 | </user-comp>
                4 | <main-comp {data: {profile: {name: String, active: String}}}>
                  |             ^^^^

                data: Record[profile: Record[active: String, name: String]]
                  --> main.hop (line 5, col 20)
                4 | <main-comp {data: {profile: {name: String, active: String}}}>
                5 |     <user-comp {user: data.profile}/>
                  |                       ^^^^

                user: Record[active: String, name: String]
                  --> main.hop (line 5, col 20)
                4 | <main-comp {data: {profile: {name: String, active: String}}}>
                5 |     <user-comp {user: data.profile}/>
                  |                       ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_string() {
        check(
            indoc! {r#"
                -- main.hop --
                <new-comp {user: {name: String}}>
                	<div>{user.name}</div>
                </new-comp>
                <main-comp>
                	<new-comp {user: "invalid"}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'user' of type String is incompatible with expected type Record[name: String]
                  --> main.hop (line 5, col 19)
                4 | <main-comp>
                5 |     <new-comp {user: "invalid"}/>
                  |                      ^^^^^^^^^
            "#]],
        );
    }

    // Component with explicit object parameter type.
    #[test]
    fn test_explicit_object_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <user-comp {user: {name: String}}>
                	<div>{user.name}</div>
                </user-comp>
            "#},
            expect![[r#"
                user: Record[name: String]
                  --> main.hop (line 1, col 13)
                1 | <user-comp {user: {name: String}}>
                  |             ^^^^

                user: Record[name: String]
                  --> main.hop (line 2, col 8)
                1 | <user-comp {user: {name: String}}>
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
                <list-comp {items: Array[String]}>
                	<for {item in items}>
                		<div>{item}</div>
                	</for>
                </list-comp>
            "#},
            expect![[r#"
                items: Array[String]
                  --> main.hop (line 1, col 13)
                1 | <list-comp {items: Array[String]}>
                  |             ^^^^^

                items: Array[String]
                  --> main.hop (line 2, col 16)
                1 | <list-comp {items: Array[String]}>
                2 |     <for {item in items}>
                  |                   ^^^^^

                item: String
                  --> main.hop (line 2, col 8)
                1 | <list-comp {items: Array[String]}>
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
                <toggle-comp {enabled: Bool}>
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                </toggle-comp>
            "#},
            expect![[r#"
                enabled: Bool
                  --> main.hop (line 1, col 15)
                1 | <toggle-comp {enabled: Bool}>
                  |               ^^^^^^^

                enabled: Bool
                  --> main.hop (line 2, col 7)
                1 | <toggle-comp {enabled: Bool}>
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
                <counter-comp {count: Float}>
                	<if {count == 0}>
                		<div>Zero</div>
                	</if>
                </counter-comp>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> main.hop (line 2, col 7)
                1 | <counter-comp {count: Float}>
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
                <profile-comp {profile: {user: {name: String, age: Float}}}>
                	<div>{profile.user.name}</div>
                	<if {profile.user.age == 25}>
                		<div>Quarter century</div>
                	</if>
                </profile-comp>
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
                <matrix-comp {matrix: Array[Array[Float]]}>
                	<for {row in matrix}>
                		<for {cell in row}>
                			<if {cell == 1}>
                				<span>One</span>
                			</if>
                		</for>
                	</for>
                </matrix-comp>
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
                <card-comp {data: {
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
                </card-comp>
            "#},
            expect![[r#"
                data: Record[
                  content: String,
                  metadata: Record[author: String, published: Bool],
                  tags: Array[String],
                  title: String,
                ]
                  --> main.hop (line 1, col 13)
                 1 | <card-comp {data: {
                   |             ^^^^

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
                <toggle-comp {enabled: Bool}>
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                </toggle-comp>
                <main-comp>
                	<toggle-comp {enabled: "not a boolean"}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'enabled' of type String is incompatible with expected type Bool
                  --> main.hop (line 7, col 25)
                6 | <main-comp>
                7 |     <toggle-comp {enabled: "not a boolean"}/>
                  |                            ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Multi-file component with complex parameter types.
    #[test]
    fn test_multi_file_complex_parameters() {
        check(
            indoc! {r#"
                -- item-display.hop --
                <item-display {item: {id: Float, name: String, active: Bool}}>
                	<div>{item.name}</div>
                	<if {item.active}>
                		<span>Active item</span>
                	</if>
                	<if {item.id == 1}>
                		<span>First item</span>
                	</if>
                </item-display>
                -- data-list.hop --
                <import component="item-display" from="@/item-display">
                <data-list {items: Array[{id: Float, name: String, active: Bool}]}>
                	<for {item in items}>
                		<item-display {item: item}/>
                	</for>
                </data-list>
                -- main.hop --
                <import component="data-list" from="@/data-list">
                <main-comp {items: Array[{id: Float, name: String, active: Bool}]}>
                	<data-list {items: items}/>
                </main-comp>
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
                <needs-a {data: {a: String}}>
                	<div>{data.a}</div>
                </needs-a>

                <main-comp {params: {data: {a: String, b: String}}}>
                	<needs-a {data: params.data}/>
                </main-comp>
            "#},
            expect![[r#"
                data: Record[a: String]
                  --> main.hop (line 1, col 11)
                1 | <needs-a {data: {a: String}}>
                  |           ^^^^

                data: Record[a: String]
                  --> main.hop (line 2, col 8)
                1 | <needs-a {data: {a: String}}>
                2 |     <div>{data.a}</div>
                  |           ^^^^

                params: Record[data: Record[a: String, b: String]]
                  --> main.hop (line 5, col 13)
                4 | 
                5 | <main-comp {params: {data: {a: String, b: String}}}>
                  |             ^^^^^^

                params: Record[data: Record[a: String, b: String]]
                  --> main.hop (line 6, col 18)
                5 | <main-comp {params: {data: {a: String, b: String}}}>
                6 |     <needs-a {data: params.data}/>
                  |                     ^^^^^^

                data: Record[a: String, b: String]
                  --> main.hop (line 6, col 18)
                5 | <main-comp {params: {data: {a: String, b: String}}}>
                6 |     <needs-a {data: params.data}/>
                  |                     ^^^^^^^^^^^
            "#]],
        );
    }

    // Typechecker should print an error if an object lacks required properties.
    #[test]
    fn test_structural_subtyping_missing_property() {
        check(
            indoc! {r#"
                -- main.hop --
                <needs-a {data: {a: String}}>
                	<div>{data.a}</div>
                </needs-a>

                <main-comp {params: {data: {b: String}}}>
                	<needs-a {data: params.data}/>
                </main-comp>
            "#},
            expect![[r#"
                error: Argument 'data' of type Record[b: String] is incompatible with expected type Record[a: String]
                  --> main.hop (line 6, col 18)
                5 | <main-comp {params: {data: {b: String}}}>
                6 |     <needs-a {data: params.data}/>
                  |                     ^^^^^^^^^^^
            "#]],
        );
    }

    // Using a condition that is not a Bool should produce an error
    #[test]
    fn test_if_condition_must_be_boolean() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-component>
                    <if {"str"}>
                      is str?
                    </if>
                </main-component>
            "#},
            expect![[r#"
                error: Expected boolean condition, got String
                  --> main.hop (line 2, col 10)
                1 | <main-component>
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
                <main-comp {a: String}>
                  {a}
                </main-comp>
                <foo-comp>
                    <main-comp {a: "", b: 1}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Unexpected argument 'b'
                  --> main.hop (line 5, col 24)
                4 | <foo-comp>
                5 |     <main-comp {a: "", b: 1}/>
                  |                        ^
            "#]],
        );
    }

    // Type errors in argument list should be reported
    #[test]
    fn test_type_errors_in_argument_list() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp {a: String, b: String}>
                  {a} {b}
                </main-comp>
                <foo-comp>
                    <main-comp {a: 1 == "", b: 1 == ""}/>
                </foo-comp>
            "#},
            expect![[r#"
                error: Can not compare Int to String
                  --> main.hop (line 5, col 20)
                4 | <foo-comp>
                5 |     <main-comp {a: 1 == "", b: 1 == ""}/>
                  |                    ^^^^^^^

                error: Can not compare Int to String
                  --> main.hop (line 5, col 32)
                4 | <foo-comp>
                5 |     <main-comp {a: 1 == "", b: 1 == ""}/>
                  |                                ^^^^^^^
            "#]],
        );
    }

    // Trying to iterate over an empty array should produce an error
    #[test]
    fn test_iterate_over_empty_array_error() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-component>
                    <for {x in []}>
                      ?
                    </for>
                </main-component>
            "#},
            expect![[r#"
                error: Cannot iterate over an empty array with unknown element type
                  --> main.hop (line 2, col 16)
                1 | <main-component>
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
                <main-component>
                    {false}
                </main-component>
            "#},
            expect![[r#"
                error: Expected string for text expression, got Bool
                  --> main.hop (line 2, col 5)
                1 | <main-component>
                2 |     {false}
                  |     ^^^^^^^
            "#]],
        );
    }

    // Test that duplicate entrypoints with different component names but same entrypoint are detected
    #[test]
    fn test_duplicate_entrypoint_same_name_different_modules() {
        check(
            indoc! {r#"
                -- main.hop --
                <app-main entrypoint>
                    <div>First entrypoint</div>
                </app-main>
                
                -- other.hop --
                <app-main entrypoint>
                    <div>Duplicate entrypoint</div>
                </app-main>
            "#},
            expect![[r#"
                error: Duplicate entrypoint: component 'app-main' in module 'other' is already defined as an entrypoint in module 'main'
                  --> other.hop (line 1, col 2)
                1 | <app-main entrypoint>
                  |  ^^^^^^^^
            "#]],
        );
    }

    // Test that duplicate entrypoints across different modules are detected
    #[test]
    fn test_duplicate_entrypoint_different_modules() {
        check(
            indoc! {r#"
                -- module1.hop --
                <app-comp entrypoint>
                    <div>Module 1 entrypoint</div>
                </app-comp>
                
                -- module2.hop --
                <app-comp entrypoint>
                    <div>Module 2 entrypoint</div>
                </app-comp>
            "#},
            expect![[r#"
                error: Duplicate entrypoint: component 'app-comp' in module 'module2' is already defined as an entrypoint in module 'module1'
                  --> module2.hop (line 1, col 2)
                1 | <app-comp entrypoint>
                  |  ^^^^^^^^
            "#]],
        );
    }

    // Test that non-entrypoint components with the same name are allowed
    #[test]
    fn test_same_name_non_entrypoint_allowed() {
        check(
            indoc! {r#"
                -- module1.hop --
                <card-comp>
                    <div>Module 1 card</div>
                </card-comp>
                
                -- module2.hop --
                <card-comp>
                    <div>Module 2 card</div>
                </card-comp>
            "#},
            expect![""],
        );
    }

    // Test mixed entrypoint and non-entrypoint with same name
    #[test]
    fn test_entrypoint_and_non_entrypoint_same_name() {
        check(
            indoc! {r#"
                -- module1.hop --
                <main-comp entrypoint>
                    <div>Entrypoint component</div>
                </main-comp>
                
                -- module2.hop --
                <main-comp>
                    <div>Regular component</div>
                </main-comp>
            "#},
            expect![""],
        );
    }

    // Test multiple entrypoints with different names are allowed
    #[test]
    fn test_multiple_different_entrypoints_allowed() {
        check(
            indoc! {r#"
                -- module1.hop --
                <first-app entrypoint>
                    <div>First app</div>
                </first-app>
                
                -- module2.hop --
                <second-app entrypoint>
                    <div>Second app</div>
                </second-app>
            "#},
            expect![""],
        );
    }
}
