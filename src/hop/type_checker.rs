use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::parser::RecordField;
use crate::dop::{self, Argument, Parameter, Type, resolve_type};
use crate::error_collector::ErrorCollector;
use crate::hop::ast::Ast;
use crate::hop::ast::{Attribute, ComponentDefinition};
use crate::hop::environment::Environment;
use crate::hop::type_error::TypeError;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{self, Display};

use super::ast::{AttributeValue, TypedAst, TypedAttribute, TypedRecord, UntypedAst};
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
    // Track the resolved parameter types for the component (name, type).
    parameter_types: Vec<(String, Type)>,
}

#[derive(Debug, Clone, Default)]
pub struct ModuleTypeInformation {
    components: HashMap<String, ComponentTypeInformation>,
    typed_records: HashMap<String, dop::RecordDeclaration<Type>>,
}

impl ModuleTypeInformation {
    fn get_parameter_types(&self, component_name: &str) -> Option<&[(String, Type)]> {
        let params = &self.components.get(component_name)?.parameter_types;
        if params.is_empty() {
            None
        } else {
            Some(params.as_slice())
        }
    }

    /// Check if the component accepts children (has a `children: TrustedHTML` parameter)
    fn component_accepts_children(&self, component_name: &str) -> bool {
        self.components.get(component_name).is_some_and(|c| {
            c.parameter_types
                .iter()
                .any(|(name, typ)| name == "children" && *typ == Type::TrustedHTML)
        })
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

    fn record_is_declared(&self, record_name: &str) -> bool {
        self.typed_records.contains_key(record_name)
    }

    fn get_typed_record(&self, record_name: &str) -> Option<&dop::RecordDeclaration<Type>> {
        self.typed_records.get(record_name)
    }

    fn set_typed_record(&mut self, record_name: &str, record: dop::RecordDeclaration<Type>) {
        self.typed_records.insert(record_name.to_string(), record);
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

    fn set_typed_record(
        &mut self,
        module_name: &ModuleName,
        record_name: &str,
        record: dop::RecordDeclaration<Type>,
    ) {
        self.modules
            .entry(module_name.clone())
            .or_default()
            .set_typed_record(record_name, record);
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
                        import_node.from.clone(),
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
    // Build records environment - start with imported records
    let mut records_env: Environment<Type> = Environment::new();

    // Validate imports and collect imported records
    for import in module.get_imports() {
        let imported_module = import.imported_module();
        let imported_name = import.imported_component();
        let Some(module_state) = state.modules.get(imported_module) else {
            errors.push(TypeError::ModuleNotFound {
                module: imported_module.as_str().to_string(),
                range: import.from.clone(),
            });
            continue;
        };

        // Check if the import is a component or a record
        let is_component = module_state.component_is_declared(imported_name.as_str());
        let is_record = module_state.record_is_declared(imported_name.as_str());

        if !is_component && !is_record {
            errors.push(TypeError::UndeclaredComponent {
                module_name: import.from.clone(),
                component_name: imported_name.clone(),
            });
        }

        // If it's a record, add it to our available records
        if is_record {
            if let Some(record) = module_state.get_typed_record(imported_name.as_str()) {
                let record_type = Type::Record {
                    module: imported_module.clone(),
                    name: imported_name.as_str().to_string(),
                    fields: record
                        .fields
                        .iter()
                        .map(|f| (f.name.clone(), f.field_type.clone()))
                        .collect(),
                };
                let _ = records_env.push(imported_name.as_str().to_string(), record_type);
            }
        }
    }

    // Validate record field types and build typed records
    let mut typed_records: Vec<TypedRecord> = Vec::new();
    for record in module.get_records() {
        let record_name = record.name();
        let mut typed_fields = Vec::new();
        let mut has_errors = false;

        for field in &record.declaration.fields {
            match resolve_type(&field.field_type, &mut records_env) {
                Ok(resolved_type) => {
                    typed_fields.push(RecordField {
                        name: field.name.clone(),
                        name_range: field.name_range.clone(),
                        field_type: resolved_type,
                    });
                }
                Err(e) => {
                    errors.push(e.into());
                    has_errors = true;
                }
            }
        }

        // Only add record if all fields resolved successfully
        if !has_errors {
            let typed_decl = dop::RecordDeclaration {
                name: record.declaration.name.clone(),
                fields: typed_fields.clone(),
            };
            // Store typed record in state so other modules can import it
            state.set_typed_record(&module.name, record.name(), typed_decl.clone());
            typed_records.push(TypedRecord {
                declaration: typed_decl,
                range: record.range.clone(),
            });
            // Add this record to records_env so subsequent records can reference it
            let record_type = Type::Record {
                module: module.name.clone(),
                name: record_name.to_string(),
                fields: typed_fields
                    .iter()
                    .map(|f| (f.name.clone(), f.field_type.clone()))
                    .collect(),
            };
            let _ = records_env.push(record_name.to_string(), record_type);
        }
    }

    let mut env = Environment::new();

    // Build typed component definitions
    let mut typed_component_definitions = Vec::new();

    for component_def in module.get_component_definitions() {
        let ComponentDefinition {
            component_name,
            tag_name: name,
            params,
            children,
            range,
            closing_tag_name,
        } = component_def;

        // Push parameters to environment and validate their types
        // Track which parameters were successfully pushed for later popping
        let mut pushed_params: Vec<&Parameter> = Vec::new();
        // Collect resolved parameter types for ComponentTypeInformation
        let mut resolved_param_types: Vec<(String, Type)> = Vec::new();
        // Build typed parameters with resolved type annotations
        let mut typed_params: Vec<Parameter<Type>> = Vec::new();
        if let Some((params, _)) = params {
            for param in params {
                match resolve_type(&param.var_type, &mut records_env) {
                    Ok(param_type) => {
                        annotations.push(TypeAnnotation {
                            range: param.var_name_range.clone(),
                            typ: param_type.clone(),
                            name: param.var_name.to_string(),
                        });
                        let _ = env.push(param.var_name.to_string(), param_type.clone());
                        pushed_params.push(param);
                        resolved_param_types.push((param.var_name.to_string(), param_type.clone()));
                        typed_params.push(Parameter {
                            var_name: param.var_name.clone(),
                            var_name_range: param.var_name_range.clone(),
                            var_type: param_type,
                        });
                    }
                    Err(e) => {
                        errors.push(e.into());
                    }
                }
            }
        }

        // Typecheck children and collect typed versions
        let typed_children: Vec<_> = children
            .iter()
            .filter_map(|child| {
                typecheck_node(
                    child,
                    state,
                    &mut env,
                    annotations,
                    errors,
                    &mut records_env,
                )
            })
            .collect();

        // Pop parameters from environment (only the ones that were pushed)
        for param in pushed_params.iter().rev() {
            let (_, _, accessed) = env.pop();
            if !accessed {
                errors.push(TypeError::UnusedVariable {
                    var_name: param.var_name_range.clone(),
                })
            }
        }

        // Store type information in state
        state.set_type_info(
            &module.name,
            name.as_str(),
            ComponentTypeInformation {
                parameter_types: resolved_param_types,
            },
        );

        // Build typed ComponentDefinition with resolved parameter types
        let typed_params_option = if params.is_some() {
            Some((typed_params, params.as_ref().unwrap().1.clone()))
        } else {
            None
        };
        typed_component_definitions.push(ComponentDefinition {
            component_name: component_name.clone(),
            tag_name: name.clone(),
            closing_tag_name: closing_tag_name.clone(),
            params: typed_params_option,
            range: range.clone(),
            children: typed_children,
        });
    }

    // Build and return the typed AST
    Ast::new(
        module.name.clone(),
        typed_component_definitions,
        module.get_imports().to_vec(),
        typed_records,
    )
}

fn typecheck_node(
    node: &UntypedNode,
    state: &State,
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut ErrorCollector<TypeError>,
    records: &mut Environment<Type>,
) -> Option<TypedNode> {
    match node {
        Node::If {
            condition,
            children,
            range,
        } => {
            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors, records))
                .collect();

            let typed_condition = errors.ok_or_add(
                dop::typecheck_expr(condition, env, records, annotations, None).map_err(Into::into),
            )?;

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
            let typed_array = errors.ok_or_add(
                dop::typecheck_expr(array_expr, env, records, annotations, None)
                    .map_err(Into::into),
            )?;
            let array_type = typed_array.as_type();
            let element_type = match &array_type {
                Type::Array(inner) => (**inner).clone(),
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
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors, records))
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
            component_name,
            component_name_opening_range: tag_name,
            definition_module,
            component_name_closing_range: closing_tag_name,
            args,
            children,
            range,
        } => {
            // Transform children
            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors, records))
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

            // Validate that content is only passed to components with children: TrustedHTML parameter
            if !children.is_empty()
                && !module_info.component_accepts_children(component_name.as_str())
            {
                errors.push(TypeError::ComponentDoesNotAcceptChildren {
                    component: component_name.as_str().to_string(),
                    range: tag_name.clone(),
                });
            }

            // Check if children: TrustedHTML is required but not provided
            let accepts_children = module_info.component_accepts_children(component_name.as_str());
            if accepts_children && children.is_empty() {
                errors.push(TypeError::MissingChildren {
                    component: component_name.as_str().to_string(),
                    range: tag_name.clone(),
                });
            }

            // Validate arguments and build typed versions
            // Filter out children parameter - it's handled separately via component children
            let params_without_children: Option<Vec<_>> = module_info
                .get_parameter_types(component_name.as_str())
                .map(|params| {
                    params
                        .iter()
                        .filter(|(name, _)| name != "children")
                        .cloned()
                        .collect()
                });

            let typed_args = match (params_without_children.as_deref(), args) {
                (None | Some([]), None) => None,
                (None | Some([]), Some((_, args_range))) => {
                    errors.push(TypeError::UnexpectedArguments {
                        range: args_range.clone(),
                    });
                    None
                }
                (Some(params), None) if !params.is_empty() => {
                    errors.push(TypeError::missing_arguments(params, tag_name.clone()));
                    None
                }
                (Some(_), None) => None, // params is empty, no args needed
                (Some(params), Some((args, args_range))) => {
                    let mut typed_arguments = Vec::new();
                    for (param_name, _) in params {
                        if !args
                            .iter()
                            .any(|a| a.var_name.as_str() == param_name.as_str())
                        {
                            errors.push(TypeError::MissingRequiredParameter {
                                param: param_name.clone(),
                                range: args_range.clone(),
                            });
                        }
                    }

                    // Get all params (including children) for type checking provided args
                    let all_params = module_info.get_parameter_types(component_name.as_str());

                    for arg in args {
                        // Skip children arg - it's handled separately via component children
                        if arg.var_name.as_str() == "children" {
                            errors.push(TypeError::ChildrenArgNotAllowed {
                                range: arg.var_name_range.clone(),
                            });
                            continue;
                        }

                        let (_, param_type) = match all_params.and_then(|p| {
                            p.iter()
                                .find(|(name, _)| name.as_str() == arg.var_name.as_str())
                        }) {
                            None => {
                                errors.push(TypeError::UnexpectedArgument {
                                    arg: arg.var_name.as_str().to_string(),
                                    range: arg.var_name_range.clone(),
                                });
                                continue;
                            }
                            Some(param) => param,
                        };

                        let typed_expr = match dop::typecheck_expr(
                            &arg.var_expr,
                            env,
                            records,
                            annotations,
                            Some(param_type),
                        ) {
                            Ok(t) => t,
                            Err(err) => {
                                errors.push(err.into());
                                continue;
                            }
                        };
                        let arg_type = typed_expr.as_type().clone();

                        // param_type is already resolved from the component's defining module
                        if !arg_type.is_subtype(param_type) {
                            errors.push(TypeError::ArgumentIsIncompatible {
                                expected: param_type.clone(),
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
                component_name: component_name.clone(),
                component_name_opening_range: tag_name.clone(),
                definition_module: definition_module.clone(),
                component_name_closing_range: closing_tag_name.clone(),
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
            let typed_attributes =
                typecheck_attributes(attributes, env, annotations, errors, records);

            let typed_children = children
                .iter()
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors, records))
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
            if let Some(typed_expr) = errors.ok_or_add(
                dop::typecheck_expr(expression, env, records, annotations, None)
                    .map_err(Into::into),
            ) {
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
                .filter_map(|child| typecheck_node(child, state, env, annotations, errors, records))
                .collect();

            Some(Node::Placeholder {
                range: range.clone(),
                children: typed_children,
            })
        }

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
    records: &mut Environment<Type>,
) -> BTreeMap<StringSpan, TypedAttribute> {
    let mut typed_attributes = BTreeMap::new();

    for (key, attr) in attributes {
        let typed_value = match &attr.value {
            Some(AttributeValue::Expressions(exprs)) => {
                let mut typed_exprs = Vec::new();
                for expr in exprs {
                    if let Some(typed_expr) = errors.ok_or_add(
                        dop::typecheck_expr(expr, env, records, annotations, None)
                            .map_err(Into::into),
                    ) {
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
            let module_name =
                ModuleName::new(file.name.trim_end_matches(".hop").to_string()).unwrap();
            let module = parse(module_name, source_code.to_string(), &mut parse_errors);

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

    #[test]
    fn should_accept_empty_file() {
        check("-- main.hop --", expect![[r#""#]]);
    }

    #[test]
    fn should_accept_component_definition_without_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                </Main>
            "#},
            expect![""],
        );
    }

    #[test]
    fn should_reject_when_an_undefined_component_is_referenced() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                	<h1>Hello,
                        <Bar>
                            <div></div>
                        </Bar>
                    </h1>
                </Main>
            "#},
            expect![[r#"
                error: Component Bar is not defined
                  --> main.hop (line 3, col 10)
                2 |     <h1>Hello,
                3 |         <Bar>
                  |          ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_component_references_itself() {
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

    #[test]
    fn should_reject_when_an_import_references_a_module_that_does_not_exist() {
        check(
            indoc! {r#"
                -- main.hop --
                import Foo from "@/other"

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module other was not found
                  --> main.hop (line 1, col 18)
                1 | import Foo from "@/other"
                  |                  ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_an_import_references_a_component_that_does_not_exist() {
        check(
            indoc! {r#"
                -- other.hop --
                <Bar>
                </Bar>
                -- main.hop --
                import Foo from "@/other"

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module @/other does not declare a component named Foo
                  --> main.hop (line 1, col 8)
                1 | import Foo from "@/other"
                  |        ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_a_component_to_be_imported_without_being_used() {
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

    #[test]
    fn should_collect_all_errors_rather_than_returning_at_the_first_error() {
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

    #[test]
    fn should_accept_components_in_different_modules_to_have_same_name() {
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

    #[test]
    fn should_reject_when_children_are_passed_to_component_that_does_not_accept_them() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <strong>No children parameter here</strong>
                </Main>

                <Bar>
                    <Main>
                        This component has no children parameter
                    </Main>
                </Bar>
            "#},
            expect![[r#"
                error: Component Main does not accept children (missing `children: TrustedHTML` parameter)
                  --> main.hop (line 6, col 6)
                5 | <Bar>
                6 |     <Main>
                  |      ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_children_are_passed_to_an_imported_component_that_does_not_accept_them() {
        check(
            indoc! {r#"
                -- other.hop --
                <Foo>
                    <strong>No children parameter here</strong>
                </Foo>
                -- main.hop --
                import Foo from "@/other"

                <Bar>
                    <Foo>
                        This component has no children parameter
                    </Foo>
                </Bar>
            "#},
            expect![[r#"
                error: Component Foo does not accept children (missing `children: TrustedHTML` parameter)
                  --> main.hop (line 4, col 6)
                3 | <Bar>
                4 |     <Foo>
                  |      ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_variable_shadows_a_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record Items {
                  foo: Array[String],
                }

                <Main {items: Items}>
                  <for {items in items.foo}>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 6, col 9)
                5 | <Main {items: Items}>
                6 |   <for {items in items.foo}>
                  |         ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_variable_shadows_another_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                record Items {
                  a: Array[String],
                  b: Array[String],
                }

                <Main {items: Items}>
                  <for {item in items.a}>
                    <for {item in items.b}>
                      <div>{item}</div>
                    </for>
                  </for>
                </Main>
            "#},
            expect![[r#"
                error: Variable item is already defined
                  --> main.hop (line 8, col 11)
                 7 |   <for {item in items.a}>
                 8 |     <for {item in items.b}>
                   |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_an_undefined_variable_is_referenced() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {
                  active: Bool,
                }

                <Main {params: Array[Item]}>
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
                  --> main.hop (line 10, col 7)
                 9 |     </for>
                10 |     <if {item.active}>
                   |          ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_loop_variable_is_unused() {
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

    #[test]
    fn should_reject_when_a_component_parameter_is_unused() {
        check(
            indoc! {r#"
                -- main.hop --
                <Bar {p: String}>
                  <div>
                  </div>
                </Bar>
            "#},
            expect![[r#"
                error: Unused variable p
                  --> main.hop (line 1, col 7)
                1 | <Bar {p: String}>
                  |       ^
            "#]],
        );
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

    #[test]
    fn should_accept_component_arguments_to_be_passed_in_any_order() {
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

    #[test]
    fn should_reject_when_a_component_is_missing_an_argument() {
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

    #[test]
    fn should_reject_when_a_component_is_passed_an_extra_argument() {
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

    #[test]
    fn should_reject_when_no_arguments_are_passed_to_component() {
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

    #[test]
    fn should_reject_when_arguments_are_passed_to_component_that_does_not_accept_them() {
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
    fn should_reject_when_iterating_over_a_boolean() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {
                  k: Bool
                }
                <Main {params: Array[Item]}>
                	<for {item in params}>
                		<if {item.k}>
                          ok!
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
                  --> main.hop (line 11, col 18)
                10 |     <for {item in params}>
                11 |         <for {inner in item.k}>
                   |                        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_component_with_string_parameter() {
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
    fn should_accept_component_with_bool_parameter() {
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

    #[test]
    fn should_accept_component_with_float_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <CounterComp {count: Float}>
                	<if {count == 0.0}>
                		<div>Zero</div>
                	</if>
                </CounterComp>
            "#},
            expect![[r#"
                count: Float
                  --> main.hop (line 1, col 15)
                1 | <CounterComp {count: Float}>
                  |               ^^^^^

                count: Float
                  --> main.hop (line 2, col 7)
                1 | <CounterComp {count: Float}>
                2 |     <if {count == 0.0}>
                  |          ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_component_with_record_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {
                  active: Bool,
                  name: Bool,
                }
                record Params {
                  items: Array[Item],
                }

                <Main {params: Params}>
                	<for {item in params.items}>
                		<if {item.active}>
                		</if>
                		<if {item.name}>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 9, col 8)
                 8 | 
                 9 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 10, col 16)
                 9 | <Main {params: Params}>
                10 |     <for {item in params.items}>
                   |                   ^^^^^^

                item: main::Item
                  --> main.hop (line 10, col 8)
                 9 | <Main {params: Params}>
                10 |     <for {item in params.items}>
                   |           ^^^^

                item: main::Item
                  --> main.hop (line 11, col 8)
                10 |     <for {item in params.items}>
                11 |         <if {item.active}>
                   |              ^^^^

                item: main::Item
                  --> main.hop (line 13, col 8)
                12 |         </if>
                13 |         <if {item.name}>
                   |              ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_component_with_array_parameter() {
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

    #[test]
    fn should_accept_deeply_nested_field_accesses() {
        check(
            indoc! {r#"
                -- main.hop --
                record Theme {dark: Bool}
                record UI {theme: Theme}
                record Users {enabled: Bool}
                record Endpoints {users: Users}
                record API {endpoints: Endpoints}
                record Connection {ssl: Bool}
                record Database {connection: Connection}
                record App {ui: UI, api: API, database: Database}
                record Params {app: App}
                <Main {params: Params}>
                	<if {params.app.ui.theme.dark}>
                      ok!
                	</if>
                	<if {params.app.api.endpoints.users.enabled}>
                      ok!
                	</if>
                	<if {params.app.database.connection.ssl}>
                      ok!
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 10, col 8)
                 9 | record Params {app: App}
                10 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 11, col 7)
                10 | <Main {params: Params}>
                11 |     <if {params.app.ui.theme.dark}>
                   |          ^^^^^^

                params: main::Params
                  --> main.hop (line 14, col 7)
                13 |     </if>
                14 |     <if {params.app.api.endpoints.users.enabled}>
                   |          ^^^^^^

                params: main::Params
                  --> main.hop (line 17, col 7)
                16 |     </if>
                17 |     <if {params.app.database.connection.ssl}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_strings_to_be_used_in_equals_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                record Params {
                  x: String,
                  y: String,
                }

                <Main {params: Params}>
                  <if {params.x == params.y}>
                    <div>Values are equal</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 6, col 8)
                 5 | 
                 6 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 7, col 8)
                 6 | <Main {params: Params}>
                 7 |   <if {params.x == params.y}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 7, col 20)
                 6 | <Main {params: Params}>
                 7 |   <if {params.x == params.y}>
                   |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_loops_to_use_same_variable_name() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {
                  a: Bool,
                  b: Bool,
                }

                <Main {params: Array[Item]}>
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
                params: Array[main::Item]
                  --> main.hop (line 6, col 8)
                 5 | 
                 6 | <Main {params: Array[Item]}>
                   |        ^^^^^^

                params: Array[main::Item]
                  --> main.hop (line 7, col 13)
                 6 | <Main {params: Array[Item]}>
                 7 |     <for {j in params}>
                   |                ^^^^^^

                j: main::Item
                  --> main.hop (line 7, col 8)
                 6 | <Main {params: Array[Item]}>
                 7 |     <for {j in params}>
                   |           ^

                j: main::Item
                  --> main.hop (line 8, col 8)
                 7 |     <for {j in params}>
                 8 |         <if {j.a}>
                   |              ^

                params: Array[main::Item]
                  --> main.hop (line 11, col 13)
                10 |     </for>
                11 |     <for {j in params}>
                   |                ^^^^^^

                j: main::Item
                  --> main.hop (line 11, col 8)
                10 |     </for>
                11 |     <for {j in params}>
                   |           ^

                j: main::Item
                  --> main.hop (line 12, col 8)
                11 |     <for {j in params}>
                12 |         <if {j.b}>
                   |              ^
            "#]],
        );
    }

    #[test]
    fn should_accept_iteration_over_array() {
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
    fn should_accept_iteration_over_nested_array() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {i: Array[Array[Bool]]}>
                	<for {j in i}>
                		<for {k in j}>
                			<if {k}>
                              ok!
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
    fn should_accept_components_to_call_each_other_in_a_chain() {
        check(
            indoc! {r#"
                -- bar.hop --
                record Config {
                  enabled: Bool,
                  title: String,
                }

                <WidgetComp {config: Config}>
                  <if {config.enabled}>
                    <div>{config.title}</div>
                  </if>
                </WidgetComp>

                -- foo.hop --
                import WidgetComp from "@/bar"
                import Config from "@/bar"

                record Data {
                  items: Array[Config],
                }

                <PanelComp {data: Data}>
                  <for {item in data.items}>
                    <WidgetComp {config: item}/>
                  </for>
                </PanelComp>

                -- main.hop --
                import PanelComp from "@/foo"
                import Data from "@/foo"

                record Dashboard {
                  items: Array[Data],
                }
                record Settings {
                  dashboard: Data,
                }

                <Main {settings: Settings}>
                  <PanelComp {data: settings.dashboard}/>
                </Main>
            "#},
            expect![[r#"
                config: bar::Config
                  --> bar.hop (line 6, col 14)
                 5 | 
                 6 | <WidgetComp {config: Config}>
                   |              ^^^^^^

                config: bar::Config
                  --> bar.hop (line 8, col 11)
                 7 |   <if {config.enabled}>
                 8 |     <div>{config.title}</div>
                   |           ^^^^^^

                config: bar::Config
                  --> bar.hop (line 7, col 8)
                 6 | <WidgetComp {config: Config}>
                 7 |   <if {config.enabled}>
                   |        ^^^^^^

                data: foo::Data
                  --> foo.hop (line 8, col 13)
                 7 | 
                 8 | <PanelComp {data: Data}>
                   |             ^^^^

                data: foo::Data
                  --> foo.hop (line 9, col 17)
                 8 | <PanelComp {data: Data}>
                 9 |   <for {item in data.items}>
                   |                 ^^^^

                item: bar::Config
                  --> foo.hop (line 9, col 9)
                 8 | <PanelComp {data: Data}>
                 9 |   <for {item in data.items}>
                   |         ^^^^

                item: bar::Config
                  --> foo.hop (line 10, col 26)
                 9 |   <for {item in data.items}>
                10 |     <WidgetComp {config: item}/>
                   |                          ^^^^

                config: bar::Config
                  --> foo.hop (line 10, col 26)
                 9 |   <for {item in data.items}>
                10 |     <WidgetComp {config: item}/>
                   |                          ^^^^

                settings: main::Settings
                  --> main.hop (line 11, col 8)
                10 | 
                11 | <Main {settings: Settings}>
                   |        ^^^^^^^^

                settings: main::Settings
                  --> main.hop (line 12, col 21)
                11 | <Main {settings: Settings}>
                12 |   <PanelComp {data: settings.dashboard}/>
                   |                     ^^^^^^^^

                data: foo::Data
                  --> main.hop (line 12, col 21)
                11 | <Main {settings: Settings}>
                12 |   <PanelComp {data: settings.dashboard}/>
                   |                     ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_different_types_with_same_name_are_used_in_place_of_eachother() {
        check(
            indoc! {r#"
                -- foo.hop --
                record User {
                    name: String,
                }

                <FooComp {user: User}>
                    <div>{user.name}</div>
                </FooComp>

                -- bar.hop --
                record User {
                    email: String,
                }

                <BarComp {user: User}>
                    <div>{user.email}</div>
                </BarComp>

                -- main.hop --
                import FooComp from "@/foo"
                import BarComp from "@/bar"
                import User from "@/foo"

                <Main {user: User}>
                    <FooComp {user: user}/>
                    <BarComp {user: user}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type foo::User is incompatible with expected type bar::User
                  --> main.hop (line 7, col 21)
                6 |     <FooComp {user: user}/>
                7 |     <BarComp {user: user}/>
                  |                     ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_identical_types_in_different_modules_are_used_in_place_of_eachother() {
        check(
            indoc! {r#"
                -- foo.hop --
                record User {
                    name: String,
                    age: Int,
                }

                <FooComp {user: User}>
                    <div>{user.name}</div>
                </FooComp>

                -- bar.hop --
                record User {
                    name: String,
                    age: Int,
                }

                <BarComp {user: User}>
                    <div>{user.name}</div>
                </BarComp>

                -- main.hop --
                import FooComp from "@/foo"
                import BarComp from "@/bar"
                import User from "@/foo"

                <Main {user: User}>
                    <FooComp {user: user}/>
                    <BarComp {user: user}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type foo::User is incompatible with expected type bar::User
                  --> main.hop (line 7, col 21)
                6 |     <FooComp {user: user}/>
                7 |     <BarComp {user: user}/>
                  |                     ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_expressions_to_be_used_as_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {url: String, theme: String}
                <Main {user: User}>
                  <a href={user.url} class={user.theme}>Link</a>
                </Main>
            "#},
            expect![[r#"
                user: main::User
                  --> main.hop (line 2, col 8)
                1 | record User {url: String, theme: String}
                2 | <Main {user: User}>
                  |        ^^^^

                user: main::User
                  --> main.hop (line 3, col 29)
                2 | <Main {user: User}>
                3 |   <a href={user.url} class={user.theme}>Link</a>
                  |                             ^^^^

                user: main::User
                  --> main.hop (line 3, col 12)
                2 | <Main {user: User}>
                3 |   <a href={user.url} class={user.theme}>Link</a>
                  |            ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_children_to_be_passed_to_component() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {children: TrustedHTML}>
                    <strong>
                        {children}
                    </strong>
                </Main>

                <Bar>
                    <Main>
                        Here's the content for the children
                    </Main>
                </Bar>
            "#},
            expect![[r#"
                children: TrustedHTML
                  --> main.hop (line 1, col 8)
                 1 | <Main {children: TrustedHTML}>
                   |        ^^^^^^^^

                children: TrustedHTML
                  --> main.hop (line 3, col 10)
                 2 |     <strong>
                 3 |         {children}
                   |          ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_field_access_is_performed_on_array() {
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
                error: Array[String] can not be used as a record
                  --> main.hop (line 5, col 13)
                4 |     </for>
                5 |     <for {y in params.foo}>
                  |                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_if_statement() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {is_active: Bool}
                <Main {user: User}>
                  <if {user.is_active}>
                    <div>User is active</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                user: main::User
                  --> main.hop (line 2, col 8)
                1 | record User {is_active: Bool}
                2 | <Main {user: User}>
                  |        ^^^^

                user: main::User
                  --> main.hop (line 3, col 8)
                2 | <Main {user: User}>
                3 |   <if {user.is_active}>
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_records_are_used_in_equals_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                record Params {
                  foo: String,
                }
                <Main {p1: Params, p2: Params}>
                  <if {p1 == p2}>
                    eq 2
                  </if>
                </Main>
            "#},
            expect![[r#"
                error: Type main::Params is not comparable
                  --> main.hop (line 5, col 8)
                4 | <Main {p1: Params, p2: Params}>
                5 |   <if {p1 == p2}>
                  |        ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_an_int_is_passed_to_component_that_accepts_string() {
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
    fn should_reject_when_a_string_is_passed_to_component_that_accepts_bool() {
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

    #[test]
    fn should_reject_when_non_bool_is_used_as_if_condition() {
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

    #[test]
    fn should_reject_when_a_type_error_occurs_in_an_argument_list() {
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

    #[test]
    fn should_reject_when_iterating_over_empty_array() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <for {x in []}>
                      not ok
                    </for>
                </Main>
            "#},
            expect![[r#"
                error: Cannot infer type of empty array
                  --> main.hop (line 2, col 16)
                1 | <Main>
                2 |     <for {x in []}>
                  |                ^^
            "#]],
        );
    }

    #[test]
    fn should_infer_type_of_empty_array_from_component_argument() {
        check(
            indoc! {r#"
                -- main.hop --
                <List {items: Array[String]}>
                    <for {item in items}>
                        <li>{item}</li>
                    </for>
                </List>
                <Main>
                    <List {items: []}/>
                </Main>
            "#},
            expect![[r#"
                items: Array[String]
                  --> main.hop (line 1, col 8)
                1 | <List {items: Array[String]}>
                  |        ^^^^^

                items: Array[String]
                  --> main.hop (line 2, col 19)
                1 | <List {items: Array[String]}>
                2 |     <for {item in items}>
                  |                   ^^^^^

                item: String
                  --> main.hop (line 2, col 11)
                1 | <List {items: Array[String]}>
                2 |     <for {item in items}>
                  |           ^^^^

                item: String
                  --> main.hop (line 3, col 14)
                2 |     <for {item in items}>
                3 |         <li>{item}</li>
                  |              ^^^^

                items: Array[String]
                  --> main.hop (line 7, col 19)
                6 | <Main>
                7 |     <List {items: []}/>
                  |                   ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_using_bool_in_text_expression() {
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

    #[test]
    fn should_reject_when_an_undefined_type_is_used_in_parameter_type() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {user: User}>
                    <div></div>
                </Main>
            "#},
            expect![[r#"
                error: Type 'User' is not defined
                  --> main.hop (line 1, col 14)
                1 | <Main {user: User}>
                  |              ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_an_undefined_type_is_used_in_nested_parameter_type() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {users: Array[User]}>
                    <div></div>
                </Main>
            "#},
            expect![[r#"
                error: Type 'User' is not defined
                  --> main.hop (line 1, col 21)
                1 | <Main {users: Array[User]}>
                  |                     ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_record_references_itself() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {
                  name: String,
                  friend: User,
                }

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Type 'User' is not defined
                  --> main.hop (line 3, col 11)
                2 |   name: String,
                3 |   friend: User,
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_referencing_a_record_defined_below() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {
                  address: Address,
                }
                record Address {
                  city: String,
                }

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Type 'Address' is not defined
                  --> main.hop (line 2, col 12)
                1 | record User {
                2 |   address: Address,
                  |            ^^^^^^^
            "#]],
        );
    }

    // Test that accessing a field on a record type works
    #[test]
    fn should_accept_accessing_a_record_field() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main {user: User}>
                    <div>{user.name}</div>
                </Main>
            "#},
            expect![[r#"
                user: main::User
                  --> main.hop (line 2, col 8)
                1 | record User {name: String}
                2 | <Main {user: User}>
                  |        ^^^^

                user: main::User
                  --> main.hop (line 3, col 11)
                2 | <Main {user: User}>
                3 |     <div>{user.name}</div>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_nonexistent_field_is_accessed_on_record() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main {user: User}>
                    <div>{user.email}</div>
                </Main>
            "#},
            expect![[r#"
                error: Field 'email' not found in record 'User'
                  --> main.hop (line 3, col 11)
                2 | <Main {user: User}>
                3 |     <div>{user.email}</div>
                  |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_access_of_nested_record_field() {
        check(
            indoc! {r#"
                -- main.hop --
                record Address {city: String}
                record User {name: String, address: Address}
                <Main {user: User}>
                    <div>{user.address.city}</div>
                </Main>
            "#},
            expect![[r#"
                user: main::User
                  --> main.hop (line 3, col 8)
                2 | record User {name: String, address: Address}
                3 | <Main {user: User}>
                  |        ^^^^

                user: main::User
                  --> main.hop (line 4, col 11)
                3 | <Main {user: User}>
                4 |     <div>{user.address.city}</div>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_not_reexport_imported_components() {
        check(
            indoc! {r#"
                -- foo.hop --
                record User {name: String}
                <Foo>
                </Foo>

                -- bar.hop --
                import User from "@/foo"
                <Bar>
                </Bar>

                -- baz.hop --
                import User from "@/bar"
                <Baz>
                </Baz>
            "#},
            expect![[r#"
                error: Module @/bar does not declare a component named User
                  --> baz.hop (line 1, col 8)
                1 | import User from "@/bar"
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_import_and_use_of_records_declared_in_other_modules() {
        check(
            indoc! {r#"
                -- foo.hop --
                record Address {
                  city: String,
                }

                <Foo>
                </Foo>
                -- bar.hop --
                import Address from "@/foo"

                record User {
                  name: String,
                  address: Address,
                }

                <Bar {user: User}>
                    <div>{user.address.city}</div>
                </Bar>
                -- baz.hop --
                import Bar from "@/bar"
                import User from "@/bar"
                import Address from "@/foo"
                <Baz>
                    <Bar {user: User(name: "Alice", address: Address(city: "NYC"))} />
                </Baz>
            "#},
            expect![[r#"
                user: bar::User
                  --> bar.hop (line 8, col 7)
                 7 | 
                 8 | <Bar {user: User}>
                   |       ^^^^

                user: bar::User
                  --> bar.hop (line 9, col 11)
                 8 | <Bar {user: User}>
                 9 |     <div>{user.address.city}</div>
                   |           ^^^^

                user: bar::User
                  --> baz.hop (line 5, col 17)
                4 | <Baz>
                5 |     <Bar {user: User(name: "Alice", address: Address(city: "NYC"))} />
                  |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
