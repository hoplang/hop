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
    // Build records environment - start with imported records
    let mut records_env: Environment<Type> = Environment::new();

    // Validate imports and collect imported records
    for import in module.get_imports() {
        let imported_module = import.imported_module();
        let imported_name = import.imported_component();
        let Some(module_state) = state.modules.get(imported_module) else {
            errors.push(TypeError::ModuleNotFound {
                module: imported_module.as_str().to_string(),
                range: import.from_attr.value.clone(),
            });
            continue;
        };

        // Check if the import is a component or a record
        let is_component = module_state.component_is_declared(imported_name.as_str());
        let is_record = module_state.record_is_declared(imported_name.as_str());

        if !is_component && !is_record {
            errors.push(TypeError::UndeclaredComponent {
                module_name: import.from_attr.value.clone(),
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
            name: component_name,
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
            name: component_name.clone(),
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
            if !children.is_empty() && !module_info.component_accepts_children(tag_name.as_str()) {
                errors.push(TypeError::ComponentDoesNotAcceptChildren {
                    component: tag_name.as_str().to_string(),
                    range: tag_name.clone(),
                });
            }

            // Check if children: TrustedHTML is required but not provided
            let accepts_children = module_info.component_accepts_children(tag_name.as_str());
            if accepts_children && children.is_empty() {
                errors.push(TypeError::MissingChildren {
                    component: tag_name.as_str().to_string(),
                    range: tag_name.clone(),
                });
            }

            // Validate arguments and build typed versions
            // Filter out children parameter - it's handled separately via component children
            let params_without_children: Option<Vec<_>> = module_info
                .get_parameter_types(tag_name.as_str())
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
                    let all_params = module_info.get_parameter_types(tag_name.as_str());

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
                        <Bar>
                            <div></div>
                        </Bar>
                    </h1>
                    <Foo></Foo>
                </Main>
            "#},
            expect![[r#"
                error: Component Bar is not defined
                  --> main.hop (line 3, col 10)
                2 |     <h1>Hello,
                3 |         <Bar>
                  |          ^^^

                error: Component Foo is not defined
                  --> main.hop (line 7, col 6)
                6 |     </h1>
                7 |     <Foo></Foo>
                  |      ^^^
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

    // When a component is imported from a module that doesn't exist the typechecker outputs an
    // error.
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
                error: Module other was not found
                  --> main.hop (line 1, col 18)
                1 | import Foo from "@/other"
                  |                  ^^^^^^^
            "#]],
        );
    }

    // When a component that doesn't exist is imported from a module that does exist the
    // typechecker outputs an error.
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
                error: Module other was not found
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

    // When content is passed to a component without children: TrustedHTML parameter, the typechecker outputs an error.
    #[test]
    fn test_component_does_not_accept_children() {
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

    // Test component does not accept children with imported component.
    #[test]
    fn test_component_does_not_accept_children_imported() {
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

    // When a variable shadows a parameter, the typechecker outputs an error.
    #[test]
    fn test_variable_shadowing_param() {
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

    // When an undefined variable is referenced, the typechecker outputs an error.
    #[test]
    fn test_undefined_variable_reference() {
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
    fn test_iterate_over_boolean_field() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {k: Bool}
                <Main {params: Array[Item]}>
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
                  --> main.hop (line 8, col 18)
                 7 |     <for {item in params}>
                 8 |         <for {inner in item.k}>
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

    // When successful, the typechecker identifies the parameter type of the component.
    #[test]
    fn test_successful_typechecking_with_complex_params() {
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
    fn test_boolean_equality_comparison() {
        check(
            indoc! {r#"
                -- main.hop --
                record Params {
                  a: String,
                  b: Bool,
                }
                <Main {params: Params}>
                  <if {(params.a == "str") == params.b}>
                    <div>Match</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 5, col 8)
                4 | }
                5 | <Main {params: Params}>
                  |        ^^^^^^

                params: main::Params
                  --> main.hop (line 6, col 9)
                5 | <Main {params: Params}>
                6 |   <if {(params.a == "str") == params.b}>
                  |         ^^^^^^

                params: main::Params
                  --> main.hop (line 6, col 31)
                5 | <Main {params: Params}>
                6 |   <if {(params.a == "str") == params.b}>
                  |                               ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                record Post {
                  published: Bool,
                }
                record Profile {
                  verified: Bool,
                }
                record User {
                  profile: Profile,
                  posts: Array[Post],
                }
                record Params {
                  enabled: Bool,
                  users: Array[User],
                }

                <Main {params: Params}>
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
                params: main::Params
                  --> main.hop (line 16, col 8)
                15 | 
                16 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 18, col 17)
                17 |     <if {params.enabled}>
                18 |         <for {user in params.users}>
                   |                       ^^^^^^

                user: main::User
                  --> main.hop (line 18, col 9)
                17 |     <if {params.enabled}>
                18 |         <for {user in params.users}>
                   |               ^^^^

                user: main::User
                  --> main.hop (line 20, col 19)
                19 |             <if {user.profile.verified}>
                20 |                 <for {post in user.posts}>
                   |                               ^^^^

                post: main::Post
                  --> main.hop (line 20, col 11)
                19 |             <if {user.profile.verified}>
                20 |                 <for {post in user.posts}>
                   |                       ^^^^

                post: main::Post
                  --> main.hop (line 21, col 11)
                20 |                 <for {post in user.posts}>
                21 |                     <if {post.published}>
                   |                          ^^^^

                user: main::User
                  --> main.hop (line 19, col 9)
                18 |         <for {user in params.users}>
                19 |             <if {user.profile.verified}>
                   |                  ^^^^

                params: main::Params
                  --> main.hop (line 17, col 7)
                16 | <Main {params: Params}>
                17 |     <if {params.enabled}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_nested_structure_2() {
        check(
            indoc! {r#"
                -- main.hop --
                record Data {
                  valid: Bool,
                }
                record Item {
                  data: Data,
                }
                record Header {
                  visible: Bool,
                }
                record Section {
                  header: Header,
                  items: Array[Item],
                }
                record Params {
                  sections: Array[Section],
                }

                <Main {params: Params}>
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
                params: main::Params
                  --> main.hop (line 18, col 8)
                17 | 
                18 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 19, col 19)
                18 | <Main {params: Params}>
                19 |     <for {section in params.sections}>
                   |                      ^^^^^^

                section: main::Section
                  --> main.hop (line 19, col 8)
                18 | <Main {params: Params}>
                19 |     <for {section in params.sections}>
                   |           ^^^^^^^

                section: main::Section
                  --> main.hop (line 21, col 18)
                20 |         <if {section.header.visible}>
                21 |             <for {item in section.items}>
                   |                           ^^^^^^^

                item: main::Item
                  --> main.hop (line 21, col 10)
                20 |         <if {section.header.visible}>
                21 |             <for {item in section.items}>
                   |                   ^^^^

                item: main::Item
                  --> main.hop (line 22, col 10)
                21 |             <for {item in section.items}>
                22 |                 <if {item.data.valid}>
                   |                      ^^^^

                section: main::Section
                  --> main.hop (line 20, col 8)
                19 |     <for {section in params.sections}>
                20 |         <if {section.header.visible}>
                   |              ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_deep_nested_object_access() {
        check(
            indoc! {r#"
                -- main.hop --
                record K {
                  l: Bool,
                }
                record J {
                  k: K,
                }
                record I {
                  j: J,
                }
                record Params {
                  i: I,
                }

                <Main {params: Params}>
                	<if {params.i.j.k.l}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 14, col 8)
                13 | 
                14 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 15, col 7)
                14 | <Main {params: Params}>
                15 |     <if {params.i.j.k.l}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_very_deep_nested_object_access() {
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
                	</if>
                	<if {params.app.api.endpoints.users.enabled}>
                	</if>
                	<if {params.app.database.connection.ssl}>
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
                  --> main.hop (line 13, col 7)
                12 |     </if>
                13 |     <if {params.app.api.endpoints.users.enabled}>
                   |          ^^^^^^

                params: main::Params
                  --> main.hop (line 15, col 7)
                14 |     </if>
                15 |     <if {params.app.database.connection.ssl}>
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
                record Data {message: String}
                <Main {data: Data}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </Main>
            "#},
            expect![[r#"
                data: main::Data
                  --> main.hop (line 2, col 8)
                1 | record Data {message: String}
                2 | <Main {data: Data}>
                  |        ^^^^

                data: main::Data
                  --> main.hop (line 4, col 9)
                3 |     <h1>Hello World</h1>
                4 |     <p>{data.message}</p>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn test_complex_string_comparisons() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                record OtherUser {name: String}
                record Data {x: String, y: String}
                record Params {user: User, other_user: OtherUser, data: Data}
                <Main {params: Params}>
                  <if {params.user.name == params.other_user.name}>
                    <div>Same name</div>
                  </if>
                  <if {(params.data.x == params.data.y)}>
                    <div>Parentheses work</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 5, col 8)
                 4 | record Params {user: User, other_user: OtherUser, data: Data}
                 5 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 6, col 8)
                 5 | <Main {params: Params}>
                 6 |   <if {params.user.name == params.other_user.name}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 6, col 28)
                 5 | <Main {params: Params}>
                 6 |   <if {params.user.name == params.other_user.name}>
                   |                            ^^^^^^

                params: main::Params
                  --> main.hop (line 9, col 9)
                 8 |   </if>
                 9 |   <if {(params.data.x == params.data.y)}>
                   |         ^^^^^^

                params: main::Params
                  --> main.hop (line 9, col 26)
                 8 |   </if>
                 9 |   <if {(params.data.x == params.data.y)}>
                   |                          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_simple_string_comparison() {
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
    fn test_nested_array_access() {
        check(
            indoc! {r#"
                -- main.hop --
                record Foo {
                  bar: Array[Bool],
                }
                record Params {
                  foo: Foo,
                }

                <Main {params: Params}>
                	<for {j in params.foo.bar}>
                		<if {j}>
                		</if>
                	</for>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 8, col 8)
                 7 | 
                 8 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 9, col 13)
                 8 | <Main {params: Params}>
                 9 |     <for {j in params.foo.bar}>
                   |                ^^^^^^

                j: Bool
                  --> main.hop (line 9, col 8)
                 8 | <Main {params: Params}>
                 9 |     <for {j in params.foo.bar}>
                   |           ^

                j: Bool
                  --> main.hop (line 10, col 8)
                 9 |     <for {j in params.foo.bar}>
                10 |         <if {j}>
                   |              ^
            "#]],
        );
    }

    #[test]
    fn test_multiple_loops_same_variable() {
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

    // Test that types with the same name from different modules are not compatible
    #[test]
    fn test_same_name_different_module_types_incompatible() {
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

    // Test that identical record definitions in different modules are still incompatible
    #[test]
    fn test_identical_records_different_modules_incompatible() {
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
                record Config {debug: Bool}
                record DataItem {id: Bool, attributes: Array[Bool]}
                record Params {config: Config, data: Array[DataItem]}
                <Main {params: Params}>
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
                params: main::Params
                  --> main.hop (line 4, col 8)
                 3 | record Params {config: Config, data: Array[DataItem]}
                 4 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 5, col 7)
                 4 | <Main {params: Params}>
                 5 |     <if {params.config.debug}>
                   |          ^^^^^^

                params: main::Params
                  --> main.hop (line 7, col 16)
                 6 |     </if>
                 7 |     <for {item in params.data}>
                   |                   ^^^^^^

                item: main::DataItem
                  --> main.hop (line 7, col 8)
                 6 |     </if>
                 7 |     <for {item in params.data}>
                   |           ^^^^

                item: main::DataItem
                  --> main.hop (line 8, col 8)
                 7 |     <for {item in params.data}>
                 8 |         <if {item.id}>
                   |              ^^^^

                item: main::DataItem
                  --> main.hop (line 10, col 17)
                 9 |         </if>
                10 |         <for {attr in item.attributes}>
                   |                       ^^^^

                attr: Bool
                  --> main.hop (line 10, col 9)
                 9 |         </if>
                10 |         <for {attr in item.attributes}>
                   |               ^^^^

                attr: Bool
                  --> main.hop (line 11, col 9)
                10 |         <for {attr in item.attributes}>
                11 |             <if {attr}>
                   |                  ^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_chain_with_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                record Settings {enabled: Bool}
                record Config {settings: Settings}
                record Data {config: Config}

                <Step3Comp {settings: Settings}>
                	<if {settings.enabled}>
                	</if>
                </Step3Comp>

                <Step2Comp {config: Config}>
                	<Step3Comp {settings: config.settings}/>
                </Step2Comp>

                <Step1Comp {data: Data}>
                	<Step2Comp {config: data.config}/>
                </Step1Comp>

                <Main {params: Data}>
                	<Step1Comp {data: params}/>
                </Main>
            "#},
            expect![[r#"
                settings: main::Settings
                  --> main.hop (line 5, col 13)
                 4 | 
                 5 | <Step3Comp {settings: Settings}>
                   |             ^^^^^^^^

                settings: main::Settings
                  --> main.hop (line 6, col 7)
                 5 | <Step3Comp {settings: Settings}>
                 6 |     <if {settings.enabled}>
                   |          ^^^^^^^^

                config: main::Config
                  --> main.hop (line 10, col 13)
                 9 | 
                10 | <Step2Comp {config: Config}>
                   |             ^^^^^^

                config: main::Config
                  --> main.hop (line 11, col 24)
                10 | <Step2Comp {config: Config}>
                11 |     <Step3Comp {settings: config.settings}/>
                   |                           ^^^^^^

                settings: main::Settings
                  --> main.hop (line 11, col 24)
                10 | <Step2Comp {config: Config}>
                11 |     <Step3Comp {settings: config.settings}/>
                   |                           ^^^^^^^^^^^^^^^

                data: main::Data
                  --> main.hop (line 14, col 13)
                13 | 
                14 | <Step1Comp {data: Data}>
                   |             ^^^^

                data: main::Data
                  --> main.hop (line 15, col 22)
                14 | <Step1Comp {data: Data}>
                15 |     <Step2Comp {config: data.config}/>
                   |                         ^^^^

                config: main::Config
                  --> main.hop (line 15, col 22)
                14 | <Step1Comp {data: Data}>
                15 |     <Step2Comp {config: data.config}/>
                   |                         ^^^^^^^^^^^

                params: main::Data
                  --> main.hop (line 18, col 8)
                17 | 
                18 | <Main {params: Data}>
                   |        ^^^^^^

                params: main::Data
                  --> main.hop (line 19, col 20)
                18 | <Main {params: Data}>
                19 |     <Step1Comp {data: params}/>
                   |                       ^^^^^^

                data: main::Data
                  --> main.hop (line 19, col 20)
                18 | <Main {params: Data}>
                19 |     <Step1Comp {data: params}/>
                   |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_three_level_component_hierarchy() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {title: String, active: Bool, status: String}
                record Data {items: Array[Item]}

                <MainCard {item: Item}>
                  <div>{item.title}
                  </div>
                  <if {item.active}>
                    <span>{item.status}
                    </span>
                  </if>
                </MainCard>

                <MainList {items: Array[Item]}>
                  <for {item in items}>
                    <MainCard {item: item}/>
                  </for>
                </MainList>

                <Main {data: Data}>
                  <MainList {items: data.items}/>
                </Main>
            "#},
            expect![[r#"
                item: main::Item
                  --> main.hop (line 4, col 12)
                 3 | 
                 4 | <MainCard {item: Item}>
                   |            ^^^^

                item: main::Item
                  --> main.hop (line 5, col 9)
                 4 | <MainCard {item: Item}>
                 5 |   <div>{item.title}
                   |         ^^^^

                item: main::Item
                  --> main.hop (line 8, col 12)
                 7 |   <if {item.active}>
                 8 |     <span>{item.status}
                   |            ^^^^

                item: main::Item
                  --> main.hop (line 7, col 8)
                 6 |   </div>
                 7 |   <if {item.active}>
                   |        ^^^^

                items: Array[main::Item]
                  --> main.hop (line 13, col 12)
                12 | 
                13 | <MainList {items: Array[Item]}>
                   |            ^^^^^

                items: Array[main::Item]
                  --> main.hop (line 14, col 17)
                13 | <MainList {items: Array[Item]}>
                14 |   <for {item in items}>
                   |                 ^^^^^

                item: main::Item
                  --> main.hop (line 14, col 9)
                13 | <MainList {items: Array[Item]}>
                14 |   <for {item in items}>
                   |         ^^^^

                item: main::Item
                  --> main.hop (line 15, col 22)
                14 |   <for {item in items}>
                15 |     <MainCard {item: item}/>
                   |                      ^^^^

                item: main::Item
                  --> main.hop (line 15, col 22)
                14 |   <for {item in items}>
                15 |     <MainCard {item: item}/>
                   |                      ^^^^

                data: main::Data
                  --> main.hop (line 19, col 8)
                18 | 
                19 | <Main {data: Data}>
                   |        ^^^^

                data: main::Data
                  --> main.hop (line 20, col 21)
                19 | <Main {data: Data}>
                20 |   <MainList {items: data.items}/>
                   |                     ^^^^

                items: Array[main::Item]
                  --> main.hop (line 20, col 21)
                19 | <Main {data: Data}>
                20 |   <MainList {items: data.items}/>
                   |                     ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_object_field_name_collision() {
        check(
            indoc! {r#"
                -- main.hop --
                record L {l: Bool}
                record J {k: L}
                record I {j: J, k: Bool}
                record Params {i: I}
                <Main {params: Params}>
                	<if {params.i.j.k.l}>
                		<if {params.i.k}>
                		</if>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 5, col 8)
                 4 | record Params {i: I}
                 5 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 7, col 8)
                 6 |     <if {params.i.j.k.l}>
                 7 |         <if {params.i.k}>
                   |              ^^^^^^

                params: main::Params
                  --> main.hop (line 6, col 7)
                 5 | <Main {params: Params}>
                 6 |     <if {params.i.j.k.l}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_object_field_separate_access() {
        check(
            indoc! {r#"
                -- main.hop --
                record L {l: Bool}
                record J {k: L}
                record I {j: J, k: Bool}
                record Params {i: I}
                <Main {params: Params}>
                	<if {params.i.j.k.l}>
                	</if>
                	<if {params.i.k}>
                	</if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 5, col 8)
                 4 | record Params {i: I}
                 5 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 6, col 7)
                 5 | <Main {params: Params}>
                 6 |     <if {params.i.j.k.l}>
                   |          ^^^^^^

                params: main::Params
                  --> main.hop (line 8, col 7)
                 7 |     </if>
                 8 |     <if {params.i.k}>
                   |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_multiple_array_properties() {
        check(
            indoc! {r#"
                -- main.hop --
                record Metadata {title: Bool}
                record Params {tags: Array[Bool], categories: Array[Bool], metadata: Metadata}
                <Main {params: Params}>
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
                params: main::Params
                  --> main.hop (line 3, col 8)
                 2 | record Params {tags: Array[Bool], categories: Array[Bool], metadata: Metadata}
                 3 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 4, col 15)
                 3 | <Main {params: Params}>
                 4 |     <for {tag in params.tags}>
                   |                  ^^^^^^

                tag: Bool
                  --> main.hop (line 4, col 8)
                 3 | <Main {params: Params}>
                 4 |     <for {tag in params.tags}>
                   |           ^^^

                tag: Bool
                  --> main.hop (line 5, col 8)
                 4 |     <for {tag in params.tags}>
                 5 |         <if {tag}>
                   |              ^^^

                params: main::Params
                  --> main.hop (line 8, col 20)
                 7 |     </for>
                 8 |     <for {category in params.categories}>
                   |                       ^^^^^^

                category: Bool
                  --> main.hop (line 8, col 8)
                 7 |     </for>
                 8 |     <for {category in params.categories}>
                   |           ^^^^^^^^

                category: Bool
                  --> main.hop (line 9, col 8)
                 8 |     <for {category in params.categories}>
                 9 |         <if {category}>
                   |              ^^^^^^^^

                params: main::Params
                  --> main.hop (line 12, col 7)
                11 |     </for>
                12 |     <if {params.metadata.title}>
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
                record Step {condition: Bool}
                record Workflow {enabled: Bool, steps: Array[Step]}
                record Params {workflows: Array[Workflow]}

                <ExecuteStep {step: Step}>
                	<if {step.condition}>
                	</if>
                </ExecuteStep>

                <ExecuteWorkflow {workflow: Workflow}>
                	<if {workflow.enabled}>
                		<for {step in workflow.steps}>
                			<ExecuteStep {step: step}/>
                		</for>
                	</if>
                </ExecuteWorkflow>

                <Main {params: Params}>
                	<for {workflow in params.workflows}>
                		<ExecuteWorkflow {workflow: workflow}/>
                	</for>
                </Main>
            "#},
            expect![[r#"
                step: main::Step
                  --> main.hop (line 5, col 15)
                 4 | 
                 5 | <ExecuteStep {step: Step}>
                   |               ^^^^

                step: main::Step
                  --> main.hop (line 6, col 7)
                 5 | <ExecuteStep {step: Step}>
                 6 |     <if {step.condition}>
                   |          ^^^^

                workflow: main::Workflow
                  --> main.hop (line 10, col 19)
                 9 | 
                10 | <ExecuteWorkflow {workflow: Workflow}>
                   |                   ^^^^^^^^

                workflow: main::Workflow
                  --> main.hop (line 12, col 17)
                11 |     <if {workflow.enabled}>
                12 |         <for {step in workflow.steps}>
                   |                       ^^^^^^^^

                step: main::Step
                  --> main.hop (line 12, col 9)
                11 |     <if {workflow.enabled}>
                12 |         <for {step in workflow.steps}>
                   |               ^^^^

                step: main::Step
                  --> main.hop (line 13, col 24)
                12 |         <for {step in workflow.steps}>
                13 |             <ExecuteStep {step: step}/>
                   |                                 ^^^^

                step: main::Step
                  --> main.hop (line 13, col 24)
                12 |         <for {step in workflow.steps}>
                13 |             <ExecuteStep {step: step}/>
                   |                                 ^^^^

                workflow: main::Workflow
                  --> main.hop (line 11, col 7)
                10 | <ExecuteWorkflow {workflow: Workflow}>
                11 |     <if {workflow.enabled}>
                   |          ^^^^^^^^

                params: main::Params
                  --> main.hop (line 18, col 8)
                17 | 
                18 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 19, col 20)
                18 | <Main {params: Params}>
                19 |     <for {workflow in params.workflows}>
                   |                       ^^^^^^

                workflow: main::Workflow
                  --> main.hop (line 19, col 8)
                18 | <Main {params: Params}>
                19 |     <for {workflow in params.workflows}>
                   |           ^^^^^^^^

                workflow: main::Workflow
                  --> main.hop (line 20, col 31)
                19 |     <for {workflow in params.workflows}>
                20 |         <ExecuteWorkflow {workflow: workflow}/>
                   |                                     ^^^^^^^^

                workflow: main::Workflow
                  --> main.hop (line 20, col 31)
                19 |     <for {workflow in params.workflows}>
                20 |         <ExecuteWorkflow {workflow: workflow}/>
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
                record Child {visible: Bool}
                record Status {active: Bool}
                record Item {children: Array[Child], status: Status}
                record Params {items: Array[Item]}
                <ProcessItem {item: Item}>
                	<if {item.status.active}>
                	</if>
                	<for {child in item.children}>
                		<if {child.visible}>
                		</if>
                	</for>
                </ProcessItem>

                <Main {params: Params}>
                	<for {item in params.items}>
                		<ProcessItem {item: item}/>
                	</for>
                </Main>
            "#},
            expect![[r#"
                item: main::Item
                  --> main.hop (line 5, col 15)
                 4 | record Params {items: Array[Item]}
                 5 | <ProcessItem {item: Item}>
                   |               ^^^^

                item: main::Item
                  --> main.hop (line 6, col 7)
                 5 | <ProcessItem {item: Item}>
                 6 |     <if {item.status.active}>
                   |          ^^^^

                item: main::Item
                  --> main.hop (line 8, col 17)
                 7 |     </if>
                 8 |     <for {child in item.children}>
                   |                    ^^^^

                child: main::Child
                  --> main.hop (line 8, col 8)
                 7 |     </if>
                 8 |     <for {child in item.children}>
                   |           ^^^^^

                child: main::Child
                  --> main.hop (line 9, col 8)
                 8 |     <for {child in item.children}>
                 9 |         <if {child.visible}>
                   |              ^^^^^

                params: main::Params
                  --> main.hop (line 14, col 8)
                13 | 
                14 | <Main {params: Params}>
                   |        ^^^^^^

                params: main::Params
                  --> main.hop (line 15, col 16)
                14 | <Main {params: Params}>
                15 |     <for {item in params.items}>
                   |                   ^^^^^^

                item: main::Item
                  --> main.hop (line 15, col 8)
                14 | <Main {params: Params}>
                15 |     <for {item in params.items}>
                   |           ^^^^

                item: main::Item
                  --> main.hop (line 16, col 23)
                15 |     <for {item in params.items}>
                16 |         <ProcessItem {item: item}/>
                   |                             ^^^^

                item: main::Item
                  --> main.hop (line 16, col 23)
                15 |     <for {item in params.items}>
                16 |         <ProcessItem {item: item}/>
                   |                             ^^^^
            "#]],
        );
    }

    #[test]
    fn test_expr_attributes() {
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
    fn test_children_usage() {
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
    fn test_simple_text_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                record Data {message: String}
                <Main {data: Data}>
                  <div>{data.message}
                  </div>
                </Main>
            "#},
            expect![[r#"
                data: main::Data
                  --> main.hop (line 2, col 8)
                1 | record Data {message: String}
                2 | <Main {data: Data}>
                  |        ^^^^

                data: main::Data
                  --> main.hop (line 3, col 9)
                2 | <Main {data: Data}>
                3 |   <div>{data.message}
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn test_string_comparison_conditional() {
        check(
            indoc! {r#"
                -- main.hop --
                record Params {role: String}
                <Main {params: Params}>
                  <if {params.role == "admin"}>
                    <div>Admin</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                params: main::Params
                  --> main.hop (line 2, col 8)
                1 | record Params {role: String}
                2 | <Main {params: Params}>
                  |        ^^^^^^

                params: main::Params
                  --> main.hop (line 3, col 8)
                2 | <Main {params: Params}>
                3 |   <if {params.role == "admin"}>
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
                error: Array[String] can not be used as a record
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
                record Data {message: String}
                <Main {data: Data}>
                	<hop-x-raw>foo bar</hop-x-raw>
                	<div>{data.message}</div>
                </Main>
            "#},
            expect![[r#"
                data: main::Data
                  --> main.hop (line 2, col 8)
                1 | record Data {message: String}
                2 | <Main {data: Data}>
                  |        ^^^^

                data: main::Data
                  --> main.hop (line 4, col 8)
                3 |     <hop-x-raw>foo bar</hop-x-raw>
                4 |     <div>{data.message}</div>
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

    // Test <if> tag with comparison expression.
    #[test]
    fn test_if_with_comparison_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                record Data {status: String}
                <Main {data: Data}>
                  <if {data.status == "approved"}>
                    <div>Status is approved</div>
                  </if>
                </Main>
            "#},
            expect![[r#"
                data: main::Data
                  --> main.hop (line 2, col 8)
                1 | record Data {status: String}
                2 | <Main {data: Data}>
                  |        ^^^^

                data: main::Data
                  --> main.hop (line 3, col 8)
                2 | <Main {data: Data}>
                3 |   <if {data.status == "approved"}>
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
                record Config {enabled: Bool, debug: Bool}
                <Main {config: Config}>
                  <if {config.enabled}>
                    <div>Feature enabled</div>
                    <if {config.debug}>
                      <div>Debug mode on</div>
                    </if>
                  </if>
                </Main>
            "#},
            expect![[r#"
                config: main::Config
                  --> main.hop (line 2, col 8)
                1 | record Config {enabled: Bool, debug: Bool}
                2 | <Main {config: Config}>
                  |        ^^^^^^

                config: main::Config
                  --> main.hop (line 5, col 10)
                4 |     <div>Feature enabled</div>
                5 |     <if {config.debug}>
                  |          ^^^^^^

                config: main::Config
                  --> main.hop (line 3, col 8)
                2 | <Main {config: Config}>
                3 |   <if {config.enabled}>
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
                record Config {debug: Bool}
                <Main {config: Config}>
                  <if {config.debug}>
                    <div>Debug mode on</div>
                  </if>
                </Main>
                <Foo>
                  <Main {config: 1}/>
                </Foo>
            "#},
            expect![[r#"
                error: Argument 'config' of type Int is incompatible with expected type main::Config
                  --> main.hop (line 8, col 18)
                7 | <Foo>
                8 |   <Main {config: 1}/>
                  |                  ^
            "#]],
        );
    }

    #[test]
    fn test_object_equality_and_string_equality() {
        check(
            indoc! {r#"
                -- main.hop --
                record Params {foo: String}
                <Main {params: Params}>
                  <if {params.foo == "foo"}>
                    eq 1
                  </if>
                  <if {params == params}>
                    eq 2
                  </if>
                </Main>
            "#},
            expect![[r#"
                error: Type main::Params is not comparable
                  --> main.hop (line 6, col 8)
                5 |   </if>
                6 |   <if {params == params}>
                  |        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_nested_data_structure_access() {
        check(
            indoc! {r#"
                -- main.hop --
                record Section {title: String, items: Array[String]}
                <Main {data: Array[Section]}>
                	<for {section in data}>
                		<h1>{section.title}</h1>
                		<for {item in section.items}>
                			<div>{item}</div>
                		</for>
                	</for>
                </Main>
            "#},
            expect![[r#"
                data: Array[main::Section]
                  --> main.hop (line 2, col 8)
                1 | record Section {title: String, items: Array[String]}
                2 | <Main {data: Array[Section]}>
                  |        ^^^^

                data: Array[main::Section]
                  --> main.hop (line 3, col 19)
                2 | <Main {data: Array[Section]}>
                3 |     <for {section in data}>
                  |                      ^^^^

                section: main::Section
                  --> main.hop (line 3, col 8)
                2 | <Main {data: Array[Section]}>
                3 |     <for {section in data}>
                  |           ^^^^^^^

                section: main::Section
                  --> main.hop (line 4, col 8)
                3 |     <for {section in data}>
                4 |         <h1>{section.title}</h1>
                  |              ^^^^^^^

                section: main::Section
                  --> main.hop (line 5, col 17)
                4 |         <h1>{section.title}</h1>
                5 |         <for {item in section.items}>
                  |                       ^^^^^^^

                item: String
                  --> main.hop (line 5, col 9)
                4 |         <h1>{section.title}</h1>
                5 |         <for {item in section.items}>
                  |               ^^^^

                item: String
                  --> main.hop (line 6, col 10)
                5 |         <for {item in section.items}>
                6 |             <div>{item}</div>
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
                record User {name: String, age: String}
                <UserComp {user: User}>
                	<div>{user.name} ({user.age})</div>
                </UserComp>
                <Main>
                	<UserComp {user: "invalid"}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type String is incompatible with expected type main::User
                  --> main.hop (line 6, col 19)
                5 | <Main>
                6 |     <UserComp {user: "invalid"}/>
                  |                      ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_correct_object_passing() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String, active: String}
                record Data {profile: User}
                <UserComp {user: User}>
                	<div>{user.name}: {user.active}</div>
                </UserComp>
                <Main {data: Data}>
                	<UserComp {user: data.profile}/>
                </Main>
            "#},
            expect![[r#"
                user: main::User
                  --> main.hop (line 3, col 12)
                2 | record Data {profile: User}
                3 | <UserComp {user: User}>
                  |            ^^^^

                user: main::User
                  --> main.hop (line 4, col 8)
                3 | <UserComp {user: User}>
                4 |     <div>{user.name}: {user.active}</div>
                  |           ^^^^

                user: main::User
                  --> main.hop (line 4, col 21)
                3 | <UserComp {user: User}>
                4 |     <div>{user.name}: {user.active}</div>
                  |                        ^^^^

                data: main::Data
                  --> main.hop (line 6, col 8)
                5 | </UserComp>
                6 | <Main {data: Data}>
                  |        ^^^^

                data: main::Data
                  --> main.hop (line 7, col 19)
                6 | <Main {data: Data}>
                7 |     <UserComp {user: data.profile}/>
                  |                      ^^^^

                user: main::User
                  --> main.hop (line 7, col 19)
                6 | <Main {data: Data}>
                7 |     <UserComp {user: data.profile}/>
                  |                      ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_component_parameter_type_error_string() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <NewComp {user: User}>
                	<div>{user.name}</div>
                </NewComp>
                <Main>
                	<NewComp {user: "invalid"}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type String is incompatible with expected type main::User
                  --> main.hop (line 6, col 18)
                5 | <Main>
                6 |     <NewComp {user: "invalid"}/>
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
                record User {name: String}
                <UserComp {user: User}>
                	<div>{user.name}</div>
                </UserComp>
            "#},
            expect![[r#"
                user: main::User
                  --> main.hop (line 2, col 12)
                1 | record User {name: String}
                2 | <UserComp {user: User}>
                  |            ^^^^

                user: main::User
                  --> main.hop (line 3, col 8)
                2 | <UserComp {user: User}>
                3 |     <div>{user.name}</div>
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
                record User {name: String, age: Float}
                record Profile {user: User}
                <ProfileComp {profile: Profile}>
                	<div>{profile.user.name}</div>
                	<if {profile.user.age == 25}>
                		<div>Quarter century</div>
                	</if>
                </ProfileComp>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> main.hop (line 5, col 7)
                4 |     <div>{profile.user.name}</div>
                5 |     <if {profile.user.age == 25}>
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
                record Metadata {author: String, published: Bool}
                record CardData {title: String, content: String, tags: Array[String], metadata: Metadata}
                <CardComp {data: CardData}>
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
                data: main::CardData
                  --> main.hop (line 3, col 12)
                 2 | record CardData {title: String, content: String, tags: Array[String], metadata: Metadata}
                 3 | <CardComp {data: CardData}>
                   |            ^^^^

                data: main::CardData
                  --> main.hop (line 4, col 7)
                 3 | <CardComp {data: CardData}>
                 4 |     <h1>{data.title}</h1>
                   |          ^^^^

                data: main::CardData
                  --> main.hop (line 5, col 6)
                 4 |     <h1>{data.title}</h1>
                 5 |     <p>{data.content}</p>
                   |         ^^^^

                data: main::CardData
                  --> main.hop (line 6, col 8)
                 5 |     <p>{data.content}</p>
                 6 |     <div>{data.metadata.author}</div>
                   |           ^^^^

                data: main::CardData
                  --> main.hop (line 7, col 7)
                 6 |     <div>{data.metadata.author}</div>
                 7 |     <if {data.metadata.published}>
                   |          ^^^^

                data: main::CardData
                  --> main.hop (line 10, col 15)
                 9 |     </if>
                10 |     <for {tag in data.tags}>
                   |                  ^^^^

                tag: String
                  --> main.hop (line 10, col 8)
                 9 |     </if>
                10 |     <for {tag in data.tags}>
                   |           ^^^

                tag: String
                  --> main.hop (line 11, col 10)
                10 |     <for {tag in data.tags}>
                11 |         <span>{tag}</span>
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
                record Item {id: Float, name: String, active: Bool}
                <ItemDisplay {item: Item}>
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
                import Item from "@/item-display"
                <DataList {items: Array[Item]}>
                	<for {item in items}>
                		<ItemDisplay {item: item}/>
                	</for>
                </DataList>
                -- main.hop --
                import DataList from "@/data-list"
                import Item from "@/item-display"
                <Main {items: Array[Item]}>
                	<DataList {items: items}/>
                </Main>
            "#},
            expect![[r#"
                error: Can not compare Float to Int
                  --> item-display.hop (line 7, col 7)
                 6 |     </if>
                 7 |     <if {item.id == 1}>
                   |          ^^^^^^^^^^^^
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
                error: Cannot infer type of empty array
                  --> main.hop (line 2, col 16)
                1 | <Main>
                2 |     <for {x in []}>
                  |                ^^
            "#]],
        );
    }

    // Empty array literals can infer their type from component arguments
    #[test]
    fn test_empty_array_infers_type_from_component_argument() {
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

    // Test that using an undefined type name in a parameter produces an error
    #[test]
    fn test_undefined_type_in_parameter() {
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

    // Test that using a declared record type in a parameter is allowed (no UndefinedType error)
    #[test]
    fn test_declared_record_type_in_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main {user: User}>
                    <div></div>
                </Main>
            "#},
            // Only get UnusedVariable error, not UndefinedType error
            expect![[r#"
                error: Unused variable user
                  --> main.hop (line 2, col 8)
                1 | record User {name: String}
                2 | <Main {user: User}>
                  |        ^^^^
            "#]],
        );
    }

    // Test that using an undefined type name inside an array type produces an error
    #[test]
    fn test_undefined_type_in_array_parameter() {
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

    // Test that using a declared record type inside an array type is allowed (no UndefinedType error)
    #[test]
    fn test_declared_record_type_in_array_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main {users: Array[User]}>
                    <div></div>
                </Main>
            "#},
            // Only get UnusedVariable error, not UndefinedType error
            expect![[r#"
                error: Unused variable users
                  --> main.hop (line 2, col 8)
                1 | record User {name: String}
                2 | <Main {users: Array[User]}>
                  |        ^^^^^
            "#]],
        );
    }

    // Test that a record can reference another declared record
    #[test]
    fn test_record_referencing_another_record() {
        check(
            indoc! {r#"
                -- main.hop --
                record Address {street: String}
                record User {name: String, address: Address}
                <Main {user: User}>
                    <div></div>
                </Main>
            "#},
            // Only unused variable error, no UndefinedType error since Address is declared
            expect![[r#"
                error: Unused variable user
                  --> main.hop (line 3, col 8)
                2 | record User {name: String, address: Address}
                3 | <Main {user: User}>
                  |        ^^^^
            "#]],
        );
    }

    // Test that a record referencing an undefined record produces an error
    #[test]
    fn test_record_referencing_undefined_record() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String, address: Address}
                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Type 'Address' is not defined
                  --> main.hop (line 1, col 37)
                1 | record User {name: String, address: Address}
                  |                                     ^^^^^^^
            "#]],
        );
    }

    // Test that a record cannot reference itself
    #[test]
    fn test_record_self_referential_disallowed() {
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

    // Test that a record cannot reference another record that is declared after it
    #[test]
    fn test_record_forward_reference_disallowed() {
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
    fn test_record_field_access() {
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

    // Test that accessing a non-existent field on a record produces an error
    #[test]
    fn test_record_field_access_undefined_field() {
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

    // Test that accessing nested record fields works
    #[test]
    fn test_record_nested_field_access() {
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

    // Test that re-exporting imports is not allowed.
    // If module Foo declares A and module Bar imports A from Foo,
    // then module Baz should not be able to import A from Bar.
    #[test]
    fn test_cannot_reexport_imports() {
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

    // Test cross-module record usage:
    // - Foo declares record A with a primitive field
    // - Bar imports A and declares record B with a field of type A
    // - Baz imports Bar and User, and passes a User to Bar component via record instantiation
    #[test]
    fn test_cross_module_record_usage() {
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
