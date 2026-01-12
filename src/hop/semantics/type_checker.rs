use super::type_error::TypeError;
use crate::document::document_cursor::{DocumentRange, Ranged, CheapString};
use crate::dop::patterns::compiler::Compiler as PatMatchCompiler;
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::{self, Type, TypedExpr, VarName, resolve_type};
use crate::environment::Environment;
use crate::error_collector::ErrorCollector;
use crate::hop::syntax::parsed_ast::ParsedParameter;
use crate::hop::syntax::parsed_ast::{ParsedAttribute, ParsedComponentDeclaration};
use std::collections::HashMap;
use std::fmt::{self, Display};

use crate::dop::patterns::compiler::Decision;
use crate::dop::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::dop::syntax::parsed::Constructor;
use crate::hop::semantics::typed_ast::{
    TypedAst, TypedComponentDeclaration, TypedEnumDeclaration, TypedRecordDeclaration,
};
use crate::hop::semantics::typed_node::{TypedAttribute, TypedAttributeValue, TypedNode};
use crate::hop::symbols::module_name::ModuleName;
use crate::hop::syntax::parsed_ast::{ParsedAst, ParsedAttributeValue};
use crate::hop::syntax::parsed_node::ParsedNode;

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
struct ComponentTypeInformation {
    /// Parameters: (name, type, has_default)
    parameters: Vec<(String, Type, bool)>,
}

#[derive(Debug, Clone)]
struct RecordTypeInformation {
    fields: Vec<(FieldName, Type)>,
}

#[derive(Debug, Clone)]
struct EnumTypeInformation {
    /// Variants with their fields: (variant_name, fields)
    variants: Vec<(TypeName, Vec<(FieldName, Type)>)>,
}

#[derive(Debug, Clone, Default)]
struct ModuleTypeInformation {
    components: HashMap<String, ComponentTypeInformation>,
    records: HashMap<String, RecordTypeInformation>,
    enums: HashMap<String, EnumTypeInformation>,
}

impl ModuleTypeInformation {
    fn get_parameter_types(&self, component_name: &str) -> Option<&[(String, Type, bool)]> {
        let params = &self.components.get(component_name)?.parameters;
        if params.is_empty() {
            None
        } else {
            Some(params.as_slice())
        }
    }

    /// Check if the component accepts children (has a `children` parameter)
    fn component_accepts_children(&self, component_name: &str) -> bool {
        self.components
            .get(component_name)
            .is_some_and(|c| c.parameters.iter().any(|(name, _, _)| name == "children"))
    }

    /// Get the children parameter's type and has_default flag if present
    fn get_children_param(&self, component_name: &str) -> Option<(&Type, bool)> {
        self.components
            .get(component_name)?
            .parameters
            .iter()
            .find(|(name, _, _)| name == "children")
            .map(|(_, typ, has_default)| (typ, *has_default))
    }

    /// Check if children are required (TrustedHTML without default)
    fn children_are_required(&self, component_name: &str) -> bool {
        matches!(
            self.get_children_param(component_name),
            Some((Type::TrustedHTML, false))
        )
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
        self.records.contains_key(record_name)
    }

    fn get_typed_record(&self, record_name: &str) -> Option<&RecordTypeInformation> {
        self.records.get(record_name)
    }

    fn set_typed_record(&mut self, record_name: &str, record: RecordTypeInformation) {
        self.records.insert(record_name.to_string(), record);
    }

    fn enum_is_declared(&self, enum_name: &str) -> bool {
        self.enums.contains_key(enum_name)
    }

    fn get_typed_enum(&self, enum_name: &str) -> Option<&EnumTypeInformation> {
        self.enums.get(enum_name)
    }

    fn set_typed_enum(&mut self, enum_name: &str, enum_info: EnumTypeInformation) {
        self.enums.insert(enum_name.to_string(), enum_info);
    }
}

#[derive(Debug, Default)]
struct State {
    modules: HashMap<ModuleName, ModuleTypeInformation>,
}

impl State {
    fn set_component_type_info(
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

    fn set_record_type_info(
        &mut self,
        module_name: &ModuleName,
        record_name: &str,
        type_info: RecordTypeInformation,
    ) {
        self.modules
            .entry(module_name.clone())
            .or_default()
            .set_typed_record(record_name, type_info);
    }

    fn set_enum_type_info(
        &mut self,
        module_name: &ModuleName,
        enum_name: &str,
        type_info: EnumTypeInformation,
    ) {
        self.modules
            .entry(module_name.clone())
            .or_default()
            .set_typed_enum(enum_name, type_info);
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
    pub fn typecheck(&mut self, modules: &[&ParsedAst]) {
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
                for import_node in module.get_import_declarations() {
                    let imported_module = import_node.imported_module();
                    type_errors.push(TypeError::import_cycle(
                        &module.name.to_string(),
                        &imported_module.to_string(),
                        &modules
                            .iter()
                            .map(|m| m.name.to_string())
                            .collect::<Vec<_>>(),
                        import_node.path.clone(),
                    ));
                }
            }
        }
    }
}

fn typecheck_module(
    module: &ParsedAst,
    state: &mut State,
    errors: &mut ErrorCollector<TypeError>,
    annotations: &mut Vec<TypeAnnotation>,
) -> TypedAst {
    state.modules.insert(
        module.name.clone(),
        ModuleTypeInformation {
            components: HashMap::new(),
            records: HashMap::new(),
            enums: HashMap::new(),
        },
    );

    let mut type_env: Environment<Type> = Environment::new();

    for import in module.get_import_declarations() {
        let imported_module = import.imported_module();
        let imported_name = import.imported_type_name();
        let Some(module_state) = state.modules.get(imported_module) else {
            errors.push(TypeError::ModuleNotFound {
                module: imported_module.to_string(),
                range: import.path.clone(),
            });
            continue;
        };

        let is_component = module_state.component_is_declared(imported_name.as_str());
        let is_record = module_state.record_is_declared(imported_name.as_str());
        let is_enum = module_state.enum_is_declared(imported_name.as_str());

        if !is_component && !is_record && !is_enum {
            errors.push(TypeError::UndeclaredComponent {
                module: imported_module.to_string(),
                component: imported_name.to_string(),
                range: import.type_name_range().clone(),
            });
        }

        if let Some(record) = module_state.get_typed_record(imported_name.as_str()) {
            let _ = type_env.push(
                imported_name.as_str().to_string(),
                Type::Record {
                    module: imported_module.clone(),
                    name: TypeName::new(imported_name.as_str()).unwrap(),
                    fields: record.fields.clone(),
                },
            );
        }

        if let Some(enum_info) = module_state.get_typed_enum(imported_name.as_str()) {
            let _ = type_env.push(
                imported_name.as_str().to_string(),
                Type::Enum {
                    module: imported_module.clone(),
                    name: TypeName::new(imported_name.as_str()).unwrap(),
                    variants: enum_info.variants.clone(),
                },
            );
        }
    }

    let mut typed_enums: Vec<TypedEnumDeclaration> = Vec::new();
    for enum_decl in module.get_enum_declarations() {
        let enum_name = enum_decl.name();

        // Resolve variant fields
        let mut typed_variants: Vec<(TypeName, Vec<(FieldName, Type)>)> = Vec::new();
        let mut has_errors = false;

        for variant in &enum_decl.variants {
            let mut typed_fields: Vec<(FieldName, Type)> = Vec::new();
            for (field_name, _, field_type) in &variant.fields {
                match resolve_type(field_type, &mut type_env) {
                    Ok(resolved_type) => {
                        typed_fields.push((field_name.clone(), resolved_type));
                    }
                    Err(e) => {
                        errors.push(e.into());
                        has_errors = true;
                    }
                }
            }
            typed_variants.push((variant.name.clone(), typed_fields));
        }

        if has_errors {
            continue;
        }

        let enum_type = Type::Enum {
            module: module.name.clone(),
            name: TypeName::new(enum_name).unwrap(),
            variants: typed_variants.clone(),
        };
        let _ = type_env.push(enum_name.to_string(), enum_type);

        state.set_enum_type_info(
            &module.name,
            enum_name,
            EnumTypeInformation {
                variants: typed_variants.clone(),
            },
        );

        typed_enums.push(TypedEnumDeclaration {
            name: enum_decl.name.clone(),
            variants: typed_variants,
        });
    }

    let mut typed_records: Vec<TypedRecordDeclaration> = Vec::new();
    for record in module.get_record_declarations() {
        let record_name = record.name();
        let mut typed_fields = Vec::new();
        let mut has_errors = false;

        for field in &record.fields {
            match resolve_type(&field.field_type, &mut type_env) {
                Ok(resolved_type) => {
                    typed_fields.push((field.name.clone(), resolved_type));
                }
                Err(e) => {
                    errors.push(e.into());
                    has_errors = true;
                }
            }
        }

        if !has_errors {
            let typed_record = TypedRecordDeclaration {
                name: record.name.clone(),
                fields: typed_fields.clone(),
            };
            state.set_record_type_info(
                &module.name,
                record.name(),
                RecordTypeInformation {
                    fields: typed_fields.clone(),
                },
            );
            typed_records.push(typed_record);
            let _ = type_env.push(
                record_name.to_string(),
                Type::Record {
                    module: module.name.clone(),
                    name: TypeName::new(record_name).unwrap(),
                    fields: typed_fields,
                },
            );
        }
    }

    let mut env = Environment::new();

    let mut typed_component_declarations = Vec::new();

    for component_def in module.get_component_declarations() {
        let ParsedComponentDeclaration {
            component_name,
            tag_name: name,
            params,
            children,
            range: _,
            closing_tag_name: _,
        } = component_def;

        let mut pushed_params: Vec<&ParsedParameter> = Vec::new();
        let mut resolved_param_types: Vec<(String, Type, bool)> = Vec::new();
        let mut typed_params: Vec<(VarName, Type, Option<TypedExpr>)> = Vec::new();
        if let Some((params, _)) = params {
            for param in params {
                match resolve_type(&param.var_type, &mut type_env) {
                    Ok(param_type) => {
                        // Validate that Option[TrustedHTML] children must have a default
                        if param.var_name.as_str() == "children" {
                            if let Type::Option(inner) = &param_type {
                                if **inner == Type::TrustedHTML && param.default_value.is_none() {
                                    errors.push(TypeError::OptionalChildrenRequiresDefault {
                                        range: param.var_name_range.clone(),
                                    });
                                }
                            }
                        }

                        // Type-check default value if present
                        let has_default = param.default_value.is_some();
                        let mut typed_default_value: Option<TypedExpr> = None;
                        if let Some(default_expr) = &param.default_value {
                            match dop::typecheck_expr(
                                default_expr,
                                &mut env,
                                &mut type_env,
                                annotations,
                                Some(&param_type),
                            ) {
                                Ok(typed_default) => {
                                    let default_type = typed_default.as_type();
                                    if *default_type != param_type {
                                        errors.push(TypeError::DefaultValueTypeMismatch {
                                            param_name: param.var_name.to_string(),
                                            expected: param_type.clone(),
                                            found: default_type.clone(),
                                            range: default_expr.range().clone(),
                                        });
                                    } else {
                                        typed_default_value = Some(typed_default);
                                    }
                                }
                                Err(e) => {
                                    errors.push(e.into());
                                }
                            }
                        }

                        annotations.push(TypeAnnotation {
                            range: param.var_name_range.clone(),
                            typ: param_type.clone(),
                            name: param.var_name.to_string(),
                        });
                        let _ = env.push(param.var_name.to_string(), param_type.clone());
                        pushed_params.push(param);
                        resolved_param_types.push((
                            param.var_name.to_string(),
                            param_type.clone(),
                            has_default,
                        ));
                        typed_params.push((
                            param.var_name.clone(),
                            param_type,
                            typed_default_value,
                        ));
                    }
                    Err(e) => {
                        errors.push(e.into());
                    }
                }
            }
        }

        let typed_children: Vec<_> = children
            .iter()
            .filter_map(|child| {
                typecheck_node(child, state, &mut env, annotations, errors, &mut type_env)
            })
            .collect();

        for param in pushed_params.iter().rev() {
            let (_, _, accessed) = env.pop();
            if !accessed {
                errors.push(TypeError::UnusedVariable {
                    var_name: param.var_name_range.clone(),
                })
            }
        }

        state.set_component_type_info(
            &module.name,
            name.as_str(),
            ComponentTypeInformation {
                parameters: resolved_param_types,
            },
        );

        typed_component_declarations.push(TypedComponentDeclaration {
            component_name: component_name.clone(),
            params: typed_params,
            children: typed_children,
        });
    }

    TypedAst::new(typed_component_declarations, typed_records, typed_enums)
}

fn typecheck_node(
    node: &ParsedNode,
    state: &State,
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut ErrorCollector<TypeError>,
    type_env: &mut Environment<Type>,
) -> Option<TypedNode> {
    match node {
        ParsedNode::If {
            condition,
            children,
            range: _,
        } => {
            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(child, state, env, annotations, errors, type_env)
                })
                .collect();

            let typed_condition = errors.ok_or_add(
                dop::typecheck_expr(condition, env, type_env, annotations, None)
                    .map_err(Into::into),
            )?;

            let condition_type = typed_condition.as_type();
            if *condition_type != Type::Bool {
                errors.push(TypeError::ExpectedBooleanCondition {
                    found: condition_type.to_string(),
                    range: condition.range().clone(),
                })
            }

            Some(TypedNode::If {
                condition: typed_condition,
                children: typed_children,
            })
        }

        ParsedNode::For {
            var_name,
            var_name_range,
            array_expr,
            children,
            range: _,
        } => {
            let typed_array = errors.ok_or_add(
                dop::typecheck_expr(array_expr, env, type_env, annotations, None)
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
                .filter_map(|child| {
                    typecheck_node(child, state, env, annotations, errors, type_env)
                })
                .collect();

            if pushed {
                let (_, _, accessed) = env.pop();
                if !accessed {
                    errors.push(TypeError::UnusedVariable {
                        var_name: var_name_range.clone(),
                    })
                }
            }

            Some(TypedNode::For {
                var_name: var_name.clone(),
                array_expr: typed_array,
                children: typed_children,
            })
        }

        ParsedNode::Let {
            bindings,
            children,
            ..
        } => {
            // Process each binding: resolve type, typecheck value, push to scope
            let mut typed_bindings = Vec::new();

            for binding in bindings {
                // Resolve the declared type
                let resolved_type = match resolve_type(&binding.var_type, type_env) {
                    Ok(t) => t,
                    Err(err) => {
                        errors.push(err.into());
                        continue;
                    }
                };

                // Type-check the value expression with the expected type
                let Some(typed_value) = errors.ok_or_add(
                    dop::typecheck_expr(
                        &binding.value_expr,
                        env,
                        type_env,
                        annotations,
                        Some(&resolved_type),
                    )
                    .map_err(Into::into),
                ) else {
                    continue;
                };

                // Validate that the expression type matches the declared type
                let value_type = typed_value.as_type();
                if *value_type != resolved_type {
                    errors.push(TypeError::LetBindingTypeMismatch {
                        expected: resolved_type.to_string(),
                        found: value_type.to_string(),
                        range: binding.value_expr.range().clone(),
                    });
                }

                // Push the variable into scope
                let pushed = match env.push(binding.var_name.to_string(), resolved_type.clone()) {
                    Ok(_) => {
                        annotations.push(TypeAnnotation {
                            range: binding.var_name_range.clone(),
                            typ: resolved_type.clone(),
                            name: binding.var_name.to_string(),
                        });
                        true
                    }
                    Err(_) => {
                        errors.push(TypeError::VariableIsAlreadyDefined {
                            var: binding.var_name.as_str().to_string(),
                            range: binding.var_name_range.clone(),
                        });
                        false
                    }
                };

                typed_bindings.push((binding, typed_value, pushed));
            }

            // Type-check children with all variables in scope
            let typed_children: Vec<TypedNode> = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(child, state, env, annotations, errors, type_env)
                })
                .collect();

            // Pop variables in reverse order and check for unused
            for (binding, _, pushed) in typed_bindings.iter().rev() {
                if *pushed {
                    let (_, _, accessed) = env.pop();
                    if !accessed {
                        errors.push(TypeError::UnusedVariable {
                            var_name: binding.var_name_range.clone(),
                        })
                    }
                }
            }

            // Build nested Let structure from innermost to outermost
            // Start with children, then wrap with each binding in reverse order
            let mut result = typed_children;
            for (binding, typed_value, _) in typed_bindings.into_iter().rev() {
                result = vec![TypedNode::Let {
                    var: binding.var_name.clone(),
                    value: typed_value,
                    children: result,
                }];
            }

            result.into_iter().next()
        }

        ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range: tag_name,
            declaring_module: definition_module,
            component_name_closing_range: _closing_tag_name,
            args,
            children,
            range: _,
        } => {
            // Transform children
            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(child, state, env, annotations, errors, type_env)
                })
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

            // Check if children are required (TrustedHTML without default) but not provided
            // Note: Option[TrustedHTML] children are optional and don't require being passed
            if module_info.children_are_required(component_name.as_str()) && children.is_empty() {
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
                        .filter(|(name, _, _)| name != "children")
                        .cloned()
                        .collect()
                });

            // Check if there are any required (non-default) params
            let has_required_params = params_without_children
                .as_ref()
                .map(|params| params.iter().any(|(_, _, has_default)| !has_default))
                .unwrap_or(false);

            let typed_args = match (params_without_children.as_deref(), args.as_slice()) {
                (None | Some([]), []) => Vec::new(),
                (None | Some([]), args) => {
                    for arg in args {
                        errors.push(TypeError::UnexpectedArgument {
                            arg: arg.name.as_str().to_string(),
                            range: arg.name.clone(),
                        });
                    }
                    Vec::new()
                }
                (Some(params), []) if has_required_params => {
                    errors.push(TypeError::missing_arguments(params, tag_name.clone()));
                    Vec::new()
                }
                (Some(_), []) => Vec::new(), // no required params, args optional
                (Some(params), args) => {
                    let mut typed_arguments = Vec::new();
                    // Only check for missing params that don't have defaults
                    for (param_name, _, has_default) in params {
                        if !has_default
                            && !args.iter().any(|a| a.name.as_str() == param_name.as_str())
                        {
                            errors.push(TypeError::MissingRequiredParameter {
                                param: param_name.clone(),
                                range: tag_name.clone(),
                            });
                        }
                    }

                    // Get all params (including children) for type checking provided args
                    let all_params = module_info.get_parameter_types(component_name.as_str());

                    for arg in args {
                        let arg_name = arg.name.as_str();

                        // Disallow children arg - it's handled separately via component children
                        if arg_name == "children" {
                            errors.push(TypeError::ChildrenArgNotAllowed {
                                range: arg.name.clone(),
                            });
                            continue;
                        }

                        let (_, param_type, _) = match all_params
                            .and_then(|p| p.iter().find(|(name, _, _)| name.as_str() == arg_name))
                        {
                            None => {
                                errors.push(TypeError::UnexpectedArgument {
                                    arg: arg_name.to_string(),
                                    range: arg.name.clone(),
                                });
                                continue;
                            }
                            Some(param) => param,
                        };

                        // Convert ParsedAttribute value to expression for type checking
                        let arg_expr = match &arg.value {
                            Some(ParsedAttributeValue::Expression(expr)) => expr.clone(),
                            Some(ParsedAttributeValue::String(content)) => {
                                let value = content
                                    .as_ref()
                                    .map(|r| r.as_str().to_string())
                                    .unwrap_or_default();
                                dop::ParsedExpr::StringLiteral {
                                    value,
                                    range: arg.name.clone(),
                                }
                            }
                            None => dop::ParsedExpr::BooleanLiteral {
                                value: true,
                                range: arg.name.clone(),
                            },
                        };

                        let typed_expr = match dop::typecheck_expr(
                            &arg_expr,
                            env,
                            type_env,
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
                        if arg_type != *param_type {
                            errors.push(TypeError::ArgumentIsIncompatible {
                                expected: param_type.clone(),
                                found: arg_type.clone(),
                                arg_name: arg.name.clone(),
                                expr_range: arg_expr.range().clone(),
                            });
                            continue;
                        }

                        let var_name = VarName::new(arg_name).unwrap();
                        typed_arguments.push((var_name, typed_expr));
                    }

                    typed_arguments
                }
            };

            Some(TypedNode::ComponentReference {
                component_name: component_name.clone(),
                declaring_module: definition_module.clone(),
                args: typed_args,
                children: typed_children,
            })
        }

        ParsedNode::Html {
            tag_name,
            closing_tag_name: _,
            attributes,
            children,
            range: _,
        } => {
            let typed_attributes =
                typecheck_attributes(attributes, env, annotations, errors, type_env);

            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(child, state, env, annotations, errors, type_env)
                })
                .collect();

            Some(TypedNode::Html {
                tag_name: tag_name.to_string_span(),
                attributes: typed_attributes,
                children: typed_children,
            })
        }

        ParsedNode::TextExpression {
            expression,
            range: _,
        } => {
            if let Some(typed_expr) = errors.ok_or_add(
                dop::typecheck_expr(expression, env, type_env, annotations, None)
                    .map_err(Into::into),
            ) {
                let expr_type = typed_expr.as_type();
                if *expr_type != Type::String && *expr_type != Type::TrustedHTML {
                    errors.push(TypeError::ExpectedStringForTextExpression {
                        found: expr_type.clone(),
                        range: expression.range().clone(),
                    });
                }
                Some(TypedNode::TextExpression {
                    expression: typed_expr,
                })
            } else {
                None
            }
        }

        ParsedNode::Match {
            subject,
            cases,
            range,
        } => {
            let typed_subject = errors.ok_or_add(
                dop::typecheck_expr(subject, env, type_env, annotations, None).map_err(Into::into),
            )?;
            let subject_type = typed_subject.as_type().clone();

            let patterns = cases
                .iter()
                .map(|case| case.pattern.clone())
                .collect::<Vec<_>>();

            let decision = match PatMatchCompiler::new(0).compile(
                &patterns,
                "match_subject",
                &subject_type,
                subject.range(),
                range,
            ) {
                Ok(decision) => decision,
                Err(err) => {
                    errors.push(err.into());
                    return None;
                }
            };

            let typed_bodies = cases
                .iter()
                .map(|case| {
                    // Extract bindings from pattern
                    let bindings = dop::extract_bindings_from_pattern(&case.pattern, &subject_type);

                    // Push bindings into scope
                    let mut pushed_count = 0;
                    for (name, typ, bind_range) in &bindings {
                        match env.push(name.clone(), typ.clone()) {
                            Ok(_) => {
                                annotations.push(TypeAnnotation {
                                    range: bind_range.clone(),
                                    typ: typ.clone(),
                                    name: name.clone(),
                                });
                                pushed_count += 1;
                            }
                            Err(_) => {
                                errors.push(TypeError::VariableIsAlreadyDefined {
                                    var: name.clone(),
                                    range: bind_range.clone(),
                                });
                            }
                        }
                    }

                    // Typecheck case children
                    let typed_children = case
                        .children
                        .iter()
                        .filter_map(|child| {
                            typecheck_node(child, state, env, annotations, errors, type_env)
                        })
                        .collect::<Vec<_>>();

                    // Pop bindings and check for unused
                    for _ in 0..pushed_count {
                        let (name, _, accessed) = env.pop();
                        if !accessed {
                            if let Some((_, _, bind_range)) =
                                bindings.iter().find(|(n, _, _)| *n == name)
                            {
                                errors.push(TypeError::UnusedVariable {
                                    var_name: bind_range.clone(),
                                });
                            }
                        }
                    }

                    typed_children
                })
                .collect::<Vec<_>>();

            let mut result = decision_to_typed_nodes(&decision, &typed_bodies);

            // Wrap with a Let to bind the subject expression to the subject variable
            result = vec![TypedNode::Let {
                var: VarName::new("match_subject").expect("invalid variable name"),
                value: typed_subject,
                children: result,
            }];

            // Return the single match node (unwrap the vec since we know it's a single Let)
            result.into_iter().next()
        }

        ParsedNode::Placeholder { .. } => Some(TypedNode::Placeholder),

        ParsedNode::Text { value, range: _ } => Some(TypedNode::Text {
            value: value.clone(),
        }),

        ParsedNode::Doctype { value, range: _ } => Some(TypedNode::Doctype {
            value: value.clone(),
        }),
    }
}

fn typecheck_attributes(
    attributes: &[ParsedAttribute],
    env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut ErrorCollector<TypeError>,
    records: &mut Environment<Type>,
) -> Vec<TypedAttribute> {
    let mut typed_attributes = Vec::new();

    for attr in attributes {
        let typed_value = match &attr.value {
            Some(ParsedAttributeValue::Expression(expr)) => {
                if let Some(typed_expr) = errors.ok_or_add(
                    dop::typecheck_expr(expr, env, records, annotations, None).map_err(Into::into),
                ) {
                    // Check that attributes evaluate to strings or booleans
                    let expr_type = typed_expr.as_type();
                    if *expr_type != Type::String && *expr_type != Type::Bool {
                        errors.push(TypeError::ExpectedStringOrBoolAttribute {
                            found: expr_type.to_string(),
                            range: expr.range().clone(),
                        });
                    }
                    Some(TypedAttributeValue::Expression(typed_expr))
                } else {
                    None
                }
            }
            Some(ParsedAttributeValue::String(s)) => {
                let string_span = match s {
                    Some(range) => range.to_string_span(),
                    None => CheapString::new("".to_string()),
                };
                Some(TypedAttributeValue::String(string_span))
            }
            None => None,
        };

        typed_attributes.push(TypedAttribute {
            name: attr.name.to_string(),
            value: typed_value,
        });
    }

    typed_attributes
}

/// Convert a compiled Decision tree into a Vec<TypedNode>.
/// This is used for node-level match statements (as opposed to expression-level matches).
fn decision_to_typed_nodes(decision: &Decision, typed_bodies: &[Vec<TypedNode>]) -> Vec<TypedNode> {
    match decision {
        Decision::Success(body) => {
            let mut result = typed_bodies[body.value].clone();
            // Wrap with Let nodes for each binding (in reverse order so first binding is outermost)
            for binding in body.bindings.iter().rev() {
                let var_name = VarName::new(&binding.name).expect("invalid variable name");
                let value = TypedExpr::Var {
                    value: VarName::new(&binding.source_name).expect("invalid variable name"),
                    kind: binding.typ.clone(),
                };
                result = vec![TypedNode::Let {
                    var: var_name,
                    value,
                    children: result,
                }];
            }
            result
        }

        Decision::Switch(var, cases) => {
            let subject = (
                VarName::new(&var.name).expect("invalid variable name"),
                var.typ.clone(),
            );

            match &var.typ {
                Type::Bool => {
                    // Find the true and false cases
                    let mut true_body = None;
                    let mut false_body = None;

                    for case in cases {
                        match &case.constructor {
                            Constructor::BooleanTrue => {
                                true_body = Some(decision_to_typed_nodes(&case.body, typed_bodies));
                            }
                            Constructor::BooleanFalse => {
                                false_body =
                                    Some(decision_to_typed_nodes(&case.body, typed_bodies));
                            }
                            _ => unreachable!("Invalid constructor for Bool type"),
                        }
                    }

                    vec![TypedNode::Match {
                        match_: Match::Bool {
                            subject,
                            true_body: Box::new(true_body.expect("BoolMatch must have a true arm")),
                            false_body: Box::new(
                                false_body.expect("BoolMatch must have a false arm"),
                            ),
                        },
                    }]
                }

                Type::Option(_) => {
                    // Find the Some and None cases
                    let mut some_arm_binding = None;
                    let mut some_arm_body = None;
                    let mut none_arm_body = None;

                    for case in cases {
                        match &case.constructor {
                            Constructor::OptionSome => {
                                some_arm_binding = case.arguments.first().map(|var| {
                                    VarName::new(&var.name).expect("invalid variable name")
                                });
                                some_arm_body =
                                    Some(decision_to_typed_nodes(&case.body, typed_bodies));
                            }
                            Constructor::OptionNone => {
                                none_arm_body =
                                    Some(decision_to_typed_nodes(&case.body, typed_bodies));
                            }
                            _ => unreachable!("Invalid constructor for Option type"),
                        }
                    }

                    vec![TypedNode::Match {
                        match_: Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body: Box::new(
                                some_arm_body.expect("OptionMatch must have a Some arm"),
                            ),
                            none_arm_body: Box::new(
                                none_arm_body.expect("OptionMatch must have a None arm"),
                            ),
                        },
                    }]
                }

                Type::Enum { variants, .. } => {
                    let arms = cases
                        .iter()
                        .map(|case| {
                            let (pattern, bindings) = match &case.constructor {
                                Constructor::EnumVariant {
                                    enum_name,
                                    variant_name,
                                } => {
                                    let pattern = EnumPattern::Variant {
                                        enum_name: enum_name.to_string(),
                                        variant_name: variant_name.clone(),
                                    };
                                    // Get the variant's fields to create bindings
                                    let empty_fields = vec![];
                                    let variant_fields = variants
                                        .iter()
                                        .find(|(v, _)| v.as_str() == variant_name)
                                        .map(|(_, fields)| fields)
                                        .unwrap_or(&empty_fields);
                                    let bindings: Vec<_> = variant_fields
                                        .iter()
                                        .zip(case.arguments.iter())
                                        .map(|((field_name, _), arg)| {
                                            (
                                                field_name.clone(),
                                                VarName::new(&arg.name)
                                                    .expect("invalid variable name"),
                                            )
                                        })
                                        .collect();
                                    (pattern, bindings)
                                }
                                _ => unreachable!("Invalid constructor for Enum type"),
                            };
                            let body = decision_to_typed_nodes(&case.body, typed_bodies);
                            EnumMatchArm {
                                pattern,
                                bindings,
                                body,
                            }
                        })
                        .collect();

                    vec![TypedNode::Match {
                        match_: Match::Enum { subject, arms },
                    }]
                }

                Type::Record {
                    fields: type_fields,
                    ..
                } => {
                    // Records have only one case (the record itself)
                    let case = &cases[0];

                    // Build the body with Let bindings for each field
                    let mut body = decision_to_typed_nodes(&case.body, typed_bodies);

                    // Wrap with Let nodes for each field (using FieldAccess)
                    // Iterate in reverse so bindings are in the correct order
                    for (i, (field_name, _field_type)) in type_fields.iter().enumerate().rev() {
                        let var = &case.arguments[i];
                        let var_name = VarName::new(&var.name).expect("invalid variable name");

                        // Create field access: subject.field_name
                        let field_access = TypedExpr::FieldAccess {
                            record: Box::new(TypedExpr::Var {
                                value: subject.0.clone(),
                                kind: subject.1.clone(),
                            }),
                            field: field_name.clone(),
                            kind: var.typ.clone(),
                        };

                        body = vec![TypedNode::Let {
                            var: var_name,
                            value: field_access,
                            children: body,
                        }];
                    }

                    body
                }

                _ => panic!("Unsupported type for pattern matching: {:?}", var.typ),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parser::parse;
    use crate::hop::syntax::transform::whitespace_removal::remove_whitespace;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn check(archive_str: &str, expected: Expect) {
        let archive = Archive::from(archive_str);
        let mut error_output = Vec::new();
        let mut ast_output = Vec::new();
        let error_annotator = DocumentAnnotator::new()
            .with_label("error")
            .with_lines_before(1)
            .with_location();

        let mut typechecker = TypeChecker::default();
        let mut module_names = Vec::new();

        // Process all .hop files in the archive
        for file in archive.iter() {
            if !file.name.ends_with(".hop") {
                panic!("Got invalid file name")
            }
            let source_code = file.content.trim();
            let mut parse_errors = ErrorCollector::new();
            let module_name = ModuleName::new(file.name.trim_end_matches(".hop")).unwrap();
            module_names.push(module_name.clone());
            let ast = parse(module_name, source_code.to_string(), &mut parse_errors);

            if !parse_errors.is_empty() {
                panic!("Got parse errors: {:#?}", parse_errors);
            }

            let ast = remove_whitespace(ast);
            typechecker.typecheck(&[&ast]);

            let type_errors = typechecker.type_errors.get(&ast.name);

            if type_errors.is_some_and(|err| !err.is_empty()) {
                error_output.push(error_annotator.annotate(
                    Some(&file.name),
                    typechecker.type_errors.get(&ast.name).unwrap(),
                ));
            }
        }

        if !error_output.is_empty() {
            expected.assert_eq(error_output.join("\n").as_str());
        } else {
            for module_name in &module_names {
                if let Some(typed_ast) = typechecker.typed_asts.get(module_name) {
                    ast_output.push(format!(
                        "-- {}.hop --\n{}",
                        module_name,
                        typed_ast.to_string()
                    ));
                }
            }
            expected.assert_eq(&ast_output.join("\n"));
        }
    }

    #[test]
    fn should_accept_empty_file() {
        check(
            "-- main.hop --",
            expect![[r#"
            -- main.hop --
        "#]],
        );
    }

    #[test]
    fn should_accept_component_declaration_without_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main></Main>
            "#]],
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
                import other::Foo

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module other was not found
                  --> main.hop (line 1, col 8)
                1 | import other::Foo
                  |        ^^^^^^^^^^
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
                import other::Foo

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module other does not declare a component named Foo
                  --> main.hop (line 1, col 15)
                1 | import other::Foo
                  |               ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_an_import_references_a_component_from_an_empty_module() {
        check(
            indoc! {r#"
                -- other.hop --
                -- main.hop --
                import other::Foo

                <Main>
                </Main>
            "#},
            expect![[r#"
                error: Module other does not declare a component named Foo
                  --> main.hop (line 1, col 15)
                1 | import other::Foo
                  |               ^^^
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
                import other::Foo

                <Main>
                </Main>
            "#},
            expect![[r#"
                -- other.hop --
                <Foo></Foo>

                -- main.hop --
                <Main></Main>
            "#]],
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
            expect![[r#"
                -- other.hop --
                <Foo></Foo>

                -- main.hop --
                <Foo></Foo>
            "#]],
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
                import other::Foo

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
                  <Main b="foo" a={true}/>
                </Foo>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {a: Bool, b: String}>
                  <if {a}>
                    <div>
                      {b}
                    </div>
                  </if>
                </Main>

                <Foo>
                  <Main b={"foo"} a={true}/>
                </Foo>
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
                  <Main b="foo"/>
                </Foo>
            "#},
            expect![[r#"
                error: Missing required parameter 'a'
                  --> main.hop (line 7, col 4)
                6 | <Foo>
                7 |   <Main b="foo"/>
                  |    ^^^^
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
                    <Main a="" b={1}/>
                </Foo>
            "#},
            expect![[r#"
                error: Unexpected argument 'b'
                  --> main.hop (line 5, col 16)
                4 | <Foo>
                5 |     <Main a="" b={1}/>
                  |                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_no_arguments_are_passed_to_component_that_requires_them() {
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
                  <Main a="foo" />
                </Foo>
            "#},
            expect![[r#"
                error: Unexpected argument 'a'
                  --> main.hop (line 5, col 9)
                4 | <Foo>
                5 |   <Main a="foo" />
                  |         ^
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
    fn should_accept_component_declaration_with_string_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {params: String}>
                	<div>{params}</div>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {params: String}>
                  <div>
                    {params}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_declaration_with_bool_parameter() {
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
                -- main.hop --
                <ToggleComp {enabled: Bool}>
                  <if {enabled}>
                    <div>
                      Enabled
                    </div>
                  </if>
                </ToggleComp>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_declaration_with_float_parameter() {
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
                -- main.hop --
                <CounterComp {count: Float}>
                  <if {(count == 0)}>
                    <div>
                      Zero
                    </div>
                  </if>
                </CounterComp>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_declaration_with_record_parameter() {
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
                -- main.hop --
                record Item {
                  active: Bool,
                  name: Bool,
                }

                record Params {
                  items: Array[main::Item],
                }

                <Main {params: main::Params}>
                  <for {item in params.items}>
                    <if {item.active}></if>
                    <if {item.name}></if>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_declaration_with_array_parameter() {
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
                -- main.hop --
                <ListComp {items: Array[String]}>
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
                </ListComp>
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
                -- main.hop --
                record Params {
                  x: String,
                  y: String,
                }

                <Main {params: main::Params}>
                  <if {(params.x == params.y)}>
                    <div>
                      Values are equal
                    </div>
                  </if>
                </Main>
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
                -- main.hop --
                record Item {
                  a: Bool,
                  b: Bool,
                }

                <Main {params: Array[main::Item]}>
                  <for {j in params}>
                    <if {j.a}></if>
                  </for>
                  <for {j in params}>
                    <if {j.b}></if>
                  </for>
                </Main>
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
                -- main.hop --
                <Main {i: Array[Bool]}>
                  <for {j in i}>
                    <if {j}></if>
                  </for>
                </Main>
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
            "#]],
        );
    }

    #[test]
    fn should_accept_components_to_call_each_other_in_a_chain() {
        check(
            indoc! {r#"
                -- a/bar.hop --
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
                import a::bar::WidgetComp
                import a::bar::Config

                record Data {
                  items: Array[Config],
                }

                <PanelComp {data: Data}>
                  <for {item in data.items}>
                    <WidgetComp config={item}/>
                  </for>
                </PanelComp>

                -- main.hop --
                import foo::PanelComp
                import foo::Data

                record Dashboard {
                  items: Array[Data],
                }
                record Settings {
                  dashboard: Data,
                }

                <Main {settings: Settings}>
                  <PanelComp data={settings.dashboard}/>
                </Main>
            "#},
            expect![[r#"
                -- a::bar.hop --
                record Config {
                  enabled: Bool,
                  title: String,
                }

                <WidgetComp {config: a::bar::Config}>
                  <if {config.enabled}>
                    <div>
                      {config.title}
                    </div>
                  </if>
                </WidgetComp>

                -- foo.hop --
                record Data {
                  items: Array[a::bar::Config],
                }

                <PanelComp {data: foo::Data}>
                  <for {item in data.items}>
                    <WidgetComp config={item}/>
                  </for>
                </PanelComp>

                -- main.hop --
                record Dashboard {
                  items: Array[foo::Data],
                }

                record Settings {
                  dashboard: foo::Data,
                }

                <Main {settings: main::Settings}>
                  <PanelComp data={settings.dashboard}/>
                </Main>
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
                import foo::FooComp
                import bar::BarComp
                import foo::User

                <Main {user: User}>
                    <FooComp user={user}/>
                    <BarComp user={user}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type foo::User is incompatible with expected type bar::User
                  --> main.hop (line 7, col 20)
                6 |     <FooComp user={user}/>
                7 |     <BarComp user={user}/>
                  |                    ^^^^
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
                import foo::FooComp
                import bar::BarComp
                import foo::User

                <Main {user: User}>
                    <FooComp user={user}/>
                    <BarComp user={user}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'user' of type foo::User is incompatible with expected type bar::User
                  --> main.hop (line 7, col 20)
                6 |     <FooComp user={user}/>
                7 |     <BarComp user={user}/>
                  |                    ^^^^
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
                -- main.hop --
                record User {
                  url: String,
                  theme: String,
                }

                <Main {user: main::User}>
                  <a href={user.url} class={user.theme}>
                    Link
                  </a>
                </Main>
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
                -- main.hop --
                record User {
                  is_active: Bool,
                }

                <Main {user: main::User}>
                  <if {user.is_active}>
                    <div>
                      User is active
                    </div>
                  </if>
                </Main>
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
                	<StringComp message={42}/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'message' of type Int is incompatible with expected type String
                  --> main.hop (line 5, col 23)
                4 | <Main>
                5 |     <StringComp message={42}/>
                  |                          ^^
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
                	<ToggleComp enabled="not a boolean"/>
                </Main>
            "#},
            expect![[r#"
                error: Argument 'enabled' of type String is incompatible with expected type Bool
                  --> main.hop (line 7, col 14)
                6 | <Main>
                7 |     <ToggleComp enabled="not a boolean"/>
                  |                 ^^^^^^^
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
                    <Main a={1 == ""} b={1 == ""}/>
                </Foo>
            "#},
            expect![[r#"
                error: Can not compare Int to String
                  --> main.hop (line 5, col 14)
                4 | <Foo>
                5 |     <Main a={1 == ""} b={1 == ""}/>
                  |              ^^^^^^^

                error: Can not compare Int to String
                  --> main.hop (line 5, col 26)
                4 | <Foo>
                5 |     <Main a={1 == ""} b={1 == ""}/>
                  |                          ^^^^^^^
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
                    <List items={[]}/>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <List {items: Array[String]}>
                  <for {item in items}>
                    <li>
                      {item}
                    </li>
                  </for>
                </List>

                <Main>
                  <List items={[]}/>
                </Main>
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
                  --> main.hop (line 2, col 6)
                1 | <Main>
                2 |     {false}
                  |      ^^^^^
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
                -- main.hop --
                record User {
                  name: String,
                }

                <Main {user: main::User}>
                  <div>
                    {user.name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_accessing_a_nested_record_field() {
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
                -- main.hop --
                record Address {
                  city: String,
                }

                record User {
                  name: String,
                  address: main::Address,
                }

                <Main {user: main::User}>
                  <div>
                    {user.address.city}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_accessing_a_deeply_nested_record_field() {
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
                -- main.hop --
                record Theme {
                  dark: Bool,
                }

                record UI {
                  theme: main::Theme,
                }

                record Users {
                  enabled: Bool,
                }

                record Endpoints {
                  users: main::Users,
                }

                record API {
                  endpoints: main::Endpoints,
                }

                record Connection {
                  ssl: Bool,
                }

                record Database {
                  connection: main::Connection,
                }

                record App {
                  ui: main::UI,
                  api: main::API,
                  database: main::Database,
                }

                record Params {
                  app: main::App,
                }

                <Main {params: main::Params}>
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
    fn should_not_reexport_imported_components() {
        check(
            indoc! {r#"
                -- foo.hop --
                record User {name: String}
                <Foo>
                </Foo>

                -- bar.hop --
                import foo::User
                <Bar>
                </Bar>

                -- baz.hop --
                import bar::User
                <Baz>
                </Baz>
            "#},
            expect![[r#"
                error: Module bar does not declare a component named User
                  --> baz.hop (line 1, col 13)
                1 | import bar::User
                  |             ^^^^
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
                import foo::Address

                record User {
                  name: String,
                  address: Address,
                }

                <Bar {user: User}>
                    <div>{user.address.city}</div>
                </Bar>
                -- baz.hop --
                import bar::Bar
                import bar::User
                import foo::Address
                <Baz>
                    <Bar user={User(name: "Alice", address: Address(city: "NYC"))} />
                </Baz>
            "#},
            expect![[r#"
                -- foo.hop --
                record Address {
                  city: String,
                }

                <Foo></Foo>

                -- bar.hop --
                record User {
                  name: String,
                  address: foo::Address,
                }

                <Bar {user: bar::User}>
                  <div>
                    {user.address.city}
                  </div>
                </Bar>

                -- baz.hop --
                <Baz>
                  <Bar user={User(name: "Alice", address: Address(city: "NYC"))}/>
                </Baz>
            "#]],
        );
    }

    #[test]
    fn should_accept_import_and_use_of_enums_declared_in_other_modules() {
        check(
            indoc! {r#"
                -- colors.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                <ColorDisplay {color: Color}>
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                    }}</div>
                </ColorDisplay>

                -- main.hop --
                import colors::Color
                import colors::ColorDisplay

                <Main>
                    <ColorDisplay color={Color::Red}/>
                </Main>
            "#},
            expect![[r#"
                -- colors.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <ColorDisplay {color: colors::Color}>
                  <div>
                    {match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}
                  </div>
                </ColorDisplay>

                -- main.hop --
                <Main>
                  <ColorDisplay color={Color::Red}/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_when_type_in_record_declaration_is_not_defined() {
        check(
            indoc! {r#"
                -- hop/button.hop --
                <Button>
                </Button>
                -- hop/input.hop --
                <Input>
                </Input>
                -- main.hop --
                import hop::button::Button
                import hop::input::Input

                record Page {
                  id: Str
                }
            "#},
            expect![[r#"
                error: Type 'Str' is not defined
                  --> main.hop (line 5, col 7)
                4 | record Page {
                5 |   id: Str
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_component_declaration_with_enum_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {Red, Green, Blue}

                <Main {a: Color, b: Color}>
                    <if {a == b}>
                    </if>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {a: main::Color, b: main::Color}>
                  <if {(a == b)}></if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_in_text_interpolation() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                <Main {color: Color}>
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                    }}</div>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {color: main::Color}>
                  <div>
                    {match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_match_expression_with_non_enum_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {name: String}>
                    <div>{match name {Color::Red => "red"}}</div>
                </Main>
            "#},
            expect![[r#"
                error: Match is not implemented for type String
                  --> main.hop (line 2, col 17)
                1 | <Main {name: String}>
                2 |     <div>{match name {Color::Red => "red"}}</div>
                  |                 ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_expression_with_mismatched_arm_types() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                }

                <Main {color: Color}>
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => 42,
                    }}</div>
                </Main>
            "#},
            expect![[r#"
                error: Match arms must all have the same type, expected String but found Int
                  --> main.hop (line 9, col 25)
                 8 |         Color::Red => "red",
                 9 |         Color::Green => 42,
                   |                         ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_expression_with_missing_variant() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                <Main {color: Color}>
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => "green",
                    }}</div>
                </Main>
            "#},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue
                  --> main.hop (line 8, col 11)
                 7 | <Main {color: Color}>
                 8 |     <div>{match color {
                   |           ^^^^^^^^^^^^^
                 9 |         Color::Red => "red",
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                10 |         Color::Green => "green",
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                11 |     }}</div>
                   | ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_expression_with_wrong_enum_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                }

                enum Size {
                    Small,
                    Large,
                }

                <Main {color: Color}>
                    <div>{match color {
                        Color::Red => "red",
                        Size::Small => "small",
                    }}</div>
                </Main>
            "#},
            expect![[r#"
                error: Match pattern enum 'Size' does not match subject enum 'Color'
                  --> main.hop (line 14, col 9)
                13 |         Color::Red => "red",
                14 |         Size::Small => "small",
                   |         ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_enum_literal() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                <Main {color: Color}>
                    <if {color == Color::Red}>
                        <div>{match color {
                            Color::Red => "red",
                            Color::Green => "green",
                            Color::Blue => "blue",
                        }}</div>
                    </if>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {color: main::Color}>
                  <if {(color == Color::Red)}>
                    <div>
                      {match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                      }}
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_as_record_field_type() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Status {
                    Active,
                    Inactive,
                }

                record User {
                    name: String,
                    status: Status,
                }

                <Main {user: User}>
                    <div>{match user.status {
                        Status::Active => "active",
                        Status::Inactive => "inactive",
                    }}</div>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: String,
                  status: main::Status,
                }

                enum Status {
                  Active,
                  Inactive,
                }

                <Main {user: main::User}>
                  <div>
                    {let v0 = user.status in match v0 {
                      Status::Active => "active",
                      Status::Inactive => "inactive",
                    }}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_field_in_conditional() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Role {
                    Admin,
                    User,
                    Guest,
                }

                record Person {
                    name: String,
                    role: Role,
                }

                <Main {person: Person}>
                    <if {person.role == Role::Admin}>
                        <div>Welcome, admin!</div>
                    </if>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                record Person {
                  name: String,
                  role: main::Role,
                }

                enum Role {
                  Admin,
                  User,
                  Guest,
                }

                <Main {person: main::Person}>
                  <if {(person.role == Role::Admin)}>
                    <div>
                      Welcome, admin!
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_with_default_parameter_when_argument_omitted() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String = "World"}>
                  Hello, {name}!
                </Greeting>
                <Main>
                  <Greeting />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Greeting {name: String = "World"}>
                  Hello,
                  {name}
                  !
                </Greeting>

                <Main>
                  <Greeting/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_with_default_parameter_when_argument_provided() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String = "World"}>
                  Hello, {name}!
                </Greeting>
                <Main>
                  <Greeting name="Claude" />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Greeting {name: String = "World"}>
                  Hello,
                  {name}
                  !
                </Greeting>

                <Main>
                  <Greeting name={"Claude"}/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_with_mixed_required_and_default_parameters() {
        check(
            indoc! {r#"
                -- main.hop --
                <UserCard {name: String, role: String = "user"}>
                  {name} ({role})
                </UserCard>
                <Main>
                  <UserCard name="Alice" />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <UserCard {name: String, role: String = "user"}>
                  {name}
                  (
                  {role}
                  )
                </UserCard>

                <Main>
                  <UserCard name={"Alice"}/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_when_required_param_is_missing_but_default_param_is_provided() {
        check(
            indoc! {r#"
                -- main.hop --
                <UserCard {name: String, role: String = "user"}>
                  {name} ({role})
                </UserCard>
                <Main>
                  <UserCard role="admin" />
                </Main>
            "#},
            expect![[r#"
                error: Missing required parameter 'name'
                  --> main.hop (line 5, col 4)
                4 | <Main>
                5 |   <UserCard role="admin" />
                  |    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_default_value_type_does_not_match_parameter_type() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String = 42}>
                  Hello, {name}!
                </Greeting>
                <Main>
                  <Greeting />
                </Main>
            "#},
            expect![[r#"
                error: Default value for parameter 'name' has type Int, expected String
                  --> main.hop (line 1, col 27)
                1 | <Greeting {name: String = 42}>
                  |                           ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_all_default_params_component_called_without_args() {
        check(
            indoc! {r#"
                -- main.hop --
                <Config {debug: Bool = false, timeout: Int = 30}>
                </Config>
                <Main>
                  <Config />
                </Main>
            "#},
            expect![[r#"
                error: Unused variable timeout
                  --> main.hop (line 1, col 31)
                1 | <Config {debug: Bool = false, timeout: Int = 30}>
                  |                               ^^^^^^^

                error: Unused variable debug
                  --> main.hop (line 1, col 10)
                1 | <Config {debug: Bool = false, timeout: Int = 30}>
                  |          ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_default_empty_array_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <ItemList {items: Array[String] = []}>
                  <for {item in items}>
                    {item}
                  </for>
                </ItemList>
                <Main>
                  <ItemList />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <ItemList {items: Array[String] = []}>
                  <for {item in items}>
                    {item}
                  </for>
                </ItemList>

                <Main>
                  <ItemList/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_default_record_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record Config { name: String, enabled: Bool }
                <Settings {config: Config = Config(name: "default", enabled: true)}>
                  {config.name}
                </Settings>
                <Main>
                  <Settings />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                record Config {
                  name: String,
                  enabled: Bool,
                }

                <Settings {config: main::Config = Config(name: "default", enabled: true)}>
                  {config.name}
                </Settings>

                <Main>
                  <Settings/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_default_enum_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Status { Active(since: Int), Inactive, Pending }
                <Badge {status: Status = Status::Active(since: 2000)}>
                  {match status {
                    Status::Active(since: _) => "active",
                    _ => "not active",
                  }}
                </Badge>
                <Main>
                  <Badge />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                enum Status {
                  Active(since: Int),
                  Inactive,
                  Pending,
                }

                <Badge {status: main::Status = Status::Active(since: 2000)}>
                  {match status {
                    Status::Active => let v0 = status.since in "active",
                    Status::Inactive => "not active",
                    Status::Pending => "not active",
                  }}
                </Badge>

                <Main>
                  <Badge/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_option_parameter_with_some_argument() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: Option[String]}>
                  <if {name == None}></if>
                </Greeting>
                <Main>
                  <Greeting name={Some("World")} />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Greeting {name: Option[String]}>
                  <if {(name == None)}></if>
                </Greeting>

                <Main>
                  <Greeting name={Some("World")}/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_option_parameter_with_none_argument() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: Option[String]}>
                  <if {name == None}></if>
                </Greeting>
                <Main>
                  <Greeting name={None} />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Greeting {name: Option[String]}>
                  <if {(name == None)}></if>
                </Greeting>

                <Main>
                  <Greeting name={None}/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_option_parameter_with_default_none() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: Option[String] = None}>
                  <if {name == None}></if>
                </Greeting>
                <Main>
                  <Greeting />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Greeting {name: Option[String] = None}>
                  <if {(name == None)}></if>
                </Greeting>

                <Main>
                  <Greeting/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_option_parameter_with_default_some() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: Option[String] = Some("World")}>
                  <if {name == None}></if>
                </Greeting>
                <Main>
                  <Greeting />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Greeting {name: Option[String] = Some("World")}>
                  <if {(name == None)}></if>
                </Greeting>

                <Main>
                  <Greeting/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_non_option_argument_for_option_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: Option[String]}>
                  <if {name == None}></if>
                </Greeting>
                <Main>
                  <Greeting name="World" />
                </Main>
            "#},
            expect![[r#"
                error: Argument 'name' of type String is incompatible with expected type Option[String]
                  --> main.hop (line 5, col 13)
                4 | <Main>
                5 |   <Greeting name="World" />
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_with_option() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(y)}>
                            found {y}
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {x: Option[String]}>
                  <let {match_subject = x}>
                    <match {match_subject}>
                      <case {Some(v0)}>
                        <let {y = v0}>
                          found
                          {y}
                        </let>
                      </case>
                      <case {None}>
                        nothing
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_with_enum() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                <Main {c: Color}>
                    <match {c}>
                        <case {Color::Red}>red</case>
                        <case {Color::Green}>green</case>
                        <case {Color::Blue}>blue</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {c: main::Color}>
                  <let {match_subject = c}>
                    <match {match_subject}>
                      <case {Color::Red}>
                        red
                      </case>
                      <case {Color::Green}>
                        green
                      </case>
                      <case {Color::Blue}>
                        blue
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_on_enum_literal_with_fields() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Status { Active(name: String), Inactive }
                <Main>
                    <match {Status::Active(name: "test")}>
                        <case {Status::Active(name: n)}>
                            {n}
                        </case>
                        <case {Status::Inactive}>
                            none
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                enum Status {
                  Active(name: String),
                  Inactive,
                }

                <Main>
                  <let {match_subject = Status::Active(name: "test")}>
                    <match {match_subject}>
                      <case {Status::Active(name: v0)}>
                        <let {n = v0}>
                          {n}
                        </let>
                      </case>
                      <case {Status::Inactive}>
                        none
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_with_bool() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {flag: Bool}>
                    <match {flag}>
                        <case {true}>yes</case>
                        <case {false}>no</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {flag: Bool}>
                  <let {match_subject = flag}>
                    <match {match_subject}>
                      <case {true}>
                        yes
                      </case>
                      <case {false}>
                        no
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_pattern_type_mismatch() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {flag: Bool}>
                    <match {flag}>
                        <case {Some(x)}>yes</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Some(x)
                  --> main.hop (line 3, col 16)
                2 |     <match {flag}>
                3 |         <case {Some(x)}>yes</case>
                  |                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_allow_binding_in_match_case_children() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(name)}>
                            <div class={name}></div>
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {x: Option[String]}>
                  <let {match_subject = x}>
                    <match {match_subject}>
                      <case {Some(v0)}>
                        <let {name = v0}>
                          <div class={name}></div>
                        </let>
                      </case>
                      <case {None}>
                        nothing
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_non_matchable_type() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {name: String}>
                    <match {name}>
                        <case {Some(x)}>yes</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match is not implemented for type String
                  --> main.hop (line 2, col 13)
                1 | <Main {name: String}>
                2 |     <match {name}>
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_missing_enum_variants() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                <Main {c: Color}>
                    <match {c}>
                        <case {Color::Red}>red</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue, Color::Green
                  --> main.hop (line 3, col 5)
                2 | <Main {c: Color}>
                3 |     <match {c}>
                  |     ^^^^^^^^^^^
                4 |         <case {Color::Red}>red</case>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                5 |     </match>
                  | ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_missing_option_arm() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(y)}>{y}</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match expression is missing arms for: None
                  --> main.hop (line 2, col 5)
                1 | <Main {x: Option[String]}>
                2 |     <match {x}>
                  |     ^^^^^^^^^^^
                3 |         <case {Some(y)}>{y}</case>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                4 |     </match>
                  | ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_missing_bool_arm() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {flag: Bool}>
                    <match {flag}>
                        <case {true}>yes</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match expression is missing arms for: false
                  --> main.hop (line 2, col 5)
                1 | <Main {flag: Bool}>
                2 |     <match {flag}>
                  |     ^^^^^^^^^^^^^^
                3 |         <case {true}>yes</case>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                4 |     </match>
                  | ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_wrong_enum_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green }
                enum Size { Small, Large }
                <Main {c: Color}>
                    <match {c}>
                        <case {Color::Red}>red</case>
                        <case {Size::Small}>small</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match pattern enum 'Size' does not match subject enum 'Color'
                  --> main.hop (line 6, col 16)
                5 |         <case {Color::Red}>red</case>
                6 |         <case {Size::Small}>small</case>
                  |                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unused_binding_in_match_case() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(unused)}>
                            found something
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Unused variable unused
                  --> main.hop (line 3, col 21)
                 2 |     <match {x}>
                 3 |         <case {Some(unused)}>
                   |                     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_binding_that_shadows_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(x)}>
                            {x}
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Variable x is already defined
                  --> main.hop (line 3, col 21)
                 2 |     <match {x}>
                 3 |         <case {Some(x)}>
                   |                     ^

                error: Expected string for text expression, got Option[String]
                  --> main.hop (line 4, col 14)
                 3 |         <case {Some(x)}>
                 4 |             {x}
                   |              ^
            "#]],
        );
    }

    #[test]
    fn should_accept_nested_match_nodes() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[Option[String]]}>
                    <match {x}>
                        <case {Some(inner)}>
                            <match {inner}>
                                <case {Some(val)}>{val}</case>
                                <case {None}>inner none</case>
                            </match>
                        </case>
                        <case {None}>
                            outer none
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {x: Option[Option[String]]}>
                  <let {match_subject = x}>
                    <match {match_subject}>
                      <case {Some(v0)}>
                        <let {inner = v0}>
                          <let {match_subject = inner}>
                            <match {match_subject}>
                              <case {Some(v0)}>
                                <let {val = v0}>
                                  {val}
                                </let>
                              </case>
                              <case {None}>
                                inner none
                              </case>
                            </match>
                          </let>
                        </let>
                      </case>
                      <case {None}>
                        outer none
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_inside_for_loop() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {items: Array[Option[String]]}>
                    <for {item in items}>
                        <match {item}>
                            <case {Some(val)}>{val}</case>
                            <case {None}>-</case>
                        </match>
                    </for>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {items: Array[Option[String]]}>
                  <for {item in items}>
                    <let {match_subject = item}>
                      <match {match_subject}>
                        <case {Some(v0)}>
                          <let {val = v0}>
                            {val}
                          </let>
                        </case>
                        <case {None}>
                          -
                        </case>
                      </match>
                    </let>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_with_wildcard_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(_)}>
                            found something
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {x: Option[String]}>
                  <let {match_subject = x}>
                    <match {match_subject}>
                      <case {Some(v0)}>
                        found something
                      </case>
                      <case {None}>
                        nothing
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_bindings_with_same_name_in_different_cases() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {r1: Option[String], r2: Option[Bool]}>
                    <match {r1}>
                        <case {Some(val)}>{val}</case>
                        <case {None}>
                            <match {r2}>
                                <case {Some(val)}><if {val}>yes</if></case>
                                <case {None}>both none</case>
                            </match>
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {r1: Option[String], r2: Option[Bool]}>
                  <let {match_subject = r1}>
                    <match {match_subject}>
                      <case {Some(v0)}>
                        <let {val = v0}>
                          {val}
                        </let>
                      </case>
                      <case {None}>
                        <let {match_subject = r2}>
                          <match {match_subject}>
                            <case {Some(v0)}>
                              <let {val = v0}>
                                <if {val}>
                                  yes
                                </if>
                              </let>
                            </case>
                            <case {None}>
                              both none
                            </case>
                          </match>
                        </let>
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_with_record_field_subject() {
        check(
            indoc! {r#"
                -- main.hop --
                record User { name: Option[String] }
                <Main {user: User}>
                    <match {user.name}>
                        <case {Some(n)}>{n}</case>
                        <case {None}>anonymous</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: Option[String],
                }

                <Main {user: main::User}>
                  <let {match_subject = user.name}>
                    <match {match_subject}>
                      <case {Some(v0)}>
                        <let {n = v0}>
                          {n}
                        </let>
                      </case>
                      <case {None}>
                        anonymous
                      </case>
                    </match>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_match_node_with_int_type() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {count: Int}>
                    <match {count}>
                        <case {Some(x)}>{x}</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Match is not implemented for type Int
                  --> main.hop (line 2, col 13)
                1 | <Main {count: Int}>
                2 |     <match {count}>
                  |             ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_inside_if_condition() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {show: Bool, x: Option[String]}>
                    <if {show}>
                        <match {x}>
                            <case {Some(v)}>{v}</case>
                            <case {None}>none</case>
                        </match>
                    </if>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {show: Bool, x: Option[String]}>
                  <if {show}>
                    <let {match_subject = x}>
                      <match {match_subject}>
                        <case {Some(v0)}>
                          <let {v = v0}>
                            {v}
                          </let>
                        </case>
                        <case {None}>
                          none
                        </case>
                      </match>
                    </let>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_node_in_html_element() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {x: Option[String]}>
                    <div>
                        <match {x}>
                            <case {Some(v)}><span>{v}</span></case>
                            <case {None}><span>none</span></case>
                        </match>
                    </div>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {x: Option[String]}>
                  <div>
                    <let {match_subject = x}>
                      <match {match_subject}>
                        <case {Some(v0)}>
                          <let {v = v0}>
                            <span>
                              {v}
                            </span>
                          </let>
                        </case>
                        <case {None}>
                          <span>
                            none
                          </span>
                        </case>
                      </match>
                    </let>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_shadowed_variable_in_nested_match_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {c: Option[String]}>
                  <match {c}>
                    <case {Some(x)}>
                      {match Some("foo") {
                        None    => x,
                        Some(x) => x,
                      }}
                    </case>
                    <case {None}>
                    </case>
                  </match>
                </Main>
            "#},
            expect![[r#"
                error: Variable 'x' is already defined
                  --> main.hop (line 6, col 14)
                 5 |         None    => x,
                 6 |         Some(x) => x,
                   |              ^
            "#]],
        );
    }

    #[test]
    fn should_accept_boolean_expressions_in_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {is_required: Bool}>
                  <input required={is_required}>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main {is_required: Bool}>
                  <input required={is_required}></input>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_boolean_literal_in_attributes() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <input required={true}>
                  <input disabled={false}>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main>
                  <input required={true}></input>
                  <input disabled={false}></input>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_non_string_or_bool_attribute_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {count: Int}>
                  <div data-count={count}></div>
                </Main>
            "#},
            expect![[r#"
                error: Expected String or Bool attribute, got Int
                  --> main.hop (line 2, col 20)
                1 | <Main {count: Int}>
                2 |   <div data-count={count}></div>
                  |                    ^^^^^
            "#]],
        );
    }

    #[test]
    fn optional_children_requires_default() {
        // children: Option[TrustedHTML] without a default value should error
        check(
            indoc! {r#"
                -- main.hop --
                <Separator {children: Option[TrustedHTML]}>
                  <li>{children}</li>
                </Separator>
                <Main>
                  <Separator />
                </Main>
            "#},
            expect![[r#"
                error: Optional children (`children: Option[TrustedHTML]`) must have a default value (e.g., `= None`)
                  --> main.hop (line 1, col 13)
                1 | <Separator {children: Option[TrustedHTML]}>
                  |             ^^^^^^^^

                error: Expected string for text expression, got Option[TrustedHTML]
                  --> main.hop (line 2, col 8)
                1 | <Separator {children: Option[TrustedHTML]}>
                2 |   <li>{children}</li>
                  |        ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn optional_children_with_default_none_allows_omitting_children() {
        // children: Option[TrustedHTML] = None should allow omitting children
        check(
            indoc! {r#"
                -- main.hop --
                <Separator {children: Option[TrustedHTML] = None}>
                  <li>
                    <match {children}>
                      <case {Some(c)}>{c}</case>
                      <case {None}><span>Default</span></case>
                    </match>
                  </li>
                </Separator>
                <Main>
                  <Separator />
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Separator {children: Option[TrustedHTML] = None}>
                  <li>
                    <let {match_subject = children}>
                      <match {match_subject}>
                        <case {Some(v0)}>
                          <let {c = v0}>
                            {c}
                          </let>
                        </case>
                        <case {None}>
                          <span>
                            Default
                          </span>
                        </case>
                      </match>
                    </let>
                  </li>
                </Separator>

                <Main>
                  <Separator/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn optional_children_with_default_none_allows_passing_children() {
        check(
            indoc! {r#"
                -- main.hop --
                <Separator {children: Option[TrustedHTML] = None}>
                  <li>
                    <match {children}>
                      <case {Some(c)}>{c}</case>
                      <case {None}><span>Default</span></case>
                    </match>
                  </li>
                </Separator>
                <Main>
                  <Separator>
                    <strong>Custom</strong>
                  </Separator>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Separator {children: Option[TrustedHTML] = None}>
                  <li>
                    <let {match_subject = children}>
                      <match {match_subject}>
                        <case {Some(v0)}>
                          <let {c = v0}>
                            {c}
                          </let>
                        </case>
                        <case {None}>
                          <span>
                            Default
                          </span>
                        </case>
                      </match>
                    </let>
                  </li>
                </Separator>

                <Main>
                  <Separator>
                    <strong>
                      Custom
                    </strong>
                  </Separator>
                </Main>
            "#]],
        );
    }

    #[test]
    fn optional_children_cannot_be_used_directly_without_match() {
        check(
            indoc! {r#"
                -- main.hop --
                <Wrapper {children: Option[TrustedHTML] = None}>
                  <div>{children}</div>
                </Wrapper>
                <Main>
                  <Wrapper />
                </Main>
            "#},
            expect![[r#"
                error: Expected string for text expression, got Option[TrustedHTML]
                  --> main.hop (line 2, col 9)
                1 | <Wrapper {children: Option[TrustedHTML] = None}>
                2 |   <div>{children}</div>
                  |         ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_let_binding_used_in_children() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "World"}>
                    <div>Hello {name}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main>
                  <let {name = "World"}>
                    <div>
                      Hello
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_let_with_unused_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "World"}>
                    <div>Hello</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                error: Unused variable name
                  --> main.hop (line 2, col 9)
                1 | <Main>
                2 |   <let {name: String = "World"}>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_shadowing_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main {name: String}>
                  <let {name: String = "Shadow"}>
                    <div>{name}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                error: Variable name is already defined
                  --> main.hop (line 2, col 9)
                1 | <Main {name: String}>
                2 |   <let {name: String = "Shadow"}>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_shadowing_another_let() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "First"}>
                    <let {name: String = "Second"}>
                      <div>{name}</div>
                    </let>
                  </let>
                </Main>
            "#},
            expect![[r#"
                error: Variable name is already defined
                  --> main.hop (line 3, col 11)
                2 |   <let {name: String = "First"}>
                3 |     <let {name: String = "Second"}>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_same_name_in_sibling_scope() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "First"}>
                    <div>{name}</div>
                  </let>
                  <let {name: String = "Second"}>
                    <div>{name}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main>
                  <let {name = "First"}>
                    <div>
                      {name}
                    </div>
                  </let>
                  <let {name = "Second"}>
                    <div>
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_let_with_type_mismatch() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = 42}>
                    <div>{name}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                error: Let binding has type Int, expected String
                  --> main.hop (line 2, col 24)
                1 | <Main>
                2 |   <let {name: String = 42}>
                  |                        ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_multiple_bindings() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {first: String = "Hello", second: String = "World"}>
                    <div>{first} {second}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main>
                  <let {first = "Hello"}>
                    <let {second = "World"}>
                      <div>
                        {first}
                        {second}
                      </div>
                    </let>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_let_with_duplicate_bindings() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "Hello", name: String = "World"}>
                    <div>{name}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                error: Variable name is already defined
                  --> main.hop (line 2, col 33)
                1 | <Main>
                2 |   <let {name: String = "Hello", name: String = "World"}>
                  |                                 ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_with_unused_second_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "Hello", count: Int = 42}>
                    <div>{name}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                error: Unused variable count
                  --> main.hop (line 2, col 33)
                1 | <Main>
                2 |   <let {name: String = "Hello", count: Int = 42}>
                  |                                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_allow_later_binding_to_reference_earlier_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {greeting: String = "Hello", message: String = greeting}>
                    <div>{message}</div>
                  </let>
                </Main>
            "#},
            expect![[r#"
                -- main.hop --
                <Main>
                  <let {greeting = "Hello"}>
                    <let {message = greeting}>
                      <div>
                        {message}
                      </div>
                    </let>
                  </let>
                </Main>
            "#]],
        );
    }
}
