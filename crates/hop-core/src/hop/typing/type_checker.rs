use super::type_annotation::TypeAnnotation;
use crate::asset_reference::AssetReference;
use crate::document::{CheapString, DocumentRange};
use crate::error_collection::ErrorCollectionExt;
use crate::expr::patterns::compiler::Compiler;
use crate::expr::typing::r#type::EnumVariant;
use crate::expr::typing::type_checker::{resolve_type, typecheck_expr};
use crate::expr::typing::type_registry::{TypeDef, TypeRegistry};
use crate::expr::{
    self, ComponentSignature, NamedKind, ParamEntry, Tail, Type, TypeBinding, TypedExpr,
};
use crate::hop::parsing::parsed_ast::ParsedDeclaration;
use crate::hop::parsing::parsed_ast::{
    ParsedAttribute, ParsedComponentDeclaration, ParsedEnumDeclaration, ParsedImportDeclaration,
    ParsedRecordDeclaration, ParsedViewDeclaration, RestSpreadTarget,
};
use crate::hop::typing::definition_link::DefinitionLink;
use crate::html::HtmlElement;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};
use crate::variable_scope::VariableScope;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::document_id::DocumentId;
use crate::expr::patterns::compiler::Decision;
use crate::expr::patterns::typed::typecheck_pattern;
use crate::expr::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::hop::parsing::parsed_ast::{ParsedAst, ParsedAttributeValue};
use crate::hop::parsing::parsed_node::{ParsedLetBinding, ParsedLoopSource, ParsedNode};
use crate::hop::typing::typed_ast::{
    TypedAst, TypedComponentDeclaration, TypedEnumDeclaration, TypedParameter,
    TypedRecordDeclaration, TypedViewDeclaration,
};
use crate::hop::typing::typed_node::{
    TypedArgument, TypedAttribute, TypedAttributeValue, TypedLoopSource, TypedNode,
};

pub fn typecheck(
    modules: &[&ParsedAst],
    state: &mut HashMap<DocumentId, HashMap<TypeName, (TypeBinding, DocumentRange, bool)>>,
    registry: &mut TypeRegistry,
    typed_asts: &mut HashMap<DocumentId, TypedAst>,
    errors: &mut HashMap<DocumentId, Vec<TypeError>>,
    annotations: &mut HashMap<DocumentId, Vec<TypeAnnotation>>,
    definition_links: &mut HashMap<DocumentId, Vec<DefinitionLink>>,
    asset_references: &mut HashMap<DocumentId, Vec<AssetReference>>,
) {
    for module in modules {
        let mut module_errors: Vec<TypeError> = Vec::new();
        let module_annotations = annotations.entry(module.document_id.clone()).or_default();
        let module_definition_links = definition_links
            .entry(module.document_id.clone())
            .or_default();
        let module_asset_references = asset_references
            .entry(module.document_id.clone())
            .or_default();

        module_annotations.clear();
        module_definition_links.clear();
        module_asset_references.clear();
        registry.remove_module(&module.document_id);

        let typed_ast = typecheck_module(
            module,
            state,
            registry,
            &mut module_errors,
            module_annotations,
            module_definition_links,
            module_asset_references,
        );
        typed_asts.insert(module.document_id.clone(), typed_ast);

        if modules.len() > 1 {
            module_errors.clear();
            for import_node in module.get_import_declarations() {
                let imported_module = import_node.imported_module();
                module_errors.push(TypeError::import_cycle(
                    &module.document_id.to_string(),
                    &imported_module.to_string(),
                    &modules
                        .iter()
                        .map(|m| m.document_id.to_string())
                        .collect::<Vec<_>>(),
                    import_node.path.clone(),
                ));
            }
        }

        errors.insert(module.document_id.clone(), module_errors);
    }
}

fn typecheck_module(
    parsed_ast: &ParsedAst,
    state: &mut HashMap<DocumentId, HashMap<TypeName, (TypeBinding, DocumentRange, bool)>>,
    registry: &mut TypeRegistry,
    errors: &mut Vec<TypeError>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> TypedAst {
    let mut type_env: VariableScope<TypeName, (TypeBinding, DocumentRange)> = VariableScope::new();
    let mut var_env: VariableScope<VarName, (Arc<Type>, DocumentRange)> = VariableScope::new();

    let mut typed_records = Vec::new();
    let mut typed_enums = Vec::new();
    let mut typed_component_declarations = Vec::new();
    let mut typed_views = Vec::new();

    // Track imported names and their path ranges for unused import detection
    let mut imported_names: Vec<(TypeName, DocumentRange)> = Vec::new();

    for decl in parsed_ast.get_declarations() {
        match decl {
            ParsedDeclaration::Import(ParsedImportDeclaration {
                module_name: imported_module,
                type_name_range: imported_name_range,
                type_name: imported_name,
                path: import_path,
                import_range,
                ..
            }) => {
                let Some(imported_module_type_info) = state.get(&imported_module.to_document_id())
                else {
                    errors.push(TypeError::new(
                        TypeErrorKind::ModuleNotFound {
                            module: imported_module.clone(),
                        },
                        import_path.clone(),
                    ));
                    continue;
                };

                let Some((typ, def_range, is_pub)) = imported_module_type_info.get(imported_name)
                else {
                    errors.push(TypeError::new(
                        TypeErrorKind::UndeclaredType {
                            module: imported_module.clone(),
                            type_name: imported_name.clone(),
                        },
                        imported_name_range.clone(),
                    ));
                    continue;
                };

                if !is_pub {
                    errors.push(TypeError::new(
                        TypeErrorKind::NotPublic {
                            module: imported_module.clone(),
                            type_name: imported_name.clone(),
                        },
                        imported_name_range.clone(),
                    ));
                    continue;
                }

                definition_links.push(DefinitionLink {
                    use_range: imported_name_range.clone(),
                    definition_range: def_range.clone(),
                });

                let _ = type_env.push(imported_name.clone(), (typ.clone(), def_range.clone()));

                imported_names.push((imported_name.clone(), import_range.clone()));
            }
            ParsedDeclaration::Component(ParsedComponentDeclaration {
                params,
                children,
                component_name,
                tag_name,
                closing_tag_name,
                rest_param,
                rest_target,
                ..
            }) => {
                let mut pushed_params = Vec::new();
                let mut declared_params: Vec<ParamEntry> = Vec::new();
                let mut typed_params = Vec::new();
                if let Some((params, _)) = params {
                    for param in params {
                        let Some(param_type) = errors.ok_or_add(resolve_type(
                            &param.var_type,
                            &mut type_env,
                            definition_links,
                        )) else {
                            continue;
                        };

                        let typed_default_value = {
                            if let Some(default_expr) = &param.default_value {
                                // Use a fresh variable scope, default values
                                // are not allowed to reference eachother.
                                let mut fresh_var_env = VariableScope::new();
                                if let Some(typed_default) = errors.ok_or_add(typecheck_expr(
                                    default_expr,
                                    Some(&param_type),
                                    &mut fresh_var_env,
                                    &mut type_env,
                                    registry,
                                    annotations,
                                    definition_links,
                                    asset_references,
                                )) {
                                    let default_type = typed_default.get_type();
                                    if *default_type != *param_type {
                                        errors.push(TypeError::new(
                                            TypeErrorKind::DefaultValueTypeMismatch {
                                                param_name: param.var_name.clone(),
                                                expected: param_type.clone(),
                                                found: default_type,
                                            },
                                            default_expr.range().clone(),
                                        ));
                                        None
                                    } else {
                                        Some(typed_default)
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        };

                        annotations.push(TypeAnnotation::TypeForVarName {
                            range: param.var_name_range.clone(),
                            typ: param_type.clone(),
                            var_name: param.var_name.clone(),
                        });
                        let _ = var_env.push(
                            param.var_name.clone(),
                            (param_type.clone(), param.var_name_range.clone()),
                        );
                        validate_examples_annotation(
                            &param.examples,
                            &param_type,
                            &param.var_name_range,
                            errors,
                        );

                        pushed_params.push(param);
                        declared_params.push(ParamEntry {
                            name: param.var_name.clone(),
                            typ: param_type.clone(),
                            default: typed_default_value,
                        });
                        typed_params.push(TypedParameter {
                            var_name: param.var_name.clone(),
                            var_type: param_type,
                            examples: param.examples.clone(),
                        });
                    }
                }

                // Register component signature BEFORE type-checking body to allow
                // self-referential (recursive) components
                let component_signature = ComponentSignature {
                    module: parsed_ast.document_id.clone(),
                    params: declared_params.clone(),
                    tail: Tail::Closed,
                    is_recursive: false,
                };

                let _ = type_env.push(
                    component_name.clone(),
                    (
                        TypeBinding::Component(component_signature),
                        tag_name.clone(),
                    ),
                );

                let declared_names: Vec<VarName> =
                    declared_params.iter().map(|p| p.name.clone()).collect();

                let typed_children = children
                    .iter()
                    .filter_map(|child| {
                        typecheck_node(
                            child,
                            &declared_names,
                            registry,
                            errors,
                            &mut var_env,
                            &mut type_env,
                            annotations,
                            definition_links,
                            asset_references,
                        )
                    })
                    .collect::<Vec<_>>();

                let is_recursive = type_env.has_been_accessed(component_name);

                for param in pushed_params.iter().rev() {
                    let (name, _, accessed) = var_env.pop();
                    if !accessed {
                        errors.push(TypeError::new(
                            TypeErrorKind::UnusedVariable { var_name: name },
                            param.var_name_range.clone(),
                        ));
                    }
                }

                // Add definition link for the opening tag (points to itself)
                definition_links.push(DefinitionLink {
                    use_range: tag_name.clone(),
                    definition_range: tag_name.clone(),
                });

                // Add definition link for the closing tag if present
                if let Some(closing_range) = closing_tag_name {
                    definition_links.push(DefinitionLink {
                        use_range: closing_range.clone(),
                        definition_range: tag_name.clone(),
                    });
                }

                if is_recursive && rest_param.is_some() {
                    errors.push(TypeError::new(
                        TypeErrorKind::RecursiveComponentWithRest {
                            component: component_name.clone(),
                        },
                        tag_name.clone(),
                    ));
                }

                let (forwarded, tail) = match rest_target {
                    Some(RestSpreadTarget::Element {
                        element,
                        supplied_attrs,
                        ..
                    }) => (
                        Vec::new(),
                        Tail::Html {
                            element: element.clone(),
                            reserved: supplied_attrs.clone(),
                        },
                    ),
                    Some(RestSpreadTarget::Component {
                        callee,
                        supplied_attrs,
                        has_children,
                        spread_range,
                        ..
                    }) => match type_env.lookup(callee) {
                        Some((TypeBinding::Component(callee_sig), _)) => {
                            if callee_sig.is_recursive {
                                errors.push(TypeError::new(
                                    TypeErrorKind::RestForwardedIntoRecursive {
                                        component: callee.clone(),
                                    },
                                    spread_range.clone(),
                                ));
                                (Vec::new(), Tail::Closed)
                            } else {
                                let tail = match callee_sig.tail.clone() {
                                    Tail::Html {
                                        element,
                                        mut reserved,
                                    } => {
                                        let callee_param_names: HashSet<&str> = callee_sig
                                            .params
                                            .iter()
                                            .map(|p| p.name.as_str())
                                            .collect();
                                        for attr in supplied_attrs {
                                            let a = attr.as_str();
                                            if !callee_param_names.contains(a)
                                                && !reserved.iter().any(|r| r.as_str() == a)
                                            {
                                                reserved.push(attr.clone());
                                            }
                                        }
                                        Tail::Html { element, reserved }
                                    }
                                    Tail::Closed => Tail::Closed,
                                };
                                let covered_by_rest = |p: &ParamEntry| {
                                    !(supplied_attrs.iter().any(|a| a.as_str() == p.name.as_str())
                                        || (*has_children && p.name.as_str() == "children")
                                        || declared_names.contains(&p.name))
                                };
                                let forwarded = callee_sig
                                    .params
                                    .iter()
                                    .filter(|p| covered_by_rest(p))
                                    .cloned()
                                    .collect::<Vec<_>>();
                                (forwarded, tail)
                            }
                        }
                        _ => (Vec::new(), Tail::Closed),
                    },
                    None => (Vec::new(), Tail::Closed),
                };
                let mut extended_params = declared_params.clone();
                extended_params.extend(forwarded);

                let _ = type_env.replace(
                    component_name,
                    (
                        TypeBinding::Component(ComponentSignature {
                            module: parsed_ast.document_id.clone(),
                            params: extended_params,
                            tail,
                            is_recursive,
                        }),
                        tag_name.clone(),
                    ),
                );

                typed_component_declarations.push(TypedComponentDeclaration {
                    component_name: component_name.clone(),
                    params: typed_params,
                    rest_param: rest_param.as_ref().map(|(name, _)| name.clone()),
                    children: typed_children,
                    is_recursive,
                });
            }
            ParsedDeclaration::Record(ParsedRecordDeclaration {
                name: record_name,
                name_range: record_name_range,
                fields,
                ..
            }) => {
                let _ = type_env.push(
                    record_name.clone(),
                    (
                        TypeBinding::Value(Arc::new(Type::Named {
                            module: parsed_ast.document_id.clone(),
                            name: record_name.clone(),
                            kind: NamedKind::Record,
                        })),
                        record_name_range.clone(),
                    ),
                );

                let mut typed_fields = Vec::new();
                let mut has_errors = false;

                for field in fields {
                    let Some(resolved_type) = errors.ok_or_add(resolve_type(
                        &field.field_type,
                        &mut type_env,
                        definition_links,
                    )) else {
                        has_errors = true;
                        continue;
                    };
                    validate_examples_annotation(
                        &field.examples,
                        &resolved_type,
                        &field.name_range,
                        errors,
                    );
                    typed_fields.push((field.name.clone(), resolved_type, field.examples.clone()));
                }

                if has_errors {
                    let _ = type_env.pop();
                    continue;
                }

                typed_records.push(TypedRecordDeclaration {
                    name: record_name.clone(),
                    fields: typed_fields.clone(),
                });
                registry.insert(
                    parsed_ast.document_id.clone(),
                    record_name.clone(),
                    TypeDef::Record {
                        fields: typed_fields,
                    },
                );

                // Add definition link for the record name (points to itself)
                definition_links.push(DefinitionLink {
                    use_range: record_name_range.clone(),
                    definition_range: record_name_range.clone(),
                });
            }
            ParsedDeclaration::Enum(ParsedEnumDeclaration {
                name: enum_name,
                name_range: enum_name_range,
                variants,
                ..
            }) => {
                let _ = type_env.push(
                    enum_name.clone(),
                    (
                        TypeBinding::Value(Arc::new(Type::Named {
                            module: parsed_ast.document_id.clone(),
                            name: enum_name.clone(),
                            kind: NamedKind::Enum,
                        })),
                        enum_name_range.clone(),
                    ),
                );

                let mut typed_variants = Vec::new();
                let mut has_errors = false;

                for variant in variants {
                    let mut typed_fields = Vec::new();
                    for (field_name, field_name_range, field_type, examples) in &variant.fields {
                        let Some(resolved_type) = errors.ok_or_add(resolve_type(
                            field_type,
                            &mut type_env,
                            definition_links,
                        )) else {
                            has_errors = true;
                            continue;
                        };
                        validate_examples_annotation(
                            examples,
                            &resolved_type,
                            field_name_range,
                            errors,
                        );
                        typed_fields.push((field_name.clone(), resolved_type, examples.clone()));
                    }
                    typed_variants.push(EnumVariant {
                        name: variant.name.clone(),
                        fields: typed_fields,
                    });
                }

                if has_errors {
                    let _ = type_env.pop();
                    continue;
                }

                registry.insert(
                    parsed_ast.document_id.clone(),
                    enum_name.clone(),
                    TypeDef::Enum {
                        variants: typed_variants.clone(),
                    },
                );

                // Add definition link for the enum name (points to itself)
                definition_links.push(DefinitionLink {
                    use_range: enum_name_range.clone(),
                    definition_range: enum_name_range.clone(),
                });

                typed_enums.push(TypedEnumDeclaration {
                    name: enum_name.clone(),
                    variants: typed_variants,
                });
            }
            ParsedDeclaration::View(ParsedViewDeclaration {
                params,
                children,
                name,
                ..
            }) => {
                let mut pushed_params = Vec::new();
                let mut typed_params = Vec::new();

                for param in params {
                    let Some(param_type) = errors.ok_or_add(resolve_type(
                        &param.var_type,
                        &mut type_env,
                        definition_links,
                    )) else {
                        continue;
                    };

                    annotations.push(TypeAnnotation::TypeForVarName {
                        range: param.var_name_range.clone(),
                        typ: param_type.clone(),
                        var_name: param.var_name.clone(),
                    });
                    let _ = var_env.push(
                        param.var_name.clone(),
                        (param_type.clone(), param.var_name_range.clone()),
                    );
                    validate_examples_annotation(
                        &param.examples,
                        &param_type,
                        &param.var_name_range,
                        errors,
                    );

                    pushed_params.push(param);
                    typed_params.push(TypedParameter {
                        var_name: param.var_name.clone(),
                        var_type: param_type,
                        examples: param.examples.clone(),
                    });
                }

                let typed_children = children
                    .iter()
                    .filter_map(|child| {
                        typecheck_node(
                            child,
                            &[],
                            registry,
                            errors,
                            &mut var_env,
                            &mut type_env,
                            annotations,
                            definition_links,
                            asset_references,
                        )
                    })
                    .collect::<Vec<_>>();

                for param in pushed_params.iter().rev() {
                    let (name, _, accessed) = var_env.pop();
                    if !accessed {
                        errors.push(TypeError::new(
                            TypeErrorKind::UnusedVariable { var_name: name },
                            param.var_name_range.clone(),
                        ));
                    }
                }

                typed_views.push(TypedViewDeclaration {
                    name: name.clone(),
                    params: typed_params,
                    children: typed_children,
                });
            }
        }
    }

    // Build module type map from locally declared types
    let mut module_types = HashMap::new();
    for comp in &typed_component_declarations {
        if let Some((typ, def_range)) = type_env.lookup(&comp.component_name) {
            let is_pub = parsed_ast
                .get_component_declaration(comp.component_name.as_str())
                .map(|c| c.pub_range.is_some())
                .unwrap_or(false);
            module_types.insert(
                comp.component_name.clone(),
                (typ.clone(), def_range.clone(), is_pub),
            );
        }
    }
    for rec in &typed_records {
        if let Some((typ, def_range)) = type_env.lookup(&rec.name) {
            let is_pub = parsed_ast
                .get_record_declaration(rec.name.as_str())
                .map(|r| r.pub_range.is_some())
                .unwrap_or(false);
            module_types.insert(rec.name.clone(), (typ.clone(), def_range.clone(), is_pub));
        }
    }
    for enm in &typed_enums {
        if let Some((typ, def_range)) = type_env.lookup(&enm.name) {
            let is_pub = parsed_ast
                .get_enum_declaration(enm.name.as_str())
                .map(|e| e.pub_range.is_some())
                .unwrap_or(false);
            module_types.insert(enm.name.clone(), (typ.clone(), def_range.clone(), is_pub));
        }
    }
    state.insert(parsed_ast.document_id.clone(), module_types);

    // Detect unused imports
    for (imported_name, import_range) in &imported_names {
        if !type_env.has_been_accessed(imported_name) {
            errors.push(TypeError::new(
                TypeErrorKind::UnusedImport {
                    import_name: imported_name.clone(),
                },
                import_range.clone(),
            ));
        }
    }

    TypedAst::new(
        typed_component_declarations,
        typed_records,
        typed_enums,
        typed_views,
    )
}

fn typecheck_node(
    node: &ParsedNode,
    caller_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut VariableScope<TypeName, (TypeBinding, DocumentRange)>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
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
                    typecheck_node(
                        child,
                        caller_params,
                        registry,
                        errors,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        asset_references,
                    )
                })
                .collect();

            let typed_condition = errors.ok_or_add(typecheck_expr(
                condition,
                None,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
            ))?;

            let condition_type = typed_condition.get_type();
            if *condition_type != Type::Bool {
                errors.push(TypeError::new(
                    TypeErrorKind::ConditionTypeMismatch {
                        found: condition_type,
                    },
                    condition.range().clone(),
                ));
            }

            Some(TypedNode::If {
                condition: typed_condition,
                children: typed_children,
            })
        }

        ParsedNode::For {
            var_name,
            var_name_range,
            source,
            children,
            range: _,
        } => {
            // Type check the loop source and determine element type
            let (typed_source, element_type) = match &**source {
                ParsedLoopSource::Array(array_expr) => {
                    let typed_array = errors.ok_or_add(typecheck_expr(
                        array_expr,
                        None,
                        var_env,
                        type_env,
                        registry,
                        annotations,
                        definition_links,
                        asset_references,
                    ))?;
                    let array_type = typed_array.get_type();
                    let element_type = match array_type.as_ref() {
                        Type::Array(inner) => inner.clone(),
                        _ => {
                            errors.push(TypeError::new(
                                TypeErrorKind::IterateeTypeMismatch { found: array_type },
                                array_expr.range().clone(),
                            ));
                            return None;
                        }
                    };
                    (TypedLoopSource::Array(typed_array), element_type)
                }
                ParsedLoopSource::RangeInclusive { start, end } => {
                    let typed_start = errors.ok_or_add(typecheck_expr(
                        start,
                        None,
                        var_env,
                        type_env,
                        registry,
                        annotations,
                        definition_links,
                        asset_references,
                    ))?;
                    let typed_end = errors.ok_or_add(typecheck_expr(
                        end,
                        None,
                        var_env,
                        type_env,
                        registry,
                        annotations,
                        definition_links,
                        asset_references,
                    ))?;

                    // Both bounds must be Int
                    let start_type = typed_start.get_type();
                    if *start_type != Type::Int {
                        errors.push(TypeError::new(
                            TypeErrorKind::RangeBoundTypeMismatch { found: start_type },
                            start.range().clone(),
                        ));
                    }
                    let end_type = typed_end.get_type();
                    if *end_type != Type::Int {
                        errors.push(TypeError::new(
                            TypeErrorKind::RangeBoundTypeMismatch { found: end_type },
                            end.range().clone(),
                        ));
                    }

                    (
                        TypedLoopSource::RangeInclusive {
                            start: typed_start,
                            end: typed_end,
                        },
                        Arc::new(Type::Int),
                    )
                }
            };

            // Push the loop variable into scope (only if not discarded with _)
            let pushed = if let (Some(var_name), Some(var_name_range)) = (var_name, var_name_range)
            {
                match var_env.push(
                    var_name.clone(),
                    (element_type.clone(), var_name_range.clone()),
                ) {
                    Ok(_) => {
                        annotations.push(TypeAnnotation::TypeForVarName {
                            range: var_name_range.clone(),
                            typ: element_type,
                            var_name: var_name.clone(),
                        });
                        true
                    }
                    Err(_) => {
                        errors.push(TypeError::new(
                            TypeErrorKind::VariableAlreadyDefined {
                                name: var_name.clone(),
                            },
                            var_name_range.clone(),
                        ));
                        false
                    }
                }
            } else {
                // Underscore binding - no variable to push
                false
            };

            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        caller_params,
                        registry,
                        errors,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        asset_references,
                    )
                })
                .collect();

            if pushed {
                let (name, _, accessed) = var_env.pop();
                if !accessed {
                    if let Some(var_name_range) = var_name_range {
                        errors.push(TypeError::new(
                            TypeErrorKind::UnusedVariable { var_name: name },
                            var_name_range.clone(),
                        ));
                    }
                }
            }

            Some(TypedNode::For {
                var_name: var_name.clone(),
                source: typed_source,
                children: typed_children,
            })
        }

        ParsedNode::Let {
            bindings, children, ..
        } => {
            // Track which bindings were pushed to scope (for popping later)
            let mut pushed_bindings: Vec<&ParsedLetBinding> = Vec::new();
            // Only store successfully typechecked bindings
            let mut typed_bindings: Vec<(&ParsedLetBinding, TypedExpr)> = Vec::new();

            for binding in bindings {
                // Resolve the declared type, if an annotation is present.
                let declared_type = match &binding.var_type {
                    Some(parsed_type) => {
                        let Some(t) =
                            errors.ok_or_add(resolve_type(parsed_type, type_env, definition_links))
                        else {
                            continue;
                        };
                        Some(t)
                    }
                    None => None,
                };

                // For annotated bindings the type is known up front, so push the
                // variable into scope before typechecking the value. This lets
                // later bindings reference it even if the value fails to typecheck.
                if let Some(declared) = &declared_type {
                    match var_env.push(
                        binding.var_name.clone(),
                        (declared.clone(), binding.var_name_range.clone()),
                    ) {
                        Ok(_) => {
                            annotations.push(TypeAnnotation::TypeForVarName {
                                range: binding.var_name_range.clone(),
                                typ: declared.clone(),
                                var_name: binding.var_name.clone(),
                            });
                            pushed_bindings.push(binding);
                        }
                        Err(_) => {
                            errors.push(TypeError::new(
                                TypeErrorKind::VariableAlreadyDefined {
                                    name: binding.var_name.clone(),
                                },
                                binding.var_name_range.clone(),
                            ));
                        }
                    }
                }

                // Type-check the value. An annotation acts as the expected type;
                // otherwise the value is checked with no expectation and its
                // inferred type becomes the binding's type.
                let Some(typed_value) = errors.ok_or_add(typecheck_expr(
                    &binding.value_expr,
                    declared_type.as_ref(),
                    var_env,
                    type_env,
                    registry,
                    annotations,
                    definition_links,
                    asset_references,
                )) else {
                    continue;
                };

                match &declared_type {
                    Some(declared) => {
                        // Validate that the value type matches the declared type.
                        let value_type = typed_value.get_type();
                        if *value_type != **declared {
                            errors.push(TypeError::new(
                                TypeErrorKind::LetBindingTypeMismatch {
                                    expected: declared.clone(),
                                    found: value_type,
                                },
                                binding.value_expr.range().clone(),
                            ));
                        }
                    }
                    None => {
                        // Inferred binding: adopt the value's type and push the
                        // variable into scope now that the type is known.
                        let inferred = typed_value.get_type();
                        match var_env.push(
                            binding.var_name.clone(),
                            (inferred.clone(), binding.var_name_range.clone()),
                        ) {
                            Ok(_) => {
                                annotations.push(TypeAnnotation::TypeForVarName {
                                    range: binding.var_name_range.clone(),
                                    typ: inferred.clone(),
                                    var_name: binding.var_name.clone(),
                                });
                                pushed_bindings.push(binding);
                            }
                            Err(_) => {
                                errors.push(TypeError::new(
                                    TypeErrorKind::VariableAlreadyDefined {
                                        name: binding.var_name.clone(),
                                    },
                                    binding.var_name_range.clone(),
                                ));
                            }
                        }
                    }
                }

                typed_bindings.push((binding, typed_value));
            }

            // Type-check children with all variables in scope
            let typed_children: Vec<TypedNode> = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        caller_params,
                        registry,
                        errors,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        asset_references,
                    )
                })
                .collect();

            // Pop variables in reverse order and check for unused
            for binding in pushed_bindings.iter().rev() {
                let (name, _, accessed) = var_env.pop();
                if !accessed {
                    errors.push(TypeError::new(
                        TypeErrorKind::UnusedVariable { var_name: name },
                        binding.var_name_range.clone(),
                    ));
                }
            }

            // Build nested Let structure from innermost to outermost
            // Start with children, then wrap with each binding in reverse order
            let mut result = typed_children;
            for (binding, typed_value) in typed_bindings.into_iter().rev() {
                result = vec![TypedNode::Let {
                    var: binding.var_name.clone(),
                    value: typed_value,
                    children: result,
                }];
            }

            result.into_iter().next()
        }

        ParsedNode::ComponentInvocation {
            component_name,
            component_name_opening_range,
            declaring_module: _,
            component_name_closing_range,
            args,
            children,
            range: _,
        } => {
            let typed_children = children
                .iter()
                .flatten()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        caller_params,
                        registry,
                        errors,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        asset_references,
                    )
                })
                .collect::<Vec<_>>();

            // Look up the component signature from type_env
            let (component_module, callee_params, callee_tail, component_def_range) =
                match type_env.lookup(component_name) {
                    Some((TypeBinding::Component(sig), def_range)) => (
                        sig.module.clone(),
                        sig.params.clone(),
                        sig.tail.clone(),
                        def_range.clone(),
                    ),
                    _ => {
                        errors.push(TypeError::new(
                            TypeErrorKind::UndefinedComponent {
                                tag_name: component_name.clone(),
                            },
                            component_name_opening_range.clone(),
                        ));
                        return None;
                    }
                };

            // Add definition link for the opening tag
            definition_links.push(DefinitionLink {
                use_range: component_name_opening_range.clone(),
                definition_range: component_def_range.clone(),
            });

            // Add definition link for the closing tag if present
            if let Some(closing_range) = component_name_closing_range {
                definition_links.push(DefinitionLink {
                    use_range: closing_range.clone(),
                    definition_range: component_def_range,
                });
            }

            let (children_var, resolved_args, extra_attributes, rest_spread) = typecheck_arguments(
                args,
                &callee_params,
                &callee_tail,
                children.is_some(),
                component_name,
                component_name_opening_range,
                caller_params,
                registry,
                errors,
                var_env,
                type_env,
                annotations,
                definition_links,
                asset_references,
            );

            if let Some(var) = children_var {
                return Some(TypedNode::LetFragment {
                    var,
                    fragment_body: typed_children,
                    body: vec![TypedNode::ComponentInvocation {
                        component_name: component_name.clone(),
                        component_module,
                        args: resolved_args,
                        extra_attributes,
                        rest_spread,
                    }],
                });
            }

            Some(TypedNode::ComponentInvocation {
                component_name: component_name.clone(),
                component_module,
                args: resolved_args,
                extra_attributes,
                rest_spread,
            })
        }

        ParsedNode::Html {
            element,
            tag_name: _,
            closing_tag_name: _,
            attributes,
            children,
            range: _,
        } => {
            let typed_attributes = typecheck_attributes(
                attributes,
                element,
                registry,
                errors,
                var_env,
                type_env,
                annotations,
                definition_links,
                asset_references,
            );

            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        caller_params,
                        registry,
                        errors,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        asset_references,
                    )
                })
                .collect();

            Some(TypedNode::Html {
                element: element.clone(),
                attributes: typed_attributes,
                rest_spread: attributes.iter().find_map(|a| match a {
                    ParsedAttribute::Spread { name, .. } => Some(name.clone()),
                    ParsedAttribute::Named { .. } => None,
                }),
                children: typed_children,
            })
        }

        ParsedNode::TextExpression {
            expression,
            range: _,
        } => {
            if let Some(typed_expr) = errors.ok_or_add(typecheck_expr(
                expression,
                None,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
            )) {
                let expr_type = typed_expr.get_type();
                if *expr_type != Type::String && *expr_type != Type::Fragment {
                    errors.push(TypeError::new(
                        TypeErrorKind::TextExpressionTypeMismatch { found: expr_type },
                        expression.range().clone(),
                    ));
                }
                Some(TypedNode::TextExpression {
                    expression: typed_expr,
                })
            } else {
                None
            }
        }

        ParsedNode::Match { subject, cases, .. } => {
            let typed_subject = errors.ok_or_add(typecheck_expr(
                subject,
                None,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
            ))?;

            let subject_type = typed_subject.get_type();
            if !subject_type.is_matchable() {
                errors.push(TypeError::new(
                    TypeErrorKind::MatchNotImplementedForType {
                        found: subject_type,
                    },
                    subject.range().clone(),
                ));
                return None;
            }

            let typed_patterns = errors.ok_or_add(
                cases
                    .iter()
                    .map(|case| typecheck_pattern(&case.pattern, subject_type.clone(), registry))
                    .collect::<Result<Vec<_>, _>>(),
            )?;

            let decision = errors.ok_or_add(
                Compiler::new(var_env.fresh_var_counter(), registry).compile(
                    &typed_patterns,
                    subject_type,
                    subject.range(),
                ),
            )?;

            let typed_bodies = cases
                .iter()
                .zip(&typed_patterns)
                .map(|(case, typed_pattern)| {
                    let bindings = typed_pattern.bindings();

                    // Push bindings into scope
                    let mut pushed_count = 0;
                    for (name, typ, bind_range) in &bindings {
                        match var_env.push(name.clone(), (typ.clone(), bind_range.clone())) {
                            Ok(_) => {
                                annotations.push(TypeAnnotation::TypeForVarName {
                                    range: bind_range.clone(),
                                    typ: typ.clone(),
                                    var_name: name.clone(),
                                });
                                pushed_count += 1;
                            }
                            Err(_) => {
                                errors.push(TypeError::new(
                                    TypeErrorKind::VariableAlreadyDefined { name: name.clone() },
                                    bind_range.clone(),
                                ));
                            }
                        }
                    }

                    // Typecheck case children
                    let typed_children = case
                        .children
                        .iter()
                        .filter_map(|child| {
                            typecheck_node(
                                child,
                                caller_params,
                                registry,
                                errors,
                                var_env,
                                type_env,
                                annotations,
                                definition_links,
                                asset_references,
                            )
                        })
                        .collect::<Vec<_>>();

                    // Pop bindings and check for unused
                    for _ in 0..pushed_count {
                        let (name, _, accessed) = var_env.pop();
                        if !accessed {
                            if let Some((_, _, bind_range)) =
                                bindings.iter().find(|(n, _, _)| n == &name)
                            {
                                errors.push(TypeError::new(
                                    TypeErrorKind::UnusedVariable {
                                        var_name: name.clone(),
                                    },
                                    bind_range.clone(),
                                ));
                            }
                        }
                    }

                    typed_children
                })
                .collect::<Vec<_>>();

            let result = decision_to_typed_nodes(&decision, &typed_bodies, Some(typed_subject));

            result.into_iter().next()
        }

        ParsedNode::Text { range } => {
            // Skip whitespace-only text nodes (they're formatting artifacts)
            if range.as_str().trim().is_empty() {
                None
            } else {
                Some(TypedNode::Text {
                    value: range.to_cheap_string(),
                })
            }
        }

        // Newlines between inline content represent a space (HTML whitespace collapsing)
        // The tokenizer only emits Newlines when they're semantically significant
        ParsedNode::Newline { .. } => Some(TypedNode::Text {
            value: CheapString::new(" ".to_string()),
        }),

        ParsedNode::Comment { .. } => None,

        ParsedNode::Doctype { value, range: _ } => Some(TypedNode::Doctype {
            value: value.clone(),
        }),
    }
}

fn typecheck_attribute_value(
    value: &Option<ParsedAttributeValue>,
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut VariableScope<TypeName, (TypeBinding, DocumentRange)>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedAttributeValue> {
    match value {
        Some(ParsedAttributeValue::Expression(expr)) => {
            let typed_expr = errors.ok_or_add(typecheck_expr(
                expr,
                None,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
            ))?;
            if *typed_expr.get_type() != Type::String {
                errors.push(TypeError::new(
                    TypeErrorKind::ArgumentTypeMismatch {
                        expected: Arc::new(Type::String),
                        found: typed_expr.get_type(),
                    },
                    expr.range().clone(),
                ));
            }
            Some(TypedAttributeValue::Expression(typed_expr))
        }
        Some(ParsedAttributeValue::String { content, .. }) => {
            let string_span = match content {
                Some(range) => range.to_cheap_string(),
                None => CheapString::new("".to_string()),
            };
            Some(TypedAttributeValue::String(string_span))
        }
        None => None,
    }
}

fn typecheck_arguments(
    args: &[ParsedAttribute],
    callee_params: &[ParamEntry],
    callee_tail: &Tail,
    has_body: bool,
    component_name: &TypeName,
    component_name_opening_range: &DocumentRange,
    caller_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut VariableScope<TypeName, (TypeBinding, DocumentRange)>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> (
    Option<VarName>,
    Vec<TypedArgument>,
    Vec<TypedAttribute>,
    Option<VarName>,
) {
    let children_param = callee_params
        .iter()
        .find(|p| p.name.as_str() == "children" && *p.typ == Type::Fragment);
    let has_explicit_children_arg = args.iter().any(|a| match a {
        ParsedAttribute::Named { name, .. } => name.as_str() == "children",
        ParsedAttribute::Spread { .. } => false,
    });
    let synthesize_children_arg =
        has_body && children_param.is_some() && !has_explicit_children_arg;

    if has_body && children_param.is_none() {
        errors.push(TypeError::new(
            TypeErrorKind::ComponentDoesNotAcceptChildren {
                component: component_name.clone(),
            },
            component_name_opening_range.clone(),
        ));
    }

    let rest_spread = args.iter().find_map(|a| match a {
        ParsedAttribute::Spread { name, .. } => Some(name.clone()),
        ParsedAttribute::Named { .. } => None,
    });
    let mut supplied_args: Vec<VarName> = args
        .iter()
        .filter_map(|a| match a {
            ParsedAttribute::Named { name, .. } => VarName::new(name.as_str()).ok(),
            ParsedAttribute::Spread { .. } => None,
        })
        .collect();
    if has_body {
        supplied_args.push(VarName::new("children").unwrap());
    }
    let covered_by_rest = |param: &ParamEntry| {
        rest_spread.is_some()
            && !supplied_args.contains(&param.name)
            && !caller_params.contains(&param.name)
    };

    let mut typed_args = Vec::new();
    let mut extra_attributes: Vec<TypedAttribute> = Vec::new();
    for arg in args {
        let (arg_name_range, arg_value) = match arg {
            ParsedAttribute::Named { name, value } => (name, value),
            ParsedAttribute::Spread { .. } => continue,
        };
        let arg_name = arg_name_range.as_str();

        let Some(param) = callee_params.iter().find(|p| p.name.as_str() == arg_name) else {
            let accepted = match &callee_tail {
                Tail::Html { element, reserved } => {
                    element.accepts_attribute(arg_name)
                        && !reserved.iter().any(|r| r.as_str() == arg_name)
                }
                Tail::Closed => false,
            };
            if accepted {
                let value = typecheck_attribute_value(
                    arg_value,
                    registry,
                    errors,
                    var_env,
                    type_env,
                    annotations,
                    definition_links,
                    asset_references,
                );
                extra_attributes.push(TypedAttribute {
                    name: arg_name_range.to_cheap_string(),
                    value,
                });
            } else {
                errors.push(TypeError::new(
                    TypeErrorKind::ComponentDoesNotAcceptAttribute {
                        component: component_name.clone(),
                        attr: arg_name.to_string(),
                    },
                    arg_name_range.clone(),
                ));
            }
            continue;
        };
        let param_type = &param.typ;

        let arg_expr = match arg_value {
            Some(ParsedAttributeValue::Expression(expr)) => expr.clone(),
            Some(ParsedAttributeValue::String {
                content,
                quoted_range,
            }) => {
                let value = content
                    .as_ref()
                    .map(|r| r.to_cheap_string())
                    .unwrap_or_else(|| CheapString::new(String::new()));
                expr::ParsedExpr::StringLiteral {
                    value,
                    range: quoted_range.clone(),
                }
            }
            None => expr::ParsedExpr::BooleanLiteral {
                value: true,
                range: arg_name_range.clone(),
            },
        };

        let Some(typed_expr) = errors.ok_or_add(typecheck_expr(
            &arg_expr,
            Some(param_type),
            var_env,
            type_env,
            registry,
            annotations,
            definition_links,
            asset_references,
        )) else {
            continue;
        };
        let arg_type = typed_expr.get_type();

        if *arg_type != **param_type {
            errors.push(TypeError::new(
                TypeErrorKind::ArgumentTypeMismatch {
                    expected: param_type.clone(),
                    found: arg_type,
                },
                arg_expr.range().clone(),
            ));
            continue;
        }

        typed_args.push(TypedArgument {
            name: VarName::new(arg_name).unwrap(),
            expr: typed_expr,
        });
    }

    if has_body && has_explicit_children_arg {
        errors.push(TypeError::new(
            TypeErrorKind::ChildContentAmbiguous {},
            component_name_opening_range.clone(),
        ));
    }

    let children_var = if synthesize_children_arg {
        let fresh = var_env.fresh_var();
        typed_args.push(TypedArgument {
            name: VarName::new("children").unwrap(),
            expr: TypedExpr::Var {
                value: fresh.clone(),
                kind: Arc::new(Type::Fragment),
            },
        });
        Some(fresh)
    } else {
        None
    };

    let missing_args: Vec<&str> = callee_params
        .iter()
        .filter(|p| {
            if p.default.is_some() {
                return false;
            }
            let supplied = args.iter().any(|a| match a {
                ParsedAttribute::Named { name: n, .. } => n.as_str() == p.name.as_str(),
                ParsedAttribute::Spread { .. } => false,
            });
            let is_synthesized_children = synthesize_children_arg && p.name.as_str() == "children";
            !supplied && !is_synthesized_children && !covered_by_rest(p)
        })
        .map(|p| p.name.as_str())
        .collect();
    if !missing_args.is_empty() {
        errors.push(TypeError::new(
            TypeErrorKind::MissingArguments {
                args: missing_args.join(", "),
            },
            component_name_opening_range.clone(),
        ));
    }

    // Resolve the call arguments into parameter-definition order, filling
    // in default values for any omitted optional parameters.
    let resolved_args: Vec<TypedArgument> = callee_params
        .iter()
        .filter_map(|param| {
            if covered_by_rest(param) {
                return None;
            }
            typed_args
                .iter()
                .find(|arg| arg.name.as_str() == param.name.as_str())
                .map(|arg| arg.expr.clone())
                .or_else(|| param.default.clone())
                .map(|expr| TypedArgument {
                    name: param.name.clone(),
                    expr,
                })
        })
        .collect();

    (children_var, resolved_args, extra_attributes, rest_spread)
}

fn typecheck_attributes(
    attributes: &[ParsedAttribute],
    element: &HtmlElement,
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut VariableScope<TypeName, (TypeBinding, DocumentRange)>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Vec<TypedAttribute> {
    let mut typed_attributes = Vec::new();
    for attr in attributes {
        let (attr_name, attr_value) = match attr {
            ParsedAttribute::Named { name, value } => (name, value),
            ParsedAttribute::Spread { .. } => continue,
        };
        if let Some(typed) = typecheck_html_attribute(
            element,
            attr_name,
            attr_value,
            registry,
            errors,
            var_env,
            type_env,
            annotations,
            definition_links,
            asset_references,
        ) {
            typed_attributes.push(typed);
        }
    }

    typed_attributes
}

/// Type-check a single HTML attribute: validate the name; expression values must be `String`.
fn typecheck_html_attribute(
    element: &HtmlElement,
    name: &DocumentRange,
    value: &Option<ParsedAttributeValue>,
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut VariableScope<TypeName, (TypeBinding, DocumentRange)>,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedAttribute> {
    if !element.accepts_attribute(name.as_str()) {
        errors.push(TypeError::new(
            TypeErrorKind::ElementDoesNotAcceptAttribute {
                element: element.as_str().to_string(),
                attr: name.as_str().to_string(),
            },
            name.clone(),
        ));
        return None;
    }

    let typed_value = typecheck_attribute_value(
        value,
        registry,
        errors,
        var_env,
        type_env,
        annotations,
        definition_links,
        asset_references,
    );

    Some(TypedAttribute {
        name: name.to_cheap_string(),
        value: typed_value,
    })
}

/// Convert a compiled Decision tree into a Vec<TypedNode>.
/// This is used for node-level match statements (as opposed to expression-level matches).
///
/// The root subject, when present, is the expression for the root switch node,
/// used in place of a synthetic variable. Only the outermost call supplies it.
fn decision_to_typed_nodes(
    decision: &Decision,
    typed_bodies: &[Vec<TypedNode>],
    root_subject: Option<TypedExpr>,
) -> Vec<TypedNode> {
    match decision {
        Decision::Success(body) => {
            let mut result = typed_bodies[body.value].clone();
            // The root binding binds the subject expression directly; nested
            // bindings read fresh field/payload vars (root_subject is None).
            let mut root_subject = root_subject;
            // Wrap with Let nodes for each binding (in reverse order so first binding is outermost)
            for binding in body.bindings.iter().rev() {
                let value = root_subject.take().unwrap_or_else(|| TypedExpr::Var {
                    value: binding.source_name.clone(),
                    kind: binding.typ.clone(),
                });
                result = vec![TypedNode::Let {
                    var: binding.name.clone(),
                    value,
                    children: result,
                }];
            }
            result
        }

        Decision::SwitchBool {
            variable,
            true_case,
            false_case,
        } => {
            let subject = Box::new(root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            }));
            vec![TypedNode::Match {
                match_: Match::Bool {
                    subject,
                    true_body: Box::new(decision_to_typed_nodes(
                        &true_case.body,
                        typed_bodies,
                        None,
                    )),
                    false_body: Box::new(decision_to_typed_nodes(
                        &false_case.body,
                        typed_bodies,
                        None,
                    )),
                },
            }]
        }

        Decision::SwitchOption {
            variable,
            some_case,
            none_case,
        } => {
            let subject = Box::new(root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            }));
            vec![TypedNode::Match {
                match_: Match::Option {
                    subject,
                    some_arm_binding: some_case.bound_name.clone(),
                    some_arm_body: Box::new(decision_to_typed_nodes(
                        &some_case.body,
                        typed_bodies,
                        None,
                    )),
                    none_arm_body: Box::new(decision_to_typed_nodes(
                        &none_case.body,
                        typed_bodies,
                        None,
                    )),
                },
            }]
        }

        Decision::SwitchEnum { variable, cases } => {
            let subject = Box::new(root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            }));

            let arms = cases
                .iter()
                .map(|case| {
                    let pattern = EnumPattern::Variant {
                        enum_name: case.enum_name.clone(),
                        variant_name: case.variant_name.clone(),
                    };

                    // Filter out wildcard bindings (bound_name is None)
                    let bindings: Vec<_> = case
                        .bindings
                        .iter()
                        .filter_map(|b| {
                            b.bound_name
                                .as_ref()
                                .map(|name| (b.field_name.clone(), name.clone()))
                        })
                        .collect();

                    let body = decision_to_typed_nodes(&case.body, typed_bodies, None);
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

        Decision::SwitchRecord { variable, case } => {
            let subject = root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            });

            let body = decision_to_typed_nodes(&case.body, typed_bodies, None);

            // Skip wildcard bindings (bound_name is None)
            let bindings: Vec<(FieldName, VarName)> = case
                .bindings
                .iter()
                .filter_map(|binding| {
                    binding
                        .bound_name
                        .as_ref()
                        .map(|name| (binding.field_name.clone(), name.clone()))
                })
                .collect();

            vec![TypedNode::LetRecordDestructure {
                subject,
                bindings,
                children: body,
            }]
        }
    }
}

fn validate_examples_annotation(
    examples: &Option<expr::ExamplesAnnotation>,
    resolved_type: &Arc<Type>,
    range: &DocumentRange,
    errors: &mut Vec<TypeError>,
) {
    let Some(examples) = examples else { return };
    if let Some(pattern) = &examples.pattern {
        if **resolved_type != Type::String {
            errors.push(TypeError::new(
                TypeErrorKind::PatternOnNonString {
                    found: resolved_type.clone(),
                },
                range.clone(),
            ));
        } else if let Err(e) = regex_syntax::parse(pattern) {
            errors.push(TypeError::new(
                TypeErrorKind::InvalidPatternRegex {
                    message: e.to_string(),
                },
                range.clone(),
            ));
        }
    }
    if examples.min.is_some() || examples.max.is_some() {
        if **resolved_type != Type::Int {
            errors.push(TypeError::new(
                TypeErrorKind::MinMaxOnNonInt {
                    found: resolved_type.clone(),
                },
                range.clone(),
            ));
        }
        if let (Some(min), Some(max)) = (examples.min, examples.max) {
            if min > max {
                errors.push(TypeError::new(
                    TypeErrorKind::MinGreaterThanMax { min, max },
                    range.clone(),
                ));
            }
        }
    }
    if examples.min_len.is_some() || examples.max_len.is_some() {
        if !matches!(**resolved_type, Type::Array(_)) {
            errors.push(TypeError::new(
                TypeErrorKind::MinMaxLenOnNonArray {
                    found: resolved_type.clone(),
                },
                range.clone(),
            ));
        }
        for value in [examples.min_len, examples.max_len].into_iter().flatten() {
            if value < 0 {
                errors.push(TypeError::new(
                    TypeErrorKind::NegativeLen { value },
                    range.clone(),
                ));
            }
        }
        if let (Some(min_len), Some(max_len)) = (examples.min_len, examples.max_len) {
            if min_len > max_len {
                errors.push(TypeError::new(
                    TypeErrorKind::MinLenGreaterThanMaxLen { min_len, max_len },
                    range.clone(),
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::Document;
    use crate::document_annotator::DocumentAnnotator;
    use crate::document_id::DocumentId;
    use crate::hop::parsing::parser::parse;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::Archive;

    fn run_check(archive_str: &str) -> (String, bool) {
        let archive = Archive::from(archive_str);
        let mut ast_output = Vec::new();
        let mut error_annotator = DocumentAnnotator::new()
            .with_label("error")
            .with_lines_before(1)
            .with_location();
        let mut warning_annotator = DocumentAnnotator::new()
            .with_label("warning")
            .with_lines_before(1)
            .with_location();

        let mut state = HashMap::new();
        let mut registry = TypeRegistry::default();
        let mut type_errors = HashMap::new();
        let mut type_annotations = HashMap::new();
        let mut definition_links = HashMap::new();
        let mut asset_references = HashMap::new();
        let mut typed_asts = HashMap::new();
        let mut document_ids = Vec::new();

        for file in archive.iter() {
            if !file.name.ends_with(".hop") {
                panic!("Got invalid file name")
            }
            let source_code = file.content.trim();
            let mut parse_errors = Vec::new();
            let document_id = DocumentId::new(&file.name).unwrap();
            document_ids.push(document_id.clone());
            let ast = parse(
                document_id.clone(),
                Document::new(document_id.clone(), source_code.to_string()),
                &mut parse_errors,
            );

            if !parse_errors.is_empty() {
                panic!(
                    "unexpected parse errors in type checker test for {}: {:?}",
                    document_id, parse_errors
                );
            }

            typecheck(
                &[&ast],
                &mut state,
                &mut registry,
                &mut typed_asts,
                &mut type_errors,
                &mut type_annotations,
                &mut definition_links,
                &mut asset_references,
            );

            if let Some(module_errors) = type_errors.get(&ast.document_id) {
                if !module_errors.is_empty() {
                    let (real_errors, real_warnings): (Vec<_>, Vec<_>) = module_errors
                        .iter()
                        .partition(|e| e.severity() == crate::program::Severity::Error);

                    if !real_errors.is_empty() {
                        error_annotator.annotate(&document_id, &real_errors);
                    }
                    if !real_warnings.is_empty() {
                        warning_annotator.annotate(&document_id, &real_warnings);
                    }
                }
            }
        }

        let has_diagnostics = !error_annotator.is_empty() || !warning_annotator.is_empty();
        let actual = if has_diagnostics {
            let parts: Vec<String> = [error_annotator.render(), warning_annotator.render()]
                .into_iter()
                .filter(|s| !s.is_empty())
                .collect();
            parts.join("\n")
        } else {
            for document_id in &document_ids {
                if let Some(typed_ast) = typed_asts.get(document_id) {
                    ast_output.push(format!("-- {} --\n{}", document_id, typed_ast));
                }
            }
            ast_output.join("\n")
        };
        (actual, has_diagnostics)
    }

    fn accept(archive_str: &str, expected: Expect) {
        let (actual, has_diagnostics) = run_check(archive_str);
        if has_diagnostics {
            panic!("expected no diagnostics, got:\n{actual}");
        }
        expected.assert_eq(&actual);
    }

    fn reject(archive_str: &str, expected: Expect) {
        let (actual, has_diagnostics) = run_check(archive_str);
        if !has_diagnostics {
            panic!("expected diagnostics but got none");
        }
        expected.assert_eq(&actual);
    }

    #[test]
    fn rejects_default_value_referencing_earlier_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                component A(a: String, b: String = a) {
                    <span>{a}{b}</span>
                }
                view Main {
                    <A a="a" b="b"/>
                }
            "#},
            expect![[r#"
                error: Undefined variable: a
                  --> main.hop (line 1, col 36)
                1 | component A(a: String, b: String = a) {
                  |                                    ^
            "#]],
        );
    }

    #[test]
    fn rejects_default_value_referencing_later_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                component A(a: String = b, b: String) {
                    <span>{a}{b}</span>
                }
                view Main {
                    <A a="a" b="b"/>
                }
            "#},
            expect![[r#"
                error: Undefined variable: b
                  --> main.hop (line 1, col 25)
                1 | component A(a: String = b, b: String) {
                  |                         ^
            "#]],
        );
    }

    #[test]
    fn accepts_empty_file() {
        accept(
            "-- main.hop --",
            expect![[r#"
                -- main.hop --
            "#]],
        );
    }

    #[test]
    fn rejects_component_used_as_param_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component B {
                  <div></div>
                }
                component Inner(child: B) {
                  <div></div>
                }
            "#},
            expect![[r#"
                error: `B` is a component and cannot be used as a type
                  --> main.hop (line 4, col 24)
                3 | }
                4 | component Inner(child: B) {
                  |                        ^
            "#]],
        );
    }

    #[test]
    fn rejects_component_used_as_field_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component B {
                  <div></div>
                }
                record R {
                  field: B,
                }
            "#},
            expect![[r#"
                error: `B` is a component and cannot be used as a type
                  --> main.hop (line 5, col 10)
                4 | record R {
                5 |   field: B,
                  |          ^
            "#]],
        );
    }

    #[test]
    fn accepts_children_in_component() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div>
                        {children}
                    </div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(children: Fragment) {
                  <div>
                    {children}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_children_named_param_receives_body_content() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div>{children}</div>
                }

                component Main {
                    <Card></Card>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(children: Fragment) {
                  <div>
                    {children}
                  </div>
                }

                component Main {
                  <let {
                    v_0 = {}
                  }>
                    <Card children={v_0}/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_component_invoked_without_children() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div>{children}</div>
                }

                component Main {
                    <Card />
                }
            "#},
            expect![[r#"
                error: Component requires arguments: children
                  --> main.hop (line 6, col 6)
                5 | component Main {
                6 |     <Card />
                  |      ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_empty_body_supplies_empty_children() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div>{children}</div>
                }

                component Main {
                    <Card></Card>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(children: Fragment) {
                  <div>
                    {children}
                  </div>
                }

                component Main {
                  <let {
                    v_0 = {}
                  }>
                    <Card children={v_0}/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_explicit_children_argument() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div>{children}</div>
                }

                component Main(children: Fragment) {
                    <Card children={children} />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(children: Fragment) {
                  <div>
                    {children}
                  </div>
                }

                component Main(children: Fragment) {
                  <Card children={children}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_children_in_view() {
        reject(
            indoc! {r#"
                -- main.hop --
                view Main {
                    <div>
                        {children}
                    </div>
                }
            "#},
            expect![[r#"
                error: Undefined variable: children
                  --> main.hop (line 3, col 10)
                2 |     <div>
                3 |         {children}
                  |          ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_recursive_component_with_children() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Tree(children: Fragment) {
                	<div>
                		<Tree>{children}</Tree>
                	</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Tree(children: Fragment) {
                  <div>
                    <let {
                      v_0 = {
                        {children}
                      }
                    }>
                      <Tree children={v_0}/>
                    </let>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_children_in_expression_position() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div class={children}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Fragment`
                  --> main.hop (line 2, col 17)
                1 | component Card(children: Fragment) {
                2 |     <div class={children}></div>
                  |                 ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unused_let_binding_bound_from_children() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <let {content: Fragment = children}>
                        <div></div>
                    </let>
                }
            "#},
            expect![[r#"
                warning: Unused variable content
                  --> main.hop (line 2, col 11)
                1 | component Card(children: Fragment) {
                2 |     <let {content: Fragment = children}>
                  |           ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_explicit_children_arg_combined_with_element_children() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(children: Fragment) {
                    <div>{children}</div>
                }

                component Main(children: Fragment) {
                    <Card children={children}>children</Card>
                }
            "#},
            expect![[r#"
                error: Content provided both as an explicit `children` argument and as element children
                  --> main.hop (line 6, col 6)
                5 | component Main(children: Fragment) {
                6 |     <Card children={children}>children</Card>
                  |      ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_component_declaration_without_parameters() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {}
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_undefined_component_is_referenced() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                	<h1>Hello,
                        <Bar>
                            <div></div>
                        </Bar>
                    </h1>
                }
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
    fn accepts_when_a_component_invokes_itself() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                	<h1>Hello, <Main/>!</h1>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <h1>
                    Hello, 
                    <Main/>
                    !
                  </h1>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_import_references_a_module_that_does_not_exist() {
        reject(
            indoc! {r#"
                -- main.hop --
                import other::Foo

                component Main {
                }
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
    fn rejects_when_an_import_references_a_component_that_does_not_exist() {
        reject(
            indoc! {r#"
                -- other.hop --
                component Bar {
                }
                -- main.hop --
                import other::Foo

                component Main {
                }
            "#},
            expect![[r#"
                error: Module other does not declare a type Foo
                  --> main.hop (line 1, col 15)
                1 | import other::Foo
                  |               ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_import_references_a_component_from_an_empty_module() {
        reject(
            indoc! {r#"
                -- other.hop --
                -- main.hop --
                import other::Foo

                component Main {
                }
            "#},
            expect![[r#"
                error: Module other does not declare a type Foo
                  --> main.hop (line 1, col 15)
                1 | import other::Foo
                  |               ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_import_of_non_pub_component() {
        reject(
            indoc! {r#"
                -- other.hop --
                component Foo {
                }
                -- main.hop --
                import other::Foo

                component Main {
                  <Foo/>
                }
            "#},
            expect![[r#"
                error: Type Foo from module other is not public
                  --> main.hop (line 1, col 15)
                1 | import other::Foo
                  |               ^^^

                error: Component Foo is not defined
                  --> main.hop (line 4, col 4)
                3 | component Main {
                4 |   <Foo/>
                  |    ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_import_of_non_pub_record() {
        reject(
            indoc! {r#"
                -- other.hop --
                record Foo {
                  name: String,
                }
                -- main.hop --
                import other::Foo

                component Main(foo: Foo) {
                  <div>{foo.name}</div>
                }
            "#},
            expect![[r#"
                error: Type Foo from module other is not public
                  --> main.hop (line 1, col 15)
                1 | import other::Foo
                  |               ^^^

                error: Type 'Foo' is not defined
                  --> main.hop (line 3, col 21)
                2 | 
                3 | component Main(foo: Foo) {
                  |                     ^^^

                error: Undefined variable: foo
                  --> main.hop (line 4, col 9)
                3 | component Main(foo: Foo) {
                4 |   <div>{foo.name}</div>
                  |         ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_import_of_non_pub_enum() {
        reject(
            indoc! {r#"
                -- other.hop --
                enum Color {
                  Red,
                  Green,
                }
                -- main.hop --
                import other::Color

                component Main(color: Color) {
                  <div>{match color {
                    Color::Red => "red",
                    Color::Green => "green",
                  }}</div>
                }
            "#},
            expect![[r#"
                error: Type Color from module other is not public
                  --> main.hop (line 1, col 15)
                1 | import other::Color
                  |               ^^^^^

                error: Type 'Color' is not defined
                  --> main.hop (line 3, col 23)
                2 | 
                3 | component Main(color: Color) {
                  |                       ^^^^^

                error: Undefined variable: color
                  --> main.hop (line 4, col 15)
                3 | component Main(color: Color) {
                4 |   <div>{match color {
                  |               ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_import_of_pub_component() {
        accept(
            indoc! {r#"
                -- other.hop --
                pub component Foo {
                  <span>hi</span>
                }
                -- main.hop --
                import other::Foo

                component Main {
                  <Foo/>
                }
            "#},
            expect![[r#"
                -- other.hop --
                component Foo {
                  <span>
                    hi
                  </span>
                }

                -- main.hop --
                component Main {
                  <Foo/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_import_of_pub_record() {
        accept(
            indoc! {r#"
                -- other.hop --
                pub record Foo {
                  name: String,
                }
                -- main.hop --
                import other::Foo

                component Main(foo: Foo) {
                  <div>{foo.name}</div>
                }
            "#},
            expect![[r#"
                -- other.hop --
                record Foo {
                  name: String,
                }

                -- main.hop --
                component Main(foo: other::Foo) {
                  <div>
                    {foo.name}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_import_of_pub_enum() {
        accept(
            indoc! {r#"
                -- other.hop --
                pub enum Color {
                  Red,
                  Green,
                }
                -- main.hop --
                import other::Color

                component Main(color: Color) {
                  <div>{match color {
                    Color::Red => "red",
                    Color::Green => "green",
                  }}</div>
                }
            "#},
            expect![[r#"
                -- other.hop --
                enum Color {
                  Red,
                  Green,
                }

                -- main.hop --
                component Main(color: other::Color) {
                  <div>
                    {match color {Color::Red => "red", Color::Green => "green"}}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_is_imported_without_being_used() {
        reject(
            indoc! {r#"
                -- other.hop --
                pub component Foo {
                }

                -- main.hop --
                import other::Foo

                component Main {
                }
            "#},
            expect![[r#"
                warning: Unused import 'Foo'
                  --> main.hop (line 1, col 1)
                1 | import other::Foo
                  | ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_components_in_different_modules_to_have_same_name() {
        accept(
            indoc! {r#"
                -- other.hop --
                component Foo {
                }

                -- main.hop --
                component Foo {
                }
            "#},
            expect![[r#"
                -- other.hop --
                component Foo {}

                -- main.hop --
                component Foo {}
            "#]],
        );
    }

    #[test]
    fn rejects_when_children_are_passed_to_component_that_does_not_accept_them() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                    <strong>No children parameter here</strong>
                }

                component Bar {
                    <Main>
                        This component has no children parameter
                    </Main>
                }
            "#},
            expect![[r#"
                error: Component Main does not accept content (missing `children: Fragment` parameter)
                  --> main.hop (line 6, col 6)
                5 | component Bar {
                6 |     <Main>
                  |      ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_children_are_passed_to_an_imported_component_that_does_not_accept_them() {
        reject(
            indoc! {r#"
                -- other.hop --
                pub component Foo {
                    <strong>No children parameter here</strong>
                }
                -- main.hop --
                import other::Foo

                component Bar {
                    <Foo>
                        This component has no children parameter
                    </Foo>
                }
            "#},
            expect![[r#"
                error: Component Foo does not accept content (missing `children: Fragment` parameter)
                  --> main.hop (line 4, col 6)
                3 | component Bar {
                4 |     <Foo>
                  |      ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_variable_shadows_a_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Items {
                  foo: Array[String],
                }

                component Main(items: Items) {
                  <for {items in items.foo}>
                  </for>
                }
            "#},
            expect![[r#"
                error: Variable items is already defined
                  --> main.hop (line 6, col 9)
                5 | component Main(items: Items) {
                6 |   <for {items in items.foo}>
                  |         ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_variable_shadows_another_variable() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Items {
                  a: Array[String],
                  b: Array[String],
                }

                component Main(items: Items) {
                  <for {item in items.a}>
                    <for {item in items.b}>
                      <div>{item}</div>
                    </for>
                  </for>
                }
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
    fn rejects_when_an_undefined_variable_is_referenced() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Item {
                  active: Bool,
                }

                component Main(params: Array[Item]) {
                	<for {item in params}>
                	  <if {item.active}>
                	  </if>
                	</for>
                	<if {item.active}>
                	</if>
                }
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
    fn rejects_when_a_loop_variable_is_unused() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[String]) {
                  <for {item in items}>
                  </for>
                }
            "#},
            expect![[r#"
                warning: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | component Main(items: Array[String]) {
                2 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[String]) {
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                  <for {item in items}>
                  </for>
                }
            "#},
            expect![[r#"
                warning: Unused variable item
                  --> main.hop (line 5, col 9)
                4 |   </for>
                5 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[String]) {
                  <for {item in items}>
                  </for>
                  <for {item in items}>
                      <div>{item}</div>
                  </for>
                }
            "#},
            expect![[r#"
                warning: Unused variable item
                  --> main.hop (line 2, col 9)
                1 | component Main(items: Array[String]) {
                2 |   <for {item in items}>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_parameter_is_unused() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Bar(p: String) {
                  <div>
                  </div>
                }
            "#},
            expect![[r#"
                warning: Unused variable p
                  --> main.hop (line 1, col 15)
                1 | component Bar(p: String) {
                  |               ^
            "#]],
        );
        reject(
            indoc! {r#"
                -- main.hop --
                component Bar(p: String, s: String) {
                  <div>
                  	{s}
                  </div>
                }
            "#},
            expect![[r#"
                warning: Unused variable p
                  --> main.hop (line 1, col 15)
                1 | component Bar(p: String, s: String) {
                  |               ^
            "#]],
        );
    }

    #[test]
    fn accepts_component_arguments_to_be_passed_in_any_order() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(a: Bool, b: String) {
                  <if {a}>
                    <div>{b}</div>
                  </if>
                }
                component Foo {
                  <Main b="foo" a={true}/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(a: Bool, b: String) {
                  <if {a}>
                    <div>
                      {b}
                    </div>
                  </if>
                }

                component Foo {
                  <Main a={true} b={"foo"}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_is_missing_an_argument() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(a: Bool, b: String) {
                  <if {a}>
                    <div>{b}</div>
                  </if>
                }
                component Foo {
                  <Main b="foo"/>
                }
            "#},
            expect![[r#"
                error: Component requires arguments: a
                  --> main.hop (line 7, col 4)
                6 | component Foo {
                7 |   <Main b="foo"/>
                  |    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_is_passed_an_extra_argument() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(a: String) {
                  {a}
                }
                component Foo {
                    <Main a="" b={1}/>
                }
            "#},
            expect![[r#"
                error: Component `Main` does not accept attribute `b`
                  --> main.hop (line 5, col 16)
                4 | component Foo {
                5 |     <Main a="" b={1}/>
                  |                ^
            "#]],
        );
    }

    #[test]
    fn rejects_when_no_arguments_are_passed_to_component_that_requires_them() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(a: Bool, b: String) {
                  <if {a}>
                    <div>{b}</div>
                  </if>
                }
                component Foo {
                  <Main />
                }
            "#},
            expect![[r#"
                error: Component requires arguments: a, b
                  --> main.hop (line 7, col 4)
                6 | component Foo {
                7 |   <Main />
                  |    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_arguments_are_passed_to_component_that_does_not_accept_them() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  hello world
                }
                component Foo {
                  <Main a="foo" />
                }
            "#},
            expect![[r#"
                error: Component `Main` does not accept attribute `a`
                  --> main.hop (line 5, col 9)
                4 | component Foo {
                5 |   <Main a="foo" />
                  |         ^
            "#]],
        );
    }

    #[test]
    fn rejects_when_iterating_over_a_boolean() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Item {
                  k: Bool
                }
                component Main(params: Array[Item]) {
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
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `Array[...]` got `Bool`
                  --> main.hop (line 11, col 18)
                10 |     <for {item in params}>
                11 |         <for {inner in item.k}>
                   |                        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_component_declaration_with_string_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(params: String) {
                	<div>{params}</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(params: String) {
                  <div>
                    {params}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_declaration_with_bool_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                component ToggleComp(enabled: Bool) {
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component ToggleComp(enabled: Bool) {
                  <if {enabled}>
                    <div>
                      Enabled
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_declaration_with_float_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                component CounterComp(count: Float) {
                	<if {count == 0.0}>
                		<div>Zero</div>
                	</if>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component CounterComp(count: Float) {
                  <if {(count == 0)}>
                    <div>
                      Zero
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_declaration_with_record_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Item {
                  active: Bool,
                  name: Bool,
                }
                record Params {
                  items: Array[Item],
                }

                component Main(params: Params) {
                	<for {item in params.items}>
                		<if {item.active}>
                		</if>
                		<if {item.name}>
                		</if>
                	</for>
                }
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

                component Main(params: main::Params) {
                  <for {item in params.items}>
                    <if {item.active}></if>
                    <if {item.name}></if>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_declaration_with_array_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                component ListComp(items: Array[String]) {
                	<for {item in items}>
                		<div>{item}</div>
                	</for>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component ListComp(items: Array[String]) {
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_strings_to_be_used_in_equals_expression() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Params {
                  x: String,
                  y: String,
                }

                component Main(params: Params) {
                  <if {params.x == params.y}>
                    <div>Values are equal</div>
                  </if>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record Params {
                  x: String,
                  y: String,
                }

                component Main(params: main::Params) {
                  <if {(params.x == params.y)}>
                    <div>
                      Values are equal
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_loops_to_use_same_variable_name() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Item {
                  a: Bool,
                  b: Bool,
                }

                component Main(params: Array[Item]) {
                	<for {j in params}>
                		<if {j.a}>
                		</if>
                	</for>
                	<for {j in params}>
                		<if {j.b}>
                		</if>
                	</for>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record Item {
                  a: Bool,
                  b: Bool,
                }

                component Main(params: Array[main::Item]) {
                  <for {j in params}>
                    <if {j.a}></if>
                  </for>
                  <for {j in params}>
                    <if {j.b}></if>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_iteration_over_array() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(i: Array[Bool]) {
                	<for {j in i}>
                		<if {j}>
                		</if>
                	</for>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(i: Array[Bool]) {
                  <for {j in i}>
                    <if {j}></if>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_iteration_over_nested_array() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(i: Array[Array[Bool]]) {
                	<for {j in i}>
                		<for {k in j}>
                			<if {k}>
                              ok!
                			</if>
                		</for>
                	</for>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(i: Array[Array[Bool]]) {
                  <for {j in i}>
                    <for {k in j}>
                      <if {k}>
                        ok!
                      </if>
                    </for>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_components_to_call_each_other_in_a_chain() {
        accept(
            indoc! {r#"
                -- a/bar.hop --
                pub record Config {
                  enabled: Bool,
                  title: String,
                }

                pub component WidgetComp(config: Config) {
                  <if {config.enabled}>
                    <div>{config.title}</div>
                  </if>
                }

                -- foo.hop --
                import a::bar::WidgetComp
                import a::bar::Config

                pub record Data {
                  items: Array[Config],
                }

                pub component PanelComp(data: Data) {
                  <for {item in data.items}>
                    <WidgetComp config={item}/>
                  </for>
                }

                -- main.hop --
                import foo::PanelComp
                import foo::Data

                record Dashboard {
                  items: Array[Data],
                }
                record Settings {
                  dashboard: Data,
                }

                component Main(settings: Settings) {
                  <PanelComp data={settings.dashboard}/>
                }
            "#},
            expect![[r#"
                -- a/bar.hop --
                record Config {
                  enabled: Bool,
                  title: String,
                }

                component WidgetComp(config: a::bar::Config) {
                  <if {config.enabled}>
                    <div>
                      {config.title}
                    </div>
                  </if>
                }

                -- foo.hop --
                record Data {
                  items: Array[a::bar::Config],
                }

                component PanelComp(data: foo::Data) {
                  <for {item in data.items}>
                    <WidgetComp config={item}/>
                  </for>
                }

                -- main.hop --
                record Dashboard {
                  items: Array[foo::Data],
                }

                record Settings {
                  dashboard: foo::Data,
                }

                component Main(settings: main::Settings) {
                  <PanelComp data={settings.dashboard}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_different_types_with_same_name_are_used_in_place_of_eachother() {
        reject(
            indoc! {r#"
                -- foo.hop --
                pub record User {
                    name: String,
                }

                pub component FooComp(user: User) {
                    <div>{user.name}</div>
                }

                -- bar.hop --
                pub record User {
                    email: String,
                }

                pub component BarComp(user: User) {
                    <div>{user.email}</div>
                }

                -- main.hop --
                import foo::FooComp
                import bar::BarComp
                import foo::User

                component Main(user: User) {
                    <FooComp user={user}/>
                    <BarComp user={user}/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `bar::User` got `foo::User`
                  --> main.hop (line 7, col 20)
                6 |     <FooComp user={user}/>
                7 |     <BarComp user={user}/>
                  |                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_identical_types_in_different_modules_are_used_in_place_of_eachother() {
        reject(
            indoc! {r#"
                -- foo.hop --
                pub record User {
                    name: String,
                    age: Int,
                }

                pub component FooComp(user: User) {
                    <div>{user.name}</div>
                }

                -- bar.hop --
                pub record User {
                    name: String,
                    age: Int,
                }

                pub component BarComp(user: User) {
                    <div>{user.name}</div>
                }

                -- main.hop --
                import foo::FooComp
                import bar::BarComp
                import foo::User

                component Main(user: User) {
                    <FooComp user={user}/>
                    <BarComp user={user}/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `bar::User` got `foo::User`
                  --> main.hop (line 7, col 20)
                6 |     <FooComp user={user}/>
                7 |     <BarComp user={user}/>
                  |                    ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_expressions_to_be_used_as_attributes() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User {url: String, theme: String}
                component Main(user: User) {
                  <a href={user.url} class={user.theme}>Link</a>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  url: String,
                  theme: String,
                }

                component Main(user: main::User) {
                  <a href={user.url} class={user.theme}>
                    Link
                  </a>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_children_to_be_passed_to_component() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(children: Fragment) {
                    <strong>
                        {children}
                    </strong>
                }

                component Bar {
                    <Main>
                        Here's the content for the children
                    </Main>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(children: Fragment) {
                  <strong>
                    {children}
                  </strong>
                }

                component Bar {
                  <let {
                    v_0 = {
                      Here's the content for the children
                    }
                  }>
                    <Main children={v_0}/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_field_access_is_performed_on_array() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(params: Array[String]) {
                	<for {x in params}>
                		{x}
                	</for>
                	<for {y in params.foo}>
                		{y}
                	</for>
                }
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
    fn accepts_if_statement() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User {is_active: Bool}
                component Main(user: User) {
                  <if {user.is_active}>
                    <div>User is active</div>
                  </if>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  is_active: Bool,
                }

                component Main(user: main::User) {
                  <if {user.is_active}>
                    <div>
                      User is active
                    </div>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_records_are_used_in_equals_expression() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Params {
                  foo: String,
                }
                component Main(p1: Params, p2: Params) {
                  <if {p1 == p2}>
                    eq 2
                  </if>
                }
            "#},
            expect![[r#"
                error: Type main::Params is not comparable
                  --> main.hop (line 5, col 8)
                4 | component Main(p1: Params, p2: Params) {
                5 |   <if {p1 == p2}>
                  |        ^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_int_is_passed_to_component_that_accepts_string() {
        reject(
            indoc! {r#"
                -- main.hop --
                component StringComp(message: String) {
                	<div>{message}</div>
                }
                component Main {
                	<StringComp message={42}/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Int`
                  --> main.hop (line 5, col 23)
                4 | component Main {
                5 |     <StringComp message={42}/>
                  |                          ^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_empty_string_is_passed_to_component_that_accepts_bool() {
        reject(
            indoc! {r#"
                -- main.hop --
                component ToggleComp(enabled: Bool) {
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                }
                component Main {
                	<ToggleComp enabled=""/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `Bool` got `String`
                  --> main.hop (line 7, col 22)
                6 | component Main {
                7 |     <ToggleComp enabled=""/>
                  |                         ^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_string_is_passed_to_component_that_accepts_bool() {
        reject(
            indoc! {r#"
                -- main.hop --
                component ToggleComp(enabled: Bool) {
                	<if {enabled}>
                		<div>Enabled</div>
                	</if>
                }
                component Main {
                	<ToggleComp enabled="not a boolean"/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `Bool` got `String`
                  --> main.hop (line 7, col 22)
                6 | component Main {
                7 |     <ToggleComp enabled="not a boolean"/>
                  |                         ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_non_bool_is_used_as_if_condition() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                    <if {"str"}>
                      is str?
                    </if>
                }
            "#},
            expect![[r#"
                error: Mismatched type for condition: expected `Bool` got `String`
                  --> main.hop (line 2, col 10)
                1 | component Main {
                2 |     <if {"str"}>
                  |          ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_type_error_occurs_in_an_argument_list() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(a: String, b: String) {
                  {a} {b}
                }
                component Foo {
                    <Main a={1 == ""} b={1 == ""}/>
                }
            "#},
            expect![[r#"
                error: Cannot compare Int to String
                  --> main.hop (line 5, col 14)
                4 | component Foo {
                5 |     <Main a={1 == ""} b={1 == ""}/>
                  |              ^^^^^^^

                error: Cannot compare Int to String
                  --> main.hop (line 5, col 26)
                4 | component Foo {
                5 |     <Main a={1 == ""} b={1 == ""}/>
                  |                          ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_iterating_over_empty_array() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                    <for {x in []}>
                      not ok
                    </for>
                }
            "#},
            expect![[r#"
                error: Cannot infer type of empty array
                  --> main.hop (line 2, col 16)
                1 | component Main {
                2 |     <for {x in []}>
                  |                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_empty_array_with_type_inferred_from_component_argument() {
        accept(
            indoc! {r#"
                -- main.hop --
                component ListItems(items: Array[String]) {
                    <for {item in items}>
                        <li>{item}</li>
                    </for>
                }
                component Main {
                    <ListItems items={[]}/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component ListItems(items: Array[String]) {
                  <for {item in items}>
                    <li>
                      {item}
                    </li>
                  </for>
                }

                component Main {
                  <ListItems items={[]}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_using_bool_in_text_expression() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                    {false}
                }
            "#},
            expect![[r#"
                error: Mismatched type for text expression: expected `String` got Bool
                  --> main.hop (line 2, col 6)
                1 | component Main {
                2 |     {false}
                  |      ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_undefined_type_is_used_in_parameter_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(user: User) {
                    <div></div>
                }
            "#},
            expect![[r#"
                error: Type 'User' is not defined
                  --> main.hop (line 1, col 22)
                1 | component Main(user: User) {
                  |                      ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_undefined_type_is_used_in_nested_parameter_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(users: Array[User]) {
                    <div></div>
                }
            "#},
            expect![[r#"
                error: Type 'User' is not defined
                  --> main.hop (line 1, col 29)
                1 | component Main(users: Array[User]) {
                  |                             ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_when_a_record_references_itself() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User {
                  name: String,
                  friend: User,
                }

                component Main {}
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: String,
                  friend: main::User,
                }

                component Main {}
            "#]],
        );
    }

    #[test]
    fn rejects_when_referencing_a_record_defined_below() {
        reject(
            indoc! {r#"
                -- main.hop --
                record User {
                  address: Address,
                }
                record Address {
                  city: String,
                }

                component Main {}
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
    fn accepts_accessing_a_record_field() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                component Main(user: User) {
                    <div>{user.name}</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: String,
                }

                component Main(user: main::User) {
                  <div>
                    {user.name}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_accessing_a_nested_record_field() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Address {city: String}
                record User {name: String, address: Address}
                component Main(user: User) {
                    <div>{user.address.city}</div>
                }
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

                component Main(user: main::User) {
                  <div>
                    {user.address.city}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_accessing_a_deeply_nested_record_field() {
        accept(
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
                component Main(params: Params) {
                	<if {params.app.ui.theme.dark}>
                      ok!
                	</if>
                	<if {params.app.api.endpoints.users.enabled}>
                      ok!
                	</if>
                	<if {params.app.database.connection.ssl}>
                      ok!
                	</if>
                }
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

                component Main(params: main::Params) {
                  <if {params.app.ui.theme.dark}>
                    ok!
                  </if>
                  <if {params.app.api.endpoints.users.enabled}>
                    ok!
                  </if>
                  <if {params.app.database.connection.ssl}>
                    ok!
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_nonexistent_field_is_accessed_on_record() {
        reject(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                component Main(user: User) {
                    <div>{user.email}</div>
                }
            "#},
            expect![[r#"
                error: Field 'email' not found in record 'User'
                  --> main.hop (line 3, col 11)
                2 | component Main(user: User) {
                3 |     <div>{user.email}</div>
                  |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_trying_to_import_from_wrong_module() {
        reject(
            indoc! {r#"
                -- foo.hop --
                pub record User {name: String}
                component Foo {}

                -- bar.hop --
                import foo::User
                component Bar {}

                -- baz.hop --
                import bar::User
                component Baz {}
            "#},
            expect![[r#"
                error: Module bar does not declare a type User
                  --> baz.hop (line 1, col 13)
                1 | import bar::User
                  |             ^^^^

                warning: Unused import 'User'
                  --> bar.hop (line 1, col 1)
                1 | import foo::User
                  | ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_import_and_use_of_records_declared_in_other_modules() {
        accept(
            indoc! {r#"
                -- foo.hop --
                pub record Address {
                  city: String,
                }

                component Foo {}
                -- bar.hop --
                import foo::Address

                pub record User {
                  name: String,
                  address: Address,
                }

                pub component Bar(user: User) {
                    <div>{user.address.city}</div>
                }
                -- baz.hop --
                import bar::Bar
                import bar::User
                import foo::Address
                component Baz {
                    <Bar user={User{name: "Alice", address: Address{city: "NYC"}}} />
                }
            "#},
            expect![[r#"
                -- foo.hop --
                record Address {
                  city: String,
                }

                component Foo {}

                -- bar.hop --
                record User {
                  name: String,
                  address: foo::Address,
                }

                component Bar(user: bar::User) {
                  <div>
                    {user.address.city}
                  </div>
                }

                -- baz.hop --
                component Baz {
                  <Bar user={User {name: "Alice", address: Address {city: "NYC"}}}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_import_and_use_of_enums_declared_in_other_modules() {
        accept(
            indoc! {r#"
                -- colors.hop --
                pub enum Color {
                    Red,
                    Green,
                    Blue,
                }

                pub component ColorDisplay(color: Color) {
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                    }}</div>
                }

                -- main.hop --
                import colors::Color
                import colors::ColorDisplay

                component Main {
                    <ColorDisplay color={Color::Red}/>
                }
            "#},
            expect![[r#"
                -- colors.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component ColorDisplay(color: colors::Color) {
                  <div>
                    {match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}
                  </div>
                }

                -- main.hop --
                component Main {
                  <ColorDisplay color={Color::Red}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_type_in_record_declaration_is_not_defined() {
        reject(
            indoc! {r#"
                -- hop/button.hop --
                pub component Button {}
                -- hop/input.hop --
                pub component Input {}
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

                warning: Unused import 'Button'
                  --> main.hop (line 1, col 1)
                1 | import hop::button::Button
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^

                warning: Unused import 'Input'
                  --> main.hop (line 2, col 1)
                1 | import hop::button::Button
                2 | import hop::input::Input
                  | ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_component_declaration_with_enum_equality() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color {Red, Green, Blue}

                component Main(a: Color, b: Color) {
                    <if {a == b}>
                    </if>
                }
            "#},
            expect![[r#"
                error: Type main::Color is not comparable
                  --> main.hop (line 4, col 10)
                3 | component Main(a: Color, b: Color) {
                4 |     <if {a == b}>
                  |          ^
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_in_text_interpolation() {
        accept(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                component Main(color: Color) {
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => "green",
                        Color::Blue => "blue",
                    }}</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(color: main::Color) {
                  <div>
                    {match color {
                      Color::Red => "red",
                      Color::Green => "green",
                      Color::Blue => "blue",
                    }}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_match_expression_with_non_enum_subject() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(name: String) {
                    <div>{match name {Color::Red => "red"}}</div>
                }
            "#},
            expect![[r#"
                error: Match is not implemented for type String
                  --> main.hop (line 2, col 17)
                1 | component Main(name: String) {
                2 |     <div>{match name {Color::Red => "red"}}</div>
                  |                 ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_expression_with_mismatched_arm_types() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                }

                component Main(color: Color) {
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => 42,
                    }}</div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Int`
                  --> main.hop (line 9, col 25)
                 8 |         Color::Red => "red",
                 9 |         Color::Green => 42,
                   |                         ^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_expression_with_missing_variant() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                component Main(color: Color) {
                    <div>{match color {
                        Color::Red => "red",
                        Color::Green => "green",
                    }}</div>
                }
            "#},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue
                  --> main.hop (line 8, col 17)
                 7 | component Main(color: Color) {
                 8 |     <div>{match color {
                   |                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_expression_with_wrong_enum_pattern() {
        reject(
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

                component Main(color: Color) {
                    <div>{match color {
                        Color::Red => "red",
                        Size::Small => "small",
                    }}</div>
                }
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
    fn rejects_match_expression_with_enum_equality() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color {
                    Red,
                    Green,
                    Blue,
                }

                component Main(color: Color) {
                    <if {color == Color::Red}>
                        <div>{match color {
                            Color::Red => "red",
                            Color::Green => "green",
                            Color::Blue => "blue",
                        }}</div>
                    </if>
                }
            "#},
            expect![[r#"
                error: Type main::Color is not comparable
                  --> main.hop (line 8, col 10)
                 7 | component Main(color: Color) {
                 8 |     <if {color == Color::Red}>
                   |          ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_as_record_field_type() {
        accept(
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

                component Main(user: User) {
                    <div>{match user.status {
                        Status::Active => "active",
                        Status::Inactive => "inactive",
                    }}</div>
                }
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

                component Main(user: main::User) {
                  <div>
                    {match user.status {
                      Status::Active => "active",
                      Status::Inactive => "inactive",
                    }}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_enum_field_in_conditional() {
        reject(
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

                component Main(person: Person) {
                    <if {person.role == Role::Admin}>
                        <div>Welcome, admin!</div>
                    </if>
                }
            "#},
            expect![[r#"
                error: Type main::Role is not comparable
                  --> main.hop (line 13, col 10)
                12 | component Main(person: Person) {
                13 |     <if {person.role == Role::Admin}>
                   |          ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_component_with_default_parameter_when_argument_omitted() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: String = "World") {
                  Hello, {name}!
                }
                component Main {
                  <Greeting />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: String) {
                  Hello, 
                  {name}
                  !
                }

                component Main {
                  <Greeting name={"World"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_with_default_parameter_when_argument_provided() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: String = "World") {
                  Hello, {name}!
                }
                component Main {
                  <Greeting name="Claude" />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: String) {
                  Hello, 
                  {name}
                  !
                }

                component Main {
                  <Greeting name={"Claude"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_with_mixed_required_and_default_parameters() {
        accept(
            indoc! {r#"
                -- main.hop --
                component UserCard(name: String, role: String = "user") {
                  {name} ({role})
                }
                component Main {
                  <UserCard name="Alice" />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component UserCard(name: String, role: String) {
                  {name}
                   (
                  {role}
                  )
                }

                component Main {
                  <UserCard name={"Alice"} role={"user"}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_required_param_is_missing_but_default_param_is_provided() {
        reject(
            indoc! {r#"
                -- main.hop --
                component UserCard(name: String, role: String = "user") {
                  {name} ({role})
                }
                component Main {
                  <UserCard role="admin" />
                }
            "#},
            expect![[r#"
                error: Component requires arguments: name
                  --> main.hop (line 5, col 4)
                4 | component Main {
                5 |   <UserCard role="admin" />
                  |    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_default_value_type_does_not_match_parameter_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: String = 42) {
                  Hello, {name}!
                }
                component Main {
                  <Greeting />
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Int`
                  --> main.hop (line 1, col 35)
                1 | component Greeting(name: String = 42) {
                  |                                   ^^

                error: Component requires arguments: name
                  --> main.hop (line 5, col 4)
                4 | component Main {
                5 |   <Greeting />
                  |    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_default_params_are_unused() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Config(debug: Bool = false, timeout: Int = 30) {
                }
                component Main {
                  <Config />
                }
            "#},
            expect![[r#"
                warning: Unused variable debug
                  --> main.hop (line 1, col 18)
                1 | component Config(debug: Bool = false, timeout: Int = 30) {
                  |                  ^^^^^

                warning: Unused variable timeout
                  --> main.hop (line 1, col 39)
                1 | component Config(debug: Bool = false, timeout: Int = 30) {
                  |                                       ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_default_empty_array_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                component ItemList(items: Array[String] = []) {
                  <for {item in items}>
                    {item}
                  </for>
                }
                component Main {
                  <ItemList />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component ItemList(items: Array[String]) {
                  <for {item in items}>
                    {item}
                  </for>
                }

                component Main {
                  <ItemList items={[]}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_default_record_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Config { name: String, enabled: Bool }
                component Settings(config: Config = Config{name: "default", enabled: true}) {
                  {config.name}
                }
                component Main {
                  <Settings />
                }
            "#},
            expect![[r#"
                -- main.hop --
                record Config {
                  name: String,
                  enabled: Bool,
                }

                component Settings(config: main::Config) {
                  {config.name}
                }

                component Main {
                  <Settings config={Config {name: "default", enabled: true}}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_default_enum_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                enum Status { Active{since: Int}, Inactive, Pending }
                component Badge(status: Status = Status::Active{since: 2000}) {
                  {match status {
                    Status::Active{since: _} => "active",
                    _ => "not active",
                  }}
                }
                component Main {
                  <Badge />
                }
            "#},
            expect![[r#"
                -- main.hop --
                enum Status {
                  Active{since: Int},
                  Inactive,
                  Pending,
                }

                component Badge(status: main::Status) {
                  {match status {
                    Status::Active => "active",
                    Status::Inactive => "not active",
                    Status::Pending => "not active",
                  }}
                }

                component Main {
                  <Badge status={Status::Active {since: 2000}}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_option_parameter_with_some_argument() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {name == None}></if>
                }
                component Main {
                  <Greeting name={Some("World")} />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {(name == None)}></if>
                }

                component Main {
                  <Greeting name={Some("World")}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_option_parameter_with_none_argument() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {name == None}></if>
                }
                component Main {
                  <Greeting name={None} />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {(name == None)}></if>
                }

                component Main {
                  <Greeting name={None}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_option_parameter_with_default_none() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: Option[String] = None) {
                  <if {name == None}></if>
                }
                component Main {
                  <Greeting />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {(name == None)}></if>
                }

                component Main {
                  <Greeting name={None}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_option_parameter_with_default_some() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: Option[String] = Some("World")) {
                  <if {name == None}></if>
                }
                component Main {
                  <Greeting />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {(name == None)}></if>
                }

                component Main {
                  <Greeting name={Some("World")}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_non_option_argument_for_option_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: Option[String]) {
                  <if {name == None}></if>
                }
                component Main {
                  <Greeting name="World" />
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `Option[String]` got `String`
                  --> main.hop (line 5, col 18)
                4 | component Main {
                5 |   <Greeting name="World" />
                  |                  ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_with_option() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(y)}>
                            found {y}
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(x: Option[String]) {
                  <match {x}>
                    <case {Some(v_1)}>
                      <let {y = v_1}>
                        found 
                        {y}
                      </let>
                    </case>
                    <case {None}>
                      nothing
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_with_enum() {
        accept(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                component Main(c: Color) {
                    <match {c}>
                        <case {Color::Red}>red</case>
                        <case {Color::Green}>green</case>
                        <case {Color::Blue}>blue</case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(c: main::Color) {
                  <match {c}>
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
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_on_enum_literal_with_fields() {
        accept(
            indoc! {r#"
                -- main.hop --
                enum Status { Active{name: String}, Inactive }
                component Main {
                    <match {Status::Active{name: "test"}}>
                        <case {Status::Active{name: n}}>
                            {n}
                        </case>
                        <case {Status::Inactive}>
                            none
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                enum Status {
                  Active{name: String},
                  Inactive,
                }

                component Main {
                  <match {Status::Active {name: "test"}}>
                    <case {Status::Active{name: v_1}}>
                      <let {n = v_1}>
                        {n}
                      </let>
                    </case>
                    <case {Status::Inactive}>
                      none
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_with_bool() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(flag: Bool) {
                    <match {flag}>
                        <case {true}>yes</case>
                        <case {false}>no</case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(flag: Bool) {
                  <match {flag}>
                    <case {true}>
                      yes
                    </case>
                    <case {false}>
                      no
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_pattern_type_mismatch() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(flag: Bool) {
                    <match {flag}>
                        <case {Some(x)}>yes</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Bool` got `Some(x)`
                  --> main.hop (line 3, col 16)
                2 |     <match {flag}>
                3 |         <case {Some(x)}>yes</case>
                  |                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_binding_in_match_case_children() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(name)}>
                            <div class={name}></div>
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(x: Option[String]) {
                  <match {x}>
                    <case {Some(v_1)}>
                      <let {name = v_1}>
                        <div class={name}></div>
                      </let>
                    </case>
                    <case {None}>
                      nothing
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_non_matchable_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(name: String) {
                    <match {name}>
                        <case {Some(x)}>yes</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Match is not implemented for type String
                  --> main.hop (line 2, col 13)
                1 | component Main(name: String) {
                2 |     <match {name}>
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_missing_enum_variants() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                component Main(c: Color) {
                    <match {c}>
                        <case {Color::Red}>red</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Match expression is missing arms for: Color::Blue, Color::Green
                  --> main.hop (line 3, col 13)
                2 | component Main(c: Color) {
                3 |     <match {c}>
                  |             ^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_missing_option_arm() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(y)}>{y}</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Match expression is missing arms for: None
                  --> main.hop (line 2, col 13)
                1 | component Main(x: Option[String]) {
                2 |     <match {x}>
                  |             ^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_missing_bool_arm() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(flag: Bool) {
                    <match {flag}>
                        <case {true}>yes</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Match expression is missing arms for: false
                  --> main.hop (line 2, col 13)
                1 | component Main(flag: Bool) {
                2 |     <match {flag}>
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_wrong_enum_pattern() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green }
                enum Size { Small, Large }
                component Main(c: Color) {
                    <match {c}>
                        <case {Color::Red}>red</case>
                        <case {Size::Small}>small</case>
                    </match>
                }
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
    fn rejects_on_unused_binding_in_match_case() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(unused)}>
                            found something
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                warning: Unused variable unused
                  --> main.hop (line 3, col 21)
                 2 |     <match {x}>
                 3 |         <case {Some(unused)}>
                   |                     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_binding_that_shadows_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(x)}>
                            {x}
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Variable x is already defined
                  --> main.hop (line 3, col 21)
                 2 |     <match {x}>
                 3 |         <case {Some(x)}>
                   |                     ^

                error: Mismatched type for text expression: expected `String` got Option[String]
                  --> main.hop (line 4, col 14)
                 3 |         <case {Some(x)}>
                 4 |             {x}
                   |              ^
            "#]],
        );
    }

    #[test]
    fn rejects_shorthand_binding_that_shadows_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Event { Comment {author: String} }
                component Main(author: String, event: Event) {
                    <match {event}>
                        <case {Event::Comment {author}}>
                            {author}
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Variable author is already defined
                  --> main.hop (line 4, col 32)
                3 |     <match {event}>
                4 |         <case {Event::Comment {author}}>
                  |                                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_only_wildcard() {
        reject(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                component Main(c: Color) {
                    <match {c}>
                        <case {_}>any color</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                  --> main.hop (line 3, col 13)
                2 | component Main(c: Color) {
                3 |     <match {c}>
                  |             ^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_nested_wildcard_record_followed_by_wildcard() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Role { title: String, salary: Int }
                record User { role: Role, created_at: Int }
                component Main(user: User) {
                    <match {user}>
                        <case {User{role: Role{title: _, salary: _}, created_at: _}}>matched</case>
                        <case {_}>fallback</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for pattern '_'
                  --> main.hop (line 6, col 16)
                5 |         <case {User{role: Role{title: _, salary: _}, created_at: _}}>matched</case>
                6 |         <case {_}>fallback</case>
                  |                ^
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_nested_wildcard_record() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Role { title: String, salary: Int }
                record User { role: Role, created_at: Int }
                component Main(user: User) {
                    <match {user}>
                        <case {User{role: Role{title: _, salary: _}, created_at: _}}>matched</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                  --> main.hop (line 4, col 13)
                3 | component Main(user: User) {
                4 |     <match {user}>
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_nested_match_nodes() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[Option[String]]) {
                    <match {x}>
                        <case {Some(inner)}>
                            <match {inner}>
                                <case {Some(s)}>{s}</case>
                                <case {None}>inner none</case>
                            </match>
                        </case>
                        <case {None}>
                            outer none
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(x: Option[Option[String]]) {
                  <match {x}>
                    <case {Some(v_1)}>
                      <let {inner = v_1}>
                        <match {inner}>
                          <case {Some(v_3)}>
                            <let {s = v_3}>
                              {s}
                            </let>
                          </case>
                          <case {None}>
                            inner none
                          </case>
                        </match>
                      </let>
                    </case>
                    <case {None}>
                      outer none
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_inside_for_loop() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[Option[String]]) {
                    <for {item in items}>
                        <match {item}>
                            <case {Some(s)}>{s}</case>
                            <case {None}>-</case>
                        </match>
                    </for>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(items: Array[Option[String]]) {
                  <for {item in items}>
                    <match {item}>
                      <case {Some(v_1)}>
                        <let {s = v_1}>
                          {s}
                        </let>
                      </case>
                      <case {None}>
                        -
                      </case>
                    </match>
                  </for>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_with_wildcard_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(_)}>
                            found something
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(x: Option[String]) {
                  <match {x}>
                    <case {Some(_)}>
                      found something
                    </case>
                    <case {None}>
                      nothing
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_bindings_with_same_name_in_different_cases() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(r1: Option[String], r2: Option[Bool]) {
                    <match {r1}>
                        <case {Some(bound)}>{bound}</case>
                        <case {None}>
                            <match {r2}>
                                <case {Some(bound)}><if {bound}>yes</if></case>
                                <case {None}>both none</case>
                            </match>
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(r1: Option[String], r2: Option[Bool]) {
                  <match {r1}>
                    <case {Some(v_1)}>
                      <let {bound = v_1}>
                        {bound}
                      </let>
                    </case>
                    <case {None}>
                      <match {r2}>
                        <case {Some(v_3)}>
                          <let {bound = v_3}>
                            <if {bound}>
                              yes
                            </if>
                          </let>
                        </case>
                        <case {None}>
                          both none
                        </case>
                      </match>
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_with_record_field_subject() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User { name: Option[String] }
                component Main(user: User) {
                    <match {user.name}>
                        <case {Some(n)}>{n}</case>
                        <case {None}>anonymous</case>
                    </match>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: Option[String],
                }

                component Main(user: main::User) {
                  <match {user.name}>
                    <case {Some(v_1)}>
                      <let {n = v_1}>
                        {n}
                      </let>
                    </case>
                    <case {None}>
                      anonymous
                    </case>
                  </match>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_match_node_with_int_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(count: Int) {
                    <match {count}>
                        <case {Some(x)}>{x}</case>
                    </match>
                }
            "#},
            expect![[r#"
                error: Match is not implemented for type Int
                  --> main.hop (line 2, col 13)
                1 | component Main(count: Int) {
                2 |     <match {count}>
                  |             ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_inside_if_condition() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(show: Bool, x: Option[String]) {
                    <if {show}>
                        <match {x}>
                            <case {Some(v)}>{v}</case>
                            <case {None}>none</case>
                        </match>
                    </if>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(show: Bool, x: Option[String]) {
                  <if {show}>
                    <match {x}>
                      <case {Some(v_1)}>
                        <let {v = v_1}>
                          {v}
                        </let>
                      </case>
                      <case {None}>
                        none
                      </case>
                    </match>
                  </if>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_node_in_html_element() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main(x: Option[String]) {
                    <div>
                        <match {x}>
                            <case {Some(v)}><span>{v}</span></case>
                            <case {None}><span>none</span></case>
                        </match>
                    </div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main(x: Option[String]) {
                  <div>
                    <match {x}>
                      <case {Some(v_1)}>
                        <let {v = v_1}>
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
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_shadowed_variable_in_nested_match_expression() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(c: Option[String]) {
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
                }
            "#},
            expect![[r#"
                error: Variable x is already defined
                  --> main.hop (line 6, col 14)
                 5 |         None    => x,
                 6 |         Some(x) => x,
                   |              ^
            "#]],
        );
    }

    #[test]
    fn rejects_boolean_expression_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(is_required: Bool) {
                  <input required={is_required}>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Bool`
                  --> main.hop (line 2, col 20)
                1 | component Main(is_required: Bool) {
                2 |   <input required={is_required}>
                  |                    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_boolean_literal_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <input required={true}>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Bool`
                  --> main.hop (line 2, col 20)
                1 | component Main {
                2 |   <input required={true}>
                  |                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_option_string_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(maybe: Option[String]) {
                  <div data-x={maybe}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Option[String]`
                  --> main.hop (line 2, col 16)
                1 | component Main(maybe: Option[String]) {
                2 |   <div data-x={maybe}></div>
                  |                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_some_literal_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <div data-x={Some("hello")}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Option[String]`
                  --> main.hop (line 2, col 16)
                1 | component Main {
                2 |   <div data-x={Some("hello")}></div>
                  |                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_option_non_string_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(maybe: Option[Int]) {
                  <div data-x={maybe}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Option[Int]`
                  --> main.hop (line 2, col 16)
                1 | component Main(maybe: Option[Int]) {
                2 |   <div data-x={maybe}></div>
                  |                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_option_bool_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(maybe: Option[Bool]) {
                  <div data-x={maybe}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Option[Bool]`
                  --> main.hop (line 2, col 16)
                1 | component Main(maybe: Option[Bool]) {
                2 |   <div data-x={maybe}></div>
                  |                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_nested_option_string_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(maybe: Option[Option[String]]) {
                  <div data-x={maybe}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Option[Option[String]]`
                  --> main.hop (line 2, col 16)
                1 | component Main(maybe: Option[Option[String]]) {
                2 |   <div data-x={maybe}></div>
                  |                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_non_string_attribute_expression() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(count: Int) {
                  <div data-count={count}></div>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Int`
                  --> main.hop (line 2, col 20)
                1 | component Main(count: Int) {
                2 |   <div data-count={count}></div>
                  |                    ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_event_handler_attributes() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <button onclick="alert(1)">Click</button>
                }
            "#},
            expect![[r#"
                error: `<button>` does not accept attribute `onclick`
                  --> main.hop (line 2, col 11)
                1 | component Main {
                2 |   <button onclick="alert(1)">Click</button>
                  |           ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_event_handler_attributes_case_insensitively() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <button onClick="alert(1)">Click</button>
                  <button ONCLICK="alert(1)">Click</button>
                }
            "#},
            expect![[r#"
                error: `<button>` does not accept attribute `onClick`
                  --> main.hop (line 2, col 11)
                1 | component Main {
                2 |   <button onClick="alert(1)">Click</button>
                  |           ^^^^^^^

                error: `<button>` does not accept attribute `ONCLICK`
                  --> main.hop (line 3, col 11)
                2 |   <button onClick="alert(1)">Click</button>
                3 |   <button ONCLICK="alert(1)">Click</button>
                  |           ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_html_attribute() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <div flooble="x"></div>
                }
            "#},
            expect![[r#"
                error: `<div>` does not accept attribute `flooble`
                  --> main.hop (line 2, col 8)
                1 | component Main {
                2 |   <div flooble="x"></div>
                  |        ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_arbitrary_attribute_on_custom_element() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <my-widget foo="x"></my-widget>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <my-widget foo="x"></my-widget>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_attribute_on_wrong_element() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <button href="/"></button>
                }
            "#},
            expect![[r#"
                error: `<button>` does not accept attribute `href`
                  --> main.hop (line 2, col 11)
                1 | component Main {
                2 |   <button href="/"></button>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_attribute_on_correct_element() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <a href="/">link</a>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <a href="/">
                    link
                  </a>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_forwarded_attribute_invalid_on_component_root_element() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Btn(children: Fragment, ...rest) {
                  <button ...rest>{children}</button>
                }

                component Main {
                  <Btn href="/">click</Btn>
                }
            "#},
            expect![[r#"
                error: Component `Btn` does not accept attribute `href`
                  --> main.hop (line 6, col 8)
                5 | component Main {
                6 |   <Btn href="/">click</Btn>
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_forwarded_attribute_valid_on_component_root_element() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Btn(children: Fragment, ...rest) {
                  <button ...rest>{children}</button>
                }

                component Main {
                  <Btn disabled>click</Btn>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Btn(children: Fragment, ...rest) {
                  <button ...rest>
                    {children}
                  </button>
                }

                component Main {
                  <let {
                    v_0 = {
                      click
                    }
                  }>
                    <Btn children={v_0} disabled/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_data_and_aria_attributes() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <div data-x="1" aria-label="hello"></div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <div data-x="1" aria-label="hello"></div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_svg_attributes_without_name_validation() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <svg viewBox="0 0 10 10"><path d="M0 0 L10 10"></path></svg>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <svg viewBox="0 0 10 10">
                    <path d="M0 0 L10 10"></path>
                  </svg>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unused_children_named_param_with_non_fragment_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Separator(children: Option[Fragment] = None) {
                  <li>separator</li>
                }
                component Main {
                  <Separator />
                }
            "#},
            expect![[r#"
                warning: Unused variable children
                  --> main.hop (line 1, col 21)
                1 | component Separator(children: Option[Fragment] = None) {
                  |                     ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_let_binding_used_in_children() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = "World"}>
                    <div>Hello {name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {name = "World"}>
                    <div>
                      Hello 
                      {name}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_inferred_string_let_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name = "World"}>
                    <div>{name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {name = "World"}>
                    <div>
                      {name}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_inferred_int_let_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {count = 42}>
                    <div>{count.to_string()}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {count = 42}>
                    <div>
                      {count.to_string()}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_inferred_float_let_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {price = 2.5}>
                    <div>{price.to_int().to_string()}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {price = 2.5}>
                    <div>
                      {price.to_int().to_string()}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_inferred_nonempty_array_let_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {items = [1, 2, 3]}>
                    <div>{items.len().to_string()}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {items = [1, 2, 3]}>
                    <div>
                      {items.len().to_string()}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_inferred_record_literal_let_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User { name: String, age: Int }
                component Main {
                  <let {user = User {name: "Alice", age: 30}}>
                    <div>{user.name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: String,
                  age: Int,
                }

                component Main {
                  <let {user = User {name: "Alice", age: 30}}>
                    <div>
                      {user.name}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_mixed_annotated_and_inferred_bindings() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {first: String = "Hello", second = "World"}>
                    <div>{first}{second}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {first = "Hello"}>
                    <let {second = "World"}>
                      <div>
                        {first}
                        {second}
                      </div>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_binding_referencing_earlier_inferred_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {greeting = "Hello", shout = greeting}>
                    <div>{shout}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {greeting = "Hello"}>
                    <let {shout = greeting}>
                      <div>
                        {shout}
                      </div>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unused_inferred_let_binding() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name = "World"}>
                    <div>x</div>
                  </let>
                }
            "#},
            expect![[r#"
                warning: Unused variable name
                  --> main.hop (line 2, col 9)
                1 | component Main {
                2 |   <let {name = "World"}>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_inferring_empty_array_let_binding() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {items = []}>
                    <div>x</div>
                  </let>
                }
            "#},
            expect![[r#"
                error: Cannot infer type of empty array
                  --> main.hop (line 2, col 17)
                1 | component Main {
                2 |   <let {items = []}>
                  |                 ^^
            "#]],
        );
    }

    #[test]
    fn rejects_inferring_none_let_binding() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {maybe = None}>
                    <div>x</div>
                  </let>
                }
            "#},
            expect![[r#"
                error: Cannot infer type of None without context
                  --> main.hop (line 2, col 17)
                1 | component Main {
                2 |   <let {maybe = None}>
                  |                 ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_on_let_with_unused_variable() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = "World"}>
                    <div>Hello</div>
                  </let>
                }
            "#},
            expect![[r#"
                warning: Unused variable name
                  --> main.hop (line 2, col 9)
                1 | component Main {
                2 |   <let {name: String = "World"}>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_let_shadowing_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main(name: String) {
                  <let {name: String = "Shadow"}>
                    <div>{name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                error: Variable name is already defined
                  --> main.hop (line 2, col 9)
                1 | component Main(name: String) {
                2 |   <let {name: String = "Shadow"}>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_let_shadowing_another_let() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = "First"}>
                    <let {name: String = "Second"}>
                      <div>{name}</div>
                    </let>
                  </let>
                }
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
    fn accepts_let_with_same_name_in_sibling_scope() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = "First"}>
                    <div>{name}</div>
                  </let>
                  <let {name: String = "Second"}>
                    <div>{name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
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
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_with_type_mismatch() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = 42}>
                    <div>{name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Int`
                  --> main.hop (line 2, col 24)
                1 | component Main {
                2 |   <let {name: String = 42}>
                  |                        ^^
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_multiple_bindings() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {first: String = "Hello", second: String = "World"}>
                    <div>{first} {second}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {first = "Hello"}>
                    <let {second = "World"}>
                      <div>
                        {first}
                        {second}
                      </div>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_with_duplicate_bindings() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = "Hello", name: String = "World"}>
                    <div>{name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                error: Variable name is already defined
                  --> main.hop (line 2, col 33)
                1 | component Main {
                2 |   <let {name: String = "Hello", name: String = "World"}>
                  |                                 ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_let_with_unused_second_binding() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {name: String = "Hello", count: Int = 42}>
                    <div>{name}</div>
                  </let>
                }
            "#},
            expect![[r#"
                warning: Unused variable count
                  --> main.hop (line 2, col 33)
                1 | component Main {
                2 |   <let {name: String = "Hello", count: Int = 42}>
                  |                                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_later_binding_to_reference_earlier_binding() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {greeting: String = "Hello", message: String = greeting}>
                    <div>{message}</div>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {greeting = "Hello"}>
                    <let {message = greeting}>
                      <div>
                        {message}
                      </div>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_chained_bindings_with_arithmetic() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {x: Int = 0, y: Int = x + 1, z: Int = y + 2}>
                    <if {z == 3}>
                      <div>correct</div>
                    </if>
                  </let>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Main {
                  <let {x = 0}>
                    <let {y = (x + 1)}>
                      <let {z = (y + 2)}>
                        <if {(z == 3)}>
                          <div>
                            correct
                          </div>
                        </if>
                      </let>
                    </let>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_binding_that_references_later_binding() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <let {x: Int = y + 1, y: Int = 0}>
                    <if {x == 1}>
                      <div>correct</div>
                    </if>
                  </let>
                }
            "#},
            expect![[r#"
                error: Undefined variable: y
                  --> main.hop (line 2, col 18)
                1 | component Main {
                2 |   <let {x: Int = y + 1, y: Int = 0}>
                  |                  ^

                warning: Unused variable y
                  --> main.hop (line 2, col 25)
                1 | component Main {
                2 |   <let {x: Int = y + 1, y: Int = 0}>
                  |                         ^
            "#]],
        );
    }

    #[test]
    fn accepts_view_without_parameters() {
        accept(
            indoc! {r#"
                -- main.hop --
                view Main() {
                  <div>Hello</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                view Main() {
                  <div>
                    Hello
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_string_parameter() {
        accept(
            indoc! {r#"
                -- main.hop --
                view Main(name: String) {
                  <div>Hello, {name}!</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                view Main(name: String) {
                  <div>
                    Hello, 
                    {name}
                    !
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_multiple_parameters() {
        accept(
            indoc! {r#"
                -- main.hop --
                view Main(name: String, age: Int) {
                  <div>Hello, {name}! You are {age.to_string()} years old.</div>
                }
            "#},
            expect![[r#"
                -- main.hop --
                view Main(name: String, age: Int) {
                  <div>
                    Hello, 
                    {name}
                    ! You are 
                    {age.to_string()}
                     years old.
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_referencing_component() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: String) {
                  <div>Hello, {name}!</div>
                }

                view Main(name: String) {
                  <Greeting name={name} />
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Greeting(name: String) {
                  <div>
                    Hello, 
                    {name}
                    !
                  </div>
                }

                view Main(name: String) {
                  <Greeting name={name}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_undefined_component() {
        reject(
            indoc! {r#"
                -- main.hop --
                view Main() {
                  <UndefinedComponent />
                }
            "#},
            expect![[r#"
                error: Component UndefinedComponent is not defined
                  --> main.hop (line 2, col 4)
                1 | view Main() {
                2 |   <UndefinedComponent />
                  |    ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_unused_parameter() {
        reject(
            indoc! {r#"
                -- main.hop --
                view Main(unused: String) {
                  <div>Hello</div>
                }
            "#},
            expect![[r#"
                warning: Unused variable unused
                  --> main.hop (line 1, col 11)
                1 | view Main(unused: String) {
                  |           ^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_undefined_parameter_type() {
        reject(
            indoc! {r#"
                -- main.hop --
                view Main(foo: UndefinedType) {
                  <div>Hello</div>
                }
            "#},
            expect![[r#"
                error: Type 'UndefinedType' is not defined
                  --> main.hop (line 1, col 16)
                1 | view Main(foo: UndefinedType) {
                  |                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_self_referential_record_in_list() {
        accept(
            indoc! {r#"
                -- main.hop --
                record TreeNode {
                  value: Int,
                  children: Array[TreeNode],
                }

                component Main {
                }
            "#},
            expect![[r#"
                -- main.hop --
                record TreeNode {
                  value: Int,
                  children: Array[main::TreeNode],
                }

                component Main {}
            "#]],
        );
    }

    #[test]
    fn accepts_self_referential_enum() {
        accept(
            indoc! {r#"
                -- main.hop --
                enum Expr {
                  Literal{value: Int},
                  Neg{inner: Expr},
                }

                component Main {
                }
            "#},
            expect![[r#"
                -- main.hop --
                enum Expr {
                  Literal{value: Int},
                  Neg{inner: main::Expr},
                }

                component Main {}
            "#]],
        );
    }

    #[test]
    fn accepts_examples_min_max_on_int_field() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Product {
                  #[examples(min = 1, max = 100)]
                  price: Int,
                }
                component Main {}
            "#},
            expect![[r#"
                -- main.hop --
                record Product {
                  price: Int,
                }

                component Main {}
            "#]],
        );
    }

    #[test]
    fn rejects_min_max_on_string_field() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Product {
                  #[examples(min = 1, max = 100)]
                  name: String,
                }
                component Main {}
            "#},
            expect![[r#"
                error: #[examples(min = ..., max = ...)] is only valid on Int fields, found String
                  --> main.hop (line 3, col 3)
                2 |   #[examples(min = 1, max = 100)]
                3 |   name: String,
                  |   ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_min_greater_than_max() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Product {
                  #[examples(min = 100, max = 1)]
                  price: Int,
                }
                component Main {}
            "#},
            expect![[r#"
                error: #[examples(min = 100)] must be less than or equal to max = 1
                  --> main.hop (line 3, col 3)
                2 |   #[examples(min = 100, max = 1)]
                3 |   price: Int,
                  |   ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_examples_min_len_max_len_on_array_field() {
        accept(
            indoc! {r#"
                -- main.hop --
                record Post {
                  #[examples(min_len = 2, max_len = 5)]
                  tags: Array[String],
                }
                component Main {}
            "#},
            expect![[r#"
                -- main.hop --
                record Post {
                  tags: Array[String],
                }

                component Main {}
            "#]],
        );
    }

    #[test]
    fn rejects_min_len_max_len_on_non_array_field() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Product {
                  #[examples(min_len = 1, max_len = 5)]
                  name: String,
                }
                component Main {}
            "#},
            expect![[r#"
                error: #[examples(min_len = ..., max_len = ...)] is only valid on Array fields, found String
                  --> main.hop (line 3, col 3)
                2 |   #[examples(min_len = 1, max_len = 5)]
                3 |   name: String,
                  |   ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_negative_min_len() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Post {
                  #[examples(min_len = -1, max_len = 5)]
                  tags: Array[String],
                }
                component Main {}
            "#},
            expect![[r#"
                error: #[examples(min_len = ..., max_len = ...)] must be non-negative, found -1
                  --> main.hop (line 3, col 3)
                2 |   #[examples(min_len = -1, max_len = 5)]
                3 |   tags: Array[String],
                  |   ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_min_len_greater_than_max_len() {
        reject(
            indoc! {r#"
                -- main.hop --
                record Post {
                  #[examples(min_len = 5, max_len = 2)]
                  tags: Array[String],
                }
                component Main {}
            "#},
            expect![[r#"
                error: #[examples(min_len = 5)] must be less than or equal to max_len = 2
                  --> main.hop (line 3, col 3)
                2 |   #[examples(min_len = 5, max_len = 2)]
                3 |   tags: Array[String],
                  |   ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_rest_on_recursive_component() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Foo(...rest) {
                    <div ...rest><Foo/></div>
                }
            "#},
            expect![[r#"
                error: Component Foo is recursive and cannot use rest parameters
                  --> main.hop (line 1, col 11)
                1 | component Foo(...rest) {
                  |           ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_attr_without_rest_param() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Foo(class: String) {
                    <div class={class}></div>
                }
                view Main {
                    <Foo class="a" data-x="y"/>
                }
            "#},
            expect![[r#"
                error: Component `Foo` does not accept attribute `data-x`
                  --> main.hop (line 5, col 20)
                4 | view Main {
                5 |     <Foo class="a" data-x="y"/>
                  |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_extra_attrs_when_rest_reaches_html() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Button(class: String, children: Fragment, ...rest) {
                    <button class={class} ...rest>{children}</button>
                }
                view Main {
                    <Button class="p-2" data-foo="bar">Hi</Button>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Button(class: String, children: Fragment, ...rest) {
                  <button class={class} ...rest>
                    {children}
                  </button>
                }

                view Main() {
                  <let {
                    v_0 = {
                      Hi
                    }
                  }>
                    <Button class={"p-2"} children={v_0} data-foo="bar"/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_forwarded_attr_not_set_on_element() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Button(children: Fragment, ...rest) {
                    <button class="builtin" ...rest>{children}</button>
                }
                view Main {
                    <Button data-x="y">Hi</Button>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Button(children: Fragment, ...rest) {
                  <button class="builtin" ...rest>
                    {children}
                  </button>
                }

                view Main() {
                  <let {
                    v_0 = {
                      Hi
                    }
                  }>
                    <Button children={v_0} data-x="y"/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_forwarded_attr_already_set_on_element() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Button(children: Fragment, ...rest) {
                    <button class="builtin" ...rest>{children}</button>
                }
                view Main {
                    <Button class="forwarded">Hi</Button>
                }
            "#},
            expect![[r#"
                error: Component `Button` does not accept attribute `class`
                  --> main.hop (line 5, col 13)
                4 | view Main {
                5 |     <Button class="forwarded">Hi</Button>
                  |             ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_html_attr_forwarded_into_open_tail() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Button(class: String, ...rest) {
                    <button class={class} ...rest></button>
                }
                view Main {
                    <Button class="p-2" qwerty="z"/>
                }
            "#},
            expect![[r#"
                error: Component `Button` does not accept attribute `qwerty`
                  --> main.hop (line 5, col 25)
                4 | view Main {
                5 |     <Button class="p-2" qwerty="z"/>
                  |                         ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_svg_attributes_on_forwarded_svg() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Svg(...rest) {
                    <svg ...rest/>
                }
                view Main {
                    <Svg viewBox="0 0 100 100"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Svg(...rest) {
                  <svg ...rest></svg>
                }

                view Main() {
                  <Svg viewBox="0 0 100 100"/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_required_arg_forwarded_through_rest() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(title: String) {
                    <div>{title}</div>
                }
                component Wrapper(...rest) {
                    <Card ...rest/>
                }
                view Main {
                    <Wrapper title="hi"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(title: String) {
                  <div>
                    {title}
                  </div>
                }

                component Wrapper(...rest) {
                  <Card ...rest/>
                }

                view Main() {
                  <Wrapper title={"hi"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_explicit_arg_supplied_alongside_rest_spread() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(title: String) {
                    <div>{title}</div>
                }
                component Wrapper(...rest) {
                    <Card title="explicit" ...rest/>
                }
                view Main {
                    <Wrapper/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(title: String) {
                  <div>
                    {title}
                  </div>
                }

                component Wrapper(...rest) {
                  <Card title={"explicit"} ...rest/>
                }

                view Main() {
                  <Wrapper/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_missing_required_arg_forwarded_through_rest() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(title: String) {
                    <div>{title}</div>
                }
                component Wrapper(...rest) {
                    <Card ...rest/>
                }
                view Main {
                    <Wrapper/>
                }
            "#},
            expect![[r#"
                error: Component requires arguments: title
                  --> main.hop (line 8, col 6)
                7 | view Main {
                8 |     <Wrapper/>
                  |      ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_type_mismatch_forwarded_through_rest() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(count: Int) {
                    <if {count > 0}>
                        <div>positive</div>
                    </if>
                }
                component Wrapper(...rest) {
                    <Card ...rest/>
                }
                view Main {
                    <Wrapper count="hi"/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `Int` got `String`
                  --> main.hop (line 10, col 20)
                 9 | view Main {
                10 |     <Wrapper count="hi"/>
                   |                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_param_rest_name_collision() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Inner(class: String, ...rest) {
                    <span class={class} ...rest></span>
                }
                component Outer(class: String, ...rest) {
                    <div class={class}>
                        <Inner ...rest/>
                    </div>
                }
                view Main {
                    <Outer class="x"/>
                }
            "#},
            expect![[r#"
                error: Component requires arguments: class
                  --> main.hop (line 6, col 10)
                 5 |     <div class={class}>
                 6 |         <Inner ...rest/>
                   |          ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_rest_forwarded_into_recursive_component() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Tree(x: Int) {
                    <div>
                        <Tree x={x}/>
                    </div>
                }
                component Wrapper(...rest) {
                    <Tree ...rest/>
                }
                view Main {
                    <Wrapper/>
                }
            "#},
            expect![[r#"
                error: Rest cannot be forwarded into recursive component Tree
                  --> main.hop (line 7, col 11)
                 6 | component Wrapper(...rest) {
                 7 |     <Tree ...rest/>
                   |           ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_record_param_forwarded_through_rest() {
        accept(
            indoc! {r#"
                -- main.hop --
                record User {
                    name: String,
                }
                component Card(user: User) {
                    <div>{user.name}</div>
                }
                component Wrapper(...rest) {
                    <Card ...rest/>
                }
                component Page(user: User) {
                    <Wrapper user={user}/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                record User {
                  name: String,
                }

                component Card(user: main::User) {
                  <div>
                    {user.name}
                  </div>
                }

                component Wrapper(...rest) {
                  <Card ...rest/>
                }

                component Page(user: main::User) {
                  <Wrapper user={user}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_required_args_forwarded_transitively() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(title: String) {
                    <div>{title}</div>
                }
                component Bar(name: String, ...rest) {
                    <div>
                        {name}
                        <Card ...rest/>
                    </div>
                }
                component Baz(...rest) {
                    <Bar ...rest/>
                }
                view Main {
                    <Baz name="n" title="t"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(title: String) {
                  <div>
                    {title}
                  </div>
                }

                component Bar(name: String, ...rest) {
                  <div>
                    {name}
                    <Card ...rest/>
                  </div>
                }

                component Baz(...rest) {
                  <Bar ...rest/>
                }

                view Main() {
                  <Baz name={"n"} title={"t"}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_missing_required_arg_forwarded_transitively() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Card(title: String) {
                    <div>{title}</div>
                }
                component Bar(name: String, ...rest) {
                    <div>
                        {name}
                        <Card ...rest/>
                    </div>
                }
                component Baz(...rest) {
                    <Bar ...rest/>
                }
                view Main {
                    <Baz/>
                }
            "#},
            expect![[r#"
                error: Component requires arguments: name, title
                  --> main.hop (line 14, col 6)
                13 | view Main {
                14 |     <Baz/>
                   |      ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_missing_required_arg_through_any_rest() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Box(id: String, ...rest) {
                    <div id={id} ...rest></div>
                }
                component Wrapper(...rest) {
                    <Box ...rest/>
                }
                view Main {
                    <Wrapper/>
                }
            "#},
            expect![[r#"
                error: Component requires arguments: id
                  --> main.hop (line 8, col 6)
                7 | view Main {
                8 |     <Wrapper/>
                  |      ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_int_param_forwarded_through_rest() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Card(count: Int) {
                    <if {count > 0}>
                        <div>positive</div>
                    </if>
                }
                component Wrapper(...rest) {
                    <Card ...rest/>
                }
                view Main {
                    <Wrapper count={3}/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Card(count: Int) {
                  <if {(count > 0)}>
                    <div>
                      positive
                    </div>
                  </if>
                }

                component Wrapper(...rest) {
                  <Card ...rest/>
                }

                view Main() {
                  <Wrapper count={3}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_typed_field_and_open_html_tail_in_one_forward() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Box(count: Int, ...rest) {
                    <div ...rest>
                        <if {count > 0}>
                            positive
                        </if>
                    </div>
                }
                component Wrapper(...rest) {
                    <Box ...rest/>
                }
                view Main {
                    <Wrapper count={3} data-foo="bar"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Box(count: Int, ...rest) {
                  <div ...rest>
                    <if {(count > 0)}>
                      positive
                    </if>
                  </div>
                }

                component Wrapper(...rest) {
                  <Box ...rest/>
                }

                view Main() {
                  <Wrapper count={3} data-foo="bar"/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_attr_outside_propagated_accept_set() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Foo(class: String) {
                    <div class={class}></div>
                }
                component Bar(...rest) {
                    <Foo ...rest/>
                }
                component Baz(...rest) {
                    <Bar ...rest/>
                }
                view Main {
                    <Baz class="a" data-x="y"/>
                }
            "#},
            expect![[r#"
                error: Component `Baz` does not accept attribute `data-x`
                  --> main.hop (line 11, col 20)
                10 | view Main {
                11 |     <Baz class="a" data-x="y"/>
                   |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_attr_not_accepted_by_forwarded_component() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Foo(class: String) {
                    <div class={class}></div>
                }
                component Bar(...rest) {
                    <Foo ...rest/>
                }
                view Main {
                    <Bar class="a" data-x="y"/>
                }
            "#},
            expect![[r#"
                error: Component `Bar` does not accept attribute `data-x`
                  --> main.hop (line 8, col 20)
                7 | view Main {
                8 |     <Bar class="a" data-x="y"/>
                  |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_missing_children_forwarded_through_rest() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Foo(children: Fragment) {
                    <div>{children}</div>
                }
                component Wrapper(...rest) {
                    <Foo ...rest/>
                }
                view Main {
                    <Wrapper/>
                }
            "#},
            expect![[r#"
                error: Component requires arguments: children
                  --> main.hop (line 8, col 6)
                7 | view Main {
                8 |     <Wrapper/>
                  |      ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_children_forwarded_transitively_through_rest() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Foo(children: Fragment) {
                    <div>{children}</div>
                }
                component Bar(...rest) {
                    <Foo ...rest/>
                }
                component Baz(...rest) {
                    <Bar ...rest/>
                }
                view Main {
                    <Baz>deep</Baz>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Foo(children: Fragment) {
                  <div>
                    {children}
                  </div>
                }

                component Bar(...rest) {
                  <Foo ...rest/>
                }

                component Baz(...rest) {
                  <Bar ...rest/>
                }

                view Main() {
                  <let {
                    v_0 = {
                      deep
                    }
                  }>
                    <Baz children={v_0}/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_children_content_when_callee_children_consumed_by_inner_body() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Foo(children: Fragment, class: String, ...rest) {
                    <div class={class} ...rest>{children}</div>
                }
                component Card(...rest) {
                    <Foo ...rest>inner</Foo>
                }
                view Main {
                    <Card class="a">hi</Card>
                }
            "#},
            expect![[r#"
                error: Component Card does not accept content (missing `children: Fragment` parameter)
                  --> main.hop (line 8, col 6)
                7 | view Main {
                8 |     <Card class="a">hi</Card>
                  |      ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_param_reserved_out_of_rest_when_callee_has_default() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Inner(class: String = "x", ...rest) {
                    <span class={class} ...rest></span>
                }
                component Outer(class: String, ...rest) {
                    <div class={class}>
                        <Inner ...rest/>
                    </div>
                }
                view Main {
                    <Outer class="x"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Inner(class: String, ...rest) {
                  <span class={class} ...rest></span>
                }

                component Outer(class: String, ...rest) {
                  <div class={class}>
                    <Inner class={"x"} ...rest/>
                  </div>
                }

                view Main() {
                  <Outer class={"x"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_intercept_and_merge_wrapper() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Foo(children: Fragment, class: String, ...rest) {
                    <div class={class} ...rest>
                        {children}
                    </div>
                }
                component Button(children: Fragment, class: String = "", ...rest) {
                    <Foo class={class} ...rest>
                        {children}
                    </Foo>
                }
                view Main {
                    <Button class="primary">click</Button>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Foo(children: Fragment, class: String, ...rest) {
                  <div class={class} ...rest>
                    {children}
                  </div>
                }

                component Button(children: Fragment, class: String, ...rest) {
                  <let {
                    v_0 = {
                      {children}
                    }
                  }>
                    <Foo children={v_0} class={class} ...rest/>
                  </let>
                }

                view Main() {
                  <let {
                    v_1 = {
                      click
                    }
                  }>
                    <Button children={v_1} class={"primary"}/>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_optional_param_forwarded_and_overridden_through_rest() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Inner(class: String = "x", ...rest) {
                    <span class={class} ...rest></span>
                }
                component Wrapper(...rest) {
                    <Inner ...rest/>
                }
                view Main {
                    <Wrapper class="y"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Inner(class: String, ...rest) {
                  <span class={class} ...rest></span>
                }

                component Wrapper(...rest) {
                  <Inner ...rest/>
                }

                view Main() {
                  <Wrapper class={"y"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_optional_default_chain_with_caller_value() {
        accept(
            indoc! {r#"
                -- main.hop --
                component A(class: String = "", ...rest) {
                    <div class={class} ...rest/>
                }
                component B(class: String = "", ...rest) {
                    <A class={class} ...rest/>
                }
                view Main {
                    <B class="main"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component A(class: String, ...rest) {
                  <div class={class} ...rest></div>
                }

                component B(class: String, ...rest) {
                  <A class={class} ...rest/>
                }

                view Main() {
                  <B class={"main"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_optional_default_chain_uses_outer_default() {
        accept(
            indoc! {r#"
                -- main.hop --
                component A(class: String = "a", ...rest) {
                    <div class={class} ...rest/>
                }
                component B(class: String = "b", ...rest) {
                    <A class={class} ...rest/>
                }
                view Main {
                    <B/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component A(class: String, ...rest) {
                  <div class={class} ...rest></div>
                }

                component B(class: String, ...rest) {
                  <A class={class} ...rest/>
                }

                view Main() {
                  <B class={"b"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_forwarded_optional_default_through_rest() {
        accept(
            indoc! {r#"
                -- main.hop --
                component A(label: String = "x", ...rest) {
                    <span ...rest>{label}</span>
                }
                component B(...rest) {
                    <A ...rest/>
                }
                view Main {
                    <B/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component A(label: String, ...rest) {
                  <span ...rest>
                    {label}
                  </span>
                }

                component B(...rest) {
                  <A ...rest/>
                }

                view Main() {
                  <B label={"x"}/>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_forwarded_default_materialized_once_in_chain() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Leaf(label: String = "x", ...rest) {
                    <span ...rest>{label}</span>
                }
                component Mid(...rest) {
                    <Leaf ...rest/>
                }
                component Top(...rest) {
                    <Mid ...rest/>
                }
                view Main {
                    <Top/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Leaf(label: String, ...rest) {
                  <span ...rest>
                    {label}
                  </span>
                }

                component Mid(...rest) {
                  <Leaf ...rest/>
                }

                component Top(...rest) {
                  <Mid ...rest/>
                }

                view Main() {
                  <Top label={"x"}/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_transitive_duplicate_forwarded_attr() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Inner(...rest) {
                    <span ...rest></span>
                }
                component Wrapper(...rest) {
                    <Inner title="a" ...rest/>
                }
                view Main {
                    <Wrapper title="b"/>
                }
            "#},
            expect![[r#"
                error: Component `Wrapper` does not accept attribute `title`
                  --> main.hop (line 8, col 14)
                7 | view Main {
                8 |     <Wrapper title="b"/>
                  |              ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_transitive_duplicate_forwarded_data_attr() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Inner(...rest) {
                    <span ...rest></span>
                }
                component Wrapper(...rest) {
                    <Inner data-foo="a" ...rest/>
                }
                view Main {
                    <Wrapper data-foo="b"/>
                }
            "#},
            expect![[r#"
                error: Component `Wrapper` does not accept attribute `data-foo`
                  --> main.hop (line 8, col 14)
                7 | view Main {
                8 |     <Wrapper data-foo="b"/>
                  |              ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_forwarded_attr_distinct_from_pinned() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Inner(...rest) {
                    <span ...rest></span>
                }
                component Wrapper(...rest) {
                    <Inner title="a" ...rest/>
                }
                view Main {
                    <Wrapper lang="en"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Inner(...rest) {
                  <span ...rest></span>
                }

                component Wrapper(...rest) {
                  <Inner title="a" ...rest/>
                }

                view Main() {
                  <Wrapper lang="en"/>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_multihop_duplicate_forwarded_attr() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Inner(...rest) {
                    <span ...rest></span>
                }
                component Mid(...rest) {
                    <Inner title="a" ...rest/>
                }
                component Outer(...rest) {
                    <Mid ...rest/>
                }
                view Main {
                    <Outer title="b"/>
                }
            "#},
            expect![[r#"
                error: Component `Outer` does not accept attribute `title`
                  --> main.hop (line 11, col 12)
                10 | view Main {
                11 |     <Outer title="b"/>
                   |            ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_param_named_like_html_attr_routed_to_param_not_tail() {
        reject(
            indoc! {r#"
                -- main.hop --
                component Box(tabindex: Int, ...rest) {
                    <div ...rest>
                        <if {tabindex > 0}>focusable</if>
                    </div>
                }
                component Wrapper(...rest) {
                    <Box ...rest/>
                }
                view Main {
                    <Wrapper tabindex="nope"/>
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `Int` got `String`
                  --> main.hop (line 10, col 23)
                 9 | view Main {
                10 |     <Wrapper tabindex="nope"/>
                   |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_param_named_like_html_attr_alongside_tail_attr() {
        accept(
            indoc! {r#"
                -- main.hop --
                component Box(tabindex: Int, ...rest) {
                    <div ...rest>
                        <if {tabindex > 0}>focusable</if>
                    </div>
                }
                component Wrapper(...rest) {
                    <Box ...rest/>
                }
                view Main {
                    <Wrapper tabindex={2} data-x="y"/>
                }
            "#},
            expect![[r#"
                -- main.hop --
                component Box(tabindex: Int, ...rest) {
                  <div ...rest>
                    <if {(tabindex > 0)}>
                      focusable
                    </if>
                  </div>
                }

                component Wrapper(...rest) {
                  <Box ...rest/>
                }

                view Main() {
                  <Wrapper tabindex={2} data-x="y"/>
                }
            "#]],
        );
    }
}
