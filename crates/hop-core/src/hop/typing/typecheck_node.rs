use super::{ParamEntry, Tail, Type, TypedExpr};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::parsed_node::{
    ParsedAttribute, ParsedLetBinding, ParsedLoopSource, ParsedNode,
};
use crate::hop::patterns::Match;
use crate::hop::typing::resolve_type::resolve_type;
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::type_registry::TypeRegistry;
use crate::hop::typing::typecheck_expr::typecheck_expr;
use crate::hop::typing::typecheck_match::{MatchArms, typecheck_match};
use crate::hop::typing::variable_scope::VariableScope;
use crate::hop::typing::{TypedAttribute, TypedAttributeValue, TypedLoopSource};
use crate::hover_annotation::HoverAnnotation;
use crate::html::HtmlElementKind;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};

pub fn typecheck_node(
    node: &ParsedNode,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedExpr> {
    match node {
        ParsedNode::Fragment { children, range: _ } => {
            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        forwarded_params,
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

            Some(TypedExpr::FragmentConcat {
                nodes: typed_children,
            })
        }

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
                        forwarded_params,
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

            let typed_condition = typecheck_expr(
                condition,
                None,
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            )?;

            let condition_type = typed_condition.typ();
            if condition_type != Type::Bool {
                errors.push(TypeError::new(
                    TypeErrorKind::ConditionTypeMismatch {
                        found: condition_type,
                    },
                    condition.range().clone(),
                ));
            }

            Some(TypedExpr::Match {
                match_: Match::Bool {
                    subject: Box::new(typed_condition),
                    true_body: Box::new(TypedExpr::FragmentConcat {
                        nodes: typed_children,
                    }),
                    false_body: Box::new(TypedExpr::FragmentConcat { nodes: Vec::new() }),
                },
                typ: Type::Fragment,
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
                    let typed_array = typecheck_expr(
                        array_expr,
                        None,
                        forwarded_params,
                        var_env,
                        type_env,
                        registry,
                        annotations,
                        definition_links,
                        asset_references,
                        errors,
                    )?;
                    let array_type = typed_array.typ();
                    let element_type = match &array_type {
                        Type::Array(inner) => inner.as_ref().clone(),
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
                    let typed_start = typecheck_expr(
                        start,
                        None,
                        forwarded_params,
                        var_env,
                        type_env,
                        registry,
                        annotations,
                        definition_links,
                        asset_references,
                        errors,
                    )?;
                    let typed_end = typecheck_expr(
                        end,
                        None,
                        forwarded_params,
                        var_env,
                        type_env,
                        registry,
                        annotations,
                        definition_links,
                        asset_references,
                        errors,
                    )?;

                    // Both bounds must be Int
                    let start_type = typed_start.typ();
                    if start_type != Type::Int {
                        errors.push(TypeError::new(
                            TypeErrorKind::RangeBoundTypeMismatch { found: start_type },
                            start.range().clone(),
                        ));
                    }
                    let end_type = typed_end.typ();
                    if end_type != Type::Int {
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
                        Type::Int,
                    )
                }
            };

            // Push the loop variable into scope (only if not discarded with _)
            let pushed = if let (Some(var_name), Some(var_name_range)) = (var_name, var_name_range)
            {
                match var_env.push(
                    var_name.clone(),
                    element_type.clone(),
                    var_name_range.clone(),
                ) {
                    Ok(_) => {
                        annotations.push(HoverAnnotation::TypeForVarName {
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
                        forwarded_params,
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
                let (name, entry) = var_env.pop();
                if !entry.accessed {
                    errors.push(TypeError::new(
                        TypeErrorKind::UnusedVariable { var_name: name },
                        entry.range,
                    ));
                }
            }

            Some(TypedExpr::For {
                var_name: var_name.clone(),
                source: Box::new(typed_source),
                body: Box::new(TypedExpr::FragmentConcat {
                    nodes: typed_children,
                }),
                typ: Type::Fragment,
            })
        }

        ParsedNode::Let {
            bindings, children, ..
        } => {
            // Count the bindings pushed to scope (for popping later)
            let mut pushed_bindings = 0;
            // Only store successfully typechecked bindings
            let mut typed_bindings: Vec<(&ParsedLetBinding, TypedExpr)> = Vec::new();

            for binding in bindings {
                // Resolve the declared type, if an annotation is present.
                let declared_type = match &binding.var_type {
                    Some(parsed_type) => {
                        let Some(t) =
                            resolve_type(parsed_type, &type_env.names, definition_links, errors)
                        else {
                            continue;
                        };
                        Some(t)
                    }
                    None => None,
                };

                let typed_value = typecheck_expr(
                    &binding.value_expr,
                    declared_type.as_ref(),
                    forwarded_params,
                    var_env,
                    type_env,
                    registry,
                    annotations,
                    definition_links,
                    asset_references,
                    errors,
                );

                let binding_type = match (&declared_type, &typed_value) {
                    (Some(declared), _) => Some(declared.clone()),
                    (None, Some(typed_value)) => Some(typed_value.typ()),
                    (None, None) => None,
                };

                if let Some(binding_type) = binding_type {
                    match var_env.push(
                        binding.var_name.clone(),
                        binding_type.clone(),
                        binding.var_name_range.clone(),
                    ) {
                        Ok(_) => {
                            annotations.push(HoverAnnotation::TypeForVarName {
                                range: binding.var_name_range.clone(),
                                typ: binding_type,
                                var_name: binding.var_name.clone(),
                            });
                            pushed_bindings += 1;
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

                let Some(typed_value) = typed_value else {
                    continue;
                };

                // Validate that the value type matches the declared type.
                if let Some(declared) = &declared_type {
                    let value_type = typed_value.typ();
                    if value_type != *declared {
                        errors.push(TypeError::new(
                            TypeErrorKind::LetBindingTypeMismatch {
                                expected: declared.clone(),
                                found: value_type,
                            },
                            binding.value_expr.range().clone(),
                        ));
                    }
                }

                typed_bindings.push((binding, typed_value));
            }

            // Type-check children with all variables in scope
            let typed_children: Vec<TypedExpr> = children
                .iter()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        forwarded_params,
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
            for _ in 0..pushed_bindings {
                let (name, entry) = var_env.pop();
                if !entry.accessed {
                    errors.push(TypeError::new(
                        TypeErrorKind::UnusedVariable { var_name: name },
                        entry.range,
                    ));
                }
            }

            // Build nested Let structure from innermost to outermost
            // Start with children, then wrap with each binding in reverse order
            let mut result = TypedExpr::FragmentConcat {
                nodes: typed_children,
            };
            for (binding, typed_value) in typed_bindings.into_iter().rev() {
                let typ = result.typ();
                result = TypedExpr::Let {
                    var: binding.var_name.clone(),
                    value: Box::new(typed_value),
                    body: Box::new(result),
                    typ,
                };
            }

            Some(result)
        }

        ParsedNode::ComponentInvocation {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            attributes,
            children,
            range: _,
        } => {
            let typed_children = children
                .iter()
                .flatten()
                .filter_map(|child| {
                    typecheck_node(
                        child,
                        forwarded_params,
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
            let (callee_rest_param, callee_params, callee_tail, component_def_range) = match (
                type_env.components.get(component_name),
                type_env.names.get(component_name),
            ) {
                (Some(sig), Some(name)) => (
                    sig.rest_param.clone(),
                    sig.params.clone(),
                    sig.tail.clone(),
                    name.definition_range.clone(),
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

            let (resolved_args, extra_attributes, rest_spread) = typecheck_arguments(
                attributes,
                &callee_params,
                &callee_tail,
                children.is_some().then_some(typed_children),
                component_name,
                component_name_opening_range,
                forwarded_params,
                registry,
                errors,
                var_env,
                type_env,
                annotations,
                definition_links,
                asset_references,
            );

            let mut args = resolved_args;

            match callee_rest_param {
                Some(rest_param) => {
                    args.push((rest_param, attrs_expr(extra_attributes, rest_spread)));
                }
                None => {
                    // A spread into a callee that declares no rest is not a
                    // mistake: the spread was carrying typed parameters, and
                    // those are passed explicitly above, so nothing is left
                    // for it to forward.
                    assert!(
                        extra_attributes.is_empty(),
                        "<{}> declares no rest, but the call site supplies attributes for one",
                        component_name.as_str()
                    );
                }
            }

            Some(TypedExpr::FunctionCall {
                function_name: component_name.clone().into(),
                args,
                typ: Type::Fragment,
            })
        }

        ParsedNode::HtmlElement {
            kind: element,
            tag_name,
            closing_tag_name: _,
            attributes,
            children,
            range: _,
        } => {
            let disallowed_tag = match element {
                HtmlElementKind::Head => Some("head"),
                HtmlElementKind::Body => Some("body"),
                HtmlElementKind::Html => Some("html"),
                _ => None,
            };
            if let Some(tag) = disallowed_tag {
                errors.push(TypeError::new(
                    TypeErrorKind::HtmlStructureTagNotAllowed { tag },
                    tag_name.clone(),
                ));
            }

            let typed_attributes = typecheck_attributes(
                attributes,
                element,
                forwarded_params,
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
                        forwarded_params,
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

            Some(TypedExpr::FragmentHtml {
                element: element.clone(),
                attrs: Box::new(attrs_expr(
                    typed_attributes,
                    attributes.iter().find_map(|a| match a {
                        ParsedAttribute::Spread { name, .. } => Some(name.clone()),
                        ParsedAttribute::KeyOnly { .. }
                        | ParsedAttribute::Expression { .. }
                        | ParsedAttribute::String { .. } => None,
                    }),
                )),
                children: Box::new(TypedExpr::FragmentConcat {
                    nodes: typed_children,
                }),
            })
        }

        ParsedNode::Interpolation {
            expression,
            range: _,
        } => {
            if let Some(typed_expr) = typecheck_expr(
                expression,
                None,
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            ) {
                let expr_type = typed_expr.typ();
                match expr_type {
                    Type::Fragment => Some(typed_expr),
                    Type::String => Some(TypedExpr::FragmentEscape {
                        expr: Box::new(typed_expr),
                    }),
                    _ => {
                        errors.push(TypeError::new(
                            TypeErrorKind::InterpolationTypeMismatch { found: expr_type },
                            expression.range().clone(),
                        ));
                        None
                    }
                }
            } else {
                None
            }
        }

        ParsedNode::Match { subject, cases, .. } => typecheck_match(
            subject,
            MatchArms::Cases(cases),
            forwarded_params,
            var_env,
            type_env,
            registry,
            annotations,
            definition_links,
            asset_references,
            errors,
        ),

        ParsedNode::Text { range } => Some(TypedExpr::FragmentRaw {
            value: range.to_cheap_string(),
        }),

        ParsedNode::Newline { .. } => Some(TypedExpr::FragmentRaw {
            value: CheapString::new(" ".to_string()),
        }),

        ParsedNode::Comment { .. } => None,
    }
}

fn typecheck_attribute_value(
    attribute: &ParsedAttribute,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedAttributeValue> {
    match attribute {
        ParsedAttribute::Expression { value, .. } => {
            let typed_expr = typecheck_expr(
                value,
                None,
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            )?;
            if typed_expr.typ() != Type::String {
                errors.push(TypeError::new(
                    TypeErrorKind::ArgumentTypeMismatch {
                        expected: Type::String,
                        found: typed_expr.typ(),
                    },
                    value.range().clone(),
                ));
            }
            Some(TypedAttributeValue::Expression(typed_expr))
        }
        ParsedAttribute::String { content, .. } => {
            let string_span = match content {
                Some(range) => range.to_cheap_string(),
                None => CheapString::new("".to_string()),
            };
            Some(TypedAttributeValue::String(string_span))
        }
        ParsedAttribute::KeyOnly { .. } | ParsedAttribute::Spread { .. } => None,
    }
}

fn attrs_expr(attributes: Vec<TypedAttribute>, spread: Option<VarName>) -> TypedExpr {
    let literal = TypedExpr::AttrsLiteral { attributes };
    match spread {
        Some(name) => TypedExpr::AttrsConcat {
            parts: vec![
                literal,
                TypedExpr::Var {
                    value: name,
                    typ: Type::Attrs,
                },
            ],
        },
        None => literal,
    }
}

fn typecheck_arguments(
    attributes: &[ParsedAttribute],
    callee_params: &[ParamEntry],
    callee_tail: &Tail,
    children: Option<Vec<TypedExpr>>,
    component_name: &TypeName,
    component_name_opening_range: &DocumentRange,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> (
    Vec<(VarName, TypedExpr)>,
    Vec<TypedAttribute>,
    Option<VarName>,
) {
    let has_body = children.is_some();
    let children_param = callee_params
        .iter()
        .find(|p| p.name.as_str() == "children" && p.typ == Type::Fragment);
    let has_explicit_children_arg = attributes.iter().any(|a| {
        a.name_range()
            .is_some_and(|name| name.as_str() == "children")
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

    let rest_spread = attributes.iter().find_map(|a| match a {
        ParsedAttribute::Spread { name, .. } => Some(name.clone()),
        ParsedAttribute::KeyOnly { .. }
        | ParsedAttribute::Expression { .. }
        | ParsedAttribute::String { .. } => None,
    });
    let mut supplied_args: Vec<VarName> = attributes
        .iter()
        .filter_map(|a| {
            a.name_range()
                .and_then(|name| VarName::new(name.as_str()).ok())
        })
        .collect();
    if has_body {
        supplied_args.push(VarName::new("children").unwrap());
    }
    let covered_by_rest = |param: &ParamEntry| {
        rest_spread.is_some()
            && !supplied_args.contains(&param.name)
            && forwarded_params.contains(&param.name)
    };

    let mut typed_args: Vec<(VarName, TypedExpr)> = Vec::new();
    let mut extra_attributes: Vec<TypedAttribute> = Vec::new();
    for arg in attributes {
        let Some(arg_name_range) = arg.name_range() else {
            continue;
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
                    arg,
                    forwarded_params,
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

        let arg_expr = match arg {
            ParsedAttribute::Expression { value, .. } => value.clone(),
            ParsedAttribute::String {
                content,
                quoted_range,
                ..
            } => {
                let value = content
                    .as_ref()
                    .map(|r| r.to_cheap_string())
                    .unwrap_or_else(|| CheapString::new(String::new()));
                ParsedExpr::StringLiteral {
                    value,
                    range: quoted_range.clone(),
                }
            }
            ParsedAttribute::KeyOnly { .. } | ParsedAttribute::Spread { .. } => {
                ParsedExpr::BooleanLiteral {
                    value: true,
                    range: arg_name_range.clone(),
                }
            }
        };

        let Some(typed_expr) = typecheck_expr(
            &arg_expr,
            Some(param_type),
            forwarded_params,
            var_env,
            type_env,
            registry,
            annotations,
            definition_links,
            asset_references,
            errors,
        ) else {
            continue;
        };
        let arg_type = typed_expr.typ();

        if arg_type != *param_type {
            errors.push(TypeError::new(
                TypeErrorKind::ArgumentTypeMismatch {
                    expected: param_type.clone(),
                    found: arg_type,
                },
                arg_expr.range().clone(),
            ));
            continue;
        }

        typed_args.push((VarName::new(arg_name).unwrap(), typed_expr));
    }

    if has_body && has_explicit_children_arg {
        errors.push(TypeError::new(
            TypeErrorKind::ChildContentAmbiguous {},
            component_name_opening_range.clone(),
        ));
    }

    if synthesize_children_arg {
        typed_args.push((
            VarName::new("children").unwrap(),
            TypedExpr::FragmentConcat {
                nodes: children.unwrap_or_default(),
            },
        ));
    }

    let missing_args: Vec<&str> = callee_params
        .iter()
        .filter(|p| {
            if p.default.is_some() {
                return false;
            }
            let supplied = attributes.iter().any(|a| {
                a.name_range()
                    .is_some_and(|n| n.as_str() == p.name.as_str())
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
    let resolved_args: Vec<(VarName, TypedExpr)> = callee_params
        .iter()
        .filter_map(|param| {
            if covered_by_rest(param) {
                // Read the value from the enclosing component, whose signature
                // was extended with this parameter for exactly this purpose.
                return Some((
                    param.name.clone(),
                    TypedExpr::Var {
                        value: param.name.clone(),
                        typ: param.typ.clone(),
                    },
                ));
            }
            typed_args
                .iter()
                .find(|(name, _)| name.as_str() == param.name.as_str())
                .map(|(_, value)| value.clone())
                .or_else(|| param.default.clone())
                .map(|value| (param.name.clone(), value))
        })
        .collect();

    (resolved_args, extra_attributes, rest_spread)
}

fn typecheck_attributes(
    attributes: &[ParsedAttribute],
    element: &HtmlElementKind,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Vec<TypedAttribute> {
    let mut typed_attributes = Vec::new();
    for attr in attributes {
        if let Some(typed) = typecheck_html_attribute(
            element,
            attr,
            forwarded_params,
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
    element: &HtmlElementKind,
    attribute: &ParsedAttribute,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedAttribute> {
    let name = attribute.name_range()?;
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
        attribute,
        forwarded_params,
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
