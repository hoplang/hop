use std::borrow::Cow;

use super::{ParamEntry, Tail, Type, TypedExpr};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::parsed_node::{ParsedAttribute, ParsedLoopSource, ParsedNode};
use crate::hop::patterns::Match;
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::type_registry::TypeRegistry;
use crate::hop::typing::typecheck_call::{Argument, typecheck_call_arguments};
use crate::hop::typing::typecheck_expr::typecheck_expr;
use crate::hop::typing::typecheck_match::{MatchArms, typecheck_match};
use crate::hop::typing::variable_scope::VariableScope;
use crate::hop::typing::{TypedAttribute, TypedAttributeValue, TypedLoopSource};
use crate::hover_annotation::HoverAnnotation;
use crate::html::HtmlElementKind;
use crate::symbols::function_name::FunctionName;
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

            Some(TypedExpr::HtmlConcat {
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
                    true_body: Box::new(TypedExpr::HtmlConcat {
                        nodes: typed_children,
                    }),
                    false_body: Box::new(TypedExpr::HtmlConcat { nodes: Vec::new() }),
                },
                typ: Type::Html,
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
                body: Box::new(TypedExpr::HtmlConcat {
                    nodes: typed_children,
                }),
                typ: Type::Html,
            })
        }

        ParsedNode::FunctionInvocation {
            function_name,
            function_name_opening_range,
            function_name_closing_range,
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

            let Some(signature) = type_env.functions.get(function_name.as_str()) else {
                errors.push(TypeError::new(
                    TypeErrorKind::UndefinedFunction {
                        name: function_name.clone(),
                    },
                    function_name_opening_range.clone(),
                ));
                return None;
            };
            if signature.return_type != Type::Html {
                errors.push(TypeError::new(
                    TypeErrorKind::FunctionTagReturnTypeMismatch {
                        name: function_name.clone(),
                        found: signature.return_type.clone(),
                    },
                    function_name_opening_range.clone(),
                ));
                return None;
            }
            let callee_rest_param = signature.rest_param.clone();
            let callee_params = signature.params.clone();
            let callee_tail = signature.tail.clone();
            let function_def_range = type_env.names[function_name.as_str()]
                .definition_range
                .clone();

            let callee_module = function_def_range.document_id().clone();

            // Add definition link for the opening tag
            definition_links.push(DefinitionLink {
                use_range: function_name_opening_range.clone(),
                definition_range: function_def_range.clone(),
            });

            // Add definition link for the closing tag if present
            if let Some(closing_range) = function_name_closing_range {
                definition_links.push(DefinitionLink {
                    use_range: closing_range.clone(),
                    definition_range: function_def_range,
                });
            }

            let (resolved_args, extra_attributes, rest_spread) = typecheck_arguments(
                attributes,
                &callee_params,
                &callee_tail,
                children.is_some().then_some(typed_children),
                function_name,
                function_name_opening_range,
                forwarded_params,
                registry,
                errors,
                var_env,
                type_env,
                annotations,
                definition_links,
                asset_references,
            )?;

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
                        function_name.as_str()
                    );
                }
            }

            Some(TypedExpr::FunctionCall {
                function_name: function_name.clone(),
                module: callee_module,
                args,
                typ: Type::Html,
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

            Some(TypedExpr::HtmlElement {
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
                children: Box::new(TypedExpr::HtmlConcat {
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
                    Type::Html => Some(typed_expr),
                    Type::String => Some(TypedExpr::HtmlEscape {
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

        ParsedNode::Text { range } => Some(TypedExpr::HtmlRaw {
            value: range.to_cheap_string(),
        }),

        ParsedNode::Newline { .. } => Some(TypedExpr::HtmlRaw {
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
                    TypeErrorKind::AttributeTypeMismatch {
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
    function_name: &FunctionName,
    function_name_opening_range: &DocumentRange,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<(
    Vec<(VarName, TypedExpr)>,
    Vec<TypedAttribute>,
    Option<VarName>,
)> {
    let has_body = children.is_some();
    let children_param = callee_params
        .iter()
        .find(|p| p.name.as_str() == "children" && p.typ == Type::Html);
    let has_explicit_children_arg = attributes.iter().any(|a| {
        a.name_range()
            .is_some_and(|name| name.as_str() == "children")
    });
    let synthesize_children_arg =
        has_body && children_param.is_some() && !has_explicit_children_arg;

    if has_body && children_param.is_none() {
        errors.push(TypeError::new(
            TypeErrorKind::FunctionDoesNotAcceptChildren {
                name: function_name.clone(),
            },
            function_name_opening_range.clone(),
        ));
    }

    let rest_spread = attributes.iter().find_map(|a| match a {
        ParsedAttribute::Spread { name, .. } => Some(name.clone()),
        ParsedAttribute::KeyOnly { .. }
        | ParsedAttribute::Expression { .. }
        | ParsedAttribute::String { .. } => None,
    });

    let mut supplied: Vec<(VarName, Argument<'_>)> = Vec::new();
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
                    TypeErrorKind::FunctionDoesNotAcceptAttribute {
                        name: function_name.clone(),
                        attr: arg_name.to_string(),
                    },
                    arg_name_range.clone(),
                ));
            }
            continue;
        };

        let value = match arg {
            ParsedAttribute::Expression { value, .. } => Cow::Borrowed(value),
            ParsedAttribute::String {
                content,
                quoted_range,
                ..
            } => Cow::Owned(ParsedExpr::StringLiteral {
                value: content
                    .as_ref()
                    .map(|r| r.to_cheap_string())
                    .unwrap_or_else(|| CheapString::new(String::new())),
                range: quoted_range.clone(),
            }),
            ParsedAttribute::KeyOnly { .. } | ParsedAttribute::Spread { .. } => {
                Cow::Owned(ParsedExpr::BooleanLiteral {
                    value: true,
                    range: arg_name_range.clone(),
                })
            }
        };
        supplied.push((param.name.clone(), Argument::Written(value)));
    }

    if has_body && has_explicit_children_arg {
        errors.push(TypeError::new(
            TypeErrorKind::ChildContentAmbiguous {},
            function_name_opening_range.clone(),
        ));
    }

    if synthesize_children_arg {
        supplied.push((
            VarName::new("children").unwrap(),
            Argument::Implied(TypedExpr::HtmlConcat {
                nodes: children.unwrap_or_default(),
            }),
        ));
    }

    // A parameter the rest carries is read from the enclosing function, whose
    // signature was extended with it for exactly this purpose.
    if rest_spread.is_some() {
        for param in callee_params {
            if forwarded_params.contains(&param.name)
                && !supplied.iter().any(|(name, _)| *name == param.name)
            {
                supplied.push((
                    param.name.clone(),
                    Argument::Implied(TypedExpr::Var {
                        value: param.name.clone(),
                        typ: param.typ.clone(),
                    }),
                ));
            }
        }
    }

    let args = typecheck_call_arguments(
        function_name,
        function_name_opening_range,
        callee_params,
        supplied,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    )?;
    Some((args, extra_attributes, rest_spread))
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
