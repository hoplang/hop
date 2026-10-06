use super::{Type, TypedExpr};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::CheapString;
use crate::hop::parsing::{ParsedAttribute, ParsedExpr, ParsedNode};
use crate::hop::typing::type_env::{FunctionSignature, Tail, TypeEnv};
use crate::hop::typing::type_error::{TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hop::typing::type_registry::TypeRegistry;
use crate::hop::typing::typecheck_call::{Argument, CallArguments, NamedArgument, typecheck_call};
use crate::hop::typing::typecheck_expr::typecheck_expr;
use crate::hop::typing::variable_scope::VariableScope;
use crate::hop::typing::{TypedAttribute, TypedAttrs};
use crate::hover_annotation::HoverAnnotation;
use crate::html::HtmlElementKind;
use crate::symbols::var_name::VarName;

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

        ParsedNode::FunctionInvocation {
            function_name,
            function_name_opening_range,
            function_name_closing_range,
            attributes,
            children,
            range: _,
        } => {
            // The signature decides which attributes are arguments and which
            // the rest parameter collects. An undefined function has no
            // signature, and the call reports it, so every attribute is an
            // argument then.
            let signature = type_env.functions.get(function_name.as_str());
            let spread = attributes.iter().find_map(|attribute| match attribute {
                ParsedAttribute::Spread { name, .. } => Some(name.clone()),
                ParsedAttribute::KeyOnly { .. }
                | ParsedAttribute::Expression { .. }
                | ParsedAttribute::String { .. } => None,
            });

            let mut arguments: Vec<NamedArgument<'_>> = Vec::new();
            let mut rest_attributes: Vec<TypedAttribute> = Vec::new();
            for attribute in attributes {
                let Some(name_range) = attribute.name_range() else {
                    continue;
                };
                let name = name_range.as_str();

                // An attribute that names no parameter goes to the rest
                // parameter when the element it lands on accepts it. Any
                // other attribute is an argument, and the call rejects the
                // ones that name no parameter.
                let accepting_element = match signature {
                    Some(FunctionSignature {
                        params,
                        tail: Tail::Html { element, reserved },
                        ..
                    }) if !params.iter().any(|param| param.name.as_str() == name)
                        && element.accepts_attribute(name)
                        && !reserved.iter().any(|reserved| reserved.as_str() == name) =>
                    {
                        Some(element)
                    }
                    _ => None,
                };
                if let Some(element) = accepting_element {
                    let value = typecheck_attribute_value(
                        element,
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
                    rest_attributes.push(TypedAttribute {
                        name: name_range.to_cheap_string(),
                        value,
                    });
                    continue;
                }

                let argument = match attribute {
                    ParsedAttribute::Expression { value, .. } => Argument::Expression(value),
                    ParsedAttribute::String {
                        value,
                        quoted_range,
                        ..
                    } => Argument::Desugared(
                        TypedExpr::StringLiteral {
                            value: value
                                .cook(&mut |ch, range| {
                                    errors.push(TypeError::new(
                                        TypeErrorKind::InvalidEscapeSequence { ch },
                                        range,
                                    ));
                                })
                                .unwrap_or_else(|| CheapString::new(String::new())),
                        },
                        quoted_range.clone(),
                    ),
                    ParsedAttribute::KeyOnly { .. } => Argument::Desugared(
                        TypedExpr::BooleanLiteral { value: true },
                        name_range.clone(),
                    ),
                    // A spread has no attribute name, so the `name_range()`
                    // guard at the top of the loop skipped it long before here.
                    ParsedAttribute::Spread { .. } => {
                        unreachable!("a spread has no attribute name")
                    }
                };
                arguments.push(NamedArgument {
                    name: name_range.to_cheap_string(),
                    range: name_range.clone(),
                    argument,
                });
            }

            // A tag pair passes its content as `children`, even when the
            // content is empty, while a self-closing tag passes no `children`.
            // Empty content has no range of its own, so it is reported at the
            // end tag that passes it.
            if let Some(content) = children {
                let range = match (content.first(), content.last(), function_name_closing_range) {
                    (Some(first), Some(last), _) => first.range().clone().to(last.range().clone()),
                    (_, _, Some(closing_range)) => closing_range.clone(),
                    (_, _, None) => function_name_opening_range.clone(),
                };
                arguments.push(NamedArgument {
                    name: CheapString::new("children".to_string()),
                    range: range.clone(),
                    argument: Argument::Content(content, range),
                });
            }

            // A parameter the rest carries is read from the enclosing
            // function, whose signature was extended with it for exactly this
            // purpose.
            if let Some(signature) = signature
                && spread.is_some()
            {
                for param in &signature.params {
                    if forwarded_params.contains(&param.name)
                        && !arguments
                            .iter()
                            .any(|argument| argument.name.as_str() == param.name.as_str())
                    {
                        arguments.push(NamedArgument {
                            name: param.name.to_cheap_string(),
                            range: function_name_opening_range.clone(),
                            argument: Argument::Implied(TypedExpr::Var {
                                value: param.name.clone(),
                                typ: param.typ.clone(),
                            }),
                        });
                    }
                }
            }

            let call = typecheck_call(
                function_name,
                function_name_opening_range,
                function_name_opening_range,
                CallArguments::Named(arguments),
                TypedAttrs {
                    attributes: rest_attributes,
                    spread,
                },
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            )?;

            if let Some(closing_range) = function_name_closing_range {
                definition_links.push(DefinitionLink {
                    use_range: closing_range.clone(),
                    definition_range: type_env.names[function_name.as_str()]
                        .definition_range
                        .clone(),
                });
            }

            // A markup call is inserted like an interpolation of the call, so
            // the value is used as is when it is Html and escaped when it is a
            // String.
            let return_type = call.typ();
            match return_type {
                Type::Html => Some(call),
                Type::String => Some(TypedExpr::HtmlEscape {
                    expr: Box::new(call),
                }),
                _ => {
                    errors.push(TypeError::new(
                        TypeErrorKind::InterpolationTypeMismatch { found: return_type },
                        function_name_opening_range.clone(),
                    ));
                    None
                }
            }
        }

        ParsedNode::HtmlElement {
            kind: element,
            tag_name,
            closing_tag_name,
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

            // A void element is written as a single self-closing tag and any
            // other element with a start and an end tag, as HTML reads them.
            // An SVG element is the exception: HTML honors a self-closing tag
            // on it, so both forms are accepted.
            match closing_tag_name {
                Some(closing_tag_name) if element.is_void() => {
                    errors.push(TypeError::new(
                        TypeErrorKind::VoidElementWithEndTag {
                            tag: tag_name.to_cheap_string(),
                        },
                        closing_tag_name.clone(),
                    ));
                }
                None if !element.is_void() && !element.is_svg() => {
                    errors.push(TypeError::new(
                        TypeErrorKind::NonVoidElementSelfClosing {
                            tag: tag_name.to_cheap_string(),
                        },
                        tag_name.clone(),
                    ));
                }
                _ => {}
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
                attrs: TypedAttrs {
                    attributes: typed_attributes,
                    spread: attributes.iter().find_map(|a| match a {
                        ParsedAttribute::Spread { name, .. } => Some(name.clone()),
                        ParsedAttribute::KeyOnly { .. }
                        | ParsedAttribute::Expression { .. }
                        | ParsedAttribute::String { .. } => None,
                    }),
                },
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

        ParsedNode::Text { range } => Some(TypedExpr::HtmlRaw {
            value: range.to_cheap_string(),
        }),

        ParsedNode::Newline { .. } => Some(TypedExpr::HtmlRaw {
            value: CheapString::new(" ".to_string()),
        }),
    }
}

fn typecheck_attribute_value(
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
) -> Option<TypedExpr> {
    match attribute {
        ParsedAttribute::Expression { name, value } => {
            // These attributes load script or documents, or animate other
            // attributes. Escaping does not make a value safe there, so their
            // value must be written in the source.
            let attr = name.as_str().to_ascii_lowercase();
            let literal_only = match element {
                HtmlElementKind::Script => attr == "src",
                HtmlElementKind::Iframe => attr == "srcdoc",
                HtmlElementKind::Animate | HtmlElementKind::Set => {
                    matches!(
                        attr.as_str(),
                        "attributename" | "by" | "from" | "to" | "values"
                    )
                }
                _ => false,
            };
            if literal_only && !matches!(value, ParsedExpr::StringLiteral { .. }) {
                errors.push(TypeError::new(
                    TypeErrorKind::AttributeRequiresStringLiteral {
                        element: element.as_str().to_string(),
                        attr: name.as_str().to_string(),
                    },
                    value.range().clone(),
                ));
            }
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
                    TypeErrorKind::TypeMismatch {
                        context: TypeMismatchContext::Attribute,
                        expected: Type::String,
                        found: typed_expr.typ(),
                    },
                    value.range().clone(),
                ));
            }
            Some(typed_expr)
        }
        ParsedAttribute::String { value, .. } => {
            let value = value
                .cook(&mut |ch, range| {
                    errors.push(TypeError::new(
                        TypeErrorKind::InvalidEscapeSequence { ch },
                        range,
                    ));
                })
                .unwrap_or_else(|| CheapString::new(String::new()));
            Some(TypedExpr::StringLiteral { value })
        }
        ParsedAttribute::KeyOnly { .. } | ParsedAttribute::Spread { .. } => None,
    }
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
        element,
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
