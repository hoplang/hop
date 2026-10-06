use super::{Type, TypedExpr};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::CheapString;
use crate::hop::parsing::{ParsedAttribute, ParsedExpr, ParsedMarkup};
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::type_error::{TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hop::typing::type_registry::TypeRegistry;
use crate::hop::typing::typecheck_call::{Argument, CallArguments, NamedArgument, typecheck_call};
use crate::hop::typing::typecheck_expr::typecheck_expr;
use crate::hop::typing::variable_scope::VariableScope;
use crate::hop::typing::{TypedAttribute, TypedAttrs};
use crate::hover_annotation::HoverAnnotation;
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::var_name::VarName;

pub fn typecheck_markup(
    markup: &ParsedMarkup,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedExpr> {
    match markup {
        ParsedMarkup::Fragment { children, range: _ } => {
            let typed_children = children
                .iter()
                .filter_map(|child| {
                    typecheck_markup(
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
                parts: typed_children,
            })
        }

        ParsedMarkup::Call {
            function_name,
            function_name_opening_range,
            function_name_closing_range,
            attributes,
            children,
            range: _,
        } => {
            let mut arguments: Vec<NamedArgument<'_>> = Vec::new();
            let mut spread = None;
            for attribute in attributes {
                let (name, name_range, argument) = match attribute {
                    ParsedAttribute::Expression {
                        name,
                        name_range,
                        value,
                    } => (name, name_range, Argument::Expression(value)),
                    ParsedAttribute::String {
                        name,
                        name_range,
                        value,
                        quoted_range,
                    } => (
                        name,
                        name_range,
                        Argument::Text(
                            value
                                .cook(&mut |ch, range| {
                                    errors.push(TypeError::new(
                                        TypeErrorKind::InvalidEscapeSequence { ch },
                                        range,
                                    ));
                                })
                                .unwrap_or_else(|| CheapString::new(String::new())),
                            quoted_range.clone(),
                        ),
                    ),
                    ParsedAttribute::KeyOnly { name, name_range } => {
                        (name, name_range, Argument::Bare(name_range.clone()))
                    }
                    ParsedAttribute::Spread { name, .. } => {
                        spread = Some(name.clone());
                        continue;
                    }
                };
                arguments.push(NamedArgument {
                    name: name.clone(),
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
                    name: AttributeName::new(CheapString::new("children".to_string()))
                        .expect("children is an attribute name"),
                    range: range.clone(),
                    argument: Argument::Content(content, range),
                });
            }

            let call = typecheck_call(
                function_name,
                function_name_opening_range,
                function_name_opening_range,
                CallArguments::Named { arguments, spread },
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            );

            // The end tag names the function as much as the start tag does,
            // so it links to the declaration even when the call fails.
            if let Some(closing_range) = function_name_closing_range
                && type_env.functions.contains_key(function_name.as_str())
            {
                definition_links.push(DefinitionLink {
                    use_range: closing_range.clone(),
                    definition_range: type_env.names[function_name.as_str()]
                        .definition_range
                        .clone(),
                });
            }
            let call = call?;

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

        ParsedMarkup::Element {
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
                    typecheck_markup(
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

            Some(TypedExpr::Element {
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
                    parts: typed_children,
                }),
            })
        }

        ParsedMarkup::Interpolation {
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

        ParsedMarkup::Text { range } => Some(TypedExpr::HtmlRaw {
            value: range.to_cheap_string(),
        }),

        ParsedMarkup::Newline { .. } => Some(TypedExpr::HtmlRaw {
            value: CheapString::new(" ".to_string()),
        }),
    }
}

/// Check the expression `value` of the attribute `name` on `element`, which
/// is written on the element or reaches it through a rest.
pub fn typecheck_attribute_value(
    element: &HtmlElementKind,
    name: &AttributeName,
    value: &ParsedExpr,
    forwarded_params: &[VarName],
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Option<TypedExpr> {
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
                attr: name.to_string(),
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
    let expected = if element.is_boolean_attribute(name.as_str()) {
        Type::Bool
    } else {
        Type::String
    };
    let found = typed_expr.typ();
    if found != expected {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                context: TypeMismatchContext::Attribute,
                expected,
                found,
            },
            value.range().clone(),
        ));
    }
    Some(typed_expr)
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

/// Type-check a single HTML attribute: validate the name, and check the value
/// against the attribute's type, `Bool` for a boolean attribute and `String`
/// for any other.
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
    let (name, name_range) = (attribute.name()?, attribute.name_range()?);
    if !element.accepts_attribute(name.as_str()) {
        errors.push(TypeError::new(
            TypeErrorKind::ElementDoesNotAcceptAttribute {
                element: element.as_str().to_string(),
                attr: name.as_str().to_string(),
            },
            name_range.clone(),
        ));
        return None;
    }
    if let ParsedAttribute::String { quoted_range, .. } = attribute {
        if element.is_boolean_attribute(name.as_str()) {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    context: TypeMismatchContext::Attribute,
                    expected: Type::Bool,
                    found: Type::String,
                },
                quoted_range.clone(),
            ));
            return None;
        }
    }

    let value = match attribute {
        ParsedAttribute::Expression { value, .. } => typecheck_attribute_value(
            element,
            name,
            value,
            forwarded_params,
            registry,
            errors,
            var_env,
            type_env,
            annotations,
            definition_links,
            asset_references,
        )?,
        ParsedAttribute::String { value, .. } => TypedExpr::StringLiteral {
            value: value
                .cook(&mut |ch, range| {
                    errors.push(TypeError::new(
                        TypeErrorKind::InvalidEscapeSequence { ch },
                        range,
                    ));
                })
                .unwrap_or_else(|| CheapString::new(String::new())),
        },
        ParsedAttribute::KeyOnly { .. } => {
            errors.push(TypeError::new(
                TypeErrorKind::AttributeWithoutValue {
                    attr: name.to_string(),
                    expected: if element.is_boolean_attribute(name.as_str()) {
                        Type::Bool
                    } else {
                        Type::String
                    },
                },
                name_range.clone(),
            ));
            return None;
        }
        // A spread has no name, so it returned above.
        ParsedAttribute::Spread { .. } => return None,
    };

    Some(TypedAttribute {
        name: name.clone(),
        value,
    })
}
