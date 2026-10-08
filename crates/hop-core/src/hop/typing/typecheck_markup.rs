use super::{Type, TypedExpr};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::CheapString;
use crate::hop::parsing::{ParsedAttribute, ParsedExpr, ParsedMarkup};
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::type_error::{TypeError, TypeErrorKind};
use crate::hop::typing::type_registry::TypeRegistry;
use crate::hop::typing::typecheck_call::{
    Argument, CallArguments, Callee, NamedArgument, typecheck_call,
};
use crate::hop::typing::typecheck_expr::typecheck_expr;
use crate::hop::typing::variable_scope::VariableScope;
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
            // A tag pair passes its content as the argument `children`, a
            // fragment, even when the content is empty, while a self-closing
            // tag passes no `children`. Empty content has no range of its own,
            // so it is reported at the end tag that passes it.
            let content = children.as_ref().map(|content| {
                let range = match (content.first(), content.last(), function_name_closing_range) {
                    (Some(first), Some(last), _) => first.range().clone().to(last.range().clone()),
                    (_, _, Some(closing_range)) => closing_range.clone(),
                    (_, _, None) => function_name_opening_range.clone(),
                };
                ParsedExpr::Markup {
                    markup: Box::new(ParsedMarkup::Fragment {
                        children: content.clone(),
                        range,
                    }),
                }
            });

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
                        Argument::Text(value, quoted_range.clone()),
                    ),
                    ParsedAttribute::KeyOnly { name, name_range } => {
                        (name, name_range, Argument::Bare(name_range.clone()))
                    }
                    // A second spread is reported with the rest it spreads, so
                    // the first is kept, as on an element and in a call
                    // expression.
                    ParsedAttribute::Spread { name, .. } => {
                        if spread.is_none() {
                            spread = Some(name.clone());
                        }
                        continue;
                    }
                };
                arguments.push(NamedArgument {
                    name: name.clone(),
                    range: name_range.clone(),
                    argument,
                });
            }

            if let Some(content) = &content {
                arguments.push(NamedArgument {
                    name: AttributeName::new(CheapString::new("children".to_string()))
                        .expect("children is an attribute name"),
                    range: content.range().clone(),
                    argument: Argument::Expression(content),
                });
            }

            let call = typecheck_call(
                Callee::Function {
                    name: function_name,
                    name_range: function_name_opening_range,
                    report_range: function_name_opening_range,
                },
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
                        Argument::Text(value, quoted_range.clone()),
                    ),
                    ParsedAttribute::KeyOnly { name, name_range } => {
                        (name, name_range, Argument::Bare(name_range.clone()))
                    }
                    // A second spread is reported with the rest it spreads, so
                    // the first is kept, as in a markup call and in a call
                    // expression.
                    ParsedAttribute::Spread { name, .. } => {
                        if spread.is_none() {
                            spread = Some(name.clone());
                        }
                        continue;
                    }
                };
                arguments.push(NamedArgument {
                    name: name.clone(),
                    range: name_range.clone(),
                    argument,
                });
            }

            typecheck_call(
                Callee::Element {
                    element,
                    content: TypedExpr::HtmlConcat {
                        parts: typed_children,
                    },
                },
                CallArguments::Named { arguments, spread },
                forwarded_params,
                var_env,
                type_env,
                registry,
                annotations,
                definition_links,
                asset_references,
                errors,
            )
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

        ParsedMarkup::Text { range } => Some(TypedExpr::HtmlText {
            value: range.to_cheap_string(),
        }),

        ParsedMarkup::Newline { .. } => Some(TypedExpr::HtmlText {
            value: CheapString::new(" ".to_string()),
        }),
    }
}
