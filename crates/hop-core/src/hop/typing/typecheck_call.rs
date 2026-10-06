use super::type_env::{ParamEntry, Tail, TypeEnv};
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typecheck_markup::typecheck_attribute_value;
use super::typed_expr::TypedExpr;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::ParsedExpr;
use crate::hop::typing::type_error::{Target, TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hop::typing::{Type, TypedAttribute, TypedAttrs};
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;

/// The value supplied for a parameter or an attribute at a call site.
pub enum Argument<'a> {
    /// Written at the call site as an expression, still to be checked
    /// against the parameter.
    Expression(&'a ParsedExpr),
    /// An attribute's quoted text, cooked.
    Text(CheapString, DocumentRange),
    /// An attribute written as its name alone, which is a compile error. It is
    /// kept until the parameter or attribute it names is known, so the error
    /// can show a value of the right type.
    Bare(DocumentRange),
    /// Supplied by the call site itself: a parameter the caller's rest
    /// carries.
    Implied(TypedExpr),
}

/// An argument and the parameter it names.
pub struct NamedArgument<'a> {
    /// The name as written, which need not be a parameter of the callee.
    pub name: AttributeName,
    /// Where the argument was written, for the errors that concern its name.
    pub range: DocumentRange,
    pub argument: Argument<'a>,
}

/// The arguments of a call, as written.
pub enum CallArguments<'a> {
    Positional(&'a [ParsedExpr]),
    Named {
        arguments: Vec<NamedArgument<'a>>,
        /// The caller's own rest, spread into the call.
        spread: Option<VarName>,
    },
}

/// Check a call of `callee` and build the typed call.
///
/// A call expression and a markup call both end up here, the markup call with
/// its attributes as named arguments and its content as `children`, so the two
/// forms are checked by the same code. A named argument that names no
/// parameter is an attribute for the callee's rest, when the element the rest
/// lands on accepts it.
///
/// Expression arguments are checked in the order they were supplied. Returns
/// None once anything about the call failed, after reporting it.
pub fn typecheck_call(
    callee: &FunctionName,
    name_range: &DocumentRange,
    call_range: &DocumentRange,
    arguments: CallArguments<'_>,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let Some(signature) = type_env.functions.get(callee.as_str()) else {
        errors.push(TypeError::new(
            TypeErrorKind::UndefinedFunction {
                name: callee.clone(),
            },
            name_range.clone(),
        ));
        return None;
    };
    let definition_range = type_env.names[callee.as_str()].definition_range.clone();
    definition_links.push(DefinitionLink {
        use_range: name_range.clone(),
        definition_range: definition_range.clone(),
    });
    let module = definition_range.document_id().clone();
    // A name reaches the parameters the callee's rest carries as well as the
    // declared ones, while a position reaches only the declared ones.
    let params: Vec<&ParamEntry> = signature
        .params
        .iter()
        .chain(&signature.forwarded)
        .collect();

    let mut failed = false;
    let mut rest_attributes: Vec<TypedAttribute> = Vec::new();
    let mut rest_spread: Option<VarName> = None;
    let supplied: Vec<(&ParamEntry, Argument<'_>)> = match arguments {
        CallArguments::Positional(values) => {
            let declared = &signature.params;
            let required = declared
                .iter()
                .rposition(|param| param.fallback.is_none())
                .map_or(0, |index| index + 1);
            // Too few arguments leave out parameters, which is reported below
            // as for a call by name, so `F()` fails like `<F/>`.
            if values.len() > declared.len() {
                errors.push(TypeError::new(
                    TypeErrorKind::FunctionArgumentCountMismatch {
                        name: callee.clone(),
                        expected: if required == declared.len() {
                            required.to_string()
                        } else {
                            format!("{required} to {}", declared.len())
                        },
                        found: values.len(),
                    },
                    call_range.clone(),
                ));
                return None;
            }
            declared
                .iter()
                .zip(values)
                .map(|(param, value)| (param, Argument::Expression(value)))
                .collect()
        }
        CallArguments::Named {
            arguments: named,
            spread,
        } => {
            let mut supplied: Vec<(&ParamEntry, Argument<'_>)> = Vec::with_capacity(named.len());
            let mut written: Vec<AttributeName> = Vec::with_capacity(named.len());
            for NamedArgument {
                name,
                range,
                argument,
            } in named
            {
                // A name is written once, whether it names a parameter or an
                // attribute for the rest.
                if written.contains(&name) {
                    errors.push(TypeError::new(
                        TypeErrorKind::DuplicateName {
                            target: Target::Function(callee.clone()),
                            name,
                        },
                        range,
                    ));
                    failed = true;
                    continue;
                }
                written.push(name.clone());
                // Names are compared ignoring case, here as for duplicates
                // and attributes, so `Title` passes the parameter `title`.
                let param = params
                    .iter()
                    .copied()
                    .find(|param| param.name.as_str().eq_ignore_ascii_case(name.as_str()));
                // An argument that names no parameter goes to the rest when
                // the element the rest lands on accepts it, unless the site of
                // the spread already writes it.
                let accepting_element = match &signature.tail {
                    Tail::Html { element, reserved }
                        if param.is_none()
                            && element.accepts_attribute(name.as_str())
                            && !reserved.contains(&name) =>
                    {
                        Some(element)
                    }
                    _ => None,
                };
                match (param, accepting_element, argument) {
                    (None, Some(element), Argument::Expression(value)) => {
                        match typecheck_attribute_value(
                            element,
                            &name,
                            value,
                            forwarded_params,
                            registry,
                            errors,
                            var_env,
                            type_env,
                            annotations,
                            definition_links,
                            asset_references,
                        ) {
                            Some(value) => rest_attributes.push(TypedAttribute { name, value }),
                            None => failed = true,
                        }
                    }
                    (None, Some(element), Argument::Text(_, text_range))
                        if element.is_boolean_attribute(name.as_str()) =>
                    {
                        errors.push(TypeError::new(
                            TypeErrorKind::TypeMismatch {
                                context: TypeMismatchContext::Attribute,
                                expected: Type::Bool,
                                found: Type::String,
                            },
                            text_range,
                        ));
                        failed = true;
                    }
                    (None, Some(_), Argument::Text(value, _)) => {
                        rest_attributes.push(TypedAttribute {
                            name,
                            value: TypedExpr::StringLiteral { value },
                        });
                    }
                    (None, Some(element), Argument::Bare(bare_range)) => {
                        errors.push(TypeError::new(
                            TypeErrorKind::AttributeWithoutValue {
                                attr: name.to_string(),
                                expected: if element.is_boolean_attribute(name.as_str()) {
                                    Type::Bool
                                } else {
                                    Type::String
                                },
                            },
                            bare_range,
                        ));
                        failed = true;
                    }
                    (None, _, _) => {
                        errors.push(TypeError::new(
                            TypeErrorKind::DoesNotAccept {
                                target: Target::Function(callee.clone()),
                                name,
                            },
                            range,
                        ));
                        failed = true;
                    }
                    (Some(param), _, argument) => supplied.push((param, argument)),
                }
            }

            // A parameter the rest carries is read from the enclosing
            // function, whose signature carries it for exactly this purpose.
            if spread.is_some() {
                for &param in &params {
                    if forwarded_params.contains(&param.name)
                        && !supplied.iter().any(|(p, _)| p.name == param.name)
                    {
                        supplied.push((
                            param,
                            Argument::Implied(TypedExpr::Var {
                                value: param.name.clone(),
                                typ: param.typ.clone(),
                            }),
                        ));
                    }
                }
            }
            rest_spread = spread;
            supplied
        }
    };

    let missing: Vec<&str> = params
        .iter()
        .filter(|param| {
            param.fallback.is_none() && !supplied.iter().any(|(p, _)| p.name == param.name)
        })
        .map(|param| param.name.as_str())
        .collect();
    if !missing.is_empty() {
        errors.push(TypeError::new(
            TypeErrorKind::MissingArguments {
                name: callee.clone(),
                args: missing.join(", "),
            },
            call_range.clone(),
        ));
        failed = true;
    }

    let mut typed: Vec<(VarName, TypedExpr)> = Vec::with_capacity(params.len());
    for (param, argument) in supplied {
        let (value, range) = match argument {
            Argument::Implied(value) => {
                typed.push((param.name.clone(), value));
                continue;
            }
            Argument::Text(value, range) => (TypedExpr::StringLiteral { value }, range),
            // A name alone was supplied, so the parameter is not reported as
            // missing, but it has no value.
            Argument::Bare(range) => {
                errors.push(TypeError::new(
                    TypeErrorKind::AttributeWithoutValue {
                        attr: param.name.as_str().to_string(),
                        expected: param.typ.clone(),
                    },
                    range,
                ));
                failed = true;
                continue;
            }
            Argument::Expression(expr) => {
                let Some(value) = typecheck_expr(
                    expr,
                    Some(&param.typ),
                    forwarded_params,
                    var_env,
                    type_env,
                    registry,
                    annotations,
                    definition_links,
                    asset_references,
                    errors,
                ) else {
                    failed = true;
                    continue;
                };
                (value, expr.range().clone())
            }
        };
        let found = value.typ();
        if found != param.typ {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    context: TypeMismatchContext::FunctionArgument,
                    expected: param.typ.clone(),
                    found,
                },
                range,
            ));
            failed = true;
            continue;
        }
        typed.push((param.name.clone(), value));
    }
    if failed {
        return None;
    }

    let args = params
        .iter()
        .filter_map(|param| {
            let value = match typed.iter().position(|(name, _)| *name == param.name) {
                Some(index) => typed.swap_remove(index).1,
                None => param.fallback.clone()?,
            };
            Some((param.name.clone(), value))
        })
        .collect();

    let rest = match signature.rest_param.clone() {
        Some(rest_param) => Some((
            rest_param,
            TypedAttrs {
                attributes: rest_attributes,
                spread: rest_spread,
            },
        )),
        None => {
            // A spread into a callee that declares no rest is not a mistake:
            // the spread was carrying typed parameters, and those are passed
            // explicitly above, so nothing is left for it to forward.
            assert!(
                rest_attributes.is_empty(),
                "{} declares no rest, but the call site supplies attributes for one",
                callee.as_str()
            );
            None
        }
    };

    Some(TypedExpr::Call {
        function_name: callee.clone(),
        module,
        args,
        rest,
        typ: signature.return_type.clone(),
    })
}
