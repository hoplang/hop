use super::type_env::{ParamEntry, TypeEnv};
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typecheck_markup::typecheck_markup;
use super::typed_expr::TypedExpr;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::{ParsedExpr, ParsedMarkup};
use crate::hop::typing::TypedAttrs;
use crate::hop::typing::type_error::{TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;

/// The value supplied for a parameter at a call site.
pub enum Argument<'a> {
    /// Written at the call site as an expression, still to be checked
    /// against the parameter.
    Expression(&'a ParsedExpr),
    /// Written at the call site in shorthand rather than as an expression: an
    /// attribute's quoted text, or a bare key meaning `true`.
    Desugared(TypedExpr, DocumentRange),
    /// The content between the tags of a markup call, which is the
    /// `children` argument as a fragment.
    Content(&'a [ParsedMarkup], DocumentRange),
    /// Supplied by the call site itself: a parameter the caller's rest
    /// carries.
    Implied(TypedExpr),
}

/// An argument and the parameter it names.
pub struct NamedArgument<'a> {
    /// The name as written, which need not be a parameter of the callee.
    pub name: CheapString,
    /// Where the argument was written, for the errors that concern its name.
    pub range: DocumentRange,
    pub argument: Argument<'a>,
}

/// The arguments of a call, as written.
pub enum CallArguments<'a> {
    Positional(&'a [ParsedExpr]),
    Named(Vec<NamedArgument<'a>>),
}

/// Check a call of `callee` and build the typed call.
///
/// A call expression and a markup call both end up here, the markup call with
/// its attributes as named arguments and its content as `children`, so the two
/// forms are checked by the same code. `rest` is what the call site supplies
/// for the callee's rest parameter, which only a markup call can write.
///
/// Expression arguments are checked in the order they were supplied. Returns
/// None once anything about the call failed, after reporting it.
pub fn typecheck_call(
    callee: &FunctionName,
    name_range: &DocumentRange,
    call_range: &DocumentRange,
    arguments: CallArguments<'_>,
    rest: TypedAttrs,
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
    let params = &signature.params;

    let mut failed = false;
    let supplied: Vec<(&ParamEntry, Argument<'_>)> = match arguments {
        CallArguments::Positional(values) => {
            let required = params
                .iter()
                .rposition(|param| param.default.is_none())
                .map_or(0, |index| index + 1);
            if values.len() < required || values.len() > params.len() {
                errors.push(TypeError::new(
                    TypeErrorKind::FunctionArgumentCountMismatch {
                        name: callee.clone(),
                        expected: if required == params.len() {
                            required.to_string()
                        } else {
                            format!("{required} to {}", params.len())
                        },
                        found: values.len(),
                    },
                    call_range.clone(),
                ));
                return None;
            }
            params
                .iter()
                .zip(values)
                .map(|(param, value)| (param, Argument::Expression(value)))
                .collect()
        }
        CallArguments::Named(named) => {
            let mut supplied: Vec<(&ParamEntry, Argument<'_>)> = Vec::with_capacity(named.len());
            for NamedArgument {
                name,
                range,
                argument,
            } in named
            {
                let param = params
                    .iter()
                    .find(|param| param.name.as_str() == name.as_str());
                match param {
                    None => {
                        errors.push(TypeError::new(
                            TypeErrorKind::FunctionDoesNotAcceptArgument {
                                name: callee.clone(),
                                argument: name.as_str().to_string(),
                            },
                            range,
                        ));
                        failed = true;
                    }
                    Some(param) if supplied.iter().any(|(p, _)| p.name == param.name) => {
                        errors.push(TypeError::new(
                            TypeErrorKind::DuplicateArgument {
                                argument: param.name.clone(),
                            },
                            range,
                        ));
                        failed = true;
                    }
                    Some(param) => supplied.push((param, argument)),
                }
            }
            supplied
        }
    };

    let missing: Vec<&str> = params
        .iter()
        .filter(|param| {
            param.default.is_none() && !supplied.iter().any(|(p, _)| p.name == param.name)
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
            Argument::Desugared(value, range) => (value, range),
            Argument::Content(content, range) => {
                let parts = content
                    .iter()
                    .filter_map(|markup| {
                        typecheck_markup(
                            markup,
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
                (TypedExpr::HtmlConcat { parts }, range)
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
                None => param.default.clone()?,
            };
            Some((param.name.clone(), value))
        })
        .collect();

    let rest = match signature.rest_param.clone() {
        Some(rest_param) => Some((rest_param, rest)),
        None => {
            // A spread into a callee that declares no rest is not a mistake:
            // the spread was carrying typed parameters, and those are passed
            // explicitly above, so nothing is left for it to forward.
            assert!(
                rest.attributes.is_empty(),
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
