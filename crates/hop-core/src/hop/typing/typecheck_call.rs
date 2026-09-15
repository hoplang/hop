use super::type_env::{ParamEntry, TypeEnv};
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typed_expr::TypedExpr;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::DocumentRange;
use crate::hop::parsing::ParsedExpr;
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};

/// An argument supplied for a parameter at a call site.
pub enum Argument<'a> {
    /// Written at the call site as an expression, still to be checked
    /// against the parameter.
    Expression(&'a ParsedExpr),
    /// Written at the call site in shorthand rather than as an expression: an
    /// attribute's quoted text, or a bare key meaning `true`.
    Desugared(TypedExpr, DocumentRange),
    /// Supplied by the call site itself: the content between a tag's opening
    /// and closing, or a parameter the caller's rest carries.
    Implied(TypedExpr),
}

/// Check the arguments supplied for `callee` against its parameters.
///
/// Every supplied name must be a parameter, the caller reports the ones that
/// are not. Expression arguments are checked in the order they were supplied.
/// Returns the arguments in parameter order with defaults filled in, or None
/// once an argument was missing or mistyped.
pub fn typecheck_call_arguments(
    callee: &FunctionName,
    call_range: &DocumentRange,
    params: &[ParamEntry],
    supplied: Vec<(VarName, Argument<'_>)>,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<Vec<(VarName, TypedExpr)>> {
    let missing: Vec<&str> = params
        .iter()
        .filter(|param| {
            param.default.is_none() && !supplied.iter().any(|(name, _)| *name == param.name)
        })
        .map(|param| param.name.as_str())
        .collect();
    let mut failed = !missing.is_empty();
    if failed {
        errors.push(TypeError::new(
            TypeErrorKind::MissingArguments {
                name: callee.clone(),
                args: missing.join(", "),
            },
            call_range.clone(),
        ));
    }

    let mut typed: Vec<(VarName, TypedExpr)> = Vec::with_capacity(params.len());
    for (name, argument) in supplied {
        let param = params
            .iter()
            .find(|param| param.name == name)
            .expect("the caller reports arguments that name no parameter");
        let (value, range) = match argument {
            Argument::Implied(value) => {
                typed.push((name, value));
                continue;
            }
            Argument::Desugared(value, range) => (value, range),
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
                TypeErrorKind::FunctionArgumentTypeMismatch {
                    name: callee.clone(),
                    param_name: param.name.clone(),
                    expected: param.typ.clone(),
                    found,
                },
                range,
            ));
            failed = true;
            continue;
        }
        typed.push((name, value));
    }
    if failed {
        return None;
    }

    Some(
        params
            .iter()
            .filter_map(|param| {
                let value = match typed.iter().position(|(name, _)| *name == param.name) {
                    Some(index) => typed.swap_remove(index).1,
                    None => param.default.clone()?,
                };
                Some((param.name.clone(), value))
            })
            .collect(),
    )
}
