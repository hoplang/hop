use std::collections::HashSet;

use super::resolve_type::{check_function_type, resolve_type};
use super::r#type::{FunctionType, Modifier, Type};
use super::type_env::TypeEnv;
use super::type_registry::TypeRegistry;
use super::typecheck_call::CallArguments;
use super::typecheck_expr::{typecheck_block, typecheck_expr};
use super::typed_expr::TypedExpr;
use super::variable_scope::{Context, Position, VariableScope};
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::DocumentRange;
use crate::hop::parsing::{ParsedBlock, ParsedLambdaParameter, ParsedModifier, ParsedType};
use crate::hop::typing::type_error::{TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;

/// Check a lambda. A parameter without a type takes the type of its
/// position in the expected parameters, of which there must be as many as
/// the lambda has. The body runs once for each application, so it is
/// checked as a body that runs any number of times.
pub fn typecheck_lambda(
    declared: Option<ParsedModifier>,
    params: &[ParsedLambdaParameter],
    return_type: Option<&ParsedType>,
    body: &ParsedBlock,
    range: &DocumentRange,
    expected: Option<&Type>,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    // A parameter without a type has no position to take it from when the
    // expected function type has another number of parameters.
    if let Some(Type::Function(function)) = expected
        && function.parameters.len() != params.len()
        && params.iter().any(|param| param.var_type.is_none())
    {
        errors.push(TypeError::new(
            TypeErrorKind::LambdaParameterCountMismatch {
                expected: Type::Function(function.clone()),
                found: params.len(),
            },
            range.clone(),
        ));
        return None;
    }
    let (expected_params, expected_result) = match expected {
        Some(Type::Function(function)) => (
            Some(function.parameters.as_slice()),
            Some(function.result.as_ref()),
        ),
        _ => (None, None),
    };
    let expected_params = expected_params.filter(|parameters| parameters.len() == params.len());

    let mut failed = false;
    let mut seen: HashSet<&VarName> = HashSet::new();
    let mut typed_params: Vec<(&ParsedLambdaParameter, Type)> = Vec::with_capacity(params.len());
    for (index, param) in params.iter().enumerate() {
        if let Some(name) = &param.var_name
            && !seen.insert(name)
        {
            errors.push(TypeError::new(
                TypeErrorKind::DuplicateParameter { name: name.clone() },
                param.var_name_range.clone(),
            ));
            failed = true;
            continue;
        }
        let typ = match (&param.var_type, expected_params) {
            (Some(var_type), _) => {
                resolve_type(var_type, &type_env.names, definition_links, errors)
            }
            (None, Some(expected)) => Some(expected[index].clone()),
            (None, None) => {
                errors.push(TypeError::new(
                    TypeErrorKind::LambdaParameterNeedsType {
                        name: param.name().to_string(),
                    },
                    param.var_name_range.clone(),
                ));
                None
            }
        };
        match typ {
            Some(typ) => typed_params.push((param, typ)),
            None => failed = true,
        }
    }
    // The body reads the parameters, so it is not checked without their
    // types.
    if failed {
        return None;
    }
    let declared_result = match return_type {
        Some(return_type) => {
            let resolved = resolve_type(return_type, &type_env.names, definition_links, errors);
            failed |= resolved.is_none();
            resolved
        }
        None => None,
    };
    let expected_result = declared_result.as_ref().or(expected_result);

    let outer = var_env.enter_lambda(match (declared, expected_result) {
        (Some(ParsedModifier::Mut), _) => Context::Outside(Position::MutFn),
        (Some(ParsedModifier::Render), _) => Context::Render,
        (None, Some(result)) if result.renders_html() => Context::Render,
        (None, Some(_)) => Context::Outside(Position::PureFn),
        (None, None) => Context::Undecided,
    });
    let mut pushed = 0;
    for (param, typ) in &typed_params {
        let Some(name) = &param.var_name else {
            continue;
        };
        match var_env.push(name.clone(), typ.clone(), param.var_name_range.clone()) {
            Ok(()) => {
                annotations.push(HoverAnnotation::VarName {
                    range: param.var_name_range.clone(),
                    typ: typ.clone(),
                    var_name: name.clone(),
                });
                pushed += 1;
            }
            Err(()) => {
                errors.push(TypeError::new(
                    TypeErrorKind::VariableAlreadyDefined { name: name.clone() },
                    param.var_name_range.clone(),
                ));
                failed = true;
            }
        }
    }
    let typed_body = typecheck_block(
        body,
        expected_result,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    );
    for _ in 0..pushed {
        let (name, entry) = var_env.pop();
        if !entry.accessed {
            errors.push(TypeError::new(
                TypeErrorKind::UnusedVariable { var_name: name },
                entry.range,
            ));
        }
    }
    let renders_html = match (&typed_body, failed) {
        (Some(typed_body), false) => Some(typed_body.typ().renders_html()),
        _ => None,
    };
    var_env.exit_lambda(outer, renders_html, errors);

    let typed_body = typed_body?;
    if failed {
        return None;
    }
    let body_type = typed_body.typ();
    if let Some(declared) = &declared_result
        && body_type != *declared
    {
        errors.push(TypeError::new(
            TypeErrorKind::TypeMismatch {
                context: TypeMismatchContext::FunctionBody,
                expected: declared.clone(),
                found: body_type,
            },
            body.tail.range().clone(),
        ));
        return None;
    }

    let function = FunctionType {
        modifier: Modifier::of(declared, &body_type),
        parameters: typed_params.iter().map(|(_, typ)| typ.clone()).collect(),
        result: Box::new(body_type),
    };
    if let Some(error) = check_function_type(&function) {
        // A parameter that renders Html is reported where it is written,
        // and a result that is a function at the body.
        let range = match &error {
            TypeErrorKind::DataFunctionTakesHtml { .. } => typed_params
                .iter()
                .find(|(_, typ)| typ.renders_html())
                .map_or(body.tail.range(), |(param, _)| &param.var_name_range),
            _ => body.tail.range(),
        };
        errors.push(TypeError::new(error, range.clone()));
        return None;
    }
    Some(TypedExpr::Lambda {
        params: typed_params
            .into_iter()
            .map(|(param, typ)| (param.var_name.clone(), typ))
            .collect(),
        body: Box::new(typed_body),
        typ: Type::Function(function),
    })
}

/// Check an application of the function value bound to `callee`, which
/// takes its arguments by position, one for each parameter.
pub fn typecheck_apply(
    callee: VarName,
    function: FunctionType,
    arguments: CallArguments<'_>,
    report_range: &DocumentRange,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let CallArguments::Positional(values) = arguments else {
        errors.push(TypeError::new(
            TypeErrorKind::NamedArgumentsToFunctionValue { name: callee },
            report_range.clone(),
        ));
        return None;
    };
    if function.modifier == Modifier::Mut && !var_env.mutates() {
        errors.push(TypeError::new(
            TypeErrorKind::MutCallOutsideMut,
            report_range.clone(),
        ));
        return None;
    }
    if function.modifier == Modifier::Render && !function.result.renders_html() {
        var_env.require(
            |_| false,
            |position| TypeErrorKind::RenderCallNotAllowed { position },
            report_range,
            errors,
        );
    }
    if values.len() != function.parameters.len() {
        errors.push(TypeError::new(
            TypeErrorKind::FunctionArgumentCountMismatch {
                name: FunctionName::from(callee),
                expected: function.parameters.len().to_string(),
                found: values.len(),
            },
            report_range.clone(),
        ));
        return None;
    }
    let mut args = Vec::with_capacity(values.len());
    for (value, parameter) in values.iter().zip(&function.parameters) {
        let Some(arg) = typecheck_expr(
            value,
            Some(parameter),
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
        let found = arg.typ();
        if found != *parameter {
            errors.push(TypeError::new(
                TypeErrorKind::TypeMismatch {
                    context: TypeMismatchContext::FunctionArgument,
                    expected: parameter.clone(),
                    found,
                },
                value.range().clone(),
            ));
            continue;
        }
        args.push(arg);
    }
    if args.len() != values.len() {
        return None;
    }
    Some(TypedExpr::Apply {
        callee,
        args,
        typ: *function.result,
    })
}
