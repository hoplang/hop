use super::r#type::Type;
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typecheck_pattern::typecheck_pattern;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::hop::parsing::{ParsedExpr, ParsedMatchArm};
use crate::hop::typing::TypedExpr;
use crate::hop::typing::compile_match::{MatchErrorSite, compile_match};
use crate::hop::typing::type_env::TypeEnv;
use crate::hop::typing::{TypeError, TypeErrorKind, TypeMismatchContext};
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::var_name::VarName;

pub fn typecheck_match(
    subject: &ParsedExpr,
    arms: &[ParsedMatchArm],
    expected_type: Option<&Type>,
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<TypedExpr> {
    let typed_subject = typecheck_expr(
        subject,
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

    let subject_type = typed_subject.typ();
    if !subject_type.is_matchable() {
        errors.push(TypeError::new(
            TypeErrorKind::MatchNotImplementedForType {
                found: subject_type,
            },
            subject.range().clone(),
        ));
        return None;
    }
    let mut typed_patterns = Vec::with_capacity(arms.len());
    let mut typed_bodies = Vec::with_capacity(arms.len());
    let mut result_type: Option<Type> = None;

    for arm in arms {
        let mut bindings = Vec::new();
        // The body of an arm whose pattern does not typecheck is skipped,
        // since the variables the pattern binds are unknown
        let Some(typed_pattern) = typecheck_pattern(
            &arm.pattern,
            &subject_type,
            type_env,
            registry,
            &mut bindings,
            definition_links,
            errors,
        ) else {
            continue;
        };
        typed_patterns.push(typed_pattern);

        let mut arm_ok = true;
        let mut pushed = 0;
        for (name, typ, range) in bindings {
            match var_env.push(name.clone(), typ.clone(), range.clone()) {
                Ok(_) => {
                    annotations.push(HoverAnnotation::TypeForVarName {
                        range,
                        typ,
                        var_name: name,
                    });
                    pushed += 1;
                }
                Err(_) => {
                    errors.push(TypeError::new(
                        TypeErrorKind::VariableAlreadyDefined { name },
                        range,
                    ));
                    arm_ok = false;
                }
            }
        }

        // Use the expected type as context for every arm, falling back to
        // the first arm's type when there is no expected type
        let typed_body = typecheck_expr(
            &arm.body,
            expected_type.or(result_type.as_ref()),
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

        let Some(typed_body) = typed_body else {
            continue;
        };
        let body_type = typed_body.typ();

        match &result_type {
            None => {
                result_type = Some(body_type.clone());
            }
            Some(expected) => {
                if body_type != *expected {
                    errors.push(TypeError::new(
                        TypeErrorKind::TypeMismatch {
                            context: TypeMismatchContext::MatchArm,
                            expected: expected.clone(),
                            found: body_type,
                        },
                        arm.body.range().clone(),
                    ));
                    arm_ok = false;
                }
            }
        }

        if arm_ok {
            typed_bodies.push(typed_body);
        }
    }

    // Exhaustiveness and reachability are only checked when every pattern
    // typechecks, so that pattern indices line up with the arms
    let tree = if typed_patterns.len() == arms.len() {
        match compile_match(registry, &typed_patterns, subject_type) {
            Ok(tree) => Some(tree),
            Err(error) => {
                let range = match error.site {
                    MatchErrorSite::Subject => subject.range(),
                    MatchErrorSite::Pattern(index) => arms[index].pattern.range(),
                };
                errors.push(TypeError::new(*error.kind, range.clone()));
                None
            }
        }
    } else {
        None
    };
    if typed_bodies.len() != arms.len() {
        return None;
    }

    Some(TypedExpr::Match {
        subject: Box::new(typed_subject),
        arms: typed_patterns.into_iter().zip(typed_bodies).collect(),
        decision: tree?,
        typ: result_type?,
    })
}
