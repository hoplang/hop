use super::r#type::Type;
use super::type_registry::TypeRegistry;
use super::typecheck_expr::typecheck_expr;
use super::typecheck_pattern::typecheck_pattern;
use super::variable_scope::VariableScope;
use crate::asset_reference::AssetReference;
use crate::definition_link::DefinitionLink;
use crate::document::DocumentRange;
use crate::hop::parsing::parsed_expr::{
    Constructor, ParsedExpr, ParsedMatchArm, ParsedMatchPattern,
};
use crate::hop::typing::TypedExpr;
use crate::hop::typing::compile_match::{MatchErrorSite, compile_match};
use crate::hop::typing::type_env::TypeEnv;
use crate::hover_annotation::HoverAnnotation;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};

pub fn typecheck_match(
    subject: &ParsedExpr,
    arms: &[ParsedMatchArm],
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
    let mut arm_bindings = Vec::with_capacity(arms.len());
    for arm in arms {
        let mut bindings = Vec::new();
        typed_patterns.push(typecheck_pattern(
            &arm.pattern,
            subject_type.clone(),
            type_env,
            registry,
            &mut bindings,
            errors,
        )?);
        arm_bindings.push(bindings);
    }

    let tree = match compile_match(registry, &typed_patterns, subject_type) {
        Ok(tree) => Some(tree),
        Err(error) => {
            let range = match error.site {
                MatchErrorSite::Subject => subject.range(),
                MatchErrorSite::Pattern(index) => arms[index].pattern.range(),
            };
            errors.push(TypeError::new(*error.kind, range.clone()));
            None
        }
    };
    let arm_bodies = typecheck_arm_bodies(
        arms,
        &arm_bindings,
        forwarded_params,
        var_env,
        type_env,
        registry,
        annotations,
        definition_links,
        asset_references,
        errors,
    );
    let (tree, (typed_bodies, result_type)) = (tree?, arm_bodies?);

    Some(TypedExpr::Match {
        subject: Box::new(typed_subject),
        arms: typed_patterns.into_iter().zip(typed_bodies).collect(),
        decision: tree,
        typ: result_type,
    })
}

fn typecheck_arm_bodies(
    arms: &[ParsedMatchArm],
    arm_bindings: &[Vec<(VarName, Type, DocumentRange)>],
    forwarded_params: &[VarName],
    var_env: &mut VariableScope,
    type_env: &TypeEnv,
    registry: &TypeRegistry,
    annotations: &mut Vec<HoverAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
    errors: &mut Vec<TypeError>,
) -> Option<(Vec<TypedExpr>, Type)> {
    let mut typed_bodies = Vec::new();
    let mut result_type: Option<Type> = None;

    for (arm, bindings) in arms.iter().zip(arm_bindings) {
        collect_pattern_definition_links(&arm.pattern, type_env, definition_links);

        let mut arm_ok = true;
        let mut pushed = 0;
        for (name, typ, range) in bindings {
            match var_env.push(name.clone(), typ.clone(), range.clone()) {
                Ok(_) => {
                    annotations.push(HoverAnnotation::TypeForVarName {
                        range: range.clone(),
                        typ: typ.clone(),
                        var_name: name.clone(),
                    });
                    pushed += 1;
                }
                Err(_) => {
                    errors.push(TypeError::new(
                        TypeErrorKind::VariableAlreadyDefined { name: name.clone() },
                        range.clone(),
                    ));
                    arm_ok = false;
                }
            }
        }

        // Use the first arm's type as context for subsequent arms
        let typed_body = typecheck_expr(
            &arm.body,
            result_type.as_ref(),
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
                    TypeErrorKind::MatchUnusedBinding { name },
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
                        TypeErrorKind::MatchArmTypeMismatch {
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

    if typed_bodies.len() != arms.len() {
        return None;
    }

    Some((typed_bodies, result_type?))
}

/// Collect definition links for enum variant references in match patterns.
fn collect_pattern_definition_links(
    pattern: &ParsedMatchPattern,
    type_env: &TypeEnv,
    definition_links: &mut Vec<DefinitionLink>,
) {
    match pattern {
        ParsedMatchPattern::Constructor {
            constructor: Constructor::EnumVariant { enum_name, .. },
            enum_name_range: Some(enum_name_range),
            fields,
            args,
            ..
        } => {
            if let Some(name) = type_env.names.get(enum_name.as_str()) {
                definition_links.push(DefinitionLink {
                    use_range: enum_name_range.clone(),
                    definition_range: name.definition_range.clone(),
                });
            }
            for (_, _, field_pattern) in fields {
                collect_pattern_definition_links(field_pattern, type_env, definition_links);
            }
            for arg in args {
                collect_pattern_definition_links(arg, type_env, definition_links);
            }
        }
        ParsedMatchPattern::Constructor { fields, args, .. } => {
            for (_, _, field_pattern) in fields {
                collect_pattern_definition_links(field_pattern, type_env, definition_links);
            }
            for arg in args {
                collect_pattern_definition_links(arg, type_env, definition_links);
            }
        }
        ParsedMatchPattern::Wildcard { .. } | ParsedMatchPattern::Binding { .. } => {}
    }
}
