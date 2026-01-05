use crate::dop::patterns::compiler::{Compiler, Decision, Variable};
use super::r#type::Type;
use super::type_checker::typecheck_expr;
use super::type_error::TypeError;
use crate::dop::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::TypedExpr;
use crate::dop::symbols::var_name::VarName;
use crate::dop::syntax::parsed::{Constructor, ParsedExpr, ParsedMatchArm, ParsedMatchPattern};
use crate::environment::Environment;
use crate::hop::semantics::type_checker::TypeAnnotation;

// Typecheck a match expression and compile it to a TypedExpr.
pub fn typecheck_match(
    subject: &ParsedExpr,
    arms: &[ParsedMatchArm],
    range: &DocumentRange,
    var_env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    let typed_subject = typecheck_expr(subject, var_env, type_env, annotations, None)?;
    let subject_type = typed_subject.as_type().clone();

    if !matches!(
        &subject_type,
        Type::Enum { .. } | Type::Bool | Type::Option(_) | Type::Record { .. }
    ) {
        return Err(TypeError::MatchNotImplementedForType {
            found: subject_type.to_string(),
            range: subject.range().clone(),
        });
    }

    if arms.is_empty() {
        return Err(TypeError::MatchNoArms {
            range: range.clone(),
        });
    }

    for arm in arms {
        validate_pattern_type(&arm.pattern, &subject_type)?;
    }

    // If subject is already a variable, use it directly; otherwise wrap in a let
    let (subject_var, initial_var_counter, needs_wrapper) = match &typed_subject {
        TypedExpr::Var { value, kind } => (
            Variable::new(value.as_str().to_string(), kind.clone()),
            0,
            false,
        ),
        _ => (
            Variable::new("v0".to_string(), subject_type.clone()),
            1,
            true,
        ),
    };

    let patterns: Vec<_> = arms.iter().map(|arm| arm.pattern.clone()).collect();
    let tree = Compiler::new(initial_var_counter).compile(&patterns, &subject_var, range, type_env)?;

    let (typed_bodies, result_type) =
        typecheck_arm_bodies(arms, &subject_type, var_env, type_env, annotations)?;

    let mut result = decision_to_typed_expr(&tree, &typed_bodies, result_type.clone());

    // Wrap in let if subject was not a simple variable reference
    if needs_wrapper {
        result = TypedExpr::Let {
            var: VarName::new("v0").unwrap(),
            value: Box::new(typed_subject),
            body: Box::new(result),
            kind: result_type,
        };
    }

    Ok(result)
}

/// Recursively validate that a pattern is compatible with the subject type,
/// i.e. the type of the expression that is being matched.
pub fn validate_pattern_type(
    pattern: &ParsedMatchPattern,
    subject_type: &Type,
) -> Result<(), TypeError> {
    match pattern {
        ParsedMatchPattern::Wildcard { .. } => Ok(()),
        ParsedMatchPattern::Binding { .. } => Ok(()),
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            fields,
            range,
        } => match (constructor, subject_type) {
            (
                Constructor::EnumVariant {
                    enum_name: pattern_enum_name,
                    variant_name: pattern_variant_name,
                },
                Type::Enum {
                    name: subject_enum_name,
                    variants,
                    ..
                },
            ) => {
                if pattern_enum_name != subject_enum_name {
                    return Err(TypeError::MatchPatternEnumMismatch {
                        pattern_enum: pattern_enum_name.to_string(),
                        subject_enum: subject_enum_name.to_string(),
                        range: range.clone(),
                    });
                }

                let variant_exists = variants.iter().any(|v| v.as_str() == pattern_variant_name);
                if !variant_exists {
                    return Err(TypeError::UndefinedEnumVariant {
                        enum_name: pattern_enum_name.to_string(),
                        variant_name: pattern_variant_name.clone(),
                        range: range.clone(),
                    });
                }

                Ok(())
            }

            (Constructor::BooleanTrue | Constructor::BooleanFalse, Type::Bool) => Ok(()),

            (Constructor::OptionSome, Type::Option(inner_type)) => {
                if let Some(inner_pattern) = args.first() {
                    validate_pattern_type(inner_pattern, inner_type)?;
                }
                Ok(())
            }

            (Constructor::OptionNone, Type::Option(_)) => Ok(()),

            (
                Constructor::Record {
                    type_name: pattern_type_name,
                },
                Type::Record {
                    name: subject_type_name,
                    fields: subject_fields,
                    ..
                },
            ) => {
                // Check record type matches
                if pattern_type_name != subject_type_name {
                    return Err(TypeError::MatchPatternRecordMismatch {
                        pattern_record: pattern_type_name.to_string(),
                        subject_record: subject_type_name.to_string(),
                        range: range.clone(),
                    });
                }

                // Check all fields are specified (no partial matching)
                if fields.len() != subject_fields.len() {
                    return Err(TypeError::MatchRecordPatternFieldCount {
                        record_name: pattern_type_name.to_string(),
                        expected: subject_fields.len(),
                        found: fields.len(),
                        range: range.clone(),
                    });
                }

                // Validate each field pattern against the field type
                for (field_name, field_pattern) in fields {
                    let field_type = subject_fields
                        .iter()
                        .find(|(name, _)| name == field_name)
                        .map(|(_, typ)| typ);

                    match field_type {
                        Some(typ) => validate_pattern_type(field_pattern, typ)?,
                        None => {
                            return Err(TypeError::MatchRecordPatternUnknownField {
                                field_name: field_name.to_string(),
                                record_name: pattern_type_name.to_string(),
                                range: field_pattern.range().clone(),
                            });
                        }
                    }
                }

                Ok(())
            }

            _ => {
                let expected = match subject_type {
                    Type::Enum { .. } => "enum",
                    Type::Bool => "boolean",
                    Type::Option(_) => "option",
                    Type::Record { .. } => "record",
                    _ => {
                        return Err(TypeError::MatchNotImplementedForType {
                            found: subject_type.to_string(),
                            range: range.clone(),
                        });
                    }
                };
                Err(TypeError::MatchPatternTypeMismatch {
                    expected: expected.to_string(),
                    found: pattern.to_string(),
                    range: range.clone(),
                })
            }
        },
    }
}

/// Extract binding variables from a pattern and return them with their types and ranges.
/// The type is derived from the subject type and the position in the pattern.
pub fn extract_bindings_from_pattern(
    pattern: &ParsedMatchPattern,
    subject_type: &Type,
) -> Vec<(String, Type, DocumentRange)> {
    match pattern {
        ParsedMatchPattern::Binding { name, range } => {
            vec![(name.clone(), subject_type.clone(), range.clone())]
        }
        ParsedMatchPattern::Wildcard { .. } => vec![],
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            fields,
            ..
        } => match constructor {
            Constructor::OptionSome => {
                let Type::Option(inner_type) = subject_type else {
                    unreachable!("OptionSome pattern requires Option type")
                };
                let Some(inner_pattern) = args.first() else {
                    unreachable!("OptionSome pattern requires an argument")
                };
                extract_bindings_from_pattern(inner_pattern, inner_type)
            }
            Constructor::Record { .. } => {
                let Type::Record {
                    fields: type_fields,
                    ..
                } = subject_type
                else {
                    unreachable!("Record pattern requires Record type")
                };

                let mut bindings = Vec::new();
                for (field_name, field_pattern) in fields {
                    let field_type = type_fields
                        .iter()
                        .find(|(name, _)| name == field_name)
                        .map(|(_, typ)| typ)
                        .expect("field type not found");
                    bindings.extend(extract_bindings_from_pattern(field_pattern, field_type));
                }
                bindings
            }
            Constructor::OptionNone
            | Constructor::BooleanTrue
            | Constructor::BooleanFalse
            | Constructor::EnumVariant { .. } => vec![],
        },
    }
}

/// Typecheck all arm bodies and verify they all have the same type.
/// Returns the typed bodies and the common result type.
fn typecheck_arm_bodies(
    arms: &[ParsedMatchArm],
    subject_type: &Type,
    env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<(Vec<TypedExpr>, Type), TypeError> {
    let mut typed_bodies = Vec::new();
    let mut result_type: Option<Type> = None;

    for arm in arms {
        // Extract binding variables from the pattern and add them to the environment
        let bindings = extract_bindings_from_pattern(&arm.pattern, subject_type);
        for (name, typ, _) in &bindings {
            let _ = env.push(name.clone(), typ.clone());
        }

        // Use the first arm's type as context for subsequent arms
        let typed_body =
            typecheck_expr(&arm.body, env, type_env, annotations, result_type.as_ref())?;
        let body_type = typed_body.as_type().clone();

        // Remove bindings from environment and check for unused bindings
        for (_, _, range) in bindings.iter().rev() {
            let (name, _, accessed) = env.pop();
            if !accessed {
                return Err(TypeError::MatchUnusedBinding {
                    name,
                    range: range.clone(),
                });
            }
        }

        match &result_type {
            None => {
                result_type = Some(body_type);
            }
            Some(expected) => {
                if body_type != *expected {
                    return Err(TypeError::MatchArmTypeMismatch {
                        expected: expected.to_string(),
                        found: body_type.to_string(),
                        range: arm.body.range().clone(),
                    });
                }
            }
        }

        typed_bodies.push(typed_body);
    }

    Ok((typed_bodies, result_type.unwrap()))
}

/// Convert a compiled Decision tree into a TypedExpr.
fn decision_to_typed_expr(
    decision: &Decision,
    typed_bodies: &[TypedExpr],
    result_type: Type,
) -> TypedExpr {
    match decision {
        Decision::Success(body) => {
            let mut result = typed_bodies[body.value].clone();
            // Wrap with Let expressions for each binding (in reverse order so first binding is outermost)
            for (name, source_var) in body.bindings.iter().rev() {
                let var_name = VarName::new(name).expect("invalid variable name");
                let value = Box::new(TypedExpr::Var {
                    value: VarName::new(&source_var.name).expect("invalid variable name"),
                    kind: source_var.typ.clone(),
                });
                let kind = result.as_type().clone();
                result = TypedExpr::Let {
                    var: var_name,
                    value,
                    body: Box::new(result),
                    kind,
                };
            }
            result
        }

        Decision::Switch(var, cases) => {
            let subject = Box::new(TypedExpr::Var {
                value: VarName::new(&var.name).expect("invalid variable name"),
                kind: var.typ.clone(),
            });

            match &var.typ {
                Type::Bool => {
                    // Find the true and false cases
                    let mut true_body = None;
                    let mut false_body = None;

                    for case in cases {
                        match &case.constructor {
                            Constructor::BooleanTrue => {
                                true_body = Some(decision_to_typed_expr(
                                    &case.body,
                                    typed_bodies,
                                    result_type.clone(),
                                ));
                            }
                            Constructor::BooleanFalse => {
                                false_body = Some(decision_to_typed_expr(
                                    &case.body,
                                    typed_bodies,
                                    result_type.clone(),
                                ));
                            }
                            _ => unreachable!("Invalid constructor for Bool type"),
                        }
                    }

                    TypedExpr::Match {
                        match_: Match::Bool {
                            subject,
                            true_body: Box::new(true_body.expect("BoolMatch must have a true arm")),
                            false_body: Box::new(false_body.expect("BoolMatch must have a false arm")),
                        },
                        kind: result_type,
                    }
                }

                Type::Option(_) => {
                    // Find the Some and None cases
                    let mut some_arm_binding = None;
                    let mut some_arm_body = None;
                    let mut none_arm_body = None;

                    for case in cases {
                        match &case.constructor {
                            Constructor::OptionSome => {
                                some_arm_binding = case.arguments.first().map(|var| {
                                    (
                                        VarName::new(&var.name).expect("invalid variable name"),
                                        var.typ.clone(),
                                    )
                                });
                                some_arm_body = Some(decision_to_typed_expr(
                                    &case.body,
                                    typed_bodies,
                                    result_type.clone(),
                                ));
                            }
                            Constructor::OptionNone => {
                                none_arm_body = Some(decision_to_typed_expr(
                                    &case.body,
                                    typed_bodies,
                                    result_type.clone(),
                                ));
                            }
                            _ => unreachable!("Invalid constructor for Option type"),
                        }
                    }

                    TypedExpr::Match {
                        match_: Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body: Box::new(
                                some_arm_body.expect("OptionMatch must have a Some arm"),
                            ),
                            none_arm_body: Box::new(
                                none_arm_body.expect("OptionMatch must have a None arm"),
                            ),
                        },
                        kind: result_type,
                    }
                }

                Type::Enum { .. } => {
                    let arms = cases
                        .iter()
                        .map(|case| {
                            let pattern = match &case.constructor {
                                Constructor::EnumVariant {
                                    enum_name,
                                    variant_name,
                                } => EnumPattern::Variant {
                                    enum_name: enum_name.to_string(),
                                    variant_name: variant_name.clone(),
                                },
                                _ => unreachable!("Invalid constructor for Enum type"),
                            };
                            let body = decision_to_typed_expr(
                                &case.body,
                                typed_bodies,
                                result_type.clone(),
                            );
                            EnumMatchArm { pattern, body }
                        })
                        .collect();

                    TypedExpr::Match {
                        match_: Match::Enum { subject, arms },
                        kind: result_type,
                    }
                }

                Type::Record {
                    fields: type_fields,
                    ..
                } => {
                    // Records have only one case (the record itself)
                    let case = &cases[0];

                    // Build the body with Let bindings for each field
                    let mut body =
                        decision_to_typed_expr(&case.body, typed_bodies, result_type.clone());

                    // Wrap with Let expressions for each field (using FieldAccess)
                    // Iterate in reverse so bindings are in the correct order
                    for (i, (field_name, _field_type)) in type_fields.iter().enumerate().rev() {
                        let var = &case.arguments[i];
                        let var_name = VarName::new(&var.name).expect("invalid variable name");

                        // Create field access: subject.field_name
                        let field_access = TypedExpr::FieldAccess {
                            record: subject.clone(),
                            field: field_name.clone(),
                            kind: var.typ.clone(),
                        };

                        let kind = body.as_type().clone();
                        body = TypedExpr::Let {
                            var: var_name,
                            value: Box::new(field_access),
                            body: Box::new(body),
                            kind,
                        };
                    }

                    body
                }

                _ => panic!("Unsupported type for pattern matching: {:?}", var.typ),
            }
        }
    }
}
