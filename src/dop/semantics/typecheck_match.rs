use super::pat_match::{Body, Column, Compiler, Match, Row, Variable};
use super::r#type::Type;
use super::type_checker::typecheck_expr;
use super::type_error::TypeError;
use super::typed::{
    TypedBoolMatchArm, TypedBoolPattern, TypedEnumMatchArm, TypedEnumPattern, TypedOptionMatchArm,
    TypedOptionPattern,
};
use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::TypedExpr;
use crate::dop::syntax::parsed::{Constructor, ParsedExpr, ParsedMatchArm, ParsedMatchPattern};
use crate::environment::Environment;
use crate::hop::semantics::type_checker::TypeAnnotation;

/// Recursively validate that a pattern is compatible with the expected type.
fn validate_pattern_type(
    pattern: &ParsedMatchPattern,
    expected_type: &Type,
) -> Result<(), TypeError> {
    match pattern {
        ParsedMatchPattern::Wildcard { .. } => Ok(()),
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            range,
        } => match (constructor, expected_type) {
            // Enum variant pattern
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
                // Pattern enum must match subject enum
                if pattern_enum_name != subject_enum_name {
                    return Err(TypeError::MatchPatternEnumMismatch {
                        pattern_enum: pattern_enum_name.to_string(),
                        subject_enum: subject_enum_name.to_string(),
                        range: range.clone(),
                    });
                }

                // Variant must exist in the enum
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

            // Boolean patterns
            (Constructor::BooleanTrue | Constructor::BooleanFalse, Type::Bool) => Ok(()),

            // Option Some pattern - recursively validate the inner pattern
            (Constructor::OptionSome, Type::Option(inner_type)) => {
                if let Some(inner_pattern) = args.first() {
                    validate_pattern_type(inner_pattern, inner_type)?;
                }
                Ok(())
            }

            // Option None pattern
            (Constructor::OptionNone, Type::Option(_)) => Ok(()),

            // Type mismatches
            (_, Type::Enum { .. }) => Err(TypeError::MatchPatternTypeMismatch {
                expected: "enum".to_string(),
                found: pattern.to_string(),
                range: range.clone(),
            }),

            (_, Type::Bool) => Err(TypeError::MatchPatternTypeMismatch {
                expected: "boolean".to_string(),
                found: pattern.to_string(),
                range: range.clone(),
            }),

            (_, Type::Option(_)) => Err(TypeError::MatchPatternTypeMismatch {
                expected: "option".to_string(),
                found: pattern.to_string(),
                range: range.clone(),
            }),

            // Unsupported type for pattern matching
            _ => Err(TypeError::MatchNotImplementedForType {
                found: expected_type.to_string(),
                range: range.clone(),
            }),
        },
    }
}

/// Typecheck all arm bodies and verify they all have the same type.
/// Returns the typed bodies and the common result type.
fn typecheck_arm_bodies(
    arms: &[ParsedMatchArm],
    env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<(Vec<TypedExpr>, Type), TypeError> {
    let mut typed_bodies = Vec::new();
    let mut result_type: Option<Type> = None;

    for arm in arms {
        let typed_body = typecheck_expr(&arm.body, env, type_env, annotations, None)?;
        let body_type = typed_body.as_type().clone();

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

/// Compile patterns using pat_match and check for exhaustiveness and redundancy.
fn compile_and_check_patterns(
    arms: &[ParsedMatchArm],
    subject_type: &Type,
    match_range: &DocumentRange,
    type_env: &mut Environment<Type>,
) -> Result<Match, TypeError> {
    // Create the subject variable
    let subject_var = Variable("$subject".to_string());

    // Build rows from the match arms
    let rows: Vec<Row> = arms
        .iter()
        .enumerate()
        .map(|(idx, arm)| {
            Row::new(
                vec![Column::new(subject_var.clone(), arm.pattern.clone())],
                Body::new(idx),
            )
        })
        .collect();

    // Set up the variable environment
    let mut pat_var_env: Environment<Type> = Environment::new();
    let _ = pat_var_env.push(subject_var.0.clone(), subject_type.clone());

    // Compile the patterns
    let result = Compiler::new().compile(rows, type_env, &mut pat_var_env);

    // Check for redundant patterns (unreachable arms)
    let unreachable = result.diagnostics.unreachable(arms.len());
    if let Some(&first_unreachable) = unreachable.first() {
        let arm = &arms[first_unreachable];
        let variant_name = match &arm.pattern {
            ParsedMatchPattern::Constructor { constructor, .. } => constructor.to_string(),
            ParsedMatchPattern::Wildcard { .. } => "_".to_string(),
        };
        return Err(TypeError::MatchDuplicateVariant {
            variant: variant_name,
            range: arm.pattern.range().clone(),
        });
    }

    // Check for missing patterns
    if result.diagnostics.is_missing() {
        let missing = result.missing_patterns();
        if let Some(first_missing) = missing.first() {
            return Err(TypeError::MatchMissingVariant {
                variant: first_missing.clone(),
                range: match_range.clone(),
            });
        }
    }

    Ok(result)
}

pub fn typecheck_match(
    subject: &ParsedExpr,
    arms: &[ParsedMatchArm],
    range: &DocumentRange,
    var_env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    // Type check the subject expression
    let typed_subject = typecheck_expr(subject, var_env, type_env, annotations, None)?;
    let subject_type = typed_subject.as_type().clone();

    // Subject must be an enum, boolean, or option type
    match &subject_type {
        Type::Enum { .. } | Type::Bool | Type::Option(_) => {}
        _ => {
            return Err(TypeError::MatchNotImplementedForType {
                found: subject_type.to_string(),
                range: subject.range().clone(),
            });
        }
    }

    // Validate pattern types
    for arm in arms {
        validate_pattern_type(&arm.pattern, &subject_type)?;
    }

    // Dispatch to specific handler
    match &subject_type {
        Type::Enum { .. } => {
            typecheck_enum_match(&typed_subject, arms, range, var_env, type_env, annotations)
        }
        Type::Bool => {
            typecheck_bool_match(&typed_subject, arms, range, var_env, type_env, annotations)
        }
        Type::Option(_) => {
            typecheck_option_match(&typed_subject, arms, range, var_env, type_env, annotations)
        }
        _ => unreachable!("Already checked above"),
    }
}

fn typecheck_enum_match(
    typed_subject: &TypedExpr,
    arms: &[ParsedMatchArm],
    range: &DocumentRange,
    env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    let subject_type = typed_subject.as_type();

    // Use pat_match to check exhaustiveness and redundancy
    compile_and_check_patterns(arms, subject_type, range, type_env)?;

    // Typecheck arm bodies
    let (typed_bodies, result_type) = typecheck_arm_bodies(arms, env, type_env, annotations)?;

    // Build typed output
    let typed_arms = arms
        .iter()
        .zip(typed_bodies)
        .map(|(arm, typed_body)| {
            let typed_pattern = match &arm.pattern {
                ParsedMatchPattern::Constructor {
                    constructor:
                        Constructor::EnumVariant {
                            enum_name: pattern_enum_name,
                            variant_name: pattern_variant_name,
                        },
                    ..
                } => TypedEnumPattern::Variant {
                    enum_name: pattern_enum_name.to_string(),
                    variant_name: pattern_variant_name.clone(),
                },
                ParsedMatchPattern::Wildcard { .. } => TypedEnumPattern::Wildcard,
                _ => unreachable!("Pattern type already validated"),
            };
            TypedEnumMatchArm {
                pattern: typed_pattern,
                body: typed_body,
            }
        })
        .collect();

    Ok(TypedExpr::EnumMatch {
        subject: Box::new(typed_subject.clone()),
        arms: typed_arms,
        kind: result_type,
    })
}

fn typecheck_bool_match(
    typed_subject: &TypedExpr,
    arms: &[ParsedMatchArm],
    range: &DocumentRange,
    env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    let subject_type = typed_subject.as_type();

    // Use pat_match to check exhaustiveness and redundancy
    compile_and_check_patterns(arms, subject_type, range, type_env)?;

    // Typecheck arm bodies
    let (typed_bodies, result_type) = typecheck_arm_bodies(arms, env, type_env, annotations)?;

    // Build typed output
    let typed_arms = arms
        .iter()
        .zip(typed_bodies)
        .map(|(arm, typed_body)| {
            let typed_pattern = match &arm.pattern {
                ParsedMatchPattern::Constructor {
                    constructor: Constructor::BooleanTrue,
                    ..
                } => TypedBoolPattern::Literal(true),
                ParsedMatchPattern::Constructor {
                    constructor: Constructor::BooleanFalse,
                    ..
                } => TypedBoolPattern::Literal(false),
                ParsedMatchPattern::Wildcard { .. } => TypedBoolPattern::Wildcard,
                _ => unreachable!("Pattern type already validated"),
            };
            TypedBoolMatchArm {
                pattern: typed_pattern,
                body: typed_body,
            }
        })
        .collect();

    Ok(TypedExpr::BoolMatch {
        subject: Box::new(typed_subject.clone()),
        arms: typed_arms,
        kind: result_type,
    })
}

fn typecheck_option_match(
    typed_subject: &TypedExpr,
    arms: &[ParsedMatchArm],
    range: &DocumentRange,
    env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    let subject_type = typed_subject.as_type();

    // Use pat_match to check exhaustiveness and redundancy
    compile_and_check_patterns(arms, subject_type, range, type_env)?;

    // Typecheck arm bodies
    let (typed_bodies, result_type) = typecheck_arm_bodies(arms, env, type_env, annotations)?;

    // Build typed output
    let typed_arms = arms
        .iter()
        .zip(typed_bodies)
        .map(|(arm, typed_body)| {
            let typed_pattern = match &arm.pattern {
                ParsedMatchPattern::Constructor {
                    constructor: Constructor::OptionSome,
                    ..
                } => TypedOptionPattern::Some,
                ParsedMatchPattern::Constructor {
                    constructor: Constructor::OptionNone,
                    ..
                } => TypedOptionPattern::None,
                ParsedMatchPattern::Wildcard { .. } => TypedOptionPattern::Wildcard,
                _ => unreachable!("Pattern type already validated"),
            };
            TypedOptionMatchArm {
                pattern: typed_pattern,
                body: typed_body,
            }
        })
        .collect();

    Ok(TypedExpr::OptionMatch {
        subject: Box::new(typed_subject.clone()),
        arms: typed_arms,
        kind: result_type,
    })
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::document::DocumentAnnotator;
    use crate::dop::semantics::r#type::Type;
    use crate::dop::semantics::type_checker::{resolve_type, typecheck_expr};
    use crate::dop::symbols::type_name::TypeName;
    use crate::dop::{ParsedDeclaration, Parser};
    use crate::environment::Environment;
    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;

    fn check(declarations_str: &str, env_vars: &[(&str, &str)], expr_str: &str, expected: Expect) {
        let mut env = Environment::new();
        let mut type_env: Environment<Type> = Environment::new();
        let test_module = ModuleName::new("test").unwrap();

        // Parse and process declarations
        let mut parser = Parser::from(declarations_str);
        let mut errors = ErrorCollector::new();
        for declaration in parser.parse_declarations(&mut errors) {
            match declaration {
                ParsedDeclaration::Enum { name, variants, .. } => {
                    let enum_type = Type::Enum {
                        module: test_module.clone(),
                        name: TypeName::new(name.as_str()).unwrap(),
                        variants: variants.iter().map(|(name, _)| name.clone()).collect(),
                    };
                    let _ = type_env.push(name.to_string(), enum_type);
                }
                ParsedDeclaration::Record {
                    name,
                    fields: decl_fields,
                    ..
                } => {
                    let fields: Vec<_> = decl_fields
                        .iter()
                        .map(|(field_name, _, field_type)| {
                            let resolved_type = resolve_type(field_type, &mut type_env)
                                .expect("Test record field type should be valid");
                            (field_name.clone(), resolved_type)
                        })
                        .collect();
                    let record_type = Type::Record {
                        module: test_module.clone(),
                        name: TypeName::new(name.as_str()).unwrap(),
                        fields,
                    };
                    let _ = type_env.push(name.to_string(), record_type);
                }
                ParsedDeclaration::Import { .. } => {
                    panic!("Import declarations not supported in tests");
                }
            }
        }
        if !errors.is_empty() {
            panic!("Failed to parse declarations: {:?}", errors);
        }

        for (var_name, type_str) in env_vars {
            let mut parser = Parser::from(*type_str);
            let parsed_type = parser.parse_type().expect("Failed to parse type");
            let typ = resolve_type(&parsed_type, &mut type_env)
                .expect("Test parameter type should be valid");
            let _ = env.push(var_name.to_string(), typ);
        }

        let mut parser = Parser::from(expr_str);
        let expr = parser.parse_expr().expect("Failed to parse expression");

        let mut annotations = Vec::new();

        let actual = match typecheck_expr(&expr, &mut env, &mut type_env, &mut annotations, None) {
            Ok(typed_expr) => typed_expr.as_type().to_string(),
            Err(e) => DocumentAnnotator::new()
                .with_label("error")
                .without_location()
                .without_line_numbers()
                .annotate(None, [e]),
        };

        expected.assert_eq(&actual);
    }

    ///////////////////////////////////////////////////////////////////////////
    /// ENUM MATCH                                                          ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_match_expression_with_all_variants() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Green => "green",
                    Color::Blue => "blue",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_match_expression_returning_int() {
        check(
            indoc! {"
                enum Size {
                    Small,
                    Medium,
                    Large,
                }
            "},
            &[("size", "Size")],
            indoc! {"
                match size {
                    Size::Small => 1,
                    Size::Medium => 2,
                    Size::Large => 3,
                }
            "},
            expect!["Int"],
        );
    }

    #[test]
    fn should_accept_match_expression_returning_bool() {
        check(
            indoc! {"
                enum Status {
                    Active,
                    Inactive,
                }
            "},
            &[("status", "Status")],
            indoc! {"
                match status {
                    Status::Active => true,
                    Status::Inactive => false,
                }
            "},
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_match_on_non_enum() {
        check(
            "",
            &[("name", "String")],
            indoc! {r#"
                match name {
                    Color::Red => "red",
                }
            "#},
            expect![[r#"
                error: Match is not implemented for type String
                match name {
                      ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_with_mismatched_arm_types() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Green => 42,
                }
            "#},
            expect![[r#"
                error: Match arms must all have the same type, expected String but found Int
                    Color::Green => 42,
                                    ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_with_missing_variant() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Green => "green",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Blue'
                match color {
                ^^^^^^^^^^^^^
                    Color::Red => "red",
                ^^^^^^^^^^^^^^^^^^^^^^^^
                    Color::Green => "green",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_with_duplicate_variant() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Red => "also red",
                    Color::Green => "green",
                }
            "#},
            expect![[r#"
                error: Redundant match arm for variant 'Color::Red'
                    Color::Red => "also red",
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_with_wrong_enum_in_pattern() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
                enum Size {
                    Small,
                    Large,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Size::Small => "small",
                }
            "#},
            expect![[r#"
                error: Match pattern enum 'Size' does not match subject enum 'Color'
                    Size::Small => "small",
                    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_with_undefined_variant_in_pattern() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Yellow => "yellow",
                }
            "#},
            expect![[r#"
                error: Variant 'Yellow' is not defined in enum 'Color'
                    Color::Yellow => "yellow",
                    ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_field_access_subject() {
        check(
            indoc! {"
                enum Status {
                    Active,
                    Inactive,
                }
                record User {
                    status: Status,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user.status {
                    Status::Active => "active",
                    Status::Inactive => "inactive",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_match_with_complex_arm_bodies() {
        check(
            indoc! {"
                enum Status {
                    Active,
                    Inactive,
                }
                record User {
                    name: String,
                    status: Status,
                }
            "},
            &[("user", "User")],
            indoc! {"
                match user.status {
                    Status::Active => user.name,
                    Status::Inactive => user.name,
                }
            "},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_match_with_single_variant_enum() {
        check(
            indoc! {"
                enum Unit {
                    Value,
                }
            "},
            &[("unit", "Unit")],
            indoc! {r#"
                match unit {
                    Unit::Value => "value",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_match_with_wildcard_pattern() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    _ => "other",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_match_with_only_wildcard() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    _ => "any color",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_match_with_wildcard_at_end() {
        check(
            indoc! {"
                enum Status {
                    Active,
                    Inactive,
                    Pending,
                    Archived,
                }
            "},
            &[("status", "Status")],
            indoc! {"
                match status {
                    Status::Active => 1,
                    Status::Inactive => 2,
                    _ => 0,
                }
            "},
            expect!["Int"],
        );
    }

    #[test]
    fn should_reject_match_with_pattern_after_wildcard() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    _ => "other",
                    Color::Blue => "blue",
                }
            "#},
            expect![[r#"
                error: Redundant match arm for variant 'Color::Blue'
                    Color::Blue => "blue",
                    ^^^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// BOOLEAN MATCH                                                       ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_boolean_match_with_both_values() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true => "yes",
                    false => "no",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_boolean_match_with_wildcard() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true => "yes",
                    _ => "no",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_boolean_match_with_only_wildcard() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    _ => "always",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_reject_boolean_match_missing_false() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true => "yes",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'false'
                match flag {
                ^^^^^^^^^^^^
                    true => "yes",
                ^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_boolean_match_missing_true() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    false => "no",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'true'
                match flag {
                ^^^^^^^^^^^^
                    false => "no",
                ^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_boolean_match_duplicate_true() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true => "yes",
                    true => "also yes",
                    false => "no",
                }
            "#},
            expect![[r#"
                error: Redundant match arm for variant 'true'
                    true => "also yes",
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_pattern_in_boolean_match() {
        check(
            indoc! {"
                enum Color {
                    Red,
                }
            "},
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    Color::Red => "red",
                    false => "no",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Color::Red
                    Color::Red => "red",
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_boolean_pattern_in_enum_match() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    true => "yes",
                    Color::Green => "green",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected enum, found true
                    true => "yes",
                    ^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// OPTION MATCH                                                        ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_option_match_with_some_and_none() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(_) => "has value",
                    None    => "empty",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_option_match_returning_int() {
        check(
            "",
            &[("opt", "Option[String]")],
            indoc! {"
                match opt {
                    Some(_) => 1,
                    None    => 0,
                }
            "},
            expect!["Int"],
        );
    }

    #[test]
    fn should_accept_option_match_with_wildcard() {
        check(
            "",
            &[("opt", "Option[Bool]")],
            indoc! {r#"
                match opt {
                    Some(_) => "has value",
                    _       => "fallback",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_option_match_with_only_wildcard() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    _ => "always this",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_reject_option_match_missing_some() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    None => "empty",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Some(_)'
                match opt {
                ^^^^^^^^^^^
                    None => "empty",
                ^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_option_match_missing_none() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(_) => "has value",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'None'
                match opt {
                ^^^^^^^^^^^
                    Some(_) => "has value",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_non_exhaustive_nested_option_match() {
        check(
            "",
            &[("opt", "Option[Option[Int]]")],
            indoc! {r#"
                match opt {
                    Some(Some(_)) => "",
                    None          => "empty",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Some(None)'
                match opt {
                ^^^^^^^^^^^
                    Some(Some(_)) => "",
                ^^^^^^^^^^^^^^^^^^^^^^^^
                    None          => "empty",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_non_exhaustive_nested_option_match_with_bool() {
        check(
            "",
            &[("opt", "Option[Option[Bool]]")],
            indoc! {r#"
                match opt {
                    Some(Some(false)) => "",
                    Some(None)        => "",
                    None              => "empty",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Some(Some(true))'
                match opt {
                ^^^^^^^^^^^
                    Some(Some(false)) => "",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Some(None)        => "",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    None              => "empty",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_accept_exhaustive_nested_option_match() {
        check(
            "",
            &[("opt", "Option[Option[Int]]")],
            indoc! {r#"
                match opt {
                    Some(Some(_)) => "",
                    Some(None)    => "",
                    None          => "empty",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_reject_option_match_duplicate_some() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(_) => "first",
                    Some(_) => "second",
                    None    => "empty",
                }
            "#},
            expect![[r#"
                error: Redundant match arm for variant 'Some(_)'
                    Some(_) => "second",
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_option_match_duplicate_none() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(_) => "has value",
                    None    => "first",
                    None    => "second",
                }
            "#},
            expect![[r#"
                error: Redundant match arm for variant 'None'
                    None    => "second",
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_option_match_with_mismatched_arm_types() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {"
                match opt {
                    Some(_) => 42,
                    None    => true,
                }
            "},
            expect![[r#"
                error: Match arms must all have the same type, expected Int but found Bool
                    None    => true,
                               ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_pattern_in_option_match() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
            "},
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Color::Red => "red",
                    None       => "empty",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected option, found Color::Red
                    Color::Red => "red",
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_boolean_pattern_in_option_match() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    true => "yes",
                    None => "empty",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected option, found true
                    true => "yes",
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_option_pattern_in_enum_match() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                }
            "},
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Some(_)      => "has value",
                    Color::Green => "green",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected enum, found Some(_)(_)
                    Some(_)      => "has value",
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_option_pattern_in_bool_match() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    Some(_) => "has value",
                    false   => "no",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Some(_)(_)
                    Some(_) => "has value",
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_nested_option_pattern_when_inner_type_is_bool() {
        check(
            "",
            &[("opt", "Option[Bool]")],
            indoc! {r#"
                match opt {
                    Some(None) => "nested none",
                    None       => "empty",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found None
                    Some(None) => "nested none",
                         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_deeply_nested_option_pattern_when_inner_type_is_bool() {
        check(
            "",
            &[("opt", "Option[Bool]")],
            indoc! {r#"
                match opt {
                    Some(Some(_)) => "nested some",
                    None          => "empty",
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Some(_)(_)
                    Some(Some(_)) => "nested some",
                         ^^^^^^^
            "#]],
        );
    }
}
