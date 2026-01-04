use super::pat_match::{Body, Column, Compiler, Decision, Match, Row, Variable};
use super::r#type::Type;
use super::type_checker::typecheck_expr;
use super::type_error::TypeError;
use super::typed::{
    TypedBoolMatchArm, TypedBoolPattern, TypedEnumMatchArm, TypedEnumPattern, TypedOptionMatchArm,
    TypedOptionPattern,
};
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
        Type::Enum { .. } | Type::Bool | Type::Option(_)
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

    let compiled = compile_and_check_patterns(arms, &subject_type, range, type_env)?;

    let (typed_bodies, result_type) =
        typecheck_arm_bodies(arms, &subject_type, var_env, type_env, annotations)?;

    Ok(decision_to_typed_expr(
        &compiled.tree,
        &typed_bodies,
        result_type,
        Some(typed_subject),
    ))
}

/// Recursively validate that a pattern is compatible with the subject type,
/// i.e. the type of the expression that is being matched.
fn validate_pattern_type(
    pattern: &ParsedMatchPattern,
    subject_type: &Type,
) -> Result<(), TypeError> {
    match pattern {
        ParsedMatchPattern::Wildcard { .. } => Ok(()),
        ParsedMatchPattern::Binding { .. } => Ok(()),
        ParsedMatchPattern::Constructor {
            constructor,
            args,
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

            _ => {
                let expected = match subject_type {
                    Type::Enum { .. } => "enum",
                    Type::Bool => "boolean",
                    Type::Option(_) => "option",
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
fn extract_bindings_from_pattern(
    pattern: &ParsedMatchPattern,
    subject_type: &Type,
) -> Vec<(String, Type, DocumentRange)> {
    match pattern {
        ParsedMatchPattern::Binding { name, range } => {
            vec![(name.clone(), subject_type.clone(), range.clone())]
        }
        ParsedMatchPattern::Wildcard { .. } => vec![],
        ParsedMatchPattern::Constructor {
            constructor, args, ..
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

/// The name used for the subject variable in pattern compilation.
const SUBJECT_VAR_NAME: &str = "_subject";

/// Convert a compiled Decision tree into a TypedExpr.
fn decision_to_typed_expr(
    decision: &Decision,
    typed_bodies: &[TypedExpr],
    result_type: Type,
    subject_expr: Option<TypedExpr>,
) -> TypedExpr {
    match decision {
        Decision::Success(body) => {
            let mut result = typed_bodies[body.value].clone();
            // Wrap with Let expressions for each binding (in reverse order so first binding is outermost)
            for (name, source_var) in body.bindings.iter().rev() {
                let var_name = VarName::new(name).expect("invalid variable name");
                // If the source is the original subject, use the subject expression
                // Otherwise, create a variable reference
                let value = if source_var.name == SUBJECT_VAR_NAME {
                    Box::new(
                        subject_expr
                            .clone()
                            .expect("subject_expr required for subject binding"),
                    )
                } else {
                    Box::new(TypedExpr::Var {
                        value: VarName::new(&source_var.name).expect("invalid variable name"),
                        kind: source_var.typ.clone(),
                    })
                };
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

        Decision::Failure => {
            panic!("Non-exhaustive match reached in decision_to_typed_expr")
        }

        Decision::Switch(var, cases) => {
            // Determine the subject expression for this switch
            // If subject_expr is None, this is a nested match on an extracted variable
            let is_outer_match = var.name == SUBJECT_VAR_NAME;
            let subject_typed_expr = if is_outer_match {
                subject_expr
                    .clone()
                    .expect("subject_expr required for outer match")
            } else {
                TypedExpr::Var {
                    value: VarName::new(&var.name).expect("invalid variable name"),
                    kind: var.typ.clone(),
                }
            };
            let subject = Box::new(subject_typed_expr);

            // For recursive calls, we always pass the original subject_expr
            // so that bindings to _subject can be resolved correctly
            match &var.typ {
                Type::Bool => {
                    let arms = cases
                        .iter()
                        .map(|case| {
                            let pattern = match &case.constructor {
                                Constructor::BooleanTrue => TypedBoolPattern::Literal(true),
                                Constructor::BooleanFalse => TypedBoolPattern::Literal(false),
                                _ => unreachable!("Invalid constructor for Bool type"),
                            };
                            let body = decision_to_typed_expr(
                                &case.body,
                                typed_bodies,
                                result_type.clone(),
                                subject_expr.clone(),
                            );
                            TypedBoolMatchArm { pattern, body }
                        })
                        .collect();

                    TypedExpr::BoolMatch {
                        subject,
                        arms,
                        kind: result_type,
                    }
                }

                Type::Option(_) => {
                    let arms = cases
                        .iter()
                        .map(|case| {
                            let pattern = match &case.constructor {
                                Constructor::OptionSome => {
                                    let binding = case.arguments.first().map(|var| {
                                        (
                                            VarName::new(&var.name).expect("invalid variable name"),
                                            var.typ.clone(),
                                        )
                                    });
                                    TypedOptionPattern::Some { binding }
                                }
                                Constructor::OptionNone => TypedOptionPattern::None,
                                _ => unreachable!("Invalid constructor for Option type"),
                            };
                            let body = decision_to_typed_expr(
                                &case.body,
                                typed_bodies,
                                result_type.clone(),
                                subject_expr.clone(),
                            );
                            TypedOptionMatchArm { pattern, body }
                        })
                        .collect();

                    TypedExpr::OptionMatch {
                        subject,
                        arms,
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
                                } => TypedEnumPattern::Variant {
                                    enum_name: enum_name.to_string(),
                                    variant_name: variant_name.clone(),
                                },
                                _ => unreachable!("Invalid constructor for Enum type"),
                            };
                            let body = decision_to_typed_expr(
                                &case.body,
                                typed_bodies,
                                result_type.clone(),
                                subject_expr.clone(),
                            );
                            TypedEnumMatchArm { pattern, body }
                        })
                        .collect();

                    TypedExpr::EnumMatch {
                        subject,
                        arms,
                        kind: result_type,
                    }
                }

                _ => panic!("Unsupported type for pattern matching: {:?}", var.typ),
            }
        }
    }
}

/// Compile patterns using pat_match and check for exhaustiveness and redundancy.
fn compile_and_check_patterns(
    arms: &[ParsedMatchArm],
    subject_type: &Type,
    match_range: &DocumentRange,
    type_env: &mut Environment<Type>,
) -> Result<Match, TypeError> {
    let subject_var = Variable::new(SUBJECT_VAR_NAME.to_string(), subject_type.clone());

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

    let mut pat_var_env: Environment<Type> = Environment::new();
    let _ = pat_var_env.push(subject_var.name.clone(), subject_type.clone());

    let result = Compiler::new().compile(rows, type_env, &mut pat_var_env);

    let unreachable = result.diagnostics.unreachable(arms.len());
    if let Some(&first_unreachable) = unreachable.first() {
        let arm = &arms[first_unreachable];
        let variant_name = match &arm.pattern {
            ParsedMatchPattern::Constructor { constructor, .. } => constructor.to_string(),
            ParsedMatchPattern::Wildcard { .. } => "_".to_string(),
            ParsedMatchPattern::Binding { name, .. } => name.clone(),
        };
        return Err(TypeError::MatchUnreachableArm {
            variant: variant_name,
            range: arm.pattern.range().clone(),
        });
    }

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

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use super::typecheck_match;
    use crate::document::DocumentAnnotator;
    use crate::dop::semantics::r#type::Type;
    use crate::dop::semantics::type_checker::resolve_type;
    use crate::dop::symbols::type_name::TypeName;
    use crate::dop::syntax::parsed::ParsedExpr;
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

        let actual = match expr {
            ParsedExpr::Match {
                subject,
                arms,
                range,
            } => match typecheck_match(
                &subject,
                &arms,
                &range,
                &mut env,
                &mut type_env,
                &mut annotations,
            ) {
                Ok(typed_expr) => format!("{}\n", typed_expr.to_doc().pretty(20)),
                Err(e) => DocumentAnnotator::new()
                    .with_label("error")
                    .without_location()
                    .without_line_numbers()
                    .annotate(None, [e]),
            },
            _ => panic!("Expected a match expression, got: {:?}", expr),
        };

        expected.assert_eq(&actual);
    }

    ///////////////////////////////////////////////////////////////////////////
    /// EMPTY MATCH                                                         ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_reject_match_with_no_arms() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                }
            "#},
            expect![[r#"
                error: Match expression must have at least one arm
                match flag {
                ^^^^^^^^^^^^
                }
                ^
            "#]],
        );
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
            expect![[r#"
                match color {
                  Color::Red => "red",
                  Color::Green => "green",
                  Color::Blue => "blue",
                }
            "#]],
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
            expect![[r#"
                match size {
                  Size::Small => 1,
                  Size::Medium => 2,
                  Size::Large => 3,
                }
            "#]],
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
            expect![[r#"
                match status {
                  Status::Active => true,
                  Status::Inactive => false,
                }
            "#]],
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
                    Color::Red => 0,
                    Color::Red => 1,
                    Color::Green => 2,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant 'Color::Red'
                    Color::Red => 1,
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
                    Color::Red => 0,
                    Size::Small => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern enum 'Size' does not match subject enum 'Color'
                    Size::Small => 1,
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
                    Color::Red => 0,
                    Color::Yellow => 1,
                }
            "#},
            expect![[r#"
                error: Variant 'Yellow' is not defined in enum 'Color'
                    Color::Yellow => 1,
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
            expect![[r#"
                match user.status {
                  Status::Active => "active",
                  Status::Inactive => "inactive",
                }
            "#]],
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
                    Status::Active => user.name + user.name,
                    Status::Inactive => user.name + user.name,
                }
            "},
            expect![[r#"
                match user.status {
                  Status::Active => (user.name + user.name),
                  Status::Inactive => (user.name + user.name),
                }
            "#]],
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
            expect![[r#"
                match unit {
                  Unit::Value => "value",
                }
            "#]],
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
                    Color::Red => 0,
                    _ => 1,
                }
            "#},
            expect![[r#"
                match color {
                  Color::Red => 0,
                  Color::Green => 1,
                  Color::Blue => 1,
                }
            "#]],
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
            expect![[r#"
                "any color"
            "#]],
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
            expect![[r#"
                match status {
                  Status::Active => 1,
                  Status::Inactive => 2,
                  Status::Pending => 0,
                  Status::Archived => 0,
                }
            "#]],
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
                error: Unreachable match arm for variant 'Color::Blue'
                    Color::Blue => "blue",
                    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_match_with_multiple_wildcards() {
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
                    _ => "first",
                    _ => "second",
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant '_'
                    _ => "second",
                    ^
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
                    true => 1,
                    false => 0,
                }
            "#},
            expect![[r#"
                match flag {
                  false => 0,
                  true => 1,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_boolean_match_with_wildcard() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true => 1,
                    _ => 0,
                }
            "#},
            expect![[r#"
                match flag {
                  false => 0,
                  true => 1,
                }
            "#]],
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
            expect![[r#"
                "always"
            "#]],
        );
    }

    #[test]
    fn should_reject_boolean_match_missing_false() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true => 1,
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'false'
                match flag {
                ^^^^^^^^^^^^
                    true => 1,
                ^^^^^^^^^^^^^^
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
                    true => 0,
                    true => 1,
                    false => 2,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant 'true'
                    true => 1,
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
                    true => 0,
                    Color::Green => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected enum, found true
                    true => 0,
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
                    Some(_) => 0,
                    None    => 1,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => 0,
                  None => 1,
                }
            "#]],
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
            expect![[r#"
                match opt {
                  Some(v0) => 1,
                  None => 0,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_option_match_with_wildcard() {
        check(
            "",
            &[("opt", "Option[Bool]")],
            indoc! {r#"
                match opt {
                    Some(_) => 0,
                    _       => 1,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => 0,
                  None => 1,
                }
            "#]],
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
            expect![[r#"
                "always this"
            "#]],
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
                    Some(_) => 1,
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'None'
                match opt {
                ^^^^^^^^^^^
                    Some(_) => 1,
                ^^^^^^^^^^^^^^^^^
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
                    Some(Some(_)) => 0,
                    None          => 1,
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Some(None)'
                match opt {
                ^^^^^^^^^^^
                    Some(Some(_)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^
                    None          => 1,
                ^^^^^^^^^^^^^^^^^^^^^^^
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
                    Some(Some(false)) => 0,
                    Some(None)        => 1,
                    None              => 2,
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Some(Some(true))'
                match opt {
                ^^^^^^^^^^^
                    Some(Some(false)) => 0,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Some(None)        => 1,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    None              => 2,
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
                    Some(Some(_)) => 0,
                    Some(None)    => 1,
                    None          => 2,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => match v0 {
                    Some(v1) => 0,
                    None => 1,
                  },
                  None => 2,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_exhaustive_nested_option_match_with_literal_subject() {
        check(
            "",
            &[],
            indoc! {r#"
                match Some(Some(10)) {
                    Some(Some(_)) => 0,
                    Some(None)    => 1,
                    None          => 2,
                }
            "#},
            expect![[r#"
                match Some(Some(10)) {
                  Some(v0) => match v0 {
                    Some(v1) => 0,
                    None => 1,
                  },
                  None => 2,
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_option_match_duplicate_some() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(_) => 0,
                    Some(_) => 1,
                    None    => 2,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant 'Some'
                    Some(_) => 1,
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
                    Some(_) => 0,
                    None    => 1,
                    None    => 2,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant 'None'
                    None    => 2,
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
                    Color::Red => 0,
                    None       => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected option, found Color::Red
                    Color::Red => 0,
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
                    true => 0,
                    None => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected option, found true
                    true => 0,
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
                    Some(_)      => 0,
                    Color::Green => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected enum, found Some(_)
                    Some(_)      => 0,
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
                    Some(_) => 0,
                    false   => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Some(_)
                    Some(_) => 0,
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
                    Some(None) => 0,
                    None       => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found None
                    Some(None) => 0,
                         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_option_match_with_nested_enum() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("opt", "Option[Color]")],
            indoc! {r#"
                match opt {
                    Some(Color::Red)   => "red",
                    Some(Color::Green) => "green",
                    Some(Color::Blue)  => "blue",
                    None               => "none",
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => match v0 {
                    Color::Red => "red",
                    Color::Green => "green",
                    Color::Blue => "blue",
                  },
                  None => "none",
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_non_exhaustive_option_match_with_nested_enum() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("opt", "Option[Color]")],
            indoc! {r#"
                match opt {
                    Some(Color::Red)   => "red",
                    Some(Color::Green) => "green",
                    None               => "none",
                }
            "#},
            expect![[r#"
                error: Match expression is missing arm for variant 'Some(Blue)'
                match opt {
                ^^^^^^^^^^^
                    Some(Color::Red)   => "red",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    Some(Color::Green) => "green",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    None               => "none",
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                }
                ^
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
                    Some(Some(_)) => 0,
                    None          => 1,
                }
            "#},
            expect![[r#"
                error: Match pattern type mismatch: expected boolean, found Some(_)
                    Some(Some(_)) => 0,
                         ^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// BINDING PATTERN                                                     ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_binding_pattern_in_bool_match() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    x => x,
                }
            "#},
            expect![[r#"
                let x = flag in x
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_pattern_in_enum_match() {
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
                    x => x,
                }
            "#},
            expect![[r#"
                let x = color in x
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_pattern_in_option_match() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    x => x,
                }
            "#},
            expect![[r#"
                let x = opt in x
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_pattern_inside_some() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(x) => x,
                    None    => 0,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => let x = v0 in x,
                  None => 0,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_pattern_in_arithmetic() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(x) => x + 1,
                    None    => 0,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => let x = v0 in (x + 1),
                  None => 0,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_pattern_returning_option() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(x) => Some(x + 1),
                    None    => None,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => let x = v0 in Some((x + 1)),
                  None => None,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_pattern_as_catchall() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    true  => false,
                    other => other,
                }
            "#},
            expect![[r#"
                match flag {
                  false => let other = flag in other,
                  true => false,
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_unreachable_binding_pattern() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    x => x,
                    y => y,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant 'y'
                    y => y,
                    ^
            "#]],
        );
    }

    #[test]
    fn should_reject_pattern_after_binding() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    x    => x,
                    true => true,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for variant 'true'
                    true => true,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unused_binding() {
        check(
            "",
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    x => 42,
                }
            "#},
            expect![[r#"
                error: Unused binding 'x' in match arm
                    x => 42,
                    ^
            "#]],
        );
    }

    #[test]
    fn should_reject_unused_binding_inside_some() {
        check(
            "",
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Some(x) => 0,
                    None    => 1,
                }
            "#},
            expect![[r#"
                error: Unused binding 'x' in match arm
                    Some(x) => 0,
                         ^
            "#]],
        );
    }

    #[test]
    fn should_accept_binding_inside_nested_some() {
        check(
            "",
            &[("opt", "Option[Option[Int]]")],
            indoc! {r#"
                match opt {
                    Some(Some(x)) => x,
                    Some(None)    => 0,
                    None          => 0,
                }
            "#},
            expect![[r#"
                match opt {
                  Some(v0) => match v0 {
                    Some(v1) => let x = v1 in x,
                    None => 0,
                  },
                  None => 0,
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_unused_binding_inside_nested_some() {
        check(
            "",
            &[("opt", "Option[Option[Int]]")],
            indoc! {r#"
                match opt {
                    Some(Some(x)) => 0,
                    Some(None)    => 1,
                    None          => 2,
                }
            "#},
            expect![[r#"
                error: Unused binding 'x' in match arm
                    Some(Some(x)) => 0,
                              ^
            "#]],
        );
    }
}
