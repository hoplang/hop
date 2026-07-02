use std::sync::Arc;

use pretty::BoxDoc;

use crate::document::DocumentRange;
use crate::expr::parsing::parsed_expr::{Constructor, ParsedMatchPattern};
use crate::expr::typing::r#type::{NamedKind, Type};
use crate::expr::typing::type_registry::TypeRegistry;
use crate::symbols::field_name::FieldName;
use crate::symbols::var_name::VarName;
use crate::type_error::{TypeError, TypeErrorKind};

#[derive(Debug, Clone)]
pub enum TypedMatchPattern {
    Wildcard {
        range: DocumentRange,
    },
    Binding {
        name: VarName,
        typ: Arc<Type>,
        range: DocumentRange,
    },
    Constructor {
        constructor: Constructor,
        typ: Arc<Type>,
        args: Vec<TypedMatchPattern>,
        fields: Vec<TypedField>,
        range: DocumentRange,
    },
}

impl TypedMatchPattern {
    pub fn range(&self) -> &DocumentRange {
        match self {
            TypedMatchPattern::Wildcard { range, .. }
            | TypedMatchPattern::Binding { range, .. }
            | TypedMatchPattern::Constructor { range, .. } => range,
        }
    }

    /// Extract the binding variables introduced by this pattern with their types
    /// and ranges.
    pub fn bindings(&self) -> Vec<(VarName, Arc<Type>, DocumentRange)> {
        match self {
            TypedMatchPattern::Binding { name, typ, range } => {
                vec![(name.clone(), typ.clone(), range.clone())]
            }
            TypedMatchPattern::Wildcard { .. } => vec![],
            TypedMatchPattern::Constructor { args, fields, .. } => {
                let mut bindings = Vec::new();
                for arg in args {
                    bindings.extend(arg.bindings());
                }
                for field in fields {
                    bindings.extend(field.pattern.bindings());
                }
                bindings
            }
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedMatchPattern::Constructor {
                constructor,
                args,
                fields,
                ..
            } => {
                let base = constructor.to_doc();
                if !fields.is_empty() {
                    // Record pattern: User {name: x, age: y}
                    let fields_doc = BoxDoc::intersperse(
                        fields.iter().map(|field| {
                            if let TypedMatchPattern::Binding { name: var_name, .. } =
                                &field.pattern
                            {
                                if var_name.as_str() == field.name.as_str() {
                                    return BoxDoc::text(field.name.as_str());
                                }
                            }
                            BoxDoc::text(field.name.as_str())
                                .append(BoxDoc::text(": "))
                                .append(field.pattern.to_doc())
                        }),
                        BoxDoc::text(", "),
                    );
                    base.append(BoxDoc::text("{"))
                        .append(fields_doc)
                        .append(BoxDoc::text("}"))
                } else if args.is_empty() {
                    base
                } else {
                    // Positional args (Option Some, etc.)
                    let args_doc =
                        BoxDoc::intersperse(args.iter().map(|a| a.to_doc()), BoxDoc::text(", "));
                    base.append(BoxDoc::text("("))
                        .append(args_doc)
                        .append(BoxDoc::text(")"))
                }
            }
            TypedMatchPattern::Wildcard { .. } => BoxDoc::text("_"),
            TypedMatchPattern::Binding { name, .. } => BoxDoc::text(name.as_str()),
        }
    }
}

impl std::fmt::Display for TypedMatchPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}

#[derive(Debug, Clone)]
pub struct TypedField {
    pub name: FieldName,
    pub index: usize,
    pub pattern: TypedMatchPattern,
}

/// Validate a parsed match pattern against the subject type and produce the
/// corresponding `TypedMatchPattern`.
pub fn typecheck_pattern(
    parsed: &ParsedMatchPattern,
    subject_type: Arc<Type>,
    registry: &TypeRegistry,
) -> Result<TypedMatchPattern, TypeError> {
    match parsed {
        ParsedMatchPattern::Wildcard { range } => Ok(TypedMatchPattern::Wildcard {
            range: range.clone(),
        }),
        ParsedMatchPattern::Binding { name, range } => Ok(TypedMatchPattern::Binding {
            name: name.clone(),
            typ: subject_type,
            range: range.clone(),
        }),
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            fields,
            constructor_range,
            range,
            ..
        } => match (constructor, subject_type.as_ref()) {
            (Constructor::BooleanTrue | Constructor::BooleanFalse, Type::Bool) => {
                Ok(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: Vec::new(),
                    fields: Vec::new(),
                    range: range.clone(),
                })
            }

            (Constructor::OptionSome, Type::Option(inner_type)) => {
                let mut typed_args = Vec::new();
                if let Some(inner_pattern) = args.first() {
                    typed_args.push(typecheck_pattern(
                        inner_pattern,
                        inner_type.clone(),
                        registry,
                    )?);
                }
                Ok(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: typed_args,
                    fields: Vec::new(),
                    range: range.clone(),
                })
            }

            (Constructor::OptionNone, Type::Option(_)) => Ok(TypedMatchPattern::Constructor {
                constructor: constructor.clone(),
                typ: subject_type.clone(),
                args: Vec::new(),
                fields: Vec::new(),
                range: range.clone(),
            }),

            (
                Constructor::EnumVariant {
                    enum_name: pattern_enum_name,
                    variant_name: pattern_variant_name,
                },
                Type::Named {
                    module,
                    name: subject_enum_name,
                    kind: NamedKind::Enum,
                },
            ) => {
                if pattern_enum_name != subject_enum_name {
                    return Err(TypeError::new(
                        TypeErrorKind::MatchPatternEnumMismatch {
                            pattern_enum: pattern_enum_name.clone(),
                            subject_enum: subject_enum_name.clone(),
                        },
                        range.clone(),
                    ));
                }

                let variant_fields = registry.variant_fields(
                    module,
                    subject_enum_name,
                    pattern_variant_name.as_str(),
                );

                let Some(variant_fields) = variant_fields else {
                    return Err(TypeError::new(
                        TypeErrorKind::UndefinedEnumVariant {
                            enum_name: pattern_enum_name.clone(),
                            variant_name: pattern_variant_name.clone(),
                        },
                        range.clone(),
                    ));
                };

                let mut typed_fields = Vec::new();
                for (field_name, field_name_range, field_pattern) in fields {
                    let found = variant_fields
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _, _))| name == field_name);

                    match found {
                        Some((index, (_, typ, _))) => {
                            typed_fields.push(TypedField {
                                name: field_name.clone(),
                                index,
                                pattern: typecheck_pattern(field_pattern, typ.clone(), registry)?,
                            });
                        }
                        None => {
                            return Err(TypeError::new(
                                TypeErrorKind::EnumVariantUnknownField {
                                    enum_name: pattern_enum_name.clone(),
                                    variant_name: pattern_variant_name.clone(),
                                    field_name: field_name.clone(),
                                },
                                field_name_range.clone(),
                            ));
                        }
                    }
                }

                if fields.len() < variant_fields.len() {
                    let pattern_field_names: Vec<_> =
                        fields.iter().map(|(name, _, _)| name).collect();
                    let missing_fields = variant_fields
                        .iter()
                        .filter(|(name, _, _)| !pattern_field_names.contains(&name))
                        .map(|(name, _, _)| name.clone())
                        .collect::<Vec<_>>();
                    return Err(TypeError::new(
                        TypeErrorKind::EnumVariantMissingFields {
                            enum_name: pattern_enum_name.clone(),
                            variant_name: pattern_variant_name.clone(),
                            missing_fields,
                        },
                        constructor_range.clone(),
                    ));
                }

                Ok(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: Vec::new(),
                    fields: typed_fields,
                    range: range.clone(),
                })
            }

            (
                Constructor::Record {
                    type_name: pattern_type_name,
                },
                Type::Named {
                    module,
                    name: subject_type_name,
                    kind: NamedKind::Record,
                },
            ) => {
                if pattern_type_name != subject_type_name {
                    return Err(TypeError::new(
                        TypeErrorKind::MatchPatternRecordMismatch {
                            pattern_record: pattern_type_name.clone(),
                            subject_record: subject_type_name.clone(),
                        },
                        range.clone(),
                    ));
                }

                let subject_fields = registry
                    .record_fields(module, subject_type_name)
                    .expect("record type must be registered");

                let mut typed_fields = Vec::new();
                for (field_name, field_name_range, field_pattern) in fields {
                    let found = subject_fields
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _, _))| name == field_name);

                    match found {
                        Some((index, (_, typ, _))) => {
                            typed_fields.push(TypedField {
                                name: field_name.clone(),
                                index,
                                pattern: typecheck_pattern(field_pattern, typ.clone(), registry)?,
                            });
                        }
                        None => {
                            return Err(TypeError::new(
                                TypeErrorKind::RecordUnknownField {
                                    field_name: field_name.clone(),
                                    record_name: pattern_type_name.clone(),
                                },
                                field_name_range.clone(),
                            ));
                        }
                    }
                }

                if fields.len() < subject_fields.len() {
                    let pattern_field_names =
                        fields.iter().map(|(name, _, _)| name).collect::<Vec<_>>();
                    let missing_fields = subject_fields
                        .iter()
                        .filter(|(name, _, _)| !pattern_field_names.contains(&name))
                        .map(|(name, _, _)| name.clone())
                        .collect::<Vec<_>>();
                    return Err(TypeError::new(
                        TypeErrorKind::RecordMissingFields {
                            record_name: pattern_type_name.clone(),
                            missing_fields,
                        },
                        constructor_range.clone(),
                    ));
                }

                Ok(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: Vec::new(),
                    fields: typed_fields,
                    range: range.clone(),
                })
            }

            _ => Err(TypeError::new(
                TypeErrorKind::MatchPatternTypeMismatch {
                    expected: subject_type.clone(),
                    found: parsed.to_string(),
                },
                range.clone(),
            )),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentCursor;
    use crate::document_annotator::DocumentAnnotator;
    use crate::expr::parse_expr;
    use crate::expr::parsing::parsed_expr::ParsedExpr;
    use crate::expr::typing::type_registry_builder::TypeRegistryBuilder;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use std::collections::VecDeque;

    // Parse a match expression and run `typecheck_pattern` on each arm pattern.
    // Returns the rendered typed patterns on success, or the rendered error.
    fn run_check(types: TypeRegistryBuilder, subject: &str, expr_str: &str) -> (String, bool) {
        let types = types.build();
        let subject_type = types.resolve(subject);
        let cursor = DocumentCursor::new(types.module().clone(), expr_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = Vec::new();
        let expr = parse_expr::parse_expr(&mut iter, &mut comments, &mut errors, &range)
            .expect("Failed to parse expression");

        let patterns = match expr {
            ParsedExpr::Match { arms, .. } => {
                arms.into_iter().map(|a| a.pattern).collect::<Vec<_>>()
            }
            _ => panic!("Expected match expression"),
        };

        let result = patterns
            .iter()
            .map(|p| typecheck_pattern(p, subject_type.clone(), types.registry()))
            .collect::<Result<Vec<_>, _>>();
        match result {
            Ok(typed) => (
                typed
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
                true,
            ),
            Err(e) => (
                DocumentAnnotator::new()
                    .with_label("error")
                    .without_location()
                    .without_line_numbers()
                    .annotate(types.module(), [e])
                    .render(),
                false,
            ),
        }
    }

    fn reject(types: TypeRegistryBuilder, subject: &str, expr_str: &str, expected: Expect) {
        let (actual, ok) = run_check(types, subject, expr_str);
        if ok {
            panic!("expected a typecheck error, but patterns typechecked to:\n{actual}");
        }
        expected.assert_eq(&actual);
    }

    #[test]
    fn rejects_enum_variant_unknown_field() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Outcome",
                [
                    ("Success", vec![("value", "Int")]),
                    ("Failure", vec![("message", "String")]),
                ],
            ),
            "Outcome",
            indoc! {"
                match x {
                    Outcome::Success{unknown: v} => 0,
                    Outcome::Failure{message: _} => 1,
                }
            "},
            expect![[r#"
                error: Unknown field 'unknown' in enum variant 'Outcome::Success'
                    Outcome::Success{unknown: v} => 0,
                                     ^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_unknown_field_after_valid_field() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            indoc! {"
                match x {
                    Point::XY{x: a, unknown: b} => 0,
                }
            "},
            expect![[r#"
                error: Unknown field 'unknown' in enum variant 'Point::XY'
                    Point::XY{x: a, unknown: b} => 0,
                                    ^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_two_unknown_fields() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            indoc! {"
                match x {
                    Point::XY{foo: a, bar: b} => 0,
                }
            "},
            expect![[r#"
                error: Unknown field 'foo' in enum variant 'Point::XY'
                    Point::XY{foo: a, bar: b} => 0,
                              ^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_missing_field_in_pattern() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            indoc! {"
                match x {
                    Point::XY{x: a} => 0,
                }
            "},
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: y
                    Point::XY{x: a} => 0,
                    ^^^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_missing_two_fields_in_pattern() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Point",
                [("XYZ", vec![("x", "Int"), ("y", "Int"), ("z", "Int")])],
            ),
            "Point",
            indoc! {"
                match x {
                    Point::XYZ{x: a} => 0,
                }
            "},
            expect![[r#"
                error: Enum variant 'Point::XYZ' is missing fields: y, z
                    Point::XYZ{x: a} => 0,
                    ^^^^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_no_parens_when_fields_expected() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            indoc! {"
                match x {
                    Point::XY => 0,
                }
            "},
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: x, y
                    Point::XY => 0,
                    ^^^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_fields_provided_to_unit_variant() {
        reject(
            TypeRegistryBuilder::new().enum_(
                "Maybe",
                [("Just", vec![("value", "Int")]), ("Nothing", vec![])],
            ),
            "Maybe",
            indoc! {"
                match x {
                    Maybe::Just{value: v} => 0,
                    Maybe::Nothing{value: v} => 1,
                }
            "},
            expect![[r#"
                error: Unknown field 'value' in enum variant 'Maybe::Nothing'
                    Maybe::Nothing{value: v} => 1,
                                   ^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_undefined_enum_variant() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            indoc! {"
                match x {
                    Color::Blue => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Variant 'Blue' is not defined in enum 'Color'
                    Color::Blue => 0,
                    ^^^^^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_record_missing_fields() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            indoc! {"
                match x {
                    User{name: n} => 0,
                }
            "},
            expect![[r#"
                error: Record 'User' is missing fields: age
                    User{name: n} => 0,
                    ^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_record_unknown_field() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String")]),
            "User",
            indoc! {"
                match x {
                    User{email: e} => 0,
                }
            "},
            expect![[r#"
                error: Unknown field 'email' in record 'User'
                    User{email: e} => 0,
                         ^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_boolean_pattern_on_enum() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            indoc! {"
                match x {
                    true => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Mismatched pattern type: expected `test::Color` got `true`
                    true => 0,
                    ^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_option_pattern_on_enum() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            indoc! {"
                match x {
                    Some(v) => 0,
                    None => 1,
                }
            "},
            expect![[r#"
                error: Mismatched pattern type: expected `test::Color` got `Some(v)`
                    Some(v) => 0,
                    ^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_nested_option_wrong_inner_type() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[Bool]",
            indoc! {"
                match x {
                    Some(Some(v)) => 0,
                    _ => 1,
                }
            "},
            expect![[r#"
                error: Mismatched pattern type: expected `Bool` got `Some(v)`
                    Some(Some(v)) => 0,
                         ^^^^^^^
            "#]],
        );
    }
}
