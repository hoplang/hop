use crate::hop::parsing::parsed_expr::{Constructor, ParsedMatchPattern};
use crate::hop::patterns::typed::{TypedField, TypedMatchPattern};
use crate::hop::typing::r#type::Type;
use crate::hop::typing::type_registry::{ResolvedType, TypeRegistry};
use crate::type_error::{TypeError, TypeErrorKind};

pub fn typecheck_pattern(
    parsed: &ParsedMatchPattern,
    subject_type: Type,
    registry: &TypeRegistry,
    errors: &mut Vec<TypeError>,
) -> Option<TypedMatchPattern> {
    match parsed {
        ParsedMatchPattern::Wildcard { range } => Some(TypedMatchPattern::Wildcard {
            range: range.clone(),
        }),
        ParsedMatchPattern::Binding { name, range } => Some(TypedMatchPattern::Binding {
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
        } => match (constructor, registry.resolve(&subject_type)) {
            (Constructor::BooleanTrue | Constructor::BooleanFalse, Some(ResolvedType::Bool)) => {
                Some(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: Vec::new(),
                    fields: Vec::new(),
                    range: range.clone(),
                })
            }

            (Constructor::OptionSome, Some(ResolvedType::Option(inner_type))) => {
                let mut typed_args = Vec::new();
                if let Some(inner_pattern) = args.first() {
                    typed_args.push(typecheck_pattern(
                        inner_pattern,
                        inner_type.clone(),
                        registry,
                        errors,
                    )?);
                }
                Some(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: typed_args,
                    fields: Vec::new(),
                    range: range.clone(),
                })
            }

            (Constructor::OptionNone, Some(ResolvedType::Option(_))) => {
                Some(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: Vec::new(),
                    fields: Vec::new(),
                    range: range.clone(),
                })
            }

            (
                Constructor::EnumVariant {
                    enum_name: pattern_enum_name,
                    variant_name: pattern_variant_name,
                },
                Some(ResolvedType::Enum {
                    name: subject_enum_name,
                    variants,
                    ..
                }),
            ) => {
                if pattern_enum_name != subject_enum_name {
                    errors.push(TypeError::new(
                        TypeErrorKind::MatchPatternEnumMismatch {
                            pattern_enum: pattern_enum_name.clone(),
                            subject_enum: subject_enum_name.clone(),
                        },
                        range.clone(),
                    ));
                    return None;
                }

                let variant_fields = variants
                    .iter()
                    .find(|variant| variant.name.as_str() == pattern_variant_name.as_str())
                    .map(|variant| variant.fields.as_slice());

                let Some(variant_fields) = variant_fields else {
                    errors.push(TypeError::new(
                        TypeErrorKind::UndefinedEnumVariant {
                            enum_name: pattern_enum_name.clone(),
                            variant_name: pattern_variant_name.clone(),
                        },
                        range.clone(),
                    ));
                    return None;
                };

                let mut typed_fields: Vec<TypedField> = Vec::new();
                for (field_name, field_name_range, field_pattern) in fields {
                    let found = variant_fields
                        .iter()
                        .enumerate()
                        .find(|(_, f)| &f.name == field_name);

                    match found {
                        Some(_) if typed_fields.iter().any(|f| &f.name == field_name) => {
                            errors.push(TypeError::new(
                                TypeErrorKind::EnumVariantDuplicateField {
                                    enum_name: pattern_enum_name.clone(),
                                    variant_name: pattern_variant_name.clone(),
                                    field_name: field_name.clone(),
                                },
                                field_name_range.clone(),
                            ));
                            return None;
                        }
                        Some((index, field)) => {
                            typed_fields.push(TypedField {
                                name: field_name.clone(),
                                index,
                                pattern: typecheck_pattern(
                                    field_pattern,
                                    field.typ.clone(),
                                    registry,
                                    errors,
                                )?,
                            });
                        }
                        None => {
                            errors.push(TypeError::new(
                                TypeErrorKind::EnumVariantUnknownField {
                                    enum_name: pattern_enum_name.clone(),
                                    variant_name: pattern_variant_name.clone(),
                                    field_name: field_name.clone(),
                                },
                                field_name_range.clone(),
                            ));
                            return None;
                        }
                    }
                }

                if fields.len() < variant_fields.len() {
                    let pattern_field_names: Vec<_> =
                        fields.iter().map(|(name, _, _)| name).collect();
                    let missing_fields = variant_fields
                        .iter()
                        .filter(|f| !pattern_field_names.contains(&&f.name))
                        .map(|f| f.name.clone())
                        .collect::<Vec<_>>();
                    errors.push(TypeError::new(
                        TypeErrorKind::EnumVariantMissingFields {
                            enum_name: pattern_enum_name.clone(),
                            variant_name: pattern_variant_name.clone(),
                            missing_fields,
                        },
                        constructor_range.clone(),
                    ));
                    return None;
                }

                Some(TypedMatchPattern::Constructor {
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
                Some(ResolvedType::Record {
                    name: subject_type_name,
                    fields: subject_fields,
                    ..
                }),
            ) => {
                if pattern_type_name != subject_type_name {
                    errors.push(TypeError::new(
                        TypeErrorKind::MatchPatternRecordMismatch {
                            pattern_record: pattern_type_name.clone(),
                            subject_record: subject_type_name.clone(),
                        },
                        range.clone(),
                    ));
                    return None;
                }

                let mut typed_fields: Vec<TypedField> = Vec::new();
                for (field_name, field_name_range, field_pattern) in fields {
                    let found = subject_fields
                        .iter()
                        .enumerate()
                        .find(|(_, f)| &f.name == field_name);

                    match found {
                        Some(_) if typed_fields.iter().any(|f| &f.name == field_name) => {
                            errors.push(TypeError::new(
                                TypeErrorKind::RecordDuplicateField {
                                    field_name: field_name.clone(),
                                    record_name: pattern_type_name.clone(),
                                },
                                field_name_range.clone(),
                            ));
                            return None;
                        }
                        Some((index, field)) => {
                            typed_fields.push(TypedField {
                                name: field_name.clone(),
                                index,
                                pattern: typecheck_pattern(
                                    field_pattern,
                                    field.typ.clone(),
                                    registry,
                                    errors,
                                )?,
                            });
                        }
                        None => {
                            errors.push(TypeError::new(
                                TypeErrorKind::RecordUnknownField {
                                    field_name: field_name.clone(),
                                    record_name: pattern_type_name.clone(),
                                },
                                field_name_range.clone(),
                            ));
                            return None;
                        }
                    }
                }

                if fields.len() < subject_fields.len() {
                    let pattern_field_names =
                        fields.iter().map(|(name, _, _)| name).collect::<Vec<_>>();
                    let missing_fields = subject_fields
                        .iter()
                        .filter(|f| !pattern_field_names.contains(&&f.name))
                        .map(|f| f.name.clone())
                        .collect::<Vec<_>>();
                    errors.push(TypeError::new(
                        TypeErrorKind::RecordMissingFields {
                            record_name: pattern_type_name.clone(),
                            missing_fields,
                        },
                        constructor_range.clone(),
                    ));
                    return None;
                }

                Some(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: Vec::new(),
                    fields: typed_fields,
                    range: range.clone(),
                })
            }

            (Constructor::Tuple, Some(ResolvedType::Tuple(elements)))
                if args.len() == elements.len() =>
            {
                let typed_args = args
                    .iter()
                    .zip(elements)
                    .map(|(arg, element)| typecheck_pattern(arg, element.clone(), registry, errors))
                    .collect::<Option<Vec<_>>>()?;
                Some(TypedMatchPattern::Constructor {
                    constructor: constructor.clone(),
                    typ: subject_type.clone(),
                    args: typed_args,
                    fields: Vec::new(),
                    range: range.clone(),
                })
            }

            _ => {
                errors.push(TypeError::new(
                    TypeErrorKind::MatchPatternTypeMismatch {
                        expected: subject_type.clone(),
                        found: parsed.to_string(),
                    },
                    range.clone(),
                ));
                None
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentCursor;
    use crate::document_annotator::DocumentAnnotator;
    use crate::hop::parsing::parse_expr::parse_match_pattern;
    use crate::hop::typing::type_registry_builder::TypeRegistryBuilder;
    use expect_test::{Expect, expect};

    fn reject(types: TypeRegistryBuilder, subject: &str, pattern_str: &str, expected: Expect) {
        let types = types.build();
        let subject_type = types.resolve(subject);
        let mut iter = DocumentCursor::new(types.module().clone(), pattern_str.to_string());
        let mut comments = Vec::new();
        let mut errors = Vec::new();
        let parsed = parse_match_pattern(&mut iter, &mut comments, &mut errors);
        let Ok(parsed) = parsed else {
            panic!("failed to parse pattern `{pattern_str}`: {errors:?}");
        };
        if !errors.is_empty() {
            panic!("failed to parse pattern `{pattern_str}`: {errors:?}");
        }
        if iter.peek().is_some() {
            panic!("trailing input after pattern `{pattern_str}`");
        }

        let mut type_errors = Vec::new();
        if let Some(typed) =
            typecheck_pattern(&parsed, subject_type, types.registry(), &mut type_errors)
        {
            panic!("expected a typecheck error, but pattern typechecked to:\n{typed}");
        }
        let actual = DocumentAnnotator::new()
            .with_severity_label()
            .without_location()
            .without_line_numbers()
            .annotate(type_errors.iter().map(|e| e.to_diagnostic()))
            .render();
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
            "Outcome::Success{unknown: v}",
            expect![[r#"
                error: Unknown field 'unknown' in enum variant 'Outcome::Success'
                Outcome::Success{unknown: v}
                                 ^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_unknown_field_after_valid_field() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            "Point::XY{x: a, unknown: b}",
            expect![[r#"
                error: Unknown field 'unknown' in enum variant 'Point::XY'
                Point::XY{x: a, unknown: b}
                                ^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_two_unknown_fields() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            "Point::XY{foo: a, bar: b}",
            expect![[r#"
                error: Unknown field 'foo' in enum variant 'Point::XY'
                Point::XY{foo: a, bar: b}
                          ^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_missing_field_in_pattern() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            "Point::XY{x: a}",
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: y
                Point::XY{x: a}
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
            "Point::XYZ{x: a}",
            expect![[r#"
                error: Enum variant 'Point::XYZ' is missing fields: y, z
                Point::XYZ{x: a}
                ^^^^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_no_parens_when_fields_expected() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            "Point::XY",
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: x, y
                Point::XY
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
            "Maybe::Nothing{value: v}",
            expect![[r#"
                error: Unknown field 'value' in enum variant 'Maybe::Nothing'
                Maybe::Nothing{value: v}
                               ^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_undefined_enum_variant() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            "Color::Blue",
            expect![[r#"
                error: Variant 'Blue' is not defined in enum 'Color'
                Color::Blue
                ^^^^^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_record_missing_fields() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            "User{name: n}",
            expect![[r#"
                error: Record 'User' is missing fields: age
                User{name: n}
                ^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_record_duplicate_field_in_pattern() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String"), ("age", "Int")]),
            "User",
            "User{name: _, name: _}",
            expect![[r#"
                error: Duplicate field 'name' in record 'User'
                User{name: _, name: _}
                              ^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_enum_variant_duplicate_field_in_pattern() {
        reject(
            TypeRegistryBuilder::new().enum_("Point", [("XY", vec![("x", "Int"), ("y", "Int")])]),
            "Point",
            "Point::XY{x: _, x: _}",
            expect![[r#"
                error: Duplicate field 'x' in enum variant 'Point::XY'
                Point::XY{x: _, x: _}
                                ^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_record_unknown_field() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String")]),
            "User",
            "User{email: e}",
            expect![[r#"
                error: Unknown field 'email' in record 'User'
                User{email: e}
                     ^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_boolean_pattern_on_enum() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            "true",
            expect![[r#"
                error: Mismatched pattern type: expected Color got true
                true
                ^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_option_pattern_on_enum() {
        reject(
            TypeRegistryBuilder::new().enum_unit("Color", ["Red", "Green"]),
            "Color",
            "Some(v)",
            expect![[r#"
                error: Mismatched pattern type: expected Color got Some(v)
                Some(v)
                ^^^^^^^
            "#]],
        );
    }
    #[test]
    fn rejects_validation_nested_option_wrong_inner_type() {
        reject(
            TypeRegistryBuilder::new(),
            "Option[Bool]",
            "Some(Some(v))",
            expect![[r#"
                error: Mismatched pattern type: expected Bool got Some(v)
                Some(Some(v))
                     ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_pattern_with_too_many_elements() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool, Bool)",
            "(a, b, c)",
            expect![[r#"
                error: Mismatched pattern type: expected (Bool, Bool) got (a, b, c)
                (a, b, c)
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_pattern_with_too_few_elements() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool, Bool)",
            "(a,)",
            expect![[r#"
                error: Mismatched pattern type: expected (Bool, Bool) got (a,)
                (a,)
                ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_pattern_with_wrong_element_type() {
        reject(
            TypeRegistryBuilder::new(),
            "(Bool, Int)",
            "(true, None)",
            expect![[r#"
                error: Mismatched pattern type: expected Int got None
                (true, None)
                       ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_tuple_pattern_on_record() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String")]),
            "User",
            "(name,)",
            expect![[r#"
                error: Mismatched pattern type: expected User got (name,)
                (name,)
                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_pattern_on_tuple() {
        reject(
            TypeRegistryBuilder::new().record("User", [("name", "String")]),
            "(String,)",
            "User{name}",
            expect![[r#"
                error: Mismatched pattern type: expected (String,) got User{name}
                User{name}
                ^^^^^^^^^^
            "#]],
        );
    }
}
