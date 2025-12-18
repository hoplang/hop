use std::collections::HashSet;

use super::r#type::{NumericType, Type};
use super::type_error::TypeError;
use super::typed::{TypedEnumPattern, TypedMatchArm};
use crate::document::document_cursor::Ranged as _;
use crate::dop::TypedExpr;
use crate::dop::syntax::parsed::{ParsedBinaryOp, ParsedExpr, ParsedType};
use crate::environment::Environment;
use crate::hop::semantics::type_checker::TypeAnnotation;

/// Resolve a parsed Type to a semantic Type.
pub fn resolve_type(
    parsed_type: &ParsedType,
    type_env: &mut Environment<Type>,
) -> Result<Type, TypeError> {
    match parsed_type {
        ParsedType::String { .. } => Ok(Type::String),
        ParsedType::Bool { .. } => Ok(Type::Bool),
        ParsedType::Int { .. } => Ok(Type::Int),
        ParsedType::Float { .. } => Ok(Type::Float),
        ParsedType::TrustedHTML { .. } => Ok(Type::TrustedHTML),
        ParsedType::Array { element, .. } => {
            let elem_type = resolve_type(element, type_env)?;
            Ok(Type::Array(Box::new(elem_type)))
        }
        ParsedType::Named { name, range } => {
            let record_type = type_env
                .lookup(name)
                .ok_or_else(|| TypeError::UndefinedType {
                    type_name: name.clone(),
                    range: range.clone(),
                })?;
            Ok(record_type.clone())
        }
    }
}

/// Resolve a parsed Expr to a typed Expr.
///
/// The optional `expected_type` is used for bidirectional type checking, allowing
/// empty array literals to infer their element type from context.
pub fn typecheck_expr(
    parsed_expr: &ParsedExpr,
    env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    expected_type: Option<&Type>,
) -> Result<TypedExpr, TypeError> {
    match parsed_expr {
        ParsedExpr::Var { value: name, .. } => {
            if let Some(var_type) = env.lookup(name.as_str()) {
                annotations.push(TypeAnnotation {
                    range: parsed_expr.range().clone(),
                    typ: var_type.clone(),
                    name: name.to_string(),
                });
                Ok(TypedExpr::Var {
                    value: name.clone(),
                    kind: var_type.clone(),
                })
            } else {
                Err(TypeError::UndefinedVariable {
                    name: name.as_str().to_string(),
                    range: parsed_expr.range().clone(),
                })
            }
        }
        ParsedExpr::BooleanLiteral { value, .. } => Ok(TypedExpr::BooleanLiteral { value: *value }),
        ParsedExpr::StringLiteral { value, .. } => Ok(TypedExpr::StringLiteral {
            value: value.clone(),
        }),
        ParsedExpr::IntLiteral { value, .. } => Ok(TypedExpr::IntLiteral { value: *value }),
        ParsedExpr::FloatLiteral { value, .. } => Ok(TypedExpr::FloatLiteral { value: *value }),
        ParsedExpr::FieldAccess {
            record: base_expr,
            field,
            annotation: range,
            ..
        } => {
            let typed_base = typecheck_expr(base_expr, env, type_env, annotations, None)?;
            let base_type = typed_base.as_type();

            match &base_type {
                Type::Record {
                    name: record_name,
                    fields,
                    ..
                } => {
                    if let Some((_, field_type)) =
                        fields.iter().find(|(f, _)| f.as_str() == field.as_str())
                    {
                        Ok(TypedExpr::FieldAccess {
                            kind: field_type.clone(),
                            record: Box::new(typed_base),
                            field: field.clone(),
                        })
                    } else {
                        Err(TypeError::FieldNotFoundInRecord {
                            field: field.to_string(),
                            record_name: record_name.clone(),
                            range: range.clone(),
                        })
                    }
                }
                _ => Err(TypeError::CannotUseAsRecord {
                    typ: base_type.to_string(),
                    range: base_expr.range().clone(),
                }),
            }
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::Eq,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let Some(left_comparable) = left_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            let Some(right_comparable) = right_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: parsed_expr.range().clone(),
                });
            }

            Ok(TypedExpr::Equals {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
            })
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::NotEq,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let Some(left_comparable) = left_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            let Some(right_comparable) = right_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: left_type.clone(),
                    range: left.range().clone(),
                });
            };

            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: parsed_expr.range().clone(),
                });
            }

            Ok(TypedExpr::NotEquals {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
            })
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::LessThan,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: parsed_expr.range().clone(),
                });
            }

            Ok(TypedExpr::LessThan {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
            })
        }

        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::GreaterThan,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: parsed_expr.range().clone(),
                });
            }

            Ok(TypedExpr::GreaterThan {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
            })
        }

        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::LessThanOrEqual,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: parsed_expr.range().clone(),
                });
            }

            Ok(TypedExpr::LessThanOrEqual {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
            })
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::GreaterThanOrEqual,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: left_type.clone(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: right_type.clone(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: left_type.to_string(),
                    right: right_type.to_string(),
                    range: parsed_expr.range().clone(),
                });
            }

            Ok(TypedExpr::GreaterThanOrEqual {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
                operand_types: left_comparable,
            })
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::LogicalAnd,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // LogicalAnd only works with Bool expressions
            if !left_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalAndRequiresBoolean {
                    range: left.range().clone(),
                });
            }

            if !right_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalAndRequiresBoolean {
                    range: right.range().clone(),
                });
            }

            Ok(TypedExpr::BooleanLogicalAnd {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
            })
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::LogicalOr,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            if !left_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalOrRequiresBoolean {
                    range: left.range().clone(),
                });
            }

            if !right_type.is_subtype(&Type::Bool) {
                return Err(TypeError::LogicalOrRequiresBoolean {
                    range: right.range().clone(),
                });
            }

            Ok(TypedExpr::BooleanLogicalOr {
                left: Box::new(typed_left),
                right: Box::new(typed_right),
            })
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::Plus,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            match (left_type, right_type) {
                (Type::String, Type::String) => Ok(TypedExpr::StringConcat {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                }),
                (Type::Int, Type::Int) => Ok(TypedExpr::NumericAdd {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Int,
                }),
                (Type::Float, Type::Float) => Ok(TypedExpr::NumericAdd {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Float,
                }),
                _ => {
                    // Incompatible types for addition
                    Err(TypeError::IncompatibleTypesForAddition {
                        left_type: left_type.to_string(),
                        right_type: right_type.to_string(),
                        range: left.range().clone().to(right.range().clone()),
                    })
                }
            }
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::Minus,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            match (left_type, right_type) {
                (Type::Int, Type::Int) => Ok(TypedExpr::NumericSubtract {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Int,
                }),
                (Type::Float, Type::Float) => Ok(TypedExpr::NumericSubtract {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Float,
                }),
                _ => {
                    // Incompatible types for subtraction
                    Err(TypeError::IncompatibleTypesForSubtraction {
                        left_type: left_type.to_string(),
                        right_type: right_type.to_string(),
                        range: left.range().clone().to(right.range().clone()),
                    })
                }
            }
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::Multiply,
            right,
            ..
        } => {
            let typed_left = typecheck_expr(left, env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // Multiply operator works for:
            // 1. Integer multiplication (Int * Int)
            // 2. Float multiplication (Float * Float)

            match (left_type, right_type) {
                (Type::Int, Type::Int) => Ok(TypedExpr::NumericMultiply {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Int,
                }),
                (Type::Float, Type::Float) => Ok(TypedExpr::NumericMultiply {
                    left: Box::new(typed_left),
                    right: Box::new(typed_right),
                    operand_types: NumericType::Float,
                }),
                _ => {
                    // Incompatible types for multiplication
                    Err(TypeError::IncompatibleTypesForMultiplication {
                        left_type: left_type.to_string(),
                        right_type: right_type.to_string(),
                        range: left.range().clone().to(right.range().clone()),
                    })
                }
            }
        }
        ParsedExpr::Negation { operand, .. } => {
            let typed_operand = typecheck_expr(operand, env, type_env, annotations, None)?;
            let operand_type = typed_operand.as_type();

            // Negation only works on Bool expressions
            if !operand_type.is_subtype(&Type::Bool) {
                return Err(TypeError::NegationRequiresBoolean {
                    range: operand.range().clone(),
                });
            }

            Ok(TypedExpr::BooleanNegation {
                operand: Box::new(typed_operand),
            })
        }
        ParsedExpr::ArrayLiteral {
            elements,
            annotation: range,
        } => {
            if elements.is_empty() {
                // Empty array: infer element type from expected type
                let elem_type = match expected_type {
                    Some(Type::Array(elem)) => (**elem).clone(),
                    _ => {
                        return Err(TypeError::CannotInferEmptyArrayType {
                            range: range.clone(),
                        });
                    }
                };
                Ok(TypedExpr::ArrayLiteral {
                    elements: vec![],
                    kind: Type::Array(Box::new(elem_type)),
                })
            } else {
                // Determine expected element type from context if available
                let expected_elem_type = match expected_type {
                    Some(Type::Array(elem)) => Some(elem.as_ref()),
                    _ => None,
                };

                let mut typed_elements = Vec::new();

                // Check the type of the first element
                let first_typed =
                    typecheck_expr(&elements[0], env, type_env, annotations, expected_elem_type)?;
                let first_type = first_typed.as_type().clone();
                typed_elements.push(first_typed);

                // Check that all elements have the same type
                for element in elements.iter().skip(1) {
                    let typed_element =
                        typecheck_expr(element, env, type_env, annotations, expected_elem_type)?;
                    let element_type = typed_element.as_type();
                    if *element_type != first_type {
                        return Err(TypeError::ArrayTypeMismatch {
                            expected: first_type.to_string(),
                            found: element_type.to_string(),
                            range: element.range().clone(),
                        });
                    }
                    typed_elements.push(typed_element);
                }

                Ok(TypedExpr::ArrayLiteral {
                    elements: typed_elements,
                    kind: Type::Array(Box::new(first_type)),
                })
            }
        }
        ParsedExpr::RecordLiteral {
            record_name,
            fields,
            annotation: range,
        } => {
            // Check if the record type is defined
            let record_type = type_env
                .lookup(record_name.as_str())
                .ok_or_else(|| TypeError::UndefinedRecord {
                    record_name: record_name.clone(),
                    range: range.clone(),
                })?
                .clone();

            // Extract fields from the record type
            let record_fields = match &record_type {
                Type::Record { fields, .. } => fields,
                _ => {
                    return Err(TypeError::UndefinedRecord {
                        record_name: record_name.clone(),
                        range: range.clone(),
                    });
                }
            };

            // Build a map of expected fields from the record type
            let expected_fields: std::collections::HashMap<&str, Type> = record_fields
                .iter()
                .map(|(name, typ)| (name.as_str(), typ.clone()))
                .collect();

            // Check for unknown fields and type mismatches
            let mut typed_fields = Vec::new();
            let mut provided_fields: std::collections::HashSet<&str> =
                std::collections::HashSet::new();

            for (field_name, field_value) in fields {
                let field_name_str = field_name.as_str();

                // Check if this field exists in the record
                let expected_type = expected_fields.get(field_name_str).ok_or_else(|| {
                    TypeError::RecordLiteralUnknownRecordField {
                        field_name: field_name_str.to_string(),
                        record_name: record_name.clone(),
                        range: field_value.range().clone(),
                    }
                })?;

                // Type check the field value with expected type for bidirectional checking
                let typed_value =
                    typecheck_expr(field_value, env, type_env, annotations, Some(expected_type))?;
                let actual_type = typed_value.as_type();

                // Check that the types match
                if !actual_type.is_subtype(expected_type) {
                    return Err(TypeError::RecordLiteralFieldTypeMismatch {
                        field_name: field_name_str.to_string(),
                        expected: expected_type.to_string(),
                        found: actual_type.to_string(),
                        range: field_value.range().clone(),
                    });
                }

                provided_fields.insert(field_name_str);
                typed_fields.push((field_name.clone(), typed_value));
            }

            // Check for missing fields
            for expected_field in expected_fields.keys() {
                if !provided_fields.contains(expected_field) {
                    return Err(TypeError::RecordLiteralMissingRecordField {
                        field_name: (*expected_field).to_string(),
                        record_name: record_name.clone(),
                        range: range.clone(),
                    });
                }
            }

            Ok(TypedExpr::RecordLiteral {
                record_name: record_name.clone(),
                fields: typed_fields,
                kind: record_type,
            })
        }
        ParsedExpr::EnumLiteral {
            enum_name,
            variant_name,
            annotation: range,
        } => {
            // Look up the enum type in the type environment
            let enum_type = type_env
                .lookup(enum_name.as_str())
                .ok_or_else(|| TypeError::UndefinedEnum {
                    enum_name: enum_name.clone(),
                    range: range.clone(),
                })?
                .clone();

            // Verify it's actually an enum type and the variant exists
            match &enum_type {
                Type::Enum { variants, .. } => {
                    let variant_exists = variants.iter().any(|v| v.as_str() == variant_name);
                    if !variant_exists {
                        return Err(TypeError::UndefinedEnumVariant {
                            enum_name: enum_name.clone(),
                            variant_name: variant_name.clone(),
                            range: range.clone(),
                        });
                    }
                }
                _ => {
                    return Err(TypeError::UndefinedEnum {
                        enum_name: enum_name.clone(),
                        range: range.clone(),
                    });
                }
            }

            Ok(TypedExpr::EnumLiteral {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                kind: enum_type,
            })
        }
        ParsedExpr::Match {
            subject,
            arms,
            annotation: range,
        } => {
            // Type check the subject expression
            let typed_subject = typecheck_expr(subject, env, type_env, annotations, None)?;
            let subject_type = typed_subject.as_type().clone();

            // Subject must be an enum type
            let (_enum_module, enum_name, enum_variants) = match &subject_type {
                Type::Enum {
                    module,
                    name,
                    variants,
                } => (module.clone(), name.clone(), variants.clone()),
                _ => {
                    return Err(TypeError::MatchSubjectNotEnum {
                        found: subject_type.to_string(),
                        range: subject.range().clone(),
                    });
                }
            };

            // Track which variants have been matched
            let mut matched_variants: HashSet<String> = HashSet::new();
            let mut typed_arms: Vec<TypedMatchArm> = Vec::new();
            let mut result_type: Option<Type> = None;

            for arm in arms {
                // Pattern must be an enum literal
                let (pattern_enum_name, pattern_variant_name, pattern_range) = match &arm.pattern {
                    ParsedExpr::EnumLiteral {
                        enum_name,
                        variant_name,
                        annotation,
                    } => (enum_name.clone(), variant_name.clone(), annotation.clone()),
                    _ => {
                        return Err(TypeError::MatchPatternNotEnumVariant {
                            range: arm.pattern.range().clone(),
                        });
                    }
                };

                // Pattern enum must match subject enum
                if pattern_enum_name != enum_name.as_str() {
                    return Err(TypeError::MatchPatternEnumMismatch {
                        pattern_enum: pattern_enum_name.clone(),
                        subject_enum: enum_name.to_string(),
                        range: pattern_range.clone(),
                    });
                }

                // Variant must exist in the enum
                let variant_exists = enum_variants
                    .iter()
                    .any(|v| v.as_str() == pattern_variant_name);
                if !variant_exists {
                    return Err(TypeError::UndefinedEnumVariant {
                        enum_name: pattern_enum_name.clone(),
                        variant_name: pattern_variant_name.clone(),
                        range: pattern_range.clone(),
                    });
                }

                // Check for duplicate variants
                if matched_variants.contains(&pattern_variant_name) {
                    return Err(TypeError::MatchDuplicateVariant {
                        variant: pattern_variant_name.clone(),
                        range: pattern_range.clone(),
                    });
                }
                matched_variants.insert(pattern_variant_name.clone());

                // Type check the arm body
                let typed_body = typecheck_expr(&arm.body, env, type_env, annotations, None)?;
                let body_type = typed_body.as_type().clone();

                // Check that all arms have the same type
                match &result_type {
                    None => {
                        result_type = Some(body_type);
                    }
                    Some(expected) => {
                        if !body_type.is_subtype(expected) {
                            return Err(TypeError::MatchArmTypeMismatch {
                                expected: expected.to_string(),
                                found: body_type.to_string(),
                                range: arm.body.range().clone(),
                            });
                        }
                    }
                }

                typed_arms.push(TypedMatchArm {
                    pattern: TypedEnumPattern {
                        enum_name: pattern_enum_name,
                        variant_name: pattern_variant_name,
                    },
                    body: typed_body,
                });
            }

            // Check exhaustiveness: all variants must be matched
            for variant in &enum_variants {
                if !matched_variants.contains(variant.as_str()) {
                    return Err(TypeError::MatchMissingVariant {
                        variant: variant.to_string(),
                        range: range.clone(),
                    });
                }
            }

            Ok(TypedExpr::Match {
                subject: Box::new(typed_subject),
                arms: typed_arms,
                kind: result_type.unwrap(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::dop::symbols::type_name::TypeName;
    use crate::dop::{ParsedDeclaration, Parser};
    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use expect_test::{Expect, expect};
    use indoc::indoc;

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

    #[test]
    fn should_reject_equality_between_string_and_number() {
        check(
            "",
            &[("name", "String"), ("count", "Float")],
            "name == count",
            expect![[r#"
                error: Can not compare String to Float
                name == count
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_equality_between_boolean_and_string() {
        check(
            "",
            &[("enabled", "Bool"), ("name", "String")],
            "enabled == name",
            expect![[r#"
                error: Can not compare Bool to String
                enabled == name
                ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_undefined_variable() {
        check(
            "",
            &[],
            "undefined_var",
            expect![[r#"
                error: Undefined variable: undefined_var
                undefined_var
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_field_access_on_undefined_variable() {
        check(
            "",
            &[],
            "notdefined.foo.bar",
            expect![[r#"
                error: Undefined variable: notdefined
                notdefined.foo.bar
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_field_access_on_non_record() {
        check(
            "",
            &[("count", "Float")],
            "count.value",
            expect![[r#"
                error: Float can not be used as a record
                count.value
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_negation_of_string() {
        check(
            "",
            &[("name", "String")],
            "!name",
            expect![[r#"
                error: Negation operator can only be applied to Bool values
                !name
                 ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_negation_of_number() {
        check(
            "",
            &[("count", "Float")],
            "!count",
            expect![[r#"
                error: Negation operator can only be applied to Bool values
                !count
                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_field_access_on_nested_array() {
        check(
            indoc! {"
                record Profile {name: String, active: Bool}
                record UserInfo {profile: Profile}
                record Config {users: Array[UserInfo]}
            "},
            &[("config", "Config")],
            "config.users.profile.name",
            expect![[r#"
                error: Array[test::UserInfo] can not be used as a record
                config.users.profile.name
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_field_access_on_array() {
        check(
            "record User {name: String}",
            &[("users", "Array[User]")],
            "users.name",
            expect![[r#"
                error: Array[test::User] can not be used as a record
                users.name
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unknown_field_access() {
        check(
            "record Data {field: String}",
            &[("data", "Data")],
            "data.unknown",
            expect![[r#"
                error: Field 'unknown' not found in record 'Data'
                data.unknown
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_resolve_basic_variable_lookup() {
        check("", &[("name", "String")], "name", expect!["String"]);
    }

    #[test]
    fn should_accept_string_literal() {
        check("", &[], r#""hello world""#, expect!["String"]);
    }

    #[test]
    fn should_accept_boolean_literal_true() {
        check("", &[], "true", expect!["Bool"]);
    }

    #[test]
    fn should_accept_boolean_literal_false() {
        check("", &[], "false", expect!["Bool"]);
    }

    #[test]
    fn should_accept_int_literal() {
        check("", &[], "42", expect!["Int"]);
    }

    #[test]
    fn should_accept_float_literal() {
        check("", &[], "3.14", expect!["Float"]);
    }

    #[test]
    fn should_accept_field_access() {
        check(
            "record User {name: String}",
            &[("user", "User")],
            "user.name",
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_nested_field_access() {
        check(
            indoc! {"
                record Profile {name: String}
                record User {profile: Profile}
                record App {user: User}
            "},
            &[("app", "App")],
            "app.user.profile.name",
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_string_equality() {
        check(
            "",
            &[("name", "String")],
            r#"name == "alice""#,
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_equality_between_float_and_int() {
        check(
            "",
            &[("count", "Float")],
            "count == 42",
            expect![[r#"
                error: Can not compare Float to Int
                count == 42
                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_boolean_equality() {
        check(
            "",
            &[("enabled", "Bool")],
            "enabled == true",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_equality_of_same_field_types() {
        check(
            indoc! {"
                record User {name: String}
                record Admin {name: String}
            "},
            &[("user", "User"), ("admin", "Admin")],
            "user.name == admin.name",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_chained_equality() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool")],
            "a == b == true",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_negation_of_variable() {
        check("", &[("enabled", "Bool")], "!enabled", expect!["Bool"]);
    }

    #[test]
    fn should_accept_negation_of_true() {
        check("", &[], "!true", expect!["Bool"]);
    }

    #[test]
    fn should_accept_negation_of_false() {
        check("", &[], "!false", expect!["Bool"]);
    }

    #[test]
    fn should_accept_greater_than_with_ints() {
        check("", &[("x", "Int"), ("y", "Int")], "x > y", expect!["Bool"]);
    }

    #[test]
    fn should_accept_greater_than_with_floats() {
        check(
            "",
            &[("x", "Float"), ("y", "Float")],
            "x > y",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_greater_than_with_mixed_types() {
        check(
            "",
            &[("x", "Int"), ("y", "Float")],
            "x > y",
            expect![[r#"
                error: Can not compare Int to Float
                x > y
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_less_than_or_equal_with_ints() {
        check("", &[("x", "Int"), ("y", "Int")], "x <= y", expect!["Bool"]);
    }

    #[test]
    fn should_accept_less_than_or_equal_with_floats() {
        check(
            "",
            &[("x", "Float"), ("y", "Float")],
            "x <= y",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_less_than_or_equal_with_mixed_types() {
        check(
            "",
            &[("x", "Int"), ("y", "Float")],
            "x <= y",
            expect![[r#"
                error: Can not compare Int to Float
                x <= y
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_greater_than_or_equal_with_ints() {
        check("", &[("x", "Int"), ("y", "Int")], "x >= y", expect!["Bool"]);
    }

    #[test]
    fn should_accept_greater_than_or_equal_with_floats() {
        check(
            "",
            &[("x", "Float"), ("y", "Float")],
            "x >= y",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_greater_than_or_equal_with_mixed_types() {
        check(
            "",
            &[("x", "Int"), ("y", "Float")],
            "x >= y",
            expect![[r#"
                error: Can not compare Int to Float
                x >= y
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_negation_with_equality() {
        check(
            "record User {active: Bool}",
            &[("user", "User")],
            "!user.active == false",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_parenthesized_negation() {
        check(
            indoc! {"
                record Status {enabled: Bool}
                record Config {active: Bool}
            "},
            &[("status", "Status"), ("config", "Config")],
            "!(status.enabled == config.active)",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_record_with_array_field() {
        check(
            "record Data {items: Array[String]}",
            &[("data", "Data")],
            "data.items",
            expect!["Array[String]"],
        );
    }

    #[test]
    fn should_accept_deep_field_access() {
        check(
            indoc! {"
                record Connection {host: String}
                record Database {connection: Connection}
                record Config {database: Database}
                record System {config: Config}
            "},
            &[("system", "System")],
            "system.config.database.connection.host",
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_multiple_field_accesses() {
        check(
            "record Obj {name: String, title: String}",
            &[("obj", "Obj")],
            "obj.name == obj.title",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_array_with_different_element_types() {
        check(
            "",
            &[],
            "[1, true]",
            expect![[r#"
                error: Array elements must all have the same type, found Int and Bool
                [1, true]
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_array_with_trailing_comma() {
        check("", &[], "[\n\t1,\n\t2,\n\t3,\n]", expect!["Array[Int]"]);
    }

    #[test]
    fn should_accept_single_element_array_with_trailing_comma() {
        check(
            "",
            &[],
            indoc! {r#"
            [
            	"hello",
            ]
        "#},
            expect!["Array[String]"],
        );
    }

    #[test]
    fn should_accept_string_concatenation() {
        check("", &[], r#""hello" + "world""#, expect!["String"]);
    }

    #[test]
    fn should_accept_multiple_string_concatenation() {
        check("", &[], r#""hello" + " " + "world""#, expect!["String"]);
    }

    #[test]
    fn should_accept_string_concatenation_with_variables() {
        check(
            "",
            &[("greeting", "String"), ("name", "String")],
            r#"greeting + " " + name"#,
            expect!["String"],
        );
    }

    #[test]
    fn should_reject_concatenation_with_left_number() {
        check(
            "",
            &[],
            r#"42 + "hello""#,
            expect![[r#"
                error: Cannot add values of incompatible types: Int + String
                42 + "hello"
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_concatenation_with_right_boolean() {
        check(
            "",
            &[],
            r#""hello" + true"#,
            expect![[r#"
                error: Cannot add values of incompatible types: String + Bool
                "hello" + true
                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_int_addition() {
        check("", &[], r#"42 + 58"#, expect!["Int"]);
    }

    #[test]
    fn should_accept_string_concatenation_with_field_access() {
        check(
            "record User {first_name: String, last_name: String}",
            &[("user", "User")],
            r#"user.first_name + " " + user.last_name"#,
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_concatenation_result_comparison() {
        check("", &[], r#""a" + "b" == "ab""#, expect!["Bool"]);
    }

    #[test]
    fn should_accept_logical_and_with_boolean_variables() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool")],
            "a && b",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_logical_and_with_boolean_literals() {
        check("", &[], "true && false", expect!["Bool"]);
    }

    #[test]
    fn should_accept_logical_and_with_field_access() {
        check(
            "record User {enabled: Bool, active: Bool}",
            &[("user", "User")],
            "user.enabled && user.active",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_logical_and_with_comparison() {
        check(
            "",
            &[("x", "Int"), ("y", "Int"), ("enabled", "Bool")],
            "x > y && enabled",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_logical_and_with_left_string() {
        check(
            "",
            &[("name", "String"), ("enabled", "Bool")],
            "name && enabled",
            expect![[r#"
                error: && operator can only be applied to Bool values
                name && enabled
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_logical_and_with_right_int() {
        check(
            "",
            &[("enabled", "Bool"), ("count", "Int")],
            "enabled && count",
            expect![[r#"
                error: && operator can only be applied to Bool values
                enabled && count
                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_logical_and_with_both_strings() {
        check(
            "",
            &[("a", "String"), ("b", "String")],
            "a && b",
            expect![[r#"
                error: && operator can only be applied to Bool values
                a && b
                ^
            "#]],
        );
    }

    #[test]
    fn should_handle_logical_and_precedence() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool")],
            "a && b == c",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_logical_or_with_boolean_variables() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool")],
            "a || b",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_logical_or_with_boolean_literals() {
        check("", &[], "true || false", expect!["Bool"]);
    }

    #[test]
    fn should_accept_logical_or_with_field_access() {
        check(
            "record User {enabled: Bool, active: Bool}",
            &[("user", "User")],
            "user.enabled || user.active",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_logical_or_with_comparison() {
        check(
            "",
            &[("x", "Int"), ("y", "Int"), ("enabled", "Bool")],
            "x > y || enabled",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_logical_or_with_left_string() {
        check(
            "",
            &[("name", "String"), ("enabled", "Bool")],
            "name || enabled",
            expect![[r#"
                error: || operator can only be applied to Bool values
                name || enabled
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_logical_or_with_right_int() {
        check(
            "",
            &[("enabled", "Bool"), ("count", "Int")],
            "enabled || count",
            expect![[r#"
                error: || operator can only be applied to Bool values
                enabled || count
                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_logical_or_with_both_strings() {
        check(
            "",
            &[("a", "String"), ("b", "String")],
            "a || b",
            expect![[r#"
                error: || operator can only be applied to Bool values
                a || b
                ^
            "#]],
        );
    }

    #[test]
    fn should_handle_logical_or_precedence() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool")],
            "a || b == c",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_mixed_logical_operators() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool")],
            "a && b || c",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_handle_complex_logical_operator_precedence() {
        check(
            "",
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool"), ("d", "Bool")],
            "a || b && c || d",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_int_addition_with_variables() {
        check("", &[("x", "Int"), ("y", "Int")], "x + y", expect!["Int"]);
    }

    #[test]
    fn should_accept_float_addition_with_variables() {
        check(
            "",
            &[("x", "Float"), ("y", "Float")],
            "x + y",
            expect!["Float"],
        );
    }

    #[test]
    fn should_accept_string_addition_with_variables() {
        check(
            "",
            &[("s1", "String"), ("s2", "String")],
            "s1 + s2",
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_int_literal_addition() {
        check("", &[], "42 + 17", expect!["Int"]);
    }

    #[test]
    fn should_accept_float_literal_addition() {
        check("", &[], "3.14 + 2.71", expect!["Float"]);
    }

    #[test]
    fn should_accept_string_literal_concatenation() {
        check("", &[], r#""hello" + " world""#, expect!["String"]);
    }

    #[test]
    fn should_reject_addition_of_int_and_float() {
        check(
            "",
            &[("x", "Int"), ("y", "Float")],
            "x + y",
            expect![[r#"
                error: Cannot add values of incompatible types: Int + Float
                x + y
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_addition_of_string_and_int() {
        check(
            "",
            &[("name", "String"), ("count", "Int")],
            "name + count",
            expect![[r#"
                error: Cannot add values of incompatible types: String + Int
                name + count
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_addition_of_boolean_and_int() {
        check(
            "",
            &[("flag", "Bool"), ("count", "Int")],
            "flag + count",
            expect![[r#"
                error: Cannot add values of incompatible types: Bool + Int
                flag + count
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_addition_with_field_access() {
        check(
            "record User {x: Int, y: Int}",
            &[("user", "User")],
            "user.x + user.y",
            expect!["Int"],
        );
    }

    #[test]
    fn should_accept_mixed_addition_and_comparison() {
        check(
            "",
            &[("a", "Int"), ("b", "Int"), ("c", "Int")],
            "a + b > c",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_simple_record_literal() {
        check(
            "record User {name: String, age: Int}",
            &[],
            r#"User(name: "John", age: 30)"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn should_accept_record_literal_with_variables() {
        check(
            "record User {name: String, age: Int}",
            &[("user_name", "String"), ("user_age", "Int")],
            "User(name: user_name, age: user_age)",
            expect!["test::User"],
        );
    }

    #[test]
    fn should_reject_literal_of_undefined_record() {
        check(
            "",
            &[],
            r#"User(name: "John")"#,
            expect![[r#"
                error: Record type 'User' is not defined
                User(name: "John")
                ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_record_literal_with_missing_field() {
        check(
            "record User {name: String, age: Int}",
            &[],
            r#"User(name: "John")"#,
            expect![[r#"
                error: Missing field 'age' in record literal 'User'
                User(name: "John")
                ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_record_literal_with_unknown_field() {
        check(
            "record User {name: String}",
            &[],
            r#"User(name: "John", email: "john@example.com")"#,
            expect![[r#"
                error: Unknown field 'email' in record literal 'User'
                User(name: "John", email: "john@example.com")
                                          ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_record_literal_with_type_mismatch() {
        check(
            "record User {name: String, age: Int}",
            &[],
            r#"User(name: "John", age: "thirty")"#,
            expect![[r#"
                error: Field 'age' expects type Int, but got String
                User(name: "John", age: "thirty")
                                        ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_nested_record_literal() {
        check(
            indoc! {"
                record Address {city: String}
                record User {name: String, address: Address}
            "},
            &[],
            r#"User(name: "John", address: Address(city: "NYC"))"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn should_accept_enum_equality() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("a", "Color"), ("b", "Color")],
            "a == b",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_enum_inequality() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("a", "Color"), ("b", "Color")],
            "a != b",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_equality_between_different_enum_types() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
                enum Size {
                    Small,
                    Medium,
                    Large,
                }
            "},
            &[("color", "Color"), ("size", "Size")],
            "color == size",
            expect![[r#"
                error: Can not compare test::Color to test::Size
                color == size
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_inequality_between_different_enum_types() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
                enum Size {
                    Small,
                    Medium,
                    Large,
                }
            "},
            &[("color", "Color"), ("size", "Size")],
            "color != size",
            expect![[r#"
                error: Can not compare test::Color to test::Size
                color != size
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_equality_between_enum_and_string() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color"), ("name", "String")],
            "color == name",
            expect![[r#"
                error: Can not compare test::Color to String
                color == name
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_equality_between_enum_and_int() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color"), ("count", "Int")],
            "color == count",
            expect![[r#"
                error: Can not compare test::Color to Int
                color == count
                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_equality_between_enum_and_bool() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color"), ("flag", "Bool")],
            "color == flag",
            expect![[r#"
                error: Can not compare test::Color to Bool
                color == flag
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_less_than_comparison_on_enums() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("a", "Color"), ("b", "Color")],
            "a < b",
            expect![[r#"
                error: Type test::Color is not comparable
                a < b
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_greater_than_comparison_on_enums() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("a", "Color"), ("b", "Color")],
            "a > b",
            expect![[r#"
                error: Type test::Color is not comparable
                a > b
                ^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_in_record_field() {
        check(
            indoc! {"
                enum Status {
                    Active,
                    Inactive,
                    Pending,
                }
                record User {
                    name: String,
                    status: Status,
                }
            "},
            &[("user", "User"), ("status", "Status")],
            "user.status == status",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_enum_equality_with_field_access() {
        check(
            indoc! {"
                enum Status {
                    Active,
                    Inactive,
                }
                record User {
                    status: Status,
                }
                record Admin {
                    status: Status,
                }
            "},
            &[("user", "User"), ("admin", "Admin")],
            "user.status == admin.status",
            expect!["Bool"],
        );
    }

    // Enum literal tests

    #[test]
    fn should_accept_enum_literal() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[],
            "Color::Red",
            expect!["test::Color"],
        );
    }

    #[test]
    fn should_reject_undefined_enum_variant() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[],
            "Color::Yellow",
            expect![[r#"
                error: Variant 'Yellow' is not defined in enum 'Color'
                Color::Yellow
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_undefined_enum() {
        check(
            "",
            &[],
            "Unknown::Red",
            expect![[r#"
                error: Enum type 'Unknown' is not defined
                Unknown::Red
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_variant_in_equality() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[("color", "Color")],
            "Color::Red == color",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_accept_enum_variant_in_record_field_assignment() {
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
            &[],
            r#"User(name: "Alice", status: Status::Active)"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn should_accept_two_enum_variants_in_equality() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[],
            "Color::Red == Color::Green",
            expect!["Bool"],
        );
    }

    #[test]
    fn should_reject_equality_between_different_enums_with_same_variants() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
                enum Shade {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[],
            "Color::Red == Shade::Red",
            expect![[r#"
                error: Can not compare test::Color to test::Shade
                Color::Red == Shade::Red
                ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Match expression tests

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
                error: Match subject must be an enum type, found String
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
                error: Duplicate match arm for variant 'Red'
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
}
