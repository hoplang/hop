use super::r#type::{NumericType, Type};
use super::type_error::TypeError;
use crate::document::document_cursor::{CheapString, DocumentRange};
use crate::dop::TypedExpr;
use crate::dop::patterns::compiler::{Compiler, Decision};
use crate::dop::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::dop::symbols::var_name::VarName;
use crate::dop::syntax::parsed::{
    Constructor, ParsedBinaryOp, ParsedExpr, ParsedMatchArm, ParsedMatchPattern, ParsedType,
};
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
        ParsedType::Option { element, .. } => {
            let elem_type = resolve_type(element, type_env)?;
            Ok(Type::Option(Box::new(elem_type)))
        }
        ParsedType::Array { element, .. } => {
            let elem_type = resolve_type(element, type_env)?;
            Ok(Type::Array(Box::new(elem_type)))
        }
        ParsedType::Named { name, range } => {
            let record_type = type_env
                .lookup(name.as_str())
                .ok_or_else(|| TypeError::UndefinedType {
                    type_name: name.to_string(),
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
    var_env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
    expected_type: Option<&Type>,
) -> Result<TypedExpr, TypeError> {
    match parsed_expr {
        ParsedExpr::Var { value: name, .. } => {
            if let Some(var_type) = var_env.lookup(name.as_str()) {
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
            record,
            field,
            range,
            ..
        } => {
            let typed_base = typecheck_expr(record, var_env, type_env, annotations, None)?;
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
                    range: record.range().clone(),
                }),
            }
        }
        ParsedExpr::BinaryOp {
            left,
            operator: ParsedBinaryOp::Eq,
            right,
            ..
        } => {
            // Try left first; if it fails (e.g., None without context), try right first
            // to infer left's type from right
            let typed_left =
                typecheck_expr(left, var_env, type_env, annotations, None).or_else(|left_err| {
                    let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
                    typecheck_expr(
                        left,
                        var_env,
                        type_env,
                        annotations,
                        Some(typed_right.as_type()),
                    )
                    .map_err(|_| left_err)
                })?;
            // Use left's type as context for right (allows Some(1) == None)
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                Some(typed_left.as_type()),
            )?;

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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // LogicalAnd only works with Bool expressions
            if *left_type != Type::Bool {
                return Err(TypeError::LogicalAndRequiresBoolean {
                    range: left.range().clone(),
                });
            }

            if *right_type != Type::Bool {
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            if *left_type != Type::Bool {
                return Err(TypeError::LogicalOrRequiresBoolean {
                    range: left.range().clone(),
                });
            }

            if *right_type != Type::Bool {
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
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
            let typed_left = typecheck_expr(left, var_env, type_env, annotations, None)?;
            let typed_right = typecheck_expr(right, var_env, type_env, annotations, None)?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

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
            let typed_operand = typecheck_expr(operand, var_env, type_env, annotations, None)?;
            let operand_type = typed_operand.as_type();

            if *operand_type != Type::Bool {
                return Err(TypeError::NegationRequiresBoolean {
                    range: operand.range().clone(),
                });
            }

            Ok(TypedExpr::BooleanNegation {
                operand: Box::new(typed_operand),
            })
        }
        ParsedExpr::ArrayLiteral { elements, range } => {
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
                let first_typed = typecheck_expr(
                    &elements[0],
                    var_env,
                    type_env,
                    annotations,
                    expected_elem_type,
                )?;
                let first_type = first_typed.as_type().clone();
                typed_elements.push(first_typed);

                // Check that all elements have the same type
                // Use first element's type as context for subsequent elements
                let elem_context = expected_elem_type.unwrap_or(&first_type);
                for element in elements.iter().skip(1) {
                    let typed_element = typecheck_expr(
                        element,
                        var_env,
                        type_env,
                        annotations,
                        Some(elem_context),
                    )?;
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
            range,
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
                    TypeError::RecordUnknownField {
                        field_name: field_name_str.to_string(),
                        record_name: record_name.clone(),
                        range: field_value.range().clone(),
                    }
                })?;

                // Type check the field value with expected type for bidirectional checking
                let typed_value = typecheck_expr(
                    field_value,
                    var_env,
                    type_env,
                    annotations,
                    Some(expected_type),
                )?;
                let actual_type = typed_value.as_type();

                // Check that the types match
                if *actual_type != *expected_type {
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
            let missing_fields: Vec<String> = expected_fields
                .keys()
                .filter(|name| !provided_fields.contains(*name))
                .map(|name| (*name).to_string())
                .collect();
            if !missing_fields.is_empty() {
                return Err(TypeError::RecordMissingFields {
                    record_name: record_name.clone(),
                    missing_fields,
                    range: range.clone(),
                });
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
            fields,
            constructor_range,
            range,
        } => {
            // Look up the enum type in the type environment
            let enum_type = type_env
                .lookup(enum_name.as_str())
                .ok_or_else(|| TypeError::UndefinedEnum {
                    enum_name: enum_name.clone(),
                    range: range.clone(),
                })?
                .clone();

            // Verify it's actually an enum type and get the variant's fields
            let variant_fields = match &enum_type {
                Type::Enum { variants, .. } => {
                    let variant = variants.iter().find(|(v, _)| v.as_str() == variant_name);
                    match variant {
                        Some((_, fields)) => fields.clone(),
                        None => {
                            return Err(TypeError::UndefinedEnumVariant {
                                enum_name: enum_name.clone(),
                                variant_name: variant_name.clone(),
                                range: range.clone(),
                            });
                        }
                    }
                }
                _ => {
                    return Err(TypeError::UndefinedEnum {
                        enum_name: enum_name.clone(),
                        range: range.clone(),
                    });
                }
            };

            // Validate fields
            let typed_fields = {
                let mut typed_fields = Vec::new();
                let mut provided_field_names: std::collections::HashSet<String> =
                    std::collections::HashSet::new();

                // Type check each provided field
                for (field_name, field_name_range, field_expr) in fields {
                    // Check if field exists in variant definition
                    let expected_field = variant_fields
                        .iter()
                        .find(|(name, _)| name.as_str() == field_name.as_str());

                    match expected_field {
                        Some((_, expected_type)) => {
                            // Type check the field expression
                            let typed_field_expr = typecheck_expr(
                                field_expr,
                                var_env,
                                type_env,
                                annotations,
                                Some(expected_type),
                            )?;

                            // Verify type matches
                            let actual_type = typed_field_expr.as_type();
                            if actual_type != expected_type {
                                return Err(TypeError::EnumVariantFieldTypeMismatch {
                                    enum_name: enum_name.clone(),
                                    variant_name: variant_name.clone(),
                                    field_name: field_name.as_str().to_string(),
                                    expected: expected_type.to_string(),
                                    found: actual_type.to_string(),
                                    range: field_expr.range().clone(),
                                });
                            }

                            provided_field_names.insert(field_name.as_str().to_string());
                            typed_fields.push((field_name.clone(), typed_field_expr));
                        }
                        None => {
                            return Err(TypeError::EnumVariantUnknownField {
                                enum_name: enum_name.clone(),
                                variant_name: variant_name.clone(),
                                field_name: field_name.as_str().to_string(),
                                range: field_name_range.clone(),
                            });
                        }
                    }
                }

                // Check for missing fields
                let missing_fields: Vec<String> = variant_fields
                    .iter()
                    .filter(|(name, _)| !provided_field_names.contains(name.as_str()))
                    .map(|(name, _)| name.as_str().to_string())
                    .collect();
                if !missing_fields.is_empty() {
                    return Err(TypeError::EnumVariantMissingFields {
                        enum_name: enum_name.clone(),
                        variant_name: variant_name.clone(),
                        missing_fields,
                        range: constructor_range.clone(),
                    });
                }

                typed_fields
            };

            Ok(TypedExpr::EnumLiteral {
                enum_name: enum_name.clone(),
                variant_name: variant_name.clone(),
                fields: typed_fields,
                kind: enum_type,
            })
        }
        ParsedExpr::OptionLiteral { value, range } => {
            match value {
                Some(inner_expr) => {
                    // Some(value): determine expected inner type from context if available
                    let expected_inner_type = match expected_type {
                        Some(Type::Option(elem)) => Some(elem.as_ref()),
                        _ => None,
                    };

                    // Type check the inner value
                    let typed_inner = typecheck_expr(
                        inner_expr,
                        var_env,
                        type_env,
                        annotations,
                        expected_inner_type,
                    )?;
                    let inner_type = typed_inner.as_type().clone();

                    Ok(TypedExpr::OptionLiteral {
                        value: Some(Box::new(typed_inner)),
                        kind: Type::Option(Box::new(inner_type)),
                    })
                }
                None => {
                    // None: infer element type from expected type
                    let elem_type = match expected_type {
                        Some(Type::Option(elem)) => (**elem).clone(),
                        _ => {
                            return Err(TypeError::CannotInferNoneType {
                                range: range.clone(),
                            });
                        }
                    };
                    Ok(TypedExpr::OptionLiteral {
                        value: None,
                        kind: Type::Option(Box::new(elem_type)),
                    })
                }
            }
        }
        ParsedExpr::Match {
            subject,
            arms,
            range,
        } => typecheck_match(subject, arms, range, var_env, type_env, annotations),
        ParsedExpr::MacroInvocation { name, args, range } => match name.as_str() {
            "classes" => expand_classes_macro(args, range, var_env, type_env, annotations),
            _ => unreachable!("Unknown macro '{}' should be caught at parse time", name),
        },
    }
}

/// Expand a `classes!` macro invocation to a chain of StringConcat operations.
fn expand_classes_macro(
    args: &[ParsedExpr],
    _range: &DocumentRange,
    var_env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    if args.is_empty() {
        return Ok(TypedExpr::StringLiteral {
            value: CheapString::new(String::new()),
        });
    }

    // Type-check all arguments, expecting String
    let mut typed_args = Vec::new();
    for arg in args {
        let typed = typecheck_expr(arg, var_env, type_env, annotations, Some(&Type::String))?;
        // Verify it's actually a String
        if typed.as_type() != &Type::String {
            return Err(TypeError::MacroArgumentTypeMismatch {
                macro_name: "classes".to_string(),
                expected: "String".to_string(),
                actual: typed.as_type().to_string(),
                range: arg.range().clone(),
            });
        }
        typed_args.push(typed);
    }

    // Build: arg0 + " " + arg1 + " " + arg2 + ...
    let mut result = typed_args.remove(0);
    for arg in typed_args {
        // Add " " separator
        result = TypedExpr::StringConcat {
            left: Box::new(result),
            right: Box::new(TypedExpr::StringLiteral {
                value: CheapString::new(" ".to_string()),
            }),
        };
        // Add next argument
        result = TypedExpr::StringConcat {
            left: Box::new(result),
            right: Box::new(arg),
        };
    }

    Ok(result)
}

// Typecheck a match expression and compile it to a TypedExpr.
fn typecheck_match(
    subject: &ParsedExpr,
    arms: &[ParsedMatchArm],
    range: &DocumentRange,
    var_env: &mut Environment<Type>,
    type_env: &mut Environment<Type>,
    annotations: &mut Vec<TypeAnnotation>,
) -> Result<TypedExpr, TypeError> {
    let typed_subject = typecheck_expr(subject, var_env, type_env, annotations, None)?;
    let subject_type = typed_subject.as_type().clone();

    // If subject is already a variable, use it directly; otherwise wrap in a let
    let (subject_name, initial_var_counter, needs_wrapper) = match &typed_subject {
        TypedExpr::Var { value, .. } => (value.as_str().to_string(), 0, false),
        _ => ("v0".to_string(), 1, true),
    };

    let patterns: Vec<_> = arms.iter().map(|arm| arm.pattern.clone()).collect();
    let tree = Compiler::new(initial_var_counter).compile(
        &patterns,
        &subject_name,
        &subject_type,
        subject.range(),
        range,
    )?;

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
                for (field_name, _, field_pattern) in fields {
                    let field_type = type_fields
                        .iter()
                        .find(|(name, _)| name == field_name)
                        .map(|(_, typ)| typ)
                        .expect("field type not found");
                    bindings.extend(extract_bindings_from_pattern(field_pattern, field_type));
                }
                bindings
            }
            Constructor::EnumVariant { variant_name, .. } => {
                let Type::Enum { variants, .. } = subject_type else {
                    unreachable!("EnumVariant pattern requires Enum type")
                };

                let variant_fields = variants
                    .iter()
                    .find(|(name, _)| name.as_str() == variant_name)
                    .map(|(_, fields)| fields)
                    .expect("variant not found in enum type");

                let mut bindings = Vec::new();
                for (field_name, _, field_pattern) in fields {
                    let field_type = variant_fields
                        .iter()
                        .find(|(name, _)| name == field_name)
                        .map(|(_, typ)| typ)
                        .expect("field type not found");
                    bindings.extend(extract_bindings_from_pattern(field_pattern, field_type));
                }
                bindings
            }
            Constructor::OptionNone | Constructor::BooleanTrue | Constructor::BooleanFalse => {
                vec![]
            }
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
        let mut pushed_count = 0;
        for (name, typ, range) in &bindings {
            match env.push(name.clone(), typ.clone()) {
                Ok(_) => {
                    pushed_count += 1;
                }
                Err(_) => {
                    return Err(TypeError::VariableAlreadyDefined {
                        name: name.clone(),
                        range: range.clone(),
                    });
                }
            }
        }

        // Use the first arm's type as context for subsequent arms
        let typed_body =
            typecheck_expr(&arm.body, env, type_env, annotations, result_type.as_ref())?;
        let body_type = typed_body.as_type().clone();

        // Remove bindings from environment and check for unused bindings
        for (_, _, range) in bindings.iter().rev().take(pushed_count) {
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
            for binding in body.bindings.iter().rev() {
                let var_name = VarName::new(&binding.name).expect("invalid variable name");
                let value = Box::new(TypedExpr::Var {
                    value: VarName::new(&binding.source_name).expect("invalid variable name"),
                    kind: binding.typ.clone(),
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
            let subject = (
                VarName::new(&var.name).expect("invalid variable name"),
                var.typ.clone(),
            );

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
                            false_body: Box::new(
                                false_body.expect("BoolMatch must have a false arm"),
                            ),
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
                                    VarName::new(&var.name).expect("invalid variable name")
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

                Type::Enum { variants, .. } => {
                    let arms = cases
                        .iter()
                        .map(|case| {
                            let (pattern, variant_fields) = match &case.constructor {
                                Constructor::EnumVariant {
                                    enum_name,
                                    variant_name,
                                } => {
                                    let fields = variants
                                        .iter()
                                        .find(|(name, _)| name.as_str() == variant_name)
                                        .map(|(_, fields)| fields)
                                        .expect("variant not found");
                                    (
                                        EnumPattern::Variant {
                                            enum_name: enum_name.to_string(),
                                            variant_name: variant_name.to_string(),
                                        },
                                        fields,
                                    )
                                }
                                _ => unreachable!("Invalid constructor for Enum type"),
                            };

                            let mut body = decision_to_typed_expr(
                                &case.body,
                                typed_bodies,
                                result_type.clone(),
                            );

                            // Wrap with Let expressions for each field (using FieldAccess)
                            // Iterate in reverse so bindings are in the correct order
                            for (i, (field_name, _field_type)) in
                                variant_fields.iter().enumerate().rev()
                            {
                                let var = &case.arguments[i];
                                let var_name =
                                    VarName::new(&var.name).expect("invalid variable name");

                                // Create field access: subject.field_name
                                let field_access = TypedExpr::FieldAccess {
                                    record: Box::new(TypedExpr::Var {
                                        value: subject.0.clone(),
                                        kind: subject.1.clone(),
                                    }),
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

                            EnumMatchArm {
                                pattern,
                                bindings: vec![],
                                body,
                            }
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
                            record: Box::new(TypedExpr::Var {
                                value: subject.0.clone(),
                                kind: subject.1.clone(),
                            }),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::document::document_cursor::DocumentCursor;
    use crate::dop::ParsedDeclaration;
    use crate::dop::parser;
    use crate::dop::symbols::type_name::TypeName;
    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use std::collections::VecDeque;

    fn check(declarations_str: &str, env_vars: &[(&str, &str)], expr_str: &str, expected: Expect) {
        let mut env = Environment::new();
        let mut type_env: Environment<Type> = Environment::new();
        let test_module = ModuleName::new("test").unwrap();

        // Parse and process declarations
        let cursor = DocumentCursor::new(declarations_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut errors = ErrorCollector::new();
        let mut comments = VecDeque::new();
        for declaration in parser::parse_declarations(&mut iter, &mut comments, &range, &mut errors)
        {
            match declaration {
                ParsedDeclaration::Enum { name, variants, .. } => {
                    // Build variant types with properly resolved field types
                    let typed_variants: Vec<_> = variants
                        .iter()
                        .map(|(variant_name, _, fields)| {
                            let typed_fields: Vec<_> = fields
                                .iter()
                                .map(|(field_name, _, parsed_type)| {
                                    let resolved_type = resolve_type(parsed_type, &mut type_env)
                                        .expect("Test enum field type should be valid");
                                    (field_name.clone(), resolved_type)
                                })
                                .collect();
                            (variant_name.clone(), typed_fields)
                        })
                        .collect();
                    let enum_type = Type::Enum {
                        module: test_module.clone(),
                        name: TypeName::new(name.as_str()).unwrap(),
                        variants: typed_variants,
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
            let cursor = DocumentCursor::new(type_str.to_string());
            let range = cursor.range();
            let mut iter = cursor.peekable();
            let mut comments = VecDeque::new();
            let parsed_type =
                parser::parse_type(&mut iter, &mut comments, &range).expect("Failed to parse type");
            let typ = resolve_type(&parsed_type, &mut type_env)
                .expect("Test parameter type should be valid");
            let _ = env.push(var_name.to_string(), typ);
        }

        let cursor = DocumentCursor::new(expr_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let expr = parser::parse_expr(&mut iter, &mut comments, &range)
            .expect("Failed to parse expression");

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
    fn should_accept_mixed_addition_and_comparison() {
        check(
            "",
            &[("a", "Int"), ("b", "Int"), ("c", "Int")],
            "a + b > c",
            expect!["Bool"],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ARRAYS                                                                //
    ///////////////////////////////////////////////////////////////////////////

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
    fn should_accept_array_of_some_and_none() {
        check(
            "",
            &[],
            "[Some(1), Some(2), None]",
            expect!["Array[Option[Int]]"],
        );
    }

    #[test]
    fn should_accept_array_of_arrays_with_empty_array() {
        check("", &[], "[[1,2],[2,3],[]]", expect!["Array[Array[Int]]"]);
    }

    #[test]
    fn should_reject_array_with_mismatched_option_types() {
        check(
            "",
            &[],
            r#"[Some(1), Some("1")]"#,
            expect![[r#"
                error: Array elements must all have the same type, found Option[Int] and Option[String]
                [Some(1), Some("1")]
                          ^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // RECORDS                                                               //
    ///////////////////////////////////////////////////////////////////////////

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
    fn should_reject_record_literal_with_missing_field() {
        check(
            "record User {name: String, age: Int}",
            &[],
            r#"User(name: "John")"#,
            expect![[r#"
                error: Record 'User' is missing fields: age
                User(name: "John")
                ^^^^^^^^^^^^^^^^^^
            "#]],
        );
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
    fn should_reject_record_literal_with_unknown_field() {
        check(
            "record User {name: String}",
            &[],
            r#"User(name: "John", email: "john@example.com")"#,
            expect![[r#"
                error: Unknown field 'email' in record 'User'
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
    fn should_accept_record_with_array_field() {
        check(
            "record Data {items: Array[String]}",
            &[("data", "Data")],
            "data.items",
            expect!["Array[String]"],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ENUMS                                                                 //
    ///////////////////////////////////////////////////////////////////////////

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
    fn should_accept_enum_variant_with_fields() {
        check(
            indoc! {"
                enum Result {
                    Ok(value: Int),
                    Err(message: String),
                }
            "},
            &[],
            "Result::Ok(value: 42)",
            expect!["test::Result"],
        );
    }

    #[test]
    fn should_accept_enum_variant_with_multiple_fields() {
        check(
            indoc! {"
                enum Point {
                    XY(x: Int, y: Int),
                    Origin,
                }
            "},
            &[],
            "Point::XY(x: 10, y: 20)",
            expect!["test::Point"],
        );
    }

    #[test]
    fn should_reject_enum_variant_missing_field() {
        check(
            indoc! {"
                enum Result {
                    Ok(value: Int),
                    Err(message: String),
                }
            "},
            &[],
            "Result::Ok()",
            expect![[r#"
                error: Enum variant 'Result::Ok' is missing fields: value
                Result::Ok()
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_variant_missing_two_fields() {
        check(
            indoc! {"
                enum Point {
                    XY(x: Int, y: Int),
                }
            "},
            &[],
            "Point::XY()",
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: x, y
                Point::XY()
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_variant_unknown_field() {
        check(
            indoc! {"
                enum Result {
                    Ok(value: Int),
                    Err(message: String),
                }
            "},
            &[],
            "Result::Ok(wrong: 42)",
            expect![[r#"
                error: Unknown field 'wrong' in enum variant 'Result::Ok'
                Result::Ok(wrong: 42)
                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_variant_field_type_mismatch() {
        check(
            indoc! {"
                enum Result {
                    Ok(value: Int),
                    Err(message: String),
                }
            "},
            &[],
            r#"Result::Ok(value: "hello")"#,
            expect![[r#"
                error: Field 'value' in 'Result::Ok' expects type Int, but got String
                Result::Ok(value: "hello")
                                  ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_fields_on_unit_variant() {
        check(
            indoc! {"
                enum Maybe {
                    Just(value: Int),
                    Nothing,
                }
            "},
            &[],
            "Maybe::Nothing(value: 42)",
            expect![[r#"
                error: Unknown field 'value' in enum variant 'Maybe::Nothing'
                Maybe::Nothing(value: 42)
                               ^^^^^
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

    ///////////////////////////////////////////////////////////////////////////
    // OPTION TYPE                                                           //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_some_with_integer_literal() {
        check("", &[], "Some(42)", expect!["Option[Int]"]);
    }

    #[test]
    fn should_accept_some_with_string_literal() {
        check("", &[], r#"Some("hello")"#, expect!["Option[String]"]);
    }

    #[test]
    fn should_accept_some_with_boolean_literal() {
        check("", &[], "Some(true)", expect!["Option[Bool]"]);
    }

    #[test]
    fn should_accept_some_with_float_literal() {
        check("", &[], "Some(3.14)", expect!["Option[Float]"]);
    }

    #[test]
    fn should_accept_some_with_variable() {
        check(
            "",
            &[("name", "String")],
            "Some(name)",
            expect!["Option[String]"],
        );
    }

    #[test]
    fn should_accept_some_with_field_access() {
        check(
            "record User {name: String}",
            &[("user", "User")],
            "Some(user.name)",
            expect!["Option[String]"],
        );
    }

    #[test]
    fn should_accept_nested_some() {
        check("", &[], "Some(Some(42))", expect!["Option[Option[Int]]"]);
    }

    #[test]
    fn should_accept_some_with_array() {
        check("", &[], "Some([1, 2, 3])", expect!["Option[Array[Int]]"]);
    }

    #[test]
    fn should_accept_some_with_record_literal() {
        check(
            "record Point {x: Int, y: Int}",
            &[],
            "Some(Point(x: 1, y: 2))",
            expect!["Option[test::Point]"],
        );
    }

    #[test]
    fn should_accept_some_with_enum_literal() {
        check(
            indoc! {"
                enum Color {
                    Red,
                    Green,
                    Blue,
                }
            "},
            &[],
            "Some(Color::Red)",
            expect!["Option[test::Color]"],
        );
    }

    #[test]
    fn should_accept_option_variable() {
        check(
            "",
            &[("maybe_count", "Option[Int]")],
            "maybe_count",
            expect!["Option[Int]"],
        );
    }

    #[test]
    fn should_accept_option_in_record_field() {
        check(
            "record User {name: String, age: Option[Int]}",
            &[("user", "User")],
            "user.age",
            expect!["Option[Int]"],
        );
    }

    #[test]
    fn should_accept_none_in_record_field_assignment() {
        check(
            "record User {name: String, nickname: Option[String]}",
            &[],
            r#"User(name: "Alice", nickname: None)"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn should_accept_some_in_record_field_assignment() {
        check(
            "record User {name: String, nickname: Option[String]}",
            &[],
            r#"User(name: "Alice", nickname: Some("Ali"))"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn should_accept_nested_option_in_record_field() {
        check(
            "record Config {value: Option[Option[Int]]}",
            &[],
            "Config(value: Some(Some(42)))",
            expect!["test::Config"],
        );
    }

    #[test]
    fn should_accept_none_for_nested_option_field() {
        check(
            "record Config {value: Option[Option[Int]]}",
            &[],
            "Config(value: None)",
            expect!["test::Config"],
        );
    }

    #[test]
    fn should_accept_array_of_options() {
        check(
            "",
            &[("items", "Array[Option[Int]]")],
            "items",
            expect!["Array[Option[Int]]"],
        );
    }

    #[test]
    fn should_accept_option_of_array() {
        check(
            "",
            &[("maybe_items", "Option[Array[Int]]")],
            "maybe_items",
            expect!["Option[Array[Int]]"],
        );
    }

    #[test]
    fn should_reject_none_without_context() {
        check(
            "",
            &[],
            "None",
            expect![[r#"
                error: Cannot infer type of None without context
                None
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_type_mismatch_in_option_field() {
        check(
            "record User {name: String, age: Option[Int]}",
            &[],
            r#"User(name: "Alice", age: Some("thirty"))"#,
            expect![[r#"
                error: Field 'age' expects type Option[Int], but got Option[String]
                User(name: "Alice", age: Some("thirty"))
                                         ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_non_option_for_option_field() {
        check(
            "record User {name: String, age: Option[Int]}",
            &[],
            r#"User(name: "Alice", age: 30)"#,
            expect![[r#"
                error: Field 'age' expects type Option[Int], but got Int
                User(name: "Alice", age: 30)
                                         ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_option_equality() {
        check("", &[], "Some(1) == Some(2)", expect!["Bool"]);
    }

    #[test]
    fn should_reject_option_of_array_equality() {
        check(
            "",
            &[],
            "Some([1]) == Some([1])",
            expect![[r#"
                error: Type Option[Array[Int]] is not comparable
                Some([1]) == Some([1])
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_some_equals_none() {
        check("", &[], "Some(1) == None", expect!["Bool"]);
    }

    #[test]
    fn should_accept_none_equals_some() {
        check("", &[], "None == Some(1)", expect!["Bool"]);
    }

    #[test]
    fn should_accept_nested_some_equals_some_none() {
        check("", &[], "Some(Some(1)) == Some(None)", expect!["Bool"]);
    }

    #[test]
    fn should_accept_nested_some_equals_none() {
        check("", &[], "Some(Some(1)) == None", expect!["Bool"]);
    }

    #[test]
    fn should_reject_nested_some_with_different_inner_types() {
        check(
            "",
            &[],
            r#"Some(Some(1)) == Some(Some("2"))"#,
            expect![[r#"
                error: Can not compare Option[Option[Int]] to Option[Option[String]]
                Some(Some(1)) == Some(Some("2"))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - EMPTY                                              //
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
    // MATCH EXPRESSION - ENUM                                               //
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
                    Status::Active => user.name + user.name,
                    Status::Inactive => user.name + user.name,
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
                    Color::Red => 0,
                    _ => 1,
                }
            "#},
            expect!["Int"],
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

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - BOOLEAN                                            //
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
            expect!["Int"],
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
            expect!["Int"],
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
    // MATCH EXPRESSION - OPTION                                             //
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
            expect!["Int"],
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
                    Some(_) => 0,
                    _       => 1,
                }
            "#},
            expect!["Int"],
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
    fn should_accept_option_match_with_specific_some_and_wildcard() {
        check(
            "",
            &[("opt", "Option[Bool]")],
            indoc! {"
                match opt {
                    Some(true) => 0,
                    _          => 1,
                }
            "},
            expect!["Int"],
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
            expect!["Int"],
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
            expect!["Int"],
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
            expect!["String"],
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
    // MATCH EXPRESSION - BINDING PATTERN                                    //
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
            expect!["Bool"],
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
            expect!["test::Color"],
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
            expect!["Option[Int]"],
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
            expect!["Int"],
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
            expect!["Int"],
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
            expect!["Option[Int]"],
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
            expect!["Bool"],
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
            expect!["Int"],
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

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - RECORD                                             //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_record_match_with_all_fields() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User(name: n, age: _) => n,
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_record_match_with_wildcard_fields() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User(name: _, age: _) => "matched",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_record_match_with_nested_enum_pattern() {
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
            indoc! {r#"
                match user {
                    User(name: n, status: Status::Active)   => n,
                    User(name: _, status: Status::Inactive) => "inactive",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_reject_record_match_with_missing_fields() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User(name: n) => n,
                }
            "#},
            expect![[r#"
                error: Record 'User' is missing fields: age
                    User(name: n) => n,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_record_match_with_unknown_field() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User(name: n, email: e) => n,
                }
            "#},
            expect![[r#"
                error: Unknown field 'email' in record 'User'
                    User(name: n, email: e) => n,
                                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_record_match_with_wrong_record_type() {
        check(
            indoc! {"
                record User {
                    name: String,
                }
                record Admin {
                    name: String,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    Admin(name: n) => n,
                }
            "#},
            expect![[r#"
                error: Match pattern record 'Admin' does not match subject record 'User'
                    Admin(name: n) => n,
                    ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unused_binding_in_record_pattern() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User(name: n, age: a) => "hello",
                }
            "#},
            expect![[r#"
                error: Unused binding 'a' in match arm
                    User(name: n, age: a) => "hello",
                                       ^
            "#]],
        );
    }

    #[test]
    fn should_accept_record_match_with_nested_option_pattern() {
        check(
            indoc! {"
                record User {
                    name: String,
                    email: Option[String],
                }
            "},
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User(name: _, email: Some(e)) => e,
                    User(name: n, email: None)    => n,
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_option_match_with_nested_record_pattern() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("maybe_user", "Option[User]")],
            indoc! {r#"
                match maybe_user {
                    Some(User(name: n, age: _)) => n,
                    None                        => "Default User",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_option_of_record_match_with_some_and_none() {
        check(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            &[("maybe_user", "Option[User]")],
            indoc! {r#"
                match maybe_user {
                    Some(_) => "has user",
                    None    => "no user",
                }
            "#},
            expect!["String"],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MACROS                                                                //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_classes_macro_with_string_args() {
        check(
            "",
            &[("first", "String"), ("last", "String")],
            "classes!(first, last)",
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_classes_macro_with_single_arg() {
        check(
            "",
            &[("name", "String")],
            "classes!(name)",
            expect!["String"],
        );
    }

    #[test]
    fn should_accept_classes_macro_with_no_args() {
        check("", &[], "classes!()", expect!["String"]);
    }

    #[test]
    fn should_accept_classes_macro_with_string_literals() {
        check("", &[], r#"classes!("hello", "world")"#, expect!["String"]);
    }

    #[test]
    fn should_reject_classes_macro_with_non_string_arg() {
        check(
            "",
            &[("count", "Int")],
            "classes!(count)",
            expect![[r#"
                error: Macro 'classes' expects String arguments, but got Int
                classes!(count)
                         ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_classes_macro_with_mixed_types() {
        check(
            "",
            &[("name", "String"), ("age", "Int")],
            "classes!(name, age)",
            expect![[r#"
                error: Macro 'classes' expects String arguments, but got Int
                classes!(name, age)
                               ^^^
            "#]],
        );
    }
}
