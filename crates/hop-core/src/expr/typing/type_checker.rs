use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::join_macro::build_balanced_join;
use super::r#type::{NumericType, Type, TypeBinding, TypeEnv};
use crate::asset_reference::AssetReference;
use crate::document::{CheapString, DocumentRange};
use crate::document_id::DocumentId;
use crate::expr::TypedExpr;
use crate::expr::parsing::ParsedType;
use crate::expr::parsing::parsed_expr::{
    Constructor, ParsedBinaryOp, ParsedExpr, ParsedMatchArm, ParsedMatchPattern,
};
use crate::expr::patterns::compiler::{Compiler, Decision};
use crate::expr::patterns::{EnumMatchArm, EnumPattern, Match};
use crate::hop::typing::definition_link::DefinitionLink;
use crate::hop::typing::type_annotation::TypeAnnotation;
use crate::symbols::field_name::FieldName;
use crate::symbols::var_name::VarName;
use crate::type_error::TypeError;
use crate::variable_scope::VariableScope;

/// Resolve a parsed Type to a semantic Type.
pub fn resolve_type(
    parsed_type: &ParsedType,
    type_env: &mut TypeEnv,
    definition_links: &mut Vec<DefinitionLink>,
) -> Result<Arc<Type>, TypeError> {
    let (typ, _) = match parsed_type {
        ParsedType::String { range } => (Arc::new(Type::String), range),
        ParsedType::Bool { range } => (Arc::new(Type::Bool), range),
        ParsedType::Int { range } => (Arc::new(Type::Int), range),
        ParsedType::Float { range } => (Arc::new(Type::Float), range),
        ParsedType::Fragment { range } => (Arc::new(Type::Fragment), range),
        ParsedType::Option { element, range } => {
            let elem_type = resolve_type(element, type_env, definition_links)?;
            (Arc::new(Type::Option(elem_type)), range)
        }
        ParsedType::Array { element, range } => {
            let elem_type = resolve_type(element, type_env, definition_links)?;
            (Arc::new(Type::Array(elem_type)), range)
        }
        ParsedType::Named { name, range } => {
            let (binding, def_range) =
                type_env
                    .lookup(name)
                    .ok_or_else(|| TypeError::UndefinedType {
                        type_name: name.clone(),
                        range: range.clone(),
                    })?;
            match binding {
                TypeBinding::Value(typ) => {
                    let typ = typ.clone();
                    definition_links.push(DefinitionLink {
                        use_range: range.clone(),
                        definition_range: def_range.clone(),
                    });
                    (typ, range)
                }
                TypeBinding::Component(_) => {
                    return Err(TypeError::ComponentUsedAsType {
                        name: name.clone(),
                        range: range.clone(),
                    });
                }
            }
        }
    };
    Ok(typ)
}

/// Resolve a parsed Expr to a typed Expr.
///
/// The optional `expected_type` is used for bidirectional type checking, allowing
/// empty array literals to infer their element type from context.
pub fn typecheck_expr(
    parsed_expr: &ParsedExpr,
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut TypeEnv,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    expected_type: Option<&Arc<Type>>,
    asset_references: &mut Vec<AssetReference>,
) -> Result<TypedExpr, TypeError> {
    match parsed_expr {
        ParsedExpr::Var {
            value: var_name, ..
        } => {
            if let Some((var_type, def_range)) = var_env.lookup(var_name) {
                annotations.push(TypeAnnotation::TypeForVarName {
                    range: parsed_expr.range().clone(),
                    typ: var_type.clone(),
                    var_name: var_name.clone(),
                });
                definition_links.push(DefinitionLink {
                    use_range: parsed_expr.range().clone(),
                    definition_range: def_range.clone(),
                });
                Ok(TypedExpr::Var {
                    value: var_name.clone(),
                    kind: var_type.clone(),
                })
            } else {
                Err(TypeError::UndefinedVariable {
                    name: var_name.clone(),
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
            let typed_base = typecheck_expr(
                record,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let base_type = typed_base.as_type();

            match &base_type {
                Type::Record {
                    name: record_name,
                    fields,
                    ..
                } => {
                    if let Some((_, field_type, _)) =
                        fields.iter().find(|(f, _, _)| f.as_str() == field.as_str())
                    {
                        Ok(TypedExpr::FieldAccess {
                            kind: field_type.clone(),
                            record: Box::new(typed_base),
                            field: field.clone(),
                        })
                    } else {
                        Err(TypeError::FieldNotFoundInRecord {
                            field: field.clone(),
                            record_name: record_name.clone(),
                            range: range.clone(),
                        })
                    }
                }
                _ => Err(TypeError::CannotUseAsRecord {
                    typ: typed_base.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )
            .or_else(|left_err| {
                let typed_right = typecheck_expr(
                    right,
                    var_env,
                    type_env,
                    annotations,
                    definition_links,
                    None,
                    asset_references,
                )?;
                typecheck_expr(
                    left,
                    var_env,
                    type_env,
                    annotations,
                    definition_links,
                    Some(&typed_right.get_type()),
                    asset_references,
                )
                .map_err(|_| left_err)
            })?;
            // Use left's type as context for right (allows Some(1) == None)
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                Some(&typed_left.get_type()),
                asset_references,
            )?;

            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let Some(left_comparable) = left_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: typed_left.get_type(),
                    range: left.range().clone(),
                });
            };

            let Some(right_comparable) = right_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: typed_right.get_type(),
                    range: right.range().clone(),
                });
            };

            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: typed_left.get_type(),
                    right: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let Some(left_comparable) = left_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: typed_left.get_type(),
                    range: left.range().clone(),
                });
            };

            let Some(right_comparable) = right_type.as_equatable_type() else {
                return Err(TypeError::TypeIsNotComparable {
                    t: typed_right.get_type(),
                    range: right.range().clone(),
                });
            };

            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: typed_left.get_type(),
                    right: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_left.get_type(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_right.get_type(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: typed_left.get_type(),
                    right: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_left.get_type(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_right.get_type(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: typed_left.get_type(),
                    right: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_left.get_type(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_right.get_type(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: typed_left.get_type(),
                    right: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            let left_comparable =
                left_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_left.get_type(),
                        range: left.range().clone(),
                    })?;

            let right_comparable =
                right_type
                    .as_comparable_type()
                    .ok_or_else(|| TypeError::TypeIsNotComparable {
                        t: typed_right.get_type(),
                        range: right.range().clone(),
                    })?;

            // Both operands must be the same comparable type
            if left_comparable != right_comparable {
                return Err(TypeError::CannotCompareTypes {
                    left: typed_left.get_type(),
                    right: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            // LogicalAnd only works with Bool expressions
            if *left_type != Type::Bool {
                return Err(TypeError::LogicalAndTypeMismatch {
                    range: left.range().clone(),
                });
            }

            if *right_type != Type::Bool {
                return Err(TypeError::LogicalAndTypeMismatch {
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let left_type = typed_left.as_type();
            let right_type = typed_right.as_type();

            if *left_type != Type::Bool {
                return Err(TypeError::LogicalOrTypeMismatch {
                    range: left.range().clone(),
                });
            }

            if *right_type != Type::Bool {
                return Err(TypeError::LogicalOrTypeMismatch {
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
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
                        left_type: typed_left.get_type(),
                        right_type: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
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
                        left_type: typed_left.get_type(),
                        right_type: typed_right.get_type(),
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
            let typed_left = typecheck_expr(
                left,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let typed_right = typecheck_expr(
                right,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
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
                        left_type: typed_left.get_type(),
                        right_type: typed_right.get_type(),
                        range: left.range().clone().to(right.range().clone()),
                    })
                }
            }
        }
        ParsedExpr::BooleanNegation { operand, .. } => {
            let typed_operand = typecheck_expr(
                operand,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let operand_type = typed_operand.as_type();

            if *operand_type != Type::Bool {
                return Err(TypeError::BooleanNegationTypeMismatch {
                    found: typed_operand.get_type(),
                    range: operand.range().clone(),
                });
            }

            Ok(TypedExpr::BooleanNegation {
                operand: Box::new(typed_operand),
            })
        }
        ParsedExpr::NumericNegation { operand, .. } => {
            let typed_operand = typecheck_expr(
                operand,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let operand_type = typed_operand.as_type();

            match operand_type {
                Type::Int => Ok(TypedExpr::NumericNegation {
                    operand: Box::new(typed_operand),
                    operand_type: NumericType::Int,
                }),
                Type::Float => Ok(TypedExpr::NumericNegation {
                    operand: Box::new(typed_operand),
                    operand_type: NumericType::Float,
                }),
                _ => Err(TypeError::NumericNegationTypeMismatch {
                    found: typed_operand.get_type(),
                    range: operand.range().clone(),
                }),
            }
        }
        ParsedExpr::ArrayLiteral { elements, range } => {
            if elements.is_empty() {
                // Empty array: infer element type from expected type
                let elem_type = match expected_type.map(|t| t.as_ref()) {
                    Some(Type::Array(elem)) => elem.clone(),
                    _ => {
                        return Err(TypeError::CannotInferEmptyArrayType {
                            range: range.clone(),
                        });
                    }
                };
                Ok(TypedExpr::ArrayLiteral {
                    elements: vec![],
                    kind: Arc::new(Type::Array(elem_type)),
                })
            } else {
                // Determine expected element type from context if available
                let expected_elem_type: Option<Arc<Type>> = match expected_type.map(|t| t.as_ref())
                {
                    Some(Type::Array(elem)) => Some(elem.clone()),
                    _ => None,
                };

                let mut typed_elements = Vec::new();

                // Check the type of the first element
                let first_typed = typecheck_expr(
                    &elements[0],
                    var_env,
                    type_env,
                    annotations,
                    definition_links,
                    expected_elem_type.as_ref(),
                    asset_references,
                )?;
                let first_type = first_typed.get_type();
                typed_elements.push(first_typed);

                // Check that all elements have the same type
                // Use first element's type as context for subsequent elements
                let elem_context = expected_elem_type.as_ref().unwrap_or(&first_type);
                for element in elements.iter().skip(1) {
                    let typed_element = typecheck_expr(
                        element,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        Some(elem_context),
                        asset_references,
                    )?;
                    let element_type = typed_element.get_type();
                    if *element_type != *first_type {
                        return Err(TypeError::ArrayElementTypeMismatch {
                            expected: first_type.clone(),
                            found: element_type,
                            range: element.range().clone(),
                        });
                    }
                    typed_elements.push(typed_element);
                }

                Ok(TypedExpr::ArrayLiteral {
                    elements: typed_elements,
                    kind: Arc::new(Type::Array(first_type)),
                })
            }
        }
        ParsedExpr::RecordLiteral {
            record_name,
            record_name_range,
            fields,
            range,
        } => {
            // Check if the record type is defined
            let (binding, def_range) =
                type_env
                    .lookup(record_name)
                    .ok_or_else(|| TypeError::UndefinedRecord {
                        record_name: record_name.clone(),
                        range: range.clone(),
                    })?;
            let def_range = def_range.clone();
            let record_type = match binding {
                TypeBinding::Value(typ) => typ.clone(),
                TypeBinding::Component(_) => {
                    return Err(TypeError::UndefinedRecord {
                        record_name: record_name.clone(),
                        range: range.clone(),
                    });
                }
            };

            // Add type annotation and definition link for the record name
            annotations.push(TypeAnnotation::TypeForTypeName {
                range: record_name_range.clone(),
                typ: record_type.clone(),
                type_name: record_name.clone(),
            });
            definition_links.push(DefinitionLink {
                use_range: record_name_range.clone(),
                definition_range: def_range,
            });

            // Extract fields from the record type
            let record_fields = match record_type.as_ref() {
                Type::Record { fields, .. } => fields,
                _ => {
                    return Err(TypeError::UndefinedRecord {
                        record_name: record_name.clone(),
                        range: range.clone(),
                    });
                }
            };

            // Build a map of expected fields from the record type
            let expected_fields = record_fields
                .iter()
                .map(|(name, typ, _)| (name.clone(), typ.clone()))
                .collect::<HashMap<_, _>>();

            // Check for unknown fields and type mismatches
            let mut typed_fields = Vec::new();
            let mut provided_fields = HashSet::new();

            for (field_name, field_value) in fields {
                // Check if this field exists in the record
                let expected_type = expected_fields.get(field_name).ok_or_else(|| {
                    TypeError::RecordUnknownField {
                        field_name: field_name.clone(),
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
                    definition_links,
                    Some(expected_type),
                    asset_references,
                )?;
                let actual_type = typed_value.get_type();

                // Check that the types match
                if *actual_type != **expected_type {
                    return Err(TypeError::RecordLiteralFieldTypeMismatch {
                        field_name: field_name.clone(),
                        expected: expected_type.clone(),
                        found: actual_type,
                        range: field_value.range().clone(),
                    });
                }

                provided_fields.insert(field_name.clone());
                typed_fields.push((field_name.clone(), typed_value));
            }

            // Check for missing fields
            let missing_fields = expected_fields
                .keys()
                .filter(|name| !provided_fields.contains(name))
                .cloned()
                .collect::<Vec<_>>();
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
            enum_name_range,
            range,
        } => {
            // Look up the enum type in the type environment
            let (binding, def_range) =
                type_env
                    .lookup(enum_name)
                    .ok_or_else(|| TypeError::UndefinedEnum {
                        enum_name: enum_name.clone(),
                        range: range.clone(),
                    })?;
            let def_range = def_range.clone();
            let enum_type = match binding {
                TypeBinding::Value(typ) => typ.clone(),
                TypeBinding::Component(_) => {
                    return Err(TypeError::UndefinedEnum {
                        enum_name: enum_name.clone(),
                        range: range.clone(),
                    });
                }
            };

            // Add type annotation and definition link for the constructor
            annotations.push(TypeAnnotation::TypeForTypeName {
                range: constructor_range.clone(),
                typ: enum_type.clone(),
                type_name: enum_name.clone(),
            });
            definition_links.push(DefinitionLink {
                use_range: enum_name_range.clone(),
                definition_range: def_range,
            });

            // Verify it's actually an enum type and get the variant's fields
            let variant_fields = match enum_type.as_ref() {
                Type::Enum { variants, .. } => {
                    let variant = variants
                        .iter()
                        .find(|variant| variant.name.as_str() == variant_name.as_str());
                    match variant {
                        Some(variant) => variant.fields.clone(),
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
                let mut provided_field_names: HashSet<FieldName> = HashSet::new();

                // Type check each provided field
                for (field_name, field_name_range, field_expr) in fields {
                    // Check if field exists in variant definition
                    let expected_field = variant_fields
                        .iter()
                        .find(|(name, _, _)| name.as_str() == field_name.as_str());

                    match expected_field {
                        Some((_, expected_type, _)) => {
                            // Type check the field expression
                            let typed_field_expr = typecheck_expr(
                                field_expr,
                                var_env,
                                type_env,
                                annotations,
                                definition_links,
                                Some(expected_type),
                                asset_references,
                            )?;

                            // Verify type matches
                            let actual_type = typed_field_expr.get_type();
                            if *actual_type != **expected_type {
                                return Err(TypeError::EnumVariantFieldTypeMismatch {
                                    enum_name: enum_name.clone(),
                                    variant_name: variant_name.clone(),
                                    field_name: field_name.clone(),
                                    expected: expected_type.clone(),
                                    found: actual_type,
                                    range: field_expr.range().clone(),
                                });
                            }

                            provided_field_names.insert(field_name.clone());
                            typed_fields.push((field_name.clone(), typed_field_expr));
                        }
                        None => {
                            return Err(TypeError::EnumVariantUnknownField {
                                enum_name: enum_name.clone(),
                                variant_name: variant_name.clone(),
                                field_name: field_name.clone(),
                                range: field_name_range.clone(),
                            });
                        }
                    }
                }

                // Check for missing fields
                let missing_fields: Vec<FieldName> = variant_fields
                    .iter()
                    .filter(|(name, _, _)| !provided_field_names.contains(name))
                    .map(|(name, _, _)| name.clone())
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
                    let expected_inner_type: Option<Arc<Type>> =
                        match expected_type.map(|t| t.as_ref()) {
                            Some(Type::Option(elem)) => Some(elem.clone()),
                            _ => None,
                        };

                    // Type check the inner value
                    let typed_inner = typecheck_expr(
                        inner_expr,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        expected_inner_type.as_ref(),
                        asset_references,
                    )?;
                    let inner_type = typed_inner.get_type();
                    Ok(TypedExpr::OptionLiteral {
                        value: Some(Box::new(typed_inner)),
                        kind: Arc::new(Type::Option(inner_type)),
                    })
                }
                None => {
                    // None: infer element type from expected type
                    let elem_type = match expected_type.map(|t| t.as_ref()) {
                        Some(Type::Option(elem)) => elem.clone(),
                        _ => {
                            return Err(TypeError::CannotInferNoneType {
                                range: range.clone(),
                            });
                        }
                    };
                    Ok(TypedExpr::OptionLiteral {
                        value: None,
                        kind: Arc::new(Type::Option(elem_type)),
                    })
                }
            }
        }
        ParsedExpr::FragmentEmpty { .. } => Ok(TypedExpr::FragmentEmpty),
        ParsedExpr::Match { subject, arms, .. } => typecheck_match(
            subject,
            arms,
            var_env,
            type_env,
            annotations,
            definition_links,
            asset_references,
        ),
        ParsedExpr::MacroInvocation {
            name,
            subject_range,
            args,
            range,
            ..
        } => match name.as_str() {
            "join" => {
                let string_type = Arc::new(Type::String);

                let mut typed_args = Vec::with_capacity(args.len());
                for arg in args {
                    let typed = typecheck_expr(
                        arg,
                        var_env,
                        type_env,
                        annotations,
                        definition_links,
                        Some(&string_type),
                        asset_references,
                    )?;
                    if typed.as_type() != &Type::String {
                        return Err(TypeError::MacroArgumentTypeMismatch {
                            macro_name: "join".to_string(),
                            expected: Arc::new(Type::String),
                            found: typed.get_type(),
                            range: arg.range().clone(),
                        });
                    }
                    typed_args.push(typed);
                }

                annotations.push(TypeAnnotation::Description {
                    title: "join!(String, ...) -> String".to_string(),
                    description: "Joins strings with spaces.".to_string(),
                    range: subject_range.clone(),
                });

                match typed_args.len() {
                    0 => Ok(TypedExpr::StringLiteral {
                        value: CheapString::new(String::new()),
                    }),
                    1 => Ok(typed_args.pop().unwrap()),
                    _ => Ok(build_balanced_join(typed_args)),
                }
            }
            "asset" => {
                // Exactly one argument
                if args.len() != 1 {
                    return Err(TypeError::AssetMacroArity {
                        actual: args.len(),
                        range: range.clone(),
                    });
                }
                // Must be a string literal
                let (path, path_range) = match &args[0] {
                    ParsedExpr::StringLiteral { value, range } => (value.clone(), range.clone()),
                    other => {
                        return Err(TypeError::AssetMacroNonLiteralArg {
                            range: other.range().clone(),
                        });
                    }
                };
                // Must start with `/`
                if !path.as_str().starts_with('/') {
                    return Err(TypeError::AssetPathMustBeAbsolute { range: path_range });
                }

                let document_id = DocumentId::new(path.trim_start_matches('/')).unwrap();

                asset_references.push(AssetReference {
                    range: range.clone(),
                    document_id,
                });

                annotations.push(TypeAnnotation::Description {
                    title: "asset!(literal: String) -> String".to_string(),
                    description: "Resolves to a path served by the dev server in dev mode \
                         and prefixed by `assets.production_prefix` in production builds."
                        .to_string(),
                    range: subject_range.clone(),
                });

                Ok(TypedExpr::Asset { path })
            }
            _ => unreachable!("Unknown macro '{}' should be caught at parse time", name),
        },
        ParsedExpr::MethodCall {
            receiver,
            method,
            method_range,
            range,
        } => {
            let typed_receiver = typecheck_expr(
                receiver,
                var_env,
                type_env,
                annotations,
                definition_links,
                None,
                asset_references,
            )?;
            let receiver_type = typed_receiver.get_type();

            match (receiver_type.as_ref(), method.as_str()) {
                (Type::Array(_), "len") => {
                    annotations.push(TypeAnnotation::Description {
                        title: "Array::len() -> Int".to_string(),
                        description: "Returns the number of elements in the array.".to_string(),
                        range: method_range.clone(),
                    });
                    Ok(TypedExpr::ArrayLength {
                        array: Box::new(typed_receiver),
                    })
                }
                (Type::Array(_), "is_empty") => {
                    annotations.push(TypeAnnotation::Description {
                        title: "Array::is_empty() -> Bool".to_string(),
                        description: "Returns `true` if the array is empty.".to_string(),
                        range: method_range.clone(),
                    });
                    Ok(TypedExpr::ArrayIsEmpty {
                        array: Box::new(typed_receiver),
                    })
                }
                (Type::Int, "to_string") => Ok(TypedExpr::IntToString {
                    value: Box::new(typed_receiver),
                }),
                (Type::Int, "to_float") => Ok(TypedExpr::IntToFloat {
                    value: Box::new(typed_receiver),
                }),
                (Type::Float, "to_int") => Ok(TypedExpr::FloatToInt {
                    value: Box::new(typed_receiver),
                }),
                (Type::String, "is_empty") => {
                    annotations.push(TypeAnnotation::Description {
                        title: "String::is_empty() -> Bool".to_string(),
                        description: "Returns `true` if the string is empty.".to_string(),
                        range: method_range.clone(),
                    });
                    Ok(TypedExpr::StringIsEmpty {
                        string: Box::new(typed_receiver),
                    })
                }
                (Type::Option(_), "is_some") => {
                    annotations.push(TypeAnnotation::Description {
                        title: "Option::is_some() -> Bool".to_string(),
                        description: "Returns `true` if the option contains a value.".to_string(),
                        range: method_range.clone(),
                    });
                    Ok(TypedExpr::OptionIsSome {
                        option: Box::new(typed_receiver),
                    })
                }
                (Type::Option(_), "is_none") => {
                    annotations.push(TypeAnnotation::Description {
                        title: "Option::is_none() -> Bool".to_string(),
                        description: "Returns `true` if the option is `None`.".to_string(),
                        range: method_range.clone(),
                    });
                    Ok(TypedExpr::OptionIsNone {
                        option: Box::new(typed_receiver),
                    })
                }
                _ => Err(TypeError::MethodNotAvailable {
                    method: method.as_str().to_string(),
                    typ: receiver_type,
                    range: range.clone(),
                }),
            }
        }
    }
}

// Typecheck a match expression and compile it to a TypedExpr.
fn typecheck_match(
    subject: &ParsedExpr,
    arms: &[ParsedMatchArm],
    var_env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut TypeEnv,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Result<TypedExpr, TypeError> {
    let typed_subject = typecheck_expr(
        subject,
        var_env,
        type_env,
        annotations,
        definition_links,
        None,
        asset_references,
    )?;

    let subject_name = match &typed_subject {
        TypedExpr::Var { value, .. } => value.clone(),
        _ => var_env.fresh_var(),
    };

    let patterns: Vec<_> = arms.iter().map(|arm| arm.pattern.clone()).collect();
    let tree = Compiler::new(var_env.fresh_var_counter()).compile(
        &patterns,
        &subject_name,
        typed_subject.get_type(),
        subject.range(),
    )?;

    let (typed_bodies, result_type) = typecheck_arm_bodies(
        arms,
        typed_subject.get_type(),
        var_env,
        type_env,
        annotations,
        definition_links,
        asset_references,
    )?;

    let result = decision_to_typed_expr(&tree, &typed_bodies, result_type, Some(typed_subject));

    Ok(result)
}

/// Extract binding variables from a pattern and return them with their types and ranges.
/// The type is derived from the subject type and the position in the pattern.
pub fn extract_bindings_from_pattern(
    pattern: &ParsedMatchPattern,
    subject_type: Arc<Type>,
) -> Vec<(VarName, Arc<Type>, DocumentRange)> {
    match pattern {
        ParsedMatchPattern::Binding { name, range } => {
            vec![(name.clone(), subject_type, range.clone())]
        }
        ParsedMatchPattern::Wildcard { .. } => vec![],
        ParsedMatchPattern::Constructor {
            constructor,
            args,
            fields,
            ..
        } => match constructor {
            Constructor::OptionSome => {
                let Type::Option(inner_type) = &*subject_type else {
                    unreachable!("OptionSome pattern requires Option type")
                };
                let Some(inner_pattern) = args.first() else {
                    unreachable!("OptionSome pattern requires an argument")
                };
                extract_bindings_from_pattern(inner_pattern, inner_type.clone())
            }
            Constructor::Record { .. } => {
                let Type::Record {
                    fields: type_fields,
                    ..
                } = &*subject_type
                else {
                    unreachable!("Record pattern requires Record type")
                };

                let mut bindings = Vec::new();
                for (field_name, _, field_pattern) in fields {
                    let field_type = type_fields
                        .iter()
                        .find(|(name, _, _)| name == field_name)
                        .map(|(_, typ, _)| typ.clone())
                        .expect("field type not found");
                    bindings.extend(extract_bindings_from_pattern(field_pattern, field_type));
                }
                bindings
            }
            Constructor::EnumVariant { variant_name, .. } => {
                let Type::Enum { variants, .. } = &*subject_type else {
                    unreachable!("EnumVariant pattern requires Enum type")
                };

                let variant_fields = variants
                    .iter()
                    .find(|variant| &variant.name == variant_name)
                    .map(|variant| variant.fields.clone())
                    .expect("variant not found in enum type");

                let mut bindings = Vec::new();
                for (field_name, _, field_pattern) in fields {
                    let field_type = variant_fields
                        .iter()
                        .find(|(name, _, _)| name == field_name)
                        .map(|(_, typ, _)| typ.clone())
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

/// Collect definition links for enum variant references in match patterns.
fn collect_pattern_definition_links(
    pattern: &ParsedMatchPattern,
    type_env: &mut TypeEnv,
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
            if let Some((_, def_range)) = type_env.lookup(enum_name) {
                definition_links.push(DefinitionLink {
                    use_range: enum_name_range.clone(),
                    definition_range: def_range.clone(),
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

/// Typecheck all arm bodies and verify they all have the same type.
/// Returns the typed bodies and the common result type.
fn typecheck_arm_bodies(
    arms: &[ParsedMatchArm],
    subject_type: Arc<Type>,
    env: &mut VariableScope<VarName, (Arc<Type>, DocumentRange)>,
    type_env: &mut TypeEnv,
    annotations: &mut Vec<TypeAnnotation>,
    definition_links: &mut Vec<DefinitionLink>,
    asset_references: &mut Vec<AssetReference>,
) -> Result<(Vec<TypedExpr>, Arc<Type>), TypeError> {
    let mut typed_bodies = Vec::new();
    let mut result_type: Option<Arc<Type>> = None;

    for arm in arms {
        // Collect definition links for enum variant references in patterns
        collect_pattern_definition_links(&arm.pattern, type_env, definition_links);

        // Extract binding variables from the pattern and add them to the environment
        let bindings = extract_bindings_from_pattern(&arm.pattern, subject_type.clone());
        let mut pushed_count = 0;
        for (name, typ, range) in &bindings {
            match env.push(name.clone(), (typ.clone(), range.clone())) {
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
        let typed_body = typecheck_expr(
            &arm.body,
            env,
            type_env,
            annotations,
            definition_links,
            result_type.as_ref(),
            asset_references,
        )?;
        let body_type = typed_body.get_type();

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
                result_type = Some(body_type.clone());
            }
            Some(expected) => {
                if *body_type != **expected {
                    return Err(TypeError::MatchArmTypeMismatch {
                        expected: expected.clone(),
                        found: body_type.clone(),
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
///
/// The root subject, when present, is the expression for the root switch node,
/// used in place of a synthetic variable. Only the outermost call supplies it.
fn decision_to_typed_expr(
    decision: &Decision,
    typed_bodies: &[TypedExpr],
    result_type: Arc<Type>,
    root_subject: Option<TypedExpr>,
) -> TypedExpr {
    match decision {
        Decision::Success(body) => {
            let mut result = typed_bodies[body.value].clone();
            // The root binding binds the subject expression directly; nested
            // bindings read fresh field/payload vars (root_subject is None).
            let mut root_subject = root_subject;
            // Wrap with Let expressions for each binding (in reverse order so first binding is outermost)
            for binding in body.bindings.iter().rev() {
                let value = Box::new(root_subject.take().unwrap_or_else(|| TypedExpr::Var {
                    value: binding.source_name.clone(),
                    kind: binding.typ.clone(),
                }));
                let kind = result.get_type();
                result = TypedExpr::Let {
                    var: binding.name.clone(),
                    value,
                    body: Box::new(result),
                    kind,
                };
            }
            result
        }

        Decision::SwitchBool {
            variable,
            true_case,
            false_case,
        } => {
            let subject = Box::new(root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            }));
            TypedExpr::Match {
                match_: Match::Bool {
                    subject,
                    true_body: Box::new(decision_to_typed_expr(
                        &true_case.body,
                        typed_bodies,
                        result_type.clone(),
                        None,
                    )),
                    false_body: Box::new(decision_to_typed_expr(
                        &false_case.body,
                        typed_bodies,
                        result_type.clone(),
                        None,
                    )),
                },
                kind: result_type,
            }
        }

        Decision::SwitchOption {
            variable,
            some_case,
            none_case,
        } => {
            let subject = Box::new(root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            }));
            TypedExpr::Match {
                match_: Match::Option {
                    subject,
                    some_arm_binding: some_case.bound_name.clone(),
                    some_arm_body: Box::new(decision_to_typed_expr(
                        &some_case.body,
                        typed_bodies,
                        result_type.clone(),
                        None,
                    )),
                    none_arm_body: Box::new(decision_to_typed_expr(
                        &none_case.body,
                        typed_bodies,
                        result_type.clone(),
                        None,
                    )),
                },
                kind: result_type,
            }
        }

        Decision::SwitchEnum { variable, cases } => {
            let subject = Box::new(root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            }));

            let arms = cases
                .iter()
                .map(|case| {
                    let pattern = EnumPattern::Variant {
                        enum_name: case.enum_name.clone(),
                        variant_name: case.variant_name.clone(),
                    };

                    // Filter out wildcard bindings (bound_name is None)
                    let bindings: Vec<_> = case
                        .bindings
                        .iter()
                        .filter_map(|b| {
                            b.bound_name
                                .as_ref()
                                .map(|name| (b.field_name.clone(), name.clone()))
                        })
                        .collect();

                    let body =
                        decision_to_typed_expr(&case.body, typed_bodies, result_type.clone(), None);

                    EnumMatchArm {
                        pattern,
                        bindings,
                        body,
                    }
                })
                .collect();

            TypedExpr::Match {
                match_: Match::Enum { subject, arms },
                kind: result_type,
            }
        }

        Decision::SwitchRecord { variable, case } => {
            let subject = root_subject.unwrap_or_else(|| TypedExpr::Var {
                value: variable.name.clone(),
                kind: variable.typ.clone(),
            });

            let body = decision_to_typed_expr(&case.body, typed_bodies, result_type.clone(), None);

            // Skip wildcard bindings (bound_name is None)
            let bindings: Vec<(FieldName, VarName)> = case
                .bindings
                .iter()
                .filter_map(|binding| {
                    binding
                        .bound_name
                        .as_ref()
                        .map(|name| (binding.field_name.clone(), name.clone()))
                })
                .collect();

            TypedExpr::LetRecordDestructure {
                subject: Box::new(subject),
                bindings,
                body: Box::new(body),
                kind: result_type,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use crate::document_id::DocumentId;
    use crate::expr::parse_expr;
    use crate::expr::typing::r#type::EnumVariant;
    use crate::symbols::field_name::FieldName;
    use crate::symbols::type_name::TypeName;
    use crate::{document::DocumentCursor, expr::parsing::parse_type::parse_type};
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use std::collections::VecDeque;

    enum Decl<'a> {
        Record {
            name: &'a str,
            fields: &'a [(&'a str, &'a str)],
        },
        Enum {
            name: &'a str,
            variants: &'a [&'a str],
        },
        EnumWithFields {
            name: &'a str,
            variants: &'a [(&'a str, &'a [(&'a str, &'a str)])],
        },
    }

    fn record<'a>(name: &'a str, fields: &'a [(&'a str, &'a str)]) -> Decl<'a> {
        Decl::Record { name, fields }
    }

    fn enum_unit<'a>(name: &'a str, variants: &'a [&'a str]) -> Decl<'a> {
        Decl::Enum { name, variants }
    }

    fn enum_<'a>(name: &'a str, variants: &'a [(&'a str, &'a [(&'a str, &'a str)])]) -> Decl<'a> {
        Decl::EnumWithFields { name, variants }
    }

    fn parse_type_str(
        type_str: &str,
        type_env: &mut TypeEnv,
        definition_links: &mut Vec<DefinitionLink>,
    ) -> (Arc<Type>, DocumentRange) {
        let cursor =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), type_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = Vec::new();
        let parsed_type = parse_type(&mut iter, &mut comments, &mut errors, &range)
            .expect("Failed to parse type");
        let typ = resolve_type(&parsed_type, type_env, definition_links)
            .expect("Test type should be valid");
        (typ, range)
    }

    fn run_check(decls: &[Decl<'_>], env_vars: &[(&str, &str)], expr_str: &str) -> (String, bool) {
        let mut env: VariableScope<VarName, (Arc<Type>, DocumentRange)> = VariableScope::new();
        let mut type_env: TypeEnv = VariableScope::new();
        let mut definition_links = Vec::new();
        let mut asset_references = Vec::new();
        let test_module = DocumentId::new("test.hop").unwrap();

        // Synthetic range used as the "definition site" for builder-declared types.
        let decl_range =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), String::new()).range();

        for decl in decls {
            match decl {
                Decl::Record { name, fields } => {
                    let type_name = TypeName::new(name).unwrap();
                    // Register placeholder so self-referential fields can resolve
                    let _ = type_env.push(
                        type_name.clone(),
                        (
                            TypeBinding::Value(Arc::new(Type::Record {
                                module: test_module.clone(),
                                name: type_name.clone(),
                                fields: vec![],
                            })),
                            decl_range.clone(),
                        ),
                    );

                    let resolved_fields: Vec<_> = fields
                        .iter()
                        .map(|(field_name, type_str)| {
                            let (typ, _) =
                                parse_type_str(type_str, &mut type_env, &mut definition_links);
                            (FieldName::new(field_name).unwrap(), typ, None)
                        })
                        .collect();

                    // Remove placeholder
                    let _ = type_env.pop();

                    let record_type = Type::Record {
                        module: test_module.clone(),
                        name: type_name.clone(),
                        fields: resolved_fields,
                    };
                    let _ = type_env.push(
                        type_name,
                        (
                            TypeBinding::Value(Arc::new(record_type)),
                            decl_range.clone(),
                        ),
                    );
                }
                Decl::Enum { name, variants } => {
                    let type_name = TypeName::new(name).unwrap();
                    let typed_variants: Vec<_> = variants
                        .iter()
                        .map(|variant| EnumVariant {
                            name: TypeName::new(variant).unwrap(),
                            fields: vec![],
                        })
                        .collect();

                    let enum_type = Type::Enum {
                        module: test_module.clone(),
                        name: type_name.clone(),
                        variants: typed_variants,
                    };
                    let _ = type_env.push(
                        type_name,
                        (TypeBinding::Value(Arc::new(enum_type)), decl_range.clone()),
                    );
                }
                Decl::EnumWithFields { name, variants } => {
                    let type_name = TypeName::new(name).unwrap();
                    // Register placeholder so self-referential variants can resolve
                    let _ = type_env.push(
                        type_name.clone(),
                        (
                            TypeBinding::Value(Arc::new(Type::Enum {
                                module: test_module.clone(),
                                name: type_name.clone(),
                                variants: vec![],
                            })),
                            decl_range.clone(),
                        ),
                    );

                    let typed_variants: Vec<_> = variants
                        .iter()
                        .map(|(variant_name, fields)| {
                            let typed_fields: Vec<_> = fields
                                .iter()
                                .map(|(field_name, type_str)| {
                                    let (typ, _) = parse_type_str(
                                        type_str,
                                        &mut type_env,
                                        &mut definition_links,
                                    );
                                    (FieldName::new(field_name).unwrap(), typ, None)
                                })
                                .collect();
                            EnumVariant {
                                name: TypeName::new(variant_name).unwrap(),
                                fields: typed_fields,
                            }
                        })
                        .collect();

                    let _ = type_env.pop();

                    let enum_type = Type::Enum {
                        module: test_module.clone(),
                        name: type_name.clone(),
                        variants: typed_variants,
                    };
                    let _ = type_env.push(
                        type_name,
                        (TypeBinding::Value(Arc::new(enum_type)), decl_range.clone()),
                    );
                }
            }
        }

        for (var_name, type_str) in env_vars {
            let (typ, range) = parse_type_str(type_str, &mut type_env, &mut definition_links);
            let _ = env.push(VarName::new(var_name).unwrap(), (typ, range));
        }

        let cursor =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), expr_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = Vec::new();
        let expr = parse_expr::parse_expr(&mut iter, &mut comments, &mut errors, &range)
            .expect("Failed to parse expression");

        let mut annotations = Vec::new();
        let mut definition_links = Vec::new();

        match typecheck_expr(
            &expr,
            &mut env,
            &mut type_env,
            &mut annotations,
            &mut definition_links,
            None,
            &mut asset_references,
        ) {
            Ok(typed_expr) => (typed_expr.as_type().to_string(), true),
            Err(e) => (
                DocumentAnnotator::new()
                    .with_label("error")
                    .without_location()
                    .without_line_numbers()
                    .annotate(&DocumentId::new("test.hop").unwrap(), [e])
                    .render(),
                false,
            ),
        }
    }

    fn accept(decls: &[Decl<'_>], env_vars: &[(&str, &str)], expr_str: &str, expected: Expect) {
        let (actual, ok) = run_check(decls, env_vars, expr_str);
        if !ok {
            panic!("expected expression to typecheck, got error:\n{actual}");
        }
        expected.assert_eq(&actual);
    }

    fn reject(decls: &[Decl<'_>], env_vars: &[(&str, &str)], expr_str: &str, expected: Expect) {
        let (actual, ok) = run_check(decls, env_vars, expr_str);
        if ok {
            panic!("expected a type error, but expression typechecked to: {actual}");
        }
        expected.assert_eq(&actual);
    }

    #[test]
    fn rejects_equality_between_string_and_number() {
        reject(
            &[],
            &[("name", "String"), ("count", "Float")],
            "name == count",
            expect![[r#"
                error: Cannot compare String to Float
                name == count
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_equality_between_boolean_and_string() {
        reject(
            &[],
            &[("enabled", "Bool"), ("name", "String")],
            "enabled == name",
            expect![[r#"
                error: Cannot compare Bool to String
                enabled == name
                ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_undefined_variable() {
        reject(
            &[],
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
    fn rejects_field_access_on_undefined_variable() {
        reject(
            &[],
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
    fn rejects_negation_of_string() {
        reject(
            &[],
            &[("name", "String")],
            "!name",
            expect![[r#"
                error: Mismatched type for negation: expected `Bool` got `String`
                !name
                 ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_negation_of_number() {
        reject(
            &[],
            &[("count", "Float")],
            "!count",
            expect![[r#"
                error: Mismatched type for negation: expected `Bool` got `Float`
                !count
                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_numeric_negation_of_int() {
        accept(&[], &[("x", "Int")], "-x", expect!["Int"]);
    }

    #[test]
    fn accepts_numeric_negation_of_float() {
        accept(&[], &[("x", "Float")], "-x", expect!["Float"]);
    }

    #[test]
    fn accepts_numeric_negation_of_int_literal() {
        accept(&[], &[], "-42", expect!["Int"]);
    }

    #[test]
    fn accepts_numeric_negation_of_float_literal() {
        accept(&[], &[], "-3.14", expect!["Float"]);
    }

    #[test]
    fn rejects_numeric_negation_of_string() {
        reject(
            &[],
            &[("name", "String")],
            "-name",
            expect![[r#"
                error: Mismatched type for negation: expected `Int` or `Float` got String
                -name
                 ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_numeric_negation_of_bool() {
        reject(
            &[],
            &[("flag", "Bool")],
            "-flag",
            expect![[r#"
                error: Mismatched type for negation: expected `Int` or `Float` got Bool
                -flag
                 ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_basic_variable_lookup() {
        accept(&[], &[("name", "String")], "name", expect!["String"]);
    }

    #[test]
    fn accepts_string_literal() {
        accept(&[], &[], r#""hello world""#, expect!["String"]);
    }

    #[test]
    fn accepts_boolean_literal_true() {
        accept(&[], &[], "true", expect!["Bool"]);
    }

    #[test]
    fn accepts_boolean_literal_false() {
        accept(&[], &[], "false", expect!["Bool"]);
    }

    #[test]
    fn accepts_int_literal() {
        accept(&[], &[], "42", expect!["Int"]);
    }

    #[test]
    fn accepts_float_literal() {
        accept(&[], &[], "3.14", expect!["Float"]);
    }

    #[test]
    fn accepts_string_equality() {
        accept(
            &[],
            &[("name", "String")],
            r#"name == "alice""#,
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_equality_between_float_and_int() {
        reject(
            &[],
            &[("count", "Float")],
            "count == 42",
            expect![[r#"
                error: Cannot compare Float to Int
                count == 42
                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_boolean_equality() {
        accept(
            &[],
            &[("enabled", "Bool")],
            "enabled == true",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_chained_equality() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool")],
            "a == b == true",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_negation_of_variable() {
        accept(&[], &[("enabled", "Bool")], "!enabled", expect!["Bool"]);
    }

    #[test]
    fn accepts_negation_of_true() {
        accept(&[], &[], "!true", expect!["Bool"]);
    }

    #[test]
    fn accepts_negation_of_false() {
        accept(&[], &[], "!false", expect!["Bool"]);
    }

    #[test]
    fn accepts_greater_than_with_ints() {
        accept(&[], &[("x", "Int"), ("y", "Int")], "x > y", expect!["Bool"]);
    }

    #[test]
    fn accepts_greater_than_with_floats() {
        accept(
            &[],
            &[("x", "Float"), ("y", "Float")],
            "x > y",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_greater_than_with_mixed_types() {
        reject(
            &[],
            &[("x", "Int"), ("y", "Float")],
            "x > y",
            expect![[r#"
                error: Cannot compare Int to Float
                x > y
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_less_than_or_equal_with_ints() {
        accept(
            &[],
            &[("x", "Int"), ("y", "Int")],
            "x <= y",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_less_than_or_equal_with_floats() {
        accept(
            &[],
            &[("x", "Float"), ("y", "Float")],
            "x <= y",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_less_than_or_equal_with_mixed_types() {
        reject(
            &[],
            &[("x", "Int"), ("y", "Float")],
            "x <= y",
            expect![[r#"
                error: Cannot compare Int to Float
                x <= y
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_greater_than_or_equal_with_ints() {
        accept(
            &[],
            &[("x", "Int"), ("y", "Int")],
            "x >= y",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_greater_than_or_equal_with_floats() {
        accept(
            &[],
            &[("x", "Float"), ("y", "Float")],
            "x >= y",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_greater_than_or_equal_with_mixed_types() {
        reject(
            &[],
            &[("x", "Int"), ("y", "Float")],
            "x >= y",
            expect![[r#"
                error: Cannot compare Int to Float
                x >= y
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_negation_with_equality() {
        accept(
            &[record("User", &[("active", "Bool")])],
            &[("user", "User")],
            "!user.active == false",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_parenthesized_negation() {
        accept(
            &[
                record("Status", &[("enabled", "Bool")]),
                record("Config", &[("active", "Bool")]),
            ],
            &[("status", "Status"), ("config", "Config")],
            "!(status.enabled == config.active)",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_string_concatenation() {
        accept(&[], &[], r#""hello" + "world""#, expect!["String"]);
    }

    #[test]
    fn accepts_multiple_string_concatenation() {
        accept(&[], &[], r#""hello" + " " + "world""#, expect!["String"]);
    }

    #[test]
    fn accepts_string_concatenation_with_variables() {
        accept(
            &[],
            &[("greeting", "String"), ("name", "String")],
            r#"greeting + " " + name"#,
            expect!["String"],
        );
    }

    #[test]
    fn rejects_concatenation_with_left_number() {
        reject(
            &[],
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
    fn rejects_concatenation_with_right_boolean() {
        reject(
            &[],
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
    fn accepts_int_addition() {
        accept(&[], &[], r#"42 + 58"#, expect!["Int"]);
    }

    #[test]
    fn accepts_string_concatenation_with_field_access() {
        accept(
            &[record(
                "User",
                &[("first_name", "String"), ("last_name", "String")],
            )],
            &[("user", "User")],
            r#"user.first_name + " " + user.last_name"#,
            expect!["String"],
        );
    }

    #[test]
    fn accepts_concatenation_result_comparison() {
        accept(&[], &[], r#""a" + "b" == "ab""#, expect!["Bool"]);
    }

    #[test]
    fn accepts_logical_and_with_boolean_variables() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool")],
            "a && b",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_logical_and_with_boolean_literals() {
        accept(&[], &[], "true && false", expect!["Bool"]);
    }

    #[test]
    fn accepts_logical_and_with_field_access() {
        accept(
            &[record("User", &[("enabled", "Bool"), ("active", "Bool")])],
            &[("user", "User")],
            "user.enabled && user.active",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_logical_and_with_comparison() {
        accept(
            &[],
            &[("x", "Int"), ("y", "Int"), ("enabled", "Bool")],
            "x > y && enabled",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_logical_and_with_left_string() {
        reject(
            &[],
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
    fn rejects_logical_and_with_right_int() {
        reject(
            &[],
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
    fn rejects_logical_and_with_both_strings() {
        reject(
            &[],
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
    fn accepts_and_handle_logical_and_precedence() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool")],
            "a && b == c",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_logical_or_with_boolean_variables() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool")],
            "a || b",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_logical_or_with_boolean_literals() {
        accept(&[], &[], "true || false", expect!["Bool"]);
    }

    #[test]
    fn accepts_logical_or_with_field_access() {
        accept(
            &[record("User", &[("enabled", "Bool"), ("active", "Bool")])],
            &[("user", "User")],
            "user.enabled || user.active",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_logical_or_with_comparison() {
        accept(
            &[],
            &[("x", "Int"), ("y", "Int"), ("enabled", "Bool")],
            "x > y || enabled",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_logical_or_with_left_string() {
        reject(
            &[],
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
    fn rejects_logical_or_with_right_int() {
        reject(
            &[],
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
    fn rejects_logical_or_with_both_strings() {
        reject(
            &[],
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
    fn accepts_and_handle_logical_or_precedence() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool")],
            "a || b == c",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_mixed_logical_operators() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool")],
            "a && b || c",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_and_handle_complex_logical_operator_precedence() {
        accept(
            &[],
            &[("a", "Bool"), ("b", "Bool"), ("c", "Bool"), ("d", "Bool")],
            "a || b && c || d",
            expect!["Bool"],
        );
    }

    #[test]
    fn accepts_int_addition_with_variables() {
        accept(&[], &[("x", "Int"), ("y", "Int")], "x + y", expect!["Int"]);
    }

    #[test]
    fn accepts_float_addition_with_variables() {
        accept(
            &[],
            &[("x", "Float"), ("y", "Float")],
            "x + y",
            expect!["Float"],
        );
    }

    #[test]
    fn accepts_string_addition_with_variables() {
        accept(
            &[],
            &[("s1", "String"), ("s2", "String")],
            "s1 + s2",
            expect!["String"],
        );
    }

    #[test]
    fn accepts_int_literal_addition() {
        accept(&[], &[], "42 + 17", expect!["Int"]);
    }

    #[test]
    fn accepts_float_literal_addition() {
        accept(&[], &[], "3.14 + 2.71", expect!["Float"]);
    }

    #[test]
    fn accepts_string_literal_concatenation() {
        accept(&[], &[], r#""hello" + " world""#, expect!["String"]);
    }

    #[test]
    fn rejects_addition_of_int_and_float() {
        reject(
            &[],
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
    fn rejects_addition_of_string_and_int() {
        reject(
            &[],
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
    fn rejects_addition_of_boolean_and_int() {
        reject(
            &[],
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
    fn accepts_mixed_addition_and_comparison() {
        accept(
            &[],
            &[("a", "Int"), ("b", "Int"), ("c", "Int")],
            "a + b > c",
            expect!["Bool"],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ARRAYS                                                                //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn rejects_array_with_different_element_types() {
        reject(
            &[],
            &[],
            "[1, true]",
            expect![[r#"
                error: Mismatched type for array element: expected `Int` got `Bool`
                [1, true]
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_array_with_trailing_comma() {
        accept(&[], &[], "[\n\t1,\n\t2,\n\t3,\n]", expect!["Array[Int]"]);
    }

    #[test]
    fn accepts_single_element_array_with_trailing_comma() {
        accept(
            &[],
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
    fn rejects_field_access_on_nested_array() {
        reject(
            &[
                record("Profile", &[("name", "String"), ("active", "Bool")]),
                record("UserInfo", &[("profile", "Profile")]),
                record("Config", &[("users", "Array[UserInfo]")]),
            ],
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
    fn rejects_field_access_on_array() {
        reject(
            &[record("User", &[("name", "String")])],
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
    fn accepts_array_of_some_and_none() {
        accept(
            &[],
            &[],
            "[Some(1), Some(2), None]",
            expect!["Array[Option[Int]]"],
        );
    }

    #[test]
    fn accepts_array_of_arrays_with_empty_array() {
        accept(&[], &[], "[[1,2],[2,3],[]]", expect!["Array[Array[Int]]"]);
    }

    #[test]
    fn rejects_array_with_mismatched_option_types() {
        reject(
            &[],
            &[],
            r#"[Some(1), Some("1")]"#,
            expect![[r#"
                error: Mismatched type for array element: expected `Option[Int]` got `Option[String]`
                [Some(1), Some("1")]
                          ^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // RECORDS                                                               //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_simple_record_literal() {
        accept(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[],
            r#"User {name: "John", age: 30}"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn accepts_record_literal_with_variables() {
        accept(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("user_name", "String"), ("user_age", "Int")],
            "User {name: user_name, age: user_age}",
            expect!["test::User"],
        );
    }

    #[test]
    fn rejects_literal_of_undefined_record() {
        reject(
            &[],
            &[],
            r#"User {name: "John"}"#,
            expect![[r#"
                error: Record type 'User' is not defined
                User {name: "John"}
                ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_field_access() {
        reject(
            &[record("Data", &[("field", "String")])],
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
    fn accepts_equality_of_same_field_types() {
        accept(
            &[
                record("User", &[("name", "String")]),
                record("Admin", &[("name", "String")]),
            ],
            &[("user", "User"), ("admin", "Admin")],
            "user.name == admin.name",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_field_access_on_non_record() {
        reject(
            &[],
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
    fn accepts_deep_field_access() {
        accept(
            &[
                record("Connection", &[("host", "String")]),
                record("Database", &[("connection", "Connection")]),
                record("Config", &[("database", "Database")]),
                record("System", &[("config", "Config")]),
            ],
            &[("system", "System")],
            "system.config.database.connection.host",
            expect!["String"],
        );
    }

    #[test]
    fn accepts_multiple_field_accesses() {
        accept(
            &[record("Obj", &[("name", "String"), ("title", "String")])],
            &[("obj", "Obj")],
            "obj.name == obj.title",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_record_literal_with_missing_field() {
        reject(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[],
            r#"User {name: "John"}"#,
            expect![[r#"
                error: Record 'User' is missing fields: age
                User {name: "John"}
                ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_field_access() {
        accept(
            &[record("User", &[("name", "String")])],
            &[("user", "User")],
            "user.name",
            expect!["String"],
        );
    }

    #[test]
    fn accepts_nested_field_access() {
        accept(
            &[
                record("Profile", &[("name", "String")]),
                record("User", &[("profile", "Profile")]),
                record("App", &[("user", "User")]),
            ],
            &[("app", "App")],
            "app.user.profile.name",
            expect!["String"],
        );
    }

    #[test]
    fn rejects_record_literal_with_unknown_field() {
        reject(
            &[record("User", &[("name", "String")])],
            &[],
            r#"User {name: "John", email: "john@example.com"}"#,
            expect![[r#"
                error: Unknown field 'email' in record 'User'
                User {name: "John", email: "john@example.com"}
                                           ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_with_type_mismatch() {
        reject(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[],
            r#"User {name: "John", age: "thirty"}"#,
            expect![[r#"
                error: Mismatched type for `age`: expected `Int` got `String`
                User {name: "John", age: "thirty"}
                                         ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_nested_record_literal() {
        accept(
            &[
                record("Address", &[("city", "String")]),
                record("User", &[("name", "String"), ("address", "Address")]),
            ],
            &[],
            r#"User {name: "John", address: Address {city: "NYC"}}"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn accepts_record_with_array_field() {
        accept(
            &[record("Data", &[("items", "Array[String]")])],
            &[("data", "Data")],
            "data.items",
            expect!["Array[String]"],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ENUMS                                                                 //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn rejects_enum_equality() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("a", "Color"), ("b", "Color")],
            "a == b",
            expect![[r#"
                error: Type test::Color is not comparable
                a == b
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_inequality() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("a", "Color"), ("b", "Color")],
            "a != b",
            expect![[r#"
                error: Type test::Color is not comparable
                a != b
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_equality_between_different_enum_types() {
        reject(
            &[
                enum_unit("Color", &["Red", "Green", "Blue"]),
                enum_unit("Size", &["Small", "Medium", "Large"]),
            ],
            &[("color", "Color"), ("size", "Size")],
            "color == size",
            expect![[r#"
                error: Type test::Color is not comparable
                color == size
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_inequality_between_different_enum_types() {
        reject(
            &[
                enum_unit("Color", &["Red", "Green", "Blue"]),
                enum_unit("Size", &["Small", "Medium", "Large"]),
            ],
            &[("color", "Color"), ("size", "Size")],
            "color != size",
            expect![[r#"
                error: Type test::Color is not comparable
                color != size
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_equality_between_enum_and_string() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("color", "Color"), ("name", "String")],
            "color == name",
            expect![[r#"
                error: Type test::Color is not comparable
                color == name
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_equality_between_enum_and_int() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("color", "Color"), ("count", "Int")],
            "color == count",
            expect![[r#"
                error: Type test::Color is not comparable
                color == count
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_equality_between_enum_and_bool() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("color", "Color"), ("flag", "Bool")],
            "color == flag",
            expect![[r#"
                error: Type test::Color is not comparable
                color == flag
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_less_than_comparison_on_enums() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn rejects_greater_than_comparison_on_enums() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn rejects_enum_in_record_field_equality() {
        reject(
            &[
                enum_unit("Status", &["Active", "Inactive", "Pending"]),
                record("User", &[("name", "String"), ("status", "Status")]),
            ],
            &[("user", "User"), ("status", "Status")],
            "user.status == status",
            expect![[r#"
                error: Type test::Status is not comparable
                user.status == status
                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_equality_with_field_access() {
        reject(
            &[
                enum_unit("Status", &["Active", "Inactive"]),
                record("User", &[("status", "Status")]),
                record("Admin", &[("status", "Status")]),
            ],
            &[("user", "User"), ("admin", "Admin")],
            "user.status == admin.status",
            expect![[r#"
                error: Type test::Status is not comparable
                user.status == admin.status
                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal() {
        accept(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[],
            "Color::Red",
            expect!["test::Color"],
        );
    }

    #[test]
    fn rejects_undefined_enum_variant() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn rejects_undefined_enum() {
        reject(
            &[],
            &[],
            "Missing::Red",
            expect![[r#"
                error: Enum type 'Missing' is not defined
                Missing::Red
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_with_fields() {
        accept(
            &[enum_(
                "Outcome",
                &[
                    ("Success", &[("value", "Int")]),
                    ("Failure", &[("message", "String")]),
                ],
            )],
            &[],
            "Outcome::Success {value: 42}",
            expect!["test::Outcome"],
        );
    }

    #[test]
    fn accepts_enum_variant_with_multiple_fields() {
        accept(
            &[enum_(
                "Point",
                &[("XY", &[("x", "Int"), ("y", "Int")]), ("Origin", &[])],
            )],
            &[],
            "Point::XY {x: 10, y: 20}",
            expect!["test::Point"],
        );
    }

    #[test]
    fn rejects_enum_variant_missing_field() {
        reject(
            &[enum_(
                "Outcome",
                &[
                    ("Success", &[("value", "Int")]),
                    ("Failure", &[("message", "String")]),
                ],
            )],
            &[],
            "Outcome::Success {}",
            expect![[r#"
                error: Enum variant 'Outcome::Success' is missing fields: value
                Outcome::Success {}
                ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_missing_two_fields() {
        reject(
            &[enum_("Point", &[("XY", &[("x", "Int"), ("y", "Int")])])],
            &[],
            "Point::XY {}",
            expect![[r#"
                error: Enum variant 'Point::XY' is missing fields: x, y
                Point::XY {}
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_unknown_field() {
        reject(
            &[enum_(
                "Outcome",
                &[
                    ("Success", &[("value", "Int")]),
                    ("Failure", &[("message", "String")]),
                ],
            )],
            &[],
            "Outcome::Success {wrong: 42}",
            expect![[r#"
                error: Unknown field 'wrong' in enum variant 'Outcome::Success'
                Outcome::Success {wrong: 42}
                                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_field_type_mismatch() {
        reject(
            &[enum_(
                "Outcome",
                &[
                    ("Success", &[("value", "Int")]),
                    ("Failure", &[("message", "String")]),
                ],
            )],
            &[],
            r#"Outcome::Success {value: "hello"}"#,
            expect![[r#"
                error: Mismatched type for `value`: expected `Int` got `String`
                Outcome::Success {value: "hello"}
                                         ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_fields_on_unit_variant() {
        reject(
            &[enum_(
                "Maybe",
                &[("Just", &[("value", "Int")]), ("Nothing", &[])],
            )],
            &[],
            "Maybe::Nothing {value: 42}",
            expect![[r#"
                error: Unknown field 'value' in enum variant 'Maybe::Nothing'
                Maybe::Nothing {value: 42}
                                ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_in_equality() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("color", "Color")],
            "Color::Red == color",
            expect![[r#"
                error: Type test::Color is not comparable
                Color::Red == color
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_variant_in_record_field_assignment() {
        accept(
            &[
                enum_unit("Status", &["Active", "Inactive"]),
                record("User", &[("name", "String"), ("status", "Status")]),
            ],
            &[],
            r#"User {name: "Alice", status: Status::Active}"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn rejects_two_enum_variants_in_equality() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[],
            "Color::Red == Color::Green",
            expect![[r#"
                error: Type test::Color is not comparable
                Color::Red == Color::Green
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_equality_between_different_enums_with_same_variants() {
        reject(
            &[
                enum_unit("Color", &["Red", "Green", "Blue"]),
                enum_unit("Shade", &["Red", "Green", "Blue"]),
            ],
            &[],
            "Color::Red == Shade::Red",
            expect![[r#"
                error: Type test::Color is not comparable
                Color::Red == Shade::Red
                ^^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // OPTION TYPE                                                           //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_some_with_integer_literal() {
        accept(&[], &[], "Some(42)", expect!["Option[Int]"]);
    }

    #[test]
    fn accepts_some_with_string_literal() {
        accept(&[], &[], r#"Some("hello")"#, expect!["Option[String]"]);
    }

    #[test]
    fn accepts_some_with_boolean_literal() {
        accept(&[], &[], "Some(true)", expect!["Option[Bool]"]);
    }

    #[test]
    fn accepts_some_with_float_literal() {
        accept(&[], &[], "Some(3.14)", expect!["Option[Float]"]);
    }

    #[test]
    fn accepts_some_with_variable() {
        accept(
            &[],
            &[("name", "String")],
            "Some(name)",
            expect!["Option[String]"],
        );
    }

    #[test]
    fn accepts_some_with_field_access() {
        accept(
            &[record("User", &[("name", "String")])],
            &[("user", "User")],
            "Some(user.name)",
            expect!["Option[String]"],
        );
    }

    #[test]
    fn accepts_nested_some() {
        accept(&[], &[], "Some(Some(42))", expect!["Option[Option[Int]]"]);
    }

    #[test]
    fn accepts_some_with_array() {
        accept(&[], &[], "Some([1, 2, 3])", expect!["Option[Array[Int]]"]);
    }

    #[test]
    fn accepts_some_with_record_literal() {
        accept(
            &[record("Point", &[("x", "Int"), ("y", "Int")])],
            &[],
            "Some(Point{x: 1, y: 2})",
            expect!["Option[test::Point]"],
        );
    }

    #[test]
    fn accepts_some_with_enum_literal() {
        accept(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[],
            "Some(Color::Red)",
            expect!["Option[test::Color]"],
        );
    }

    #[test]
    fn accepts_option_variable() {
        accept(
            &[],
            &[("maybe_count", "Option[Int]")],
            "maybe_count",
            expect!["Option[Int]"],
        );
    }

    #[test]
    fn accepts_option_in_record_field() {
        accept(
            &[record(
                "User",
                &[("name", "String"), ("age", "Option[Int]")],
            )],
            &[("user", "User")],
            "user.age",
            expect!["Option[Int]"],
        );
    }

    #[test]
    fn accepts_none_in_record_field_assignment() {
        accept(
            &[record(
                "User",
                &[("name", "String"), ("nickname", "Option[String]")],
            )],
            &[],
            r#"User {name: "Alice", nickname: None}"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn accepts_some_in_record_field_assignment() {
        accept(
            &[record(
                "User",
                &[("name", "String"), ("nickname", "Option[String]")],
            )],
            &[],
            r#"User {name: "Alice", nickname: Some("Ali")}"#,
            expect!["test::User"],
        );
    }

    #[test]
    fn accepts_nested_option_in_record_field() {
        accept(
            &[record("Config", &[("value", "Option[Option[Int]]")])],
            &[],
            "Config {value: Some(Some(42))}",
            expect!["test::Config"],
        );
    }

    #[test]
    fn accepts_none_for_nested_option_field() {
        accept(
            &[record("Config", &[("value", "Option[Option[Int]]")])],
            &[],
            "Config {value: None}",
            expect!["test::Config"],
        );
    }

    #[test]
    fn accepts_array_of_options() {
        accept(
            &[],
            &[("items", "Array[Option[Int]]")],
            "items",
            expect!["Array[Option[Int]]"],
        );
    }

    #[test]
    fn accepts_option_of_array() {
        accept(
            &[],
            &[("maybe_items", "Option[Array[Int]]")],
            "maybe_items",
            expect!["Option[Array[Int]]"],
        );
    }

    #[test]
    fn rejects_none_without_context() {
        reject(
            &[],
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
    fn rejects_type_mismatch_in_option_field() {
        reject(
            &[record(
                "User",
                &[("name", "String"), ("age", "Option[Int]")],
            )],
            &[],
            r#"User {name: "Alice", age: Some("thirty")}"#,
            expect![[r#"
                error: Mismatched type for `age`: expected `Option[Int]` got `Option[String]`
                User {name: "Alice", age: Some("thirty")}
                                          ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_non_option_for_option_field() {
        reject(
            &[record(
                "User",
                &[("name", "String"), ("age", "Option[Int]")],
            )],
            &[],
            r#"User {name: "Alice", age: 30}"#,
            expect![[r#"
                error: Mismatched type for `age`: expected `Option[Int]` got `Int`
                User {name: "Alice", age: 30}
                                          ^^
            "#]],
        );
    }

    #[test]
    fn accepts_option_equality() {
        accept(&[], &[], "Some(1) == Some(2)", expect!["Bool"]);
    }

    #[test]
    fn rejects_option_of_array_equality() {
        reject(
            &[],
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
    fn accepts_some_equals_none() {
        accept(&[], &[], "Some(1) == None", expect!["Bool"]);
    }

    #[test]
    fn accepts_none_equals_some() {
        accept(&[], &[], "None == Some(1)", expect!["Bool"]);
    }

    #[test]
    fn accepts_nested_some_equals_some_none() {
        accept(&[], &[], "Some(Some(1)) == Some(None)", expect!["Bool"]);
    }

    #[test]
    fn accepts_nested_some_equals_none() {
        accept(&[], &[], "Some(Some(1)) == None", expect!["Bool"]);
    }

    #[test]
    fn rejects_nested_some_with_different_inner_types() {
        reject(
            &[],
            &[],
            r#"Some(Some(1)) == Some(Some("2"))"#,
            expect![[r#"
                error: Cannot compare Option[Option[Int]] to Option[Option[String]]
                Some(Some(1)) == Some(Some("2"))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - EMPTY                                              //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn rejects_match_with_no_arms() {
        reject(
            &[],
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                }
            "#},
            expect![[r#"
                error: Match expression must have at least one arm
                match flag {
                      ^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - ENUM                                               //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_match_expression_with_all_variants() {
        accept(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn accepts_match_expression_returning_int() {
        accept(
            &[enum_unit("Size", &["Small", "Medium", "Large"])],
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
    fn accepts_match_expression_returning_bool() {
        accept(
            &[enum_unit("Status", &["Active", "Inactive"])],
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
    fn rejects_match_on_non_enum() {
        reject(
            &[],
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
    fn rejects_match_with_mismatched_arm_types() {
        reject(
            &[enum_unit("Color", &["Red", "Green"])],
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Green => 42,
                }
            "#},
            expect![[r#"
                error: Mismatched type: expected `String` got `Int`
                    Color::Green => 42,
                                    ^^
            "#]],
        );
    }

    #[test]
    fn rejects_match_with_wrong_enum_in_pattern() {
        reject(
            &[
                enum_unit("Color", &["Red", "Green"]),
                enum_unit("Size", &["Small", "Large"]),
            ],
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
    fn rejects_match_with_undefined_variant_in_pattern() {
        reject(
            &[enum_unit("Color", &["Red", "Green"])],
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
    fn accepts_match_with_field_access_subject() {
        accept(
            &[
                enum_unit("Status", &["Active", "Inactive"]),
                record("User", &[("status", "Status")]),
            ],
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
    fn accepts_match_with_complex_arm_bodies() {
        accept(
            &[
                enum_unit("Status", &["Active", "Inactive"]),
                record("User", &[("name", "String"), ("status", "Status")]),
            ],
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
    fn accepts_match_with_single_variant_enum() {
        accept(
            &[enum_unit("Unit", &["Value"])],
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
    fn accepts_match_with_wildcard_pattern() {
        accept(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn rejects_match_with_only_wildcard() {
        reject(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    _ => "any color",
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match color {
                      ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_wildcard_at_end() {
        accept(
            &[enum_unit(
                "Status",
                &["Active", "Inactive", "Pending", "Archived"],
            )],
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
    fn accepts_boolean_match_with_both_values() {
        accept(
            &[],
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
    fn accepts_boolean_match_with_wildcard() {
        accept(
            &[],
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
    fn rejects_boolean_match_with_only_wildcard() {
        reject(
            &[],
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    _ => "always",
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match flag {
                      ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_pattern_in_boolean_match() {
        reject(
            &[enum_unit("Color", &["Red"])],
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    Color::Red => "red",
                    false => "no",
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Bool` got `Color::Red`
                    Color::Red => "red",
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_boolean_pattern_in_enum_match() {
        reject(
            &[enum_unit("Color", &["Red", "Green"])],
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    true => 0,
                    Color::Green => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `test::Color` got `true`
                    true => 0,
                    ^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - OPTION                                             //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_option_match_with_some_and_none() {
        accept(
            &[],
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
    fn accepts_option_match_returning_int() {
        accept(
            &[],
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
    fn accepts_option_match_with_wildcard() {
        accept(
            &[],
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
    fn rejects_option_match_with_only_wildcard() {
        reject(
            &[],
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    _ => "always this",
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match opt {
                      ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_option_match_with_specific_some_and_wildcard() {
        accept(
            &[],
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
    fn accepts_exhaustive_nested_option_match() {
        accept(
            &[],
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
    fn accepts_exhaustive_nested_option_match_with_literal_subject() {
        accept(
            &[],
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
    fn rejects_option_match_with_mismatched_arm_types() {
        reject(
            &[],
            &[("opt", "Option[Int]")],
            indoc! {"
                match opt {
                    Some(_) => 42,
                    None    => true,
                }
            "},
            expect![[r#"
                error: Mismatched type: expected `Int` got `Bool`
                    None    => true,
                               ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_pattern_in_option_match() {
        reject(
            &[enum_unit("Color", &["Red", "Green"])],
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    Color::Red => 0,
                    None       => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Option[Int]` got `Color::Red`
                    Color::Red => 0,
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_boolean_pattern_in_option_match() {
        reject(
            &[],
            &[("opt", "Option[Int]")],
            indoc! {r#"
                match opt {
                    true => 0,
                    None => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Option[Int]` got `true`
                    true => 0,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_option_pattern_in_enum_match() {
        reject(
            &[enum_unit("Color", &["Red", "Green"])],
            &[("color", "Color")],
            indoc! {r#"
                match color {
                    Some(_)      => 0,
                    Color::Green => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `test::Color` got `Some(_)`
                    Some(_)      => 0,
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_option_pattern_in_bool_match() {
        reject(
            &[],
            &[("flag", "Bool")],
            indoc! {r#"
                match flag {
                    Some(_) => 0,
                    false   => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Bool` got `Some(_)`
                    Some(_) => 0,
                    ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_nested_option_pattern_when_inner_type_is_bool() {
        reject(
            &[],
            &[("opt", "Option[Bool]")],
            indoc! {r#"
                match opt {
                    Some(None) => 0,
                    None       => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Bool` got `None`
                    Some(None) => 0,
                         ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_option_match_with_nested_enum() {
        accept(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn rejects_deeply_nested_option_pattern_when_inner_type_is_bool() {
        reject(
            &[],
            &[("opt", "Option[Bool]")],
            indoc! {r#"
                match opt {
                    Some(Some(_)) => 0,
                    None          => 1,
                }
            "#},
            expect![[r#"
                error: Mismatched pattern type: expected `Bool` got `Some(_)`
                    Some(Some(_)) => 0,
                         ^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION - BINDING PATTERN                                    //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_binding_pattern_in_bool_match() {
        accept(
            &[],
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
    fn accepts_binding_pattern_in_enum_match() {
        accept(
            &[enum_unit("Color", &["Red", "Green", "Blue"])],
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
    fn accepts_binding_pattern_in_option_match() {
        accept(
            &[],
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
    fn accepts_binding_pattern_inside_some() {
        accept(
            &[],
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
    fn accepts_binding_pattern_in_arithmetic() {
        accept(
            &[],
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
    fn accepts_binding_pattern_returning_option() {
        accept(
            &[],
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
    fn accepts_binding_pattern_as_catchall() {
        accept(
            &[],
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
    fn rejects_unused_binding() {
        reject(
            &[],
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
    fn rejects_unused_binding_inside_some() {
        reject(
            &[],
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
    fn accepts_binding_inside_nested_some() {
        accept(
            &[],
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
    fn rejects_unused_binding_inside_nested_some() {
        reject(
            &[],
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
    fn accepts_record_match_with_all_fields() {
        accept(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: n, age: _} => n,
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn rejects_record_match_with_wildcard_fields() {
        reject(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: _, age: _} => "matched",
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match user {
                      ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_match_with_nested_wildcard_fields() {
        reject(
            &[
                record("Role", &[("title", "String"), ("salary", "Int")]),
                record("User", &[("role", "Role"), ("created_at", "Int")]),
            ],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{role: Role{title: _, salary: _}, created_at: _} => "matched",
                }
            "#},
            expect![[r#"
                error: Useless match expression: does not branch or bind any variables
                match user {
                      ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_match_with_nested_wildcard_fields_followed_by_wildcard() {
        reject(
            &[
                record("Role", &[("title", "String"), ("salary", "Int")]),
                record("User", &[("role", "Role"), ("created_at", "Int")]),
            ],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{role: Role{title: _, salary: _}, created_at: _} => 0,
                    _ => 1,
                }
            "#},
            expect![[r#"
                error: Unreachable match arm for pattern '_'
                    _ => 1,
                    ^
            "#]],
        );
    }

    #[test]
    fn accepts_record_match_with_nested_enum_pattern() {
        accept(
            &[
                enum_unit("Status", &["Active", "Inactive"]),
                record("User", &[("name", "String"), ("status", "Status")]),
            ],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: n, status: Status::Active}   => n,
                    User{name: _, status: Status::Inactive} => "inactive",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn rejects_record_match_with_missing_fields() {
        reject(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: n} => n,
                }
            "#},
            expect![[r#"
                error: Record 'User' is missing fields: age
                    User{name: n} => n,
                    ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_match_with_unknown_field() {
        reject(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: n, email: e} => n,
                }
            "#},
            expect![[r#"
                error: Unknown field 'email' in record 'User'
                    User{name: n, email: e} => n,
                                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_match_with_wrong_record_type() {
        reject(
            &[
                record("User", &[("name", "String")]),
                record("Admin", &[("name", "String")]),
            ],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    Admin{name: n} => n,
                }
            "#},
            expect![[r#"
                error: Match pattern record 'Admin' does not match subject record 'User'
                    Admin{name: n} => n,
                    ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unused_binding_in_record_pattern() {
        reject(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: n, age: a} => "hello",
                }
            "#},
            expect![[r#"
                error: Unused binding 'a' in match arm
                    User{name: n, age: a} => "hello",
                                       ^
            "#]],
        );
    }

    #[test]
    fn accepts_record_match_with_nested_option_pattern() {
        accept(
            &[record(
                "User",
                &[("name", "String"), ("email", "Option[String]")],
            )],
            &[("user", "User")],
            indoc! {r#"
                match user {
                    User{name: _, email: Some(e)} => e,
                    User{name: n, email: None}    => n,
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn accepts_option_match_with_nested_record_pattern() {
        accept(
            &[record("User", &[("name", "String"), ("age", "Int")])],
            &[("maybe_user", "Option[User]")],
            indoc! {r#"
                match maybe_user {
                    Some(User{name: n, age: _}) => n,
                    None                        => "Default User",
                }
            "#},
            expect!["String"],
        );
    }

    #[test]
    fn accepts_option_of_record_match_with_some_and_none() {
        accept(
            &[record("User", &[("name", "String"), ("age", "Int")])],
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
    fn accepts_join_macro_with_string_args() {
        accept(
            &[],
            &[("first", "String"), ("last", "String")],
            "join!(first, last)",
            expect!["String"],
        );
    }

    #[test]
    fn accepts_join_macro_with_no_args() {
        accept(&[], &[], "join!()", expect!["String"]);
    }

    #[test]
    fn accepts_join_macro_with_string_literals() {
        accept(&[], &[], r#"join!("hello", "world")"#, expect!["String"]);
    }

    #[test]
    fn rejects_join_macro_with_non_string_arg() {
        reject(
            &[],
            &[("count", "Int")],
            "join!(count)",
            expect![[r#"
                error: Mismatched type for 'join': expected `String` got `Int`
                join!(count)
                      ^^^^^
            "#]],
        );
    }

    // Asset macro tests

    #[test]
    fn accepts_asset_macro_with_string_literal() {
        accept(&[], &[], r#"asset!("/logo.svg")"#, expect!["String"]);
    }

    #[test]
    fn accepts_asset_macro_with_nested_path() {
        accept(&[], &[], r#"asset!("/icons/star.svg")"#, expect!["String"]);
    }

    #[test]
    fn rejects_asset_macro_with_no_args() {
        reject(
            &[],
            &[],
            "asset!()",
            expect![[r#"
                error: asset! takes exactly one argument, got 0
                asset!()
                ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_asset_macro_with_two_args() {
        reject(
            &[],
            &[],
            r#"asset!("/a", "/b")"#,
            expect![[r#"
                error: asset! takes exactly one argument, got 2
                asset!("/a", "/b")
                ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_asset_macro_with_variable_arg() {
        reject(
            &[],
            &[("path", "String")],
            "asset!(path)",
            expect![[r#"
                error: asset! argument must be a string literal
                asset!(path)
                       ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_asset_macro_with_concat_arg() {
        reject(
            &[],
            &[],
            r#"asset!("/a" + "/b")"#,
            expect![[r#"
                error: asset! argument must be a string literal
                asset!("/a" + "/b")
                       ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_asset_macro_with_relative_path() {
        reject(
            &[],
            &[],
            r#"asset!("logo.svg")"#,
            expect![[r#"
                error: asset! path must start with '/'
                asset!("logo.svg")
                       ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_asset_macro_with_empty_string() {
        reject(
            &[],
            &[],
            r#"asset!("")"#,
            expect![[r#"
                error: asset! path must start with '/'
                asset!("")
                       ^^
            "#]],
        );
    }

    // Method call tests

    #[test]
    fn accepts_len_on_array() {
        accept(
            &[],
            &[("items", "Array[String]")],
            "items.len()",
            expect!["Int"],
        );
    }

    #[test]
    fn accepts_len_on_int_array() {
        accept(
            &[],
            &[("numbers", "Array[Int]")],
            "numbers.len()",
            expect!["Int"],
        );
    }

    #[test]
    fn accepts_len_in_comparison() {
        accept(
            &[],
            &[("items", "Array[String]")],
            "items.len() == 0",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_len_on_non_array() {
        reject(
            &[],
            &[("name", "String")],
            "name.len()",
            expect![[r#"
                error: Method 'len' is not available on type String
                name.len()
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_method_on_array() {
        reject(
            &[],
            &[("items", "Array[String]")],
            "items.foo()",
            expect![[r#"
                error: Method 'foo' is not available on type Array[String]
                items.foo()
                ^^^^^^^^^^^
            "#]],
        );
    }

    // Int to_string tests

    #[test]
    fn accepts_to_string_on_int() {
        accept(
            &[],
            &[("count", "Int")],
            "count.to_string()",
            expect!["String"],
        );
    }

    #[test]
    fn accepts_to_string_in_concat() {
        accept(
            &[],
            &[("n", "Int")],
            r#""Value: " + n.to_string()"#,
            expect!["String"],
        );
    }

    #[test]
    fn rejects_to_string_on_string() {
        reject(
            &[],
            &[("name", "String")],
            "name.to_string()",
            expect![[r#"
                error: Method 'to_string' is not available on type String
                name.to_string()
                ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_to_string_on_array() {
        reject(
            &[],
            &[("items", "Array[Int]")],
            "items.to_string()",
            expect![[r#"
                error: Method 'to_string' is not available on type Array[Int]
                items.to_string()
                ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Float to_int tests

    #[test]
    fn accepts_to_int_on_float() {
        accept(&[], &[("price", "Float")], "price.to_int()", expect!["Int"]);
    }

    #[test]
    fn accepts_to_int_in_comparison() {
        accept(
            &[],
            &[("price", "Float")],
            "price.to_int() == 5",
            expect!["Bool"],
        );
    }

    #[test]
    fn rejects_to_int_on_int() {
        reject(
            &[],
            &[("count", "Int")],
            "count.to_int()",
            expect![[r#"
                error: Method 'to_int' is not available on type Int
                count.to_int()
                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_to_int_on_string() {
        reject(
            &[],
            &[("name", "String")],
            "name.to_int()",
            expect![[r#"
                error: Method 'to_int' is not available on type String
                name.to_int()
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    // Int to_float tests

    #[test]
    fn accepts_to_float_on_int() {
        accept(
            &[],
            &[("count", "Int")],
            "count.to_float()",
            expect!["Float"],
        );
    }

    #[test]
    fn accepts_to_float_in_arithmetic() {
        accept(
            &[],
            &[("count", "Int"), ("rate", "Float")],
            "count.to_float() + rate",
            expect!["Float"],
        );
    }

    #[test]
    fn rejects_to_float_on_float() {
        reject(
            &[],
            &[("price", "Float")],
            "price.to_float()",
            expect![[r#"
                error: Method 'to_float' is not available on type Float
                price.to_float()
                ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_to_float_on_string() {
        reject(
            &[],
            &[("name", "String")],
            "name.to_float()",
            expect![[r#"
                error: Method 'to_float' is not available on type String
                name.to_float()
                ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    // Method calls on literals

    #[test]
    fn accepts_to_string_on_int_literal() {
        accept(&[], &[], "42.to_string()", expect!["String"]);
    }

    #[test]
    fn accepts_to_float_on_int_literal() {
        accept(&[], &[], "42.to_float()", expect!["Float"]);
    }

    #[test]
    fn accepts_to_int_on_float_literal() {
        accept(&[], &[], "3.14.to_int()", expect!["Int"]);
    }

    #[test]
    fn accepts_len_on_array_literal() {
        accept(&[], &[], "[1, 2, 3].len()", expect!["Int"]);
    }

    #[test]
    fn accepts_method_call_on_parenthesized_arithmetic() {
        accept(&[], &[], "(1 + 2).to_string()", expect!["String"]);
    }
}
