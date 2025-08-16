use super::{BinaryOp, DopExpr, DopType, UnaryOp, Unifier};
use crate::common::{Environment, Range, RangeError};
use std::collections::HashMap;

pub fn typecheck_dop_expression(
    expr: &DopExpr,
    env: &mut Environment<DopType>,
    unifier: &mut Unifier,
    annotations: &mut Vec<crate::typechecker::TypeAnnotation>,
    errors: &mut Vec<RangeError>,
    range: Range,
) -> DopType {
    match expr {
        DopExpr::Variable(name) => {
            if let Some(var_type) = env.lookup(name) {
                var_type.clone()
            } else {
                errors.push(RangeError::undefined_variable(name, range));
                unifier.new_type_var()
            }
        }
        DopExpr::BooleanLiteral(_) => DopType::Bool,
        DopExpr::StringLiteral(_) => DopType::String,
        DopExpr::NumberLiteral(_) => DopType::Number,
        DopExpr::PropertyAccess(base_expr, property) => {
            let base_type =
                typecheck_dop_expression(base_expr, env, unifier, annotations, errors, range);
            let property_type = unifier.new_type_var();
            let obj_type =
                unifier.new_object(HashMap::from([(property.clone(), property_type.clone())]));

            if let Some(err) = unifier.unify(&base_type, &obj_type) {
                errors.push(RangeError::unification_error(&err.message, range));
            }

            property_type
        }
        DopExpr::BinaryOp(left, BinaryOp::Equal, right) => {
            let left_type =
                typecheck_dop_expression(left, env, unifier, annotations, errors, range);
            let right_type =
                typecheck_dop_expression(right, env, unifier, annotations, errors, range);

            // Both operands should have the same type for equality comparison
            if let Some(err) = unifier.unify(&left_type, &right_type) {
                errors.push(RangeError::unification_error(
                    &format!("Type mismatch in equality comparison: {}", err.message),
                    range,
                ));
            }

            // The result of == is always boolean
            DopType::Bool
        }
        DopExpr::UnaryOp(UnaryOp::Not, expr) => {
            let expr_type =
                typecheck_dop_expression(expr, env, unifier, annotations, errors, range);

            // Negation only works on boolean expressions
            if let Some(err) = unifier.unify(&expr_type, &DopType::Bool) {
                errors.push(RangeError::unification_error(
                    &format!(
                        "Negation operator can only be applied to boolean values: {}",
                        err.message
                    ),
                    range,
                ));
            }

            // The result of ! is always boolean
            DopType::Bool
        }
    }
}