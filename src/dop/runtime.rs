use crate::common::Environment;
use crate::dop::{BinaryOp, DopExpr, UnaryOp};
use anyhow::Result;

pub fn evaluate_expr(
    expr: &DopExpr,
    env: &mut Environment<serde_json::Value>,
) -> Result<serde_json::Value> {
    match expr {
        DopExpr::Variable(name) => {
            if let Some(val) = env.lookup(name) {
                Ok(val.clone())
            } else {
                Err(anyhow::anyhow!("Undefined variable: {}", name))
            }
        }
        DopExpr::StringLiteral(value) => Ok(serde_json::Value::String(value.clone())),
        DopExpr::BooleanLiteral(value) => Ok(serde_json::Value::Bool(*value)),
        DopExpr::PropertyAccess(base_expr, property) => {
            let base_value = evaluate_expr(base_expr, env)?;

            if base_value.is_null() {
                return Err(anyhow::anyhow!("Cannot access property of null value"));
            }

            if !base_value.is_object() {
                return Err(anyhow::anyhow!("Cannot access property of non-object"));
            }

            base_value
                .get(property)
                .ok_or_else(|| anyhow::anyhow!("Property '{}' not found", property))
                .cloned()
        }
        DopExpr::BinaryOp(left, BinaryOp::Equal, right) => {
            let left_value = evaluate_expr(left, env)?;
            let right_value = evaluate_expr(right, env)?;

            Ok(serde_json::Value::Bool(left_value == right_value))
        }
        DopExpr::UnaryOp(UnaryOp::Not, expr) => {
            let value = evaluate_expr(expr, env)?;

            match value {
                serde_json::Value::Bool(b) => Ok(serde_json::Value::Bool(!b)),
                _ => Err(anyhow::anyhow!(
                    "Negation operator can only be applied to boolean values"
                )),
            }
        }
    }
}

