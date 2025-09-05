use super::parser::{BinaryOp, DopExpr, UnaryOp};
use crate::hop::environment::Environment;
use anyhow::Result;

pub fn evaluate_expr(
    expr: &DopExpr,
    env: &mut Environment<serde_json::Value>,
) -> Result<serde_json::Value> {
    match expr {
        DopExpr::Variable { name, .. } => {
            if let Some(val) = env.lookup(name) {
                Ok(val.clone())
            } else {
                Err(anyhow::anyhow!("Undefined variable: {}", name))
            }
        }
        DopExpr::StringLiteral { value, .. } => Ok(serde_json::Value::String(value.clone())),
        DopExpr::BooleanLiteral { value, .. } => Ok(serde_json::Value::Bool(*value)),
        DopExpr::NumberLiteral { value, .. } => Ok(serde_json::Value::Number(value.clone())),
        DopExpr::ArrayLiteral { elements, .. } => {
            let mut array_values = Vec::new();
            for element in elements {
                array_values.push(evaluate_expr(element, env)?);
            }
            Ok(serde_json::Value::Array(array_values))
        }
        DopExpr::ObjectLiteral { properties, .. } => {
            let mut object_map = serde_json::Map::new();
            for (key, value_expr) in properties {
                let value = evaluate_expr(value_expr, env)?;
                object_map.insert(key.clone(), value);
            }
            Ok(serde_json::Value::Object(object_map))
        }
        DopExpr::PropertyAccess {
            object: base_expr,
            property,
            ..
        } => {
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
        DopExpr::BinaryOp {
            left,
            operator: BinaryOp::Equal,
            right,
            ..
        } => {
            let left_value = evaluate_expr(left, env)?;
            let right_value = evaluate_expr(right, env)?;

            Ok(serde_json::Value::Bool(left_value == right_value))
        }
        DopExpr::UnaryOp {
            operator: UnaryOp::Not,
            operand: expr,
            ..
        } => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::range::Position;
    use crate::dop::{DopTokenizer, parse_expr};
    use crate::hop::environment::Environment;
    use expect_test::{Expect, expect};
    use serde_json::json;

    fn check(env_json: serde_json::Value, expr_str: &str, expected: Expect) {
        // Create environment from JSON
        let mut env = Environment::new();
        if let serde_json::Value::Object(map) = env_json {
            for (key, value) in map {
                env.push(key, value).expect("Environment name collision");
            }
        }

        // Parse the expression
        let mut tokenizer = DopTokenizer::new(expr_str, Position::default()).peekable();

        let expr = parse_expr(&mut tokenizer).expect("Failed to parse expression");

        // Evaluate the expression
        let actual = match evaluate_expr(&expr, &mut env) {
            Ok(result) => format!("{}", result),
            Err(e) => format!("error: {}", e),
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn test_variable_lookup() {
        check(json!({"name": "alice"}), "name", expect![["\"alice\""]]);
    }

    #[test]
    fn test_string_literals() {
        check(json!({}), "'hello world'", expect![["\"hello world\""]]);
    }

    #[test]
    fn test_boolean_literals() {
        check(json!({}), "true", expect!["true"]);
        check(json!({}), "false", expect!["false"]);
    }

    #[test]
    fn test_property_access() {
        check(
            json!({
                "user": {
                    "name": "alice",
                    "age": 30,
                }
            }),
            "user.name",
            expect![["\"alice\""]],
        );
        check(
            json!({
                "user": {
                    "name": "alice",
                    "age": 30,
                }
            }),
            "user.age",
            expect![["30"]],
        );
    }

    #[test]
    fn test_deep_property_access() {
        check(
            json!({
                "app": {
                    "user": {
                        "profile": {
                            "settings": {
                                "theme": "dark",
                            }
                        }
                    }
                }
            }),
            "app.user.profile.settings.theme",
            expect![["\"dark\""]],
        );
    }

    #[test]
    fn test_equality_comparison() {
        check(
            json!({
                "user": {
                    "name": "alice",
                },
                "admin": {
                    "name": "alice",
                }
            }),
            "user.name == admin.name",
            expect![["true"]],
        );
        check(
            json!({
                "user": {
                    "name": "alice"
                },
                "admin": {
                    "name": "bob",
                }
            }),
            "user.name == admin.name",
            expect![["false"]],
        );
    }

    #[test]
    fn test_equality_with_literals() {
        check(
            json!({
                "status": "active",
            }),
            "status == 'active'",
            expect![["true"]],
        );
        check(
            json!({
                "count": 5
            }),
            "count == 5",
            expect![["true"]],
        );
    }

    #[test]
    fn test_complex_equality() {
        check(
            json!({
                "a": 1,
                "b": 2,
                "c": 1
            }),
            "a == b == false",
            expect![["true"]],
        );
    }

    #[test]
    fn test_negation_operator() {
        check(json!({"enabled": true}), "!enabled", expect![["false"]]);
        check(json!({"enabled": false}), "!enabled", expect![["true"]]);
        check(json!({}), "!true", expect![["false"]]);
        check(json!({}), "!false", expect![["true"]]);
    }

    #[test]
    fn test_array_literals() {
        check(json!({}), "[]", expect![["[]"]]);
        check(json!({}), "[1, 2, 3]", expect![["[1,2,3]"]]);
        check(
            json!({}),
            "[42, 'hello', true]",
            expect![["[42,\"hello\",true]"]],
        );
        check(
            json!({
                "x": 10,
                "y": 20,
            }),
            "[x, y]",
            expect![["[10,20]"]],
        );
        check(json!({}), "[[1, 2], [3, 4]]", expect![["[[1,2],[3,4]]"]]);
        check(
            json!({
                "user": {
                    "name": "alice",
                    "age": 30,
                }
            }),
            "[user.name, user.age]",
            expect![["[\"alice\",30]"]],
        );
    }

    #[test]
    fn test_object_literals() {
        check(json!({}), "{}", expect![["{}"]]);
        check(
            json!({}),
            "{name: 'John'}",
            expect![["{\"name\":\"John\"}"]],
        );
        check(
            json!({}),
            "{a: 'foo', b: 1, c: true}",
            expect![["{\"a\":\"foo\",\"b\":1,\"c\":true}"]],
        );
        check(
            json!({
                "user": {
                    "name": "alice",
                    "disabled": false,
                }
            }),
            "{user: user.name, active: !user.disabled}",
            expect![["{\"active\":true,\"user\":\"alice\"}"]],
        );
        check(
            json!({}),
            "{nested: {inner: 'value'}}",
            expect![["{\"nested\":{\"inner\":\"value\"}}"]],
        );
    }

    #[test]
    fn test_trailing_commas() {
        check(json!({}), "[\n\t1,\n\t2,\n\t3,\n]", expect![["[1,2,3]"]]);
        check(json!({}), "['hello',]", expect![["[\"hello\"]"]]);
        check(
            json!({}),
            "{\n\ta: 'foo',\n\tb: 1,\n}",
            expect![["{\"a\":\"foo\",\"b\":1}"]],
        );
        check(
            json!({}),
            "{\n\tname: 'John',\n}",
            expect![["{\"name\":\"John\"}"]],
        );
    }

    #[test]
    fn test_errors() {
        check(
            json!({}),
            "undefined_var",
            expect![["error: Undefined variable: undefined_var"]],
        );
        check(
            json!({"user": null}),
            "user.name",
            expect![["error: Cannot access property of null value"]],
        );
        check(
            json!({"count": 42}),
            "count.value",
            expect![["error: Cannot access property of non-object"]],
        );
        check(
            json!({"user": {"name": "alice"}}),
            "user.age",
            expect![["error: Property 'age' not found"]],
        );
        check(
            json!({"name": "alice"}),
            "!name",
            expect![["error: Negation operator can only be applied to boolean values"]],
        );
    }
}
