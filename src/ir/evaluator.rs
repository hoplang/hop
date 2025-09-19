use crate::common::escape_html;
use crate::hop::environment::Environment;
use crate::ir::IrExpr;
use anyhow::{Result, anyhow};
use serde_json::Value;
use std::collections::HashMap;

use super::ast::{BinaryOp, IrEntrypoint, IrStatement, UnaryOp};

/// Evaluate an IR entrypoint with the given arguments
pub fn evaluate_entrypoint(
    entrypoint: &IrEntrypoint,
    args: HashMap<String, Value>,
    _hop_mode: &str,
) -> Result<String> {
    let mut env = Environment::new();

    for (param_name, _param_type) in &entrypoint.parameters {
        if let Some(value) = args.get(param_name.as_str()) {
            let _ = env.push(param_name.to_string(), value.clone());
        }
    }

    // Execute body
    let mut output = String::new();
    eval_statements(&entrypoint.body, &mut env, &mut output)?;

    Ok(output)
}

/// Evaluate a slice of IR statements
fn eval_statements(
    statements: &[IrStatement],
    env: &mut Environment<Value>,
    output: &mut String,
) -> Result<()> {
    for statement in statements {
        eval_statement(statement, env, output)?;
    }
    Ok(())
}

/// Evaluate a single IR node
fn eval_statement(
    node: &IrStatement,
    env: &mut Environment<Value>,
    output: &mut String,
) -> Result<()> {
    match node {
        IrStatement::Write { id: _, content } => {
            output.push_str(content);
            Ok(())
        }

        IrStatement::WriteExpr {
            id: _,
            expr,
            escape,
        } => {
            let value = evaluate_expr(expr, env)?;
            let s = value.as_str().unwrap_or("");
            if *escape {
                output.push_str(&escape_html(s));
            } else {
                output.push_str(s);
            }
            Ok(())
        }

        IrStatement::If {
            id: _,
            condition,
            body,
        } => {
            let cond_value = evaluate_expr(condition, env)?;
            if cond_value.as_bool().unwrap_or(false) {
                eval_statements(body, env, output)?;
            }
            Ok(())
        }

        IrStatement::For {
            id: _,
            var,
            array,
            body,
        } => {
            let array_value = evaluate_expr(array, env)?;
            let items = array_value.as_array().cloned().unwrap_or_default();

            for item in items {
                let _ = env.push(var.to_string(), item);
                eval_statements(body, env, output)?;
                let _ = env.pop();
            }
            Ok(())
        }

        IrStatement::Let {
            id: _,
            var,
            value,
            body,
        } => {
            let val = evaluate_expr(value, env)?;
            let _ = env.push(var.to_string(), val);
            eval_statements(body, env, output)?;
            let _ = env.pop();
            Ok(())
        }
    }
}

fn evaluate_expr(expr: &IrExpr, env: &mut Environment<Value>) -> Result<Value> {
    match expr {
        IrExpr::Var { value: name, .. } => env
            .lookup(name.as_str())
            .cloned()
            .ok_or_else(|| anyhow!("Undefined variable: {}", name)),
        IrExpr::PropertyAccess {
            object, property, ..
        } => {
            let obj_value = evaluate_expr(object, env)?;
            if let Some(obj) = obj_value.as_object() {
                Ok(obj.get(property).cloned().unwrap_or(Value::Null))
            } else {
                Ok(Value::Null)
            }
        }
        IrExpr::StringLiteral { value: s, .. } => Ok(Value::String(s.clone())),
        IrExpr::BooleanLiteral { value: b, .. } => Ok(Value::Bool(*b)),
        IrExpr::NumberLiteral { value: n, .. } => Ok(Value::Number(n.clone())),
        IrExpr::ArrayLiteral { elements, .. } => {
            let mut array = Vec::new();
            for elem in elements {
                array.push(evaluate_expr(elem, env)?);
            }
            Ok(Value::Array(array))
        }
        IrExpr::ObjectLiteral { properties, .. } => {
            let mut obj = serde_json::Map::new();
            for (key, value) in properties {
                obj.insert(key.clone(), evaluate_expr(value, env)?);
            }
            Ok(Value::Object(obj))
        }
        IrExpr::BinaryOp {
            left,
            operator: op,
            right,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            match op {
                BinaryOp::Eq => Ok(Value::Bool(left_val == right_val)),
            }
        }
        IrExpr::UnaryOp {
            operator: op,
            operand,
            ..
        } => {
            let val = evaluate_expr(operand, env)?;
            match op {
                UnaryOp::Not => {
                    let bool_val = val.as_bool().unwrap_or(false);
                    Ok(Value::Bool(!bool_val))
                }
            }
        }
        IrExpr::JsonEncode { value, .. } => {
            let val = evaluate_expr(value, env)?;
            let json_str = serde_json::to_string(&val)?;
            Ok(Value::String(json_str))
        }
        IrExpr::StringConcat { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match (left_val, right_val) {
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                _ => panic!("String concatenation requires two strings"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::test_utils::build_ir;
    use expect_test::{Expect, expect};
    use serde_json::json;

    fn check(entrypoint: IrEntrypoint, args: Vec<(&str, Value)>, expected: Expect) {
        let before = entrypoint.to_string();
        let args_map: HashMap<String, Value> =
            args.into_iter().map(|(k, v)| (k.to_string(), v)).collect();
        let result =
            evaluate_entrypoint(&entrypoint, args_map, "dev").expect("Evaluation should succeed");

        // Remove empty lines for cleaner test expectations
        let cleaned = result
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.trim())
            .collect::<Vec<_>>()
            .join("\n");

        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, cleaned);
        expected.assert_eq(&output);
    }

    #[test]
    fn test_simple_write() {
        check(
            build_ir("test", vec![], |t| vec![t.write("<div>Hello World</div>")]),
            vec![],
            expect![[r#"
                -- before --
                test() {
                  write("<div>Hello World</div>")
                }

                -- after --
                <div>Hello World</div>
            "#]],
        );
    }

    #[test]
    fn test_escape_html() {
        check(
            build_ir("test", vec![("content", Type::String)], |t| {
                vec![t.write_expr(t.var("content"), true)]
            }),
            vec![("content", json!("<script>alert('xss')</script>"))],
            expect![[r#"
                -- before --
                test(content: string) {
                  write_escaped(content)
                }

                -- after --
                &lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;
            "#]],
        );
    }

    #[test]
    fn test_if_true() {
        check(
            build_ir("test", vec![("show", Type::Bool)], |t| {
                vec![t.if_stmt(t.var("show"), vec![t.write("<div>Visible</div>")])]
            }),
            vec![("show", json!(true))],
            expect![[r#"
                -- before --
                test(show: boolean) {
                  if show {
                    write("<div>Visible</div>")
                  }
                }

                -- after --
                <div>Visible</div>
            "#]],
        );
    }

    #[test]
    fn test_if_false() {
        check(
            build_ir("test", vec![("show", Type::Bool)], |t| {
                vec![t.if_stmt(t.var("show"), vec![t.write("<div>Hidden</div>")])]
            }),
            vec![("show", json!(false))],
            expect![[r#"
                -- before --
                test(show: boolean) {
                  if show {
                    write("<div>Hidden</div>")
                  }
                }

                -- after --

            "#]],
        );
    }

    #[test]
    fn test_for_loop() {
        check(
            build_ir(
                "test",
                vec![(
                    "items",
                    Type::Array(Some(Box::new(Type::String))),
                )],
                |t| {
                    vec![t.for_loop("item", t.var("items"), |t| {
                        vec![
                            t.write("<li>"),
                            t.write_expr(t.var("item"), true),
                            t.write("</li>\n"),
                        ]
                    })]
                },
            ),
            vec![("items", json!(["Apple", "Banana", "Cherry"]))],
            expect![[r#"
                -- before --
                test(items: array[string]) {
                  for item in items {
                    write("<li>")
                    write_escaped(item)
                    write("</li>\n")
                  }
                }

                -- after --
                <li>Apple</li>
                <li>Banana</li>
                <li>Cherry</li>
            "#]],
        );
    }
}
