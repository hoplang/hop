use crate::common::escape_html;
use crate::hop::environment::Environment;
use crate::ir::IrExpr;
use anyhow::{Result, anyhow};
use serde_json::Value;
use std::collections::HashMap;

use super::ast::{BinaryOp, IrEntrypoint, IrExprValue, IrStatement, UnaryOp};

/// Evaluate an IrExpr expression
fn evaluate_ir_expr(expr: &IrExpr, env: &mut Environment<Value>) -> Result<Value> {
    match &expr.value {
        IrExprValue::Var(name) => env
            .lookup(name)
            .cloned()
            .ok_or_else(|| anyhow!("Undefined variable: {}", name)),
        IrExprValue::PropertyAccess { object, property } => {
            let obj_value = evaluate_ir_expr(object, env)?;
            if let Some(obj) = obj_value.as_object() {
                Ok(obj.get(property).cloned().unwrap_or(Value::Null))
            } else {
                Ok(Value::Null)
            }
        }
        IrExprValue::StringLiteral(s) => Ok(Value::String(s.clone())),
        IrExprValue::BooleanLiteral(b) => Ok(Value::Bool(*b)),
        IrExprValue::NumberLiteral(n) => Ok(Value::Number(
            serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
        )),
        IrExprValue::ArrayLiteral(elements) => {
            let mut array = Vec::new();
            for elem in elements {
                array.push(evaluate_ir_expr(elem, env)?);
            }
            Ok(Value::Array(array))
        }
        IrExprValue::ObjectLiteral(properties) => {
            let mut obj = serde_json::Map::new();
            for (key, value) in properties {
                obj.insert(key.clone(), evaluate_ir_expr(value, env)?);
            }
            Ok(Value::Object(obj))
        }
        IrExprValue::BinaryOp { left, op, right } => {
            let left_val = evaluate_ir_expr(left, env)?;
            let right_val = evaluate_ir_expr(right, env)?;
            match op {
                BinaryOp::Eq => Ok(Value::Bool(left_val == right_val)),
            }
        }
        IrExprValue::UnaryOp { op, operand } => {
            let val = evaluate_ir_expr(operand, env)?;
            match op {
                UnaryOp::Not => {
                    let bool_val = val.as_bool().unwrap_or(false);
                    Ok(Value::Bool(!bool_val))
                }
            }
        }
        IrExprValue::JsonEncode { value } => {
            let val = evaluate_ir_expr(value, env)?;
            let json_str = serde_json::to_string(&val)?;
            Ok(Value::String(json_str))
        }
    }
}

/// Evaluate an IR entrypoint with the given arguments
pub fn evaluate_entrypoint(
    entrypoint: &IrEntrypoint,
    args: HashMap<String, Value>,
    hop_mode: &str,
) -> Result<String> {
    let mut env = Environment::new();

    // Set up global variables
    let _ = env.push("HOP_MODE".to_string(), Value::String(hop_mode.to_string()));

    for (param_name, _param_type) in &entrypoint.parameters {
        if let Some(value) = args.get(param_name) {
            let _ = env.push(param_name.clone(), value.clone());
        }
    }

    // Execute body
    let mut output = String::new();
    eval_ir(&entrypoint.body, &mut env, &mut output)?;

    Ok(output)
}

/// Evaluate a slice of IR statements
fn eval_ir(
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
            let value = evaluate_ir_expr(expr, env)?;
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
            let cond_value = evaluate_ir_expr(condition, env)?;
            if cond_value.as_bool().unwrap_or(false) {
                eval_ir(body, env, output)?;
            }
            Ok(())
        }

        IrStatement::For {
            id: _,
            var,
            array,
            body,
        } => {
            let array_value = evaluate_ir_expr(array, env)?;
            let items = array_value.as_array().cloned().unwrap_or_default();

            for item in items {
                let _ = env.push(var.clone(), item);
                eval_ir(body, env, output)?;
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
            let val = evaluate_ir_expr(value, env)?;
            let _ = env.push(var.clone(), val);
            eval_ir(body, env, output)?;
            let _ = env.pop();
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::test_utils::IrTestBuilder;
    use expect_test::{Expect, expect};
    use serde_json::json;

    fn check_eval(entrypoint: IrEntrypoint, args: HashMap<String, Value>, expected: Expect) {
        let result =
            evaluate_entrypoint(&entrypoint, args, "dev").expect("Evaluation should succeed");

        // Remove empty lines for cleaner test expectations
        let cleaned = result
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.trim())
            .collect::<Vec<_>>()
            .join("\n");

        expected.assert_eq(&cleaned);
    }

    #[test]
    fn test_simple_write() {
        let t = IrTestBuilder::new(vec![]);

        let entrypoint = t.build(vec![t.write("<div>Hello World</div>")]);

        check_eval(
            entrypoint,
            HashMap::new(),
            expect!["<div>Hello World</div>"],
        );
    }

    #[test]
    fn test_write_expr() {
        let t = IrTestBuilder::new(vec![("name".to_string(), Type::String)]);

        let entrypoint = t.build(vec![
            t.write("<h1>Hello "),
            t.write_expr(t.var("name"), true),
            t.write("</h1>"),
        ]);

        let mut args = HashMap::new();
        args.insert("name".to_string(), json!("Alice"));

        check_eval(entrypoint, args, expect!["<h1>Hello Alice</h1>"]);
    }

    #[test]
    fn test_escape_html() {
        let t = IrTestBuilder::new(vec![("content".to_string(), Type::String)]);

        let entrypoint = t.build(vec![t.write_expr(t.var("content"), true)]);

        let mut args = HashMap::new();
        args.insert(
            "content".to_string(),
            json!("<script>alert('xss')</script>"),
        );

        check_eval(
            entrypoint,
            args,
            expect!["&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"],
        );
    }

    #[test]
    fn test_if_true() {
        let t = IrTestBuilder::new(vec![("show".to_string(), Type::Bool)]);

        let entrypoint = t.build(vec![
            t.if_stmt(t.var("show"), vec![t.write("<div>Visible</div>")]),
        ]);

        let mut args = HashMap::new();
        args.insert("show".to_string(), json!(true));

        check_eval(entrypoint, args, expect!["<div>Visible</div>"]);
    }

    #[test]
    fn test_if_false() {
        let t = IrTestBuilder::new(vec![("show".to_string(), Type::Bool)]);

        let entrypoint = t.build(vec![
            t.if_stmt(t.var("show"), vec![t.write("<div>Hidden</div>")]),
        ]);

        let mut args = HashMap::new();
        args.insert("show".to_string(), json!(false));

        check_eval(entrypoint, args, expect![""]);
    }

    #[test]
    fn test_for_loop() {
        let t = IrTestBuilder::new(vec![(
            "items".to_string(),
            Type::Array(Some(Box::new(Type::String))),
        )]);

        let entrypoint = t.build(vec![t.for_loop("item", t.var("items"), |t| {
            vec![
                t.write("<li>"),
                t.write_expr(t.var("item"), true),
                t.write("</li>\n"),
            ]
        })]);

        let mut args = HashMap::new();
        args.insert("items".to_string(), json!(["Apple", "Banana", "Cherry"]));

        check_eval(
            entrypoint,
            args,
            expect![[r#"
                    <li>Apple</li>
                    <li>Banana</li>
                    <li>Cherry</li>"#]],
        );
    }

    #[test]
    fn test_component_with_parameter() {
        let t = IrTestBuilder::new(vec![]);

        // In IR, nested components are inlined with Let bindings
        let entrypoint = t.build(vec![
            t.write("<div data-hop-id=\"test/card-comp\">\n"),
            t.let_stmt("title", t.str("Hello World"), |t| {
                vec![
                    t.write("<p>"),
                    t.write_expr(t.var("title"), true),
                    t.write("</p>\n"),
                ]
            }),
            t.write("</div>"),
        ]);

        check_eval(
            entrypoint,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/card-comp">
                <p>Hello World</p>
                </div>"#]],
        );
    }

    #[test]
    fn test_attribute_merging() {
        let t = IrTestBuilder::new(vec![]);

        // In IR, attributes are already merged
        let entrypoint = t.build(vec![
            t.write(
                "<div data-hop-id=\"test/button-comp\" class=\"btn btn-default btn-primary\">\n",
            ),
            t.write("Click me\n"),
            t.write("</div>"),
        ]);

        check_eval(
            entrypoint,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/button-comp" class="btn btn-default btn-primary">
                Click me
                </div>"#]],
        );
    }

    #[test]
    fn test_attribute_override() {
        let t = IrTestBuilder::new(vec![]);

        // In IR, attributes are already processed (overridden for non-class)
        let entrypoint = t.build(vec![
            t.write("<div data-hop-id=\"test/button-comp\" class=\"btn\" data-id=\"custom\">\n"),
            t.write("Click me\n"),
            t.write("</div>"),
        ]);

        check_eval(
            entrypoint,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/button-comp" class="btn" data-id="custom">
                Click me
                </div>"#]],
        );
    }

    #[test]
    fn test_nested_components() {
        let t = IrTestBuilder::new(vec![]);

        // In IR, nested components are fully inlined with Let bindings
        let entrypoint = t.build(vec![
            t.write("<div data-hop-id=\"test/outer-comp\">\n"),
            t.let_stmt("a", t.str("outer"), |t| {
                vec![
                    t.write("<div data-hop-id=\"test/inner-comp\">\n"),
                    t.let_stmt("x", t.var("a"), |t| {
                        vec![t.let_stmt("y", t.str("inner"), |t| {
                            vec![
                                t.write_expr(t.var("x"), true),
                                t.write(" "),
                                t.write_expr(t.var("y"), true),
                                t.write("\n"),
                            ]
                        })]
                    }),
                    t.write("</div>\n"),
                ]
            }),
            t.write("</div>"),
        ]);

        check_eval(
            entrypoint,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/outer-comp">
                <div data-hop-id="test/inner-comp">
                outer inner
                </div>
                </div>"#]],
        );
    }
}
