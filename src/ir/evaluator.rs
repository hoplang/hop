use crate::hop::environment::Environment;
use crate::ir::IrExpr;
use crate::{
    common::escape_html,
    dop::r#type::{ComparableType, EquatableType, NumericType},
};
use anyhow::{Result, anyhow};
use serde_json::Value;
use std::collections::HashMap;

use super::ast::{IrEntrypoint, IrStatement};

/// Evaluate an IR entrypoint with the given arguments
pub fn evaluate_entrypoint(
    entrypoint: &IrEntrypoint,
    args: HashMap<String, Value>,
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
            else_body,
        } => {
            let cond_value = evaluate_expr(condition, env)?;
            if cond_value.as_bool().unwrap_or(false) {
                eval_statements(body, env, output)?;
            } else if let Some(else_stmts) = else_body {
                eval_statements(else_stmts, env, output)?;
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
        IrExpr::FieldAccess {
            record: object,
            field,
            ..
        } => {
            let obj_value = evaluate_expr(object, env)?;
            if let Some(obj) = obj_value.as_object() {
                Ok(obj.get(field.as_str()).cloned().unwrap_or(Value::Null))
            } else {
                Ok(Value::Null)
            }
        }
        IrExpr::StringLiteral { value: s, .. } => Ok(Value::String(s.clone())),
        IrExpr::BooleanLiteral { value: b, .. } => Ok(Value::Bool(*b)),
        IrExpr::FloatLiteral { value: f, .. } => {
            Ok(Value::Number(serde_json::Number::from_f64(*f).unwrap()))
        }
        IrExpr::IntLiteral { value: i, .. } => Ok(Value::Number(serde_json::Number::from(*i))),
        IrExpr::ArrayLiteral { elements, .. } => {
            let mut array = Vec::new();
            for elem in elements {
                array.push(evaluate_expr(elem, env)?);
            }
            Ok(Value::Array(array))
        }
        IrExpr::RecordInstantiation { fields, .. } => {
            // Record instantiation evaluates to an object
            let mut obj = serde_json::Map::new();
            for (key, value) in fields {
                obj.insert(key.as_str().to_string(), evaluate_expr(value, env)?);
            }
            Ok(Value::Object(obj))
        }
        IrExpr::JsonEncode { value, .. } => {
            let val = evaluate_expr(value, env)?;
            let json_str = serde_json::to_string(&val)?;
            Ok(Value::String(json_str))
        }
        IrExpr::EnvLookup { key, .. } => {
            let key_val = evaluate_expr(key, env)?;
            let key_str = key_val
                .as_str()
                .ok_or_else(|| anyhow!("EnvLookup key must be a string"))?;
            let env_val = std::env::var(key_str).unwrap_or_default();
            Ok(Value::String(env_val))
        }
        IrExpr::StringConcat { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match (left_val, right_val) {
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                _ => panic!("String concatenation requires two strings"),
            }
        }
        IrExpr::BooleanNegation { operand, .. } => {
            let val = evaluate_expr(operand, env)?;
            let bool_val = val.as_bool().unwrap_or(false);
            Ok(Value::Bool(!bool_val))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Bool,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool == right_bool))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::String,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_str = left_val.as_str().unwrap();
            let right_str = right_val.as_str().unwrap();
            Ok(Value::Bool(left_str == right_str))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Int,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_int = left_val.as_i64().unwrap_or(0);
            let right_int = right_val.as_i64().unwrap_or(0);
            Ok(Value::Bool(left_int == right_int))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Float,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_float = left_val.as_f64().unwrap_or(0.0);
            let right_float = right_val.as_f64().unwrap_or(0.0);
            Ok(Value::Bool(left_float == right_float))
        }
        IrExpr::Equals {
            left,
            right,
            operand_types: EquatableType::Enum { .. },
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_str = left_val.as_str().unwrap();
            let right_str = right_val.as_str().unwrap();
            Ok(Value::Bool(left_str == right_str))
        }
        IrExpr::NotEquals {
            left,
            right,
            operand_types: EquatableType::Bool,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool != right_bool))
        }
        IrExpr::NotEquals {
            left,
            right,
            operand_types: EquatableType::String,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_str = left_val.as_str().unwrap();
            let right_str = right_val.as_str().unwrap();
            Ok(Value::Bool(left_str != right_str))
        }
        IrExpr::NotEquals {
            left,
            right,
            operand_types: EquatableType::Int,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_int = left_val.as_i64().unwrap_or(0);
            let right_int = right_val.as_i64().unwrap_or(0);
            Ok(Value::Bool(left_int != right_int))
        }
        IrExpr::NotEquals {
            left,
            right,
            operand_types: EquatableType::Float,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_float = left_val.as_f64().unwrap_or(0.0);
            let right_float = right_val.as_f64().unwrap_or(0.0);
            Ok(Value::Bool(left_float != right_float))
        }
        IrExpr::NotEquals {
            left,
            right,
            operand_types: EquatableType::Enum { .. },
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_str = left_val.as_str().unwrap();
            let right_str = right_val.as_str().unwrap();
            Ok(Value::Bool(left_str != right_str))
        }
        IrExpr::LessThan {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    left_int < right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    left_float < right_float
                }
            };
            Ok(Value::Bool(result))
        }

        IrExpr::GreaterThan {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    left_int > right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    left_float > right_float
                }
            };
            Ok(Value::Bool(result))
        }

        IrExpr::LessThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    left_int <= right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    left_float <= right_float
                }
            };
            Ok(Value::Bool(result))
        }

        IrExpr::GreaterThanOrEqual {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            let result = match operand_types {
                ComparableType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    left_int >= right_int
                }
                ComparableType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    left_float >= right_float
                }
            };
            Ok(Value::Bool(result))
        }

        IrExpr::BooleanLogicalAnd { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool && right_bool))
        }

        IrExpr::BooleanLogicalOr { left, right, .. } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;
            let left_bool = left_val.as_bool().unwrap_or(false);
            let right_bool = right_val.as_bool().unwrap_or(false);
            Ok(Value::Bool(left_bool || right_bool))
        }

        IrExpr::NumericAdd {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    Ok(Value::Number(serde_json::Number::from(
                        left_int + right_int,
                    )))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    Ok(Value::Number(
                        serde_json::Number::from_f64(left_float + right_float)
                            .unwrap_or_else(|| serde_json::Number::from(0)),
                    ))
                }
            }
        }

        IrExpr::NumericSubtract {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    Ok(Value::Number(serde_json::Number::from(
                        left_int - right_int,
                    )))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    Ok(Value::Number(
                        serde_json::Number::from_f64(left_float - right_float)
                            .unwrap_or_else(|| serde_json::Number::from(0)),
                    ))
                }
            }
        }

        IrExpr::NumericMultiply {
            left,
            right,
            operand_types,
            ..
        } => {
            let left_val = evaluate_expr(left, env)?;
            let right_val = evaluate_expr(right, env)?;

            match operand_types {
                NumericType::Int => {
                    let left_int = left_val.as_i64().unwrap_or(0);
                    let right_int = right_val.as_i64().unwrap_or(0);
                    Ok(Value::Number(serde_json::Number::from(
                        left_int * right_int,
                    )))
                }
                NumericType::Float => {
                    let left_float = left_val.as_f64().unwrap_or(0.0);
                    let right_float = right_val.as_f64().unwrap_or(0.0);
                    Ok(Value::Number(
                        serde_json::Number::from_f64(left_float * right_float)
                            .unwrap_or_else(|| serde_json::Number::from(0)),
                    ))
                }
            }
        }
        IrExpr::EnumInstantiation { variant_name, .. } => {
            // Enum variants evaluate to their string name
            Ok(Value::String(variant_name.clone()))
        }
        IrExpr::Match { .. } => todo!("Match expression evaluation not yet implemented"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::ir::test_utils::{build_ir_auto, build_ir_with_enums};
    use expect_test::{Expect, expect};
    use serde_json::json;

    fn check(entrypoint: IrEntrypoint, args: Vec<(&str, Value)>, expected: Expect) {
        let before = entrypoint.to_string();
        let args_map: HashMap<String, Value> =
            args.into_iter().map(|(k, v)| (k.to_string(), v)).collect();
        let after = evaluate_entrypoint(&entrypoint, args_map).expect("Evaluation should succeed");

        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_evaluate_simple_write() {
        check(
            build_ir_auto("Test", vec![], |t| {
                t.write("<div>Hello World</div>");
            }),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  write("<div>Hello World</div>")
                }

                -- after --
                <div>Hello World</div>
            "#]],
        );
    }

    #[test]
    fn should_escape_html_in_expressions() {
        check(
            build_ir_auto("Test", vec![("content", Type::String)], |t| {
                t.write_expr_escaped(t.var("content"));
            }),
            vec![("content", json!("<script>alert('xss')</script>"))],
            expect![[r#"
                -- before --
                Test(content: String) {
                  write_escaped(content)
                }

                -- after --
                &lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;
            "#]],
        );
    }

    #[test]
    fn should_render_if_body_when_condition_is_true() {
        check(
            build_ir_auto("Test", vec![("show", Type::Bool)], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Visible</div>");
                });
            }),
            vec![("show", json!(true))],
            expect![[r#"
                -- before --
                Test(show: Bool) {
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
    fn should_skip_if_body_when_condition_is_false() {
        check(
            build_ir_auto("Test", vec![("show", Type::Bool)], |t| {
                t.if_stmt(t.var("show"), |t| {
                    t.write("<div>Hidden</div>");
                });
            }),
            vec![("show", json!(false))],
            expect![[r#"
                -- before --
                Test(show: Bool) {
                  if show {
                    write("<div>Hidden</div>")
                  }
                }

                -- after --

            "#]],
        );
    }

    #[test]
    fn should_iterate_over_array_in_for_loop() {
        check(
            build_ir_auto(
                "Test",
                vec![("items", Type::Array(Box::new(Type::String)))],
                |t| {
                    t.for_loop("item", t.var("items"), |t| {
                        t.write("<li>");
                        t.write_expr_escaped(t.var("item"));
                        t.write("</li>\n");
                    });
                },
            ),
            vec![("items", json!(["Apple", "Banana", "Cherry"]))],
            expect![[r#"
                -- before --
                Test(items: Array[String]) {
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

    #[test]
    fn should_evaluate_enum_instantiation_in_condition() {
        check(
            build_ir_with_enums(
                "Test",
                vec![],
                vec![("Color", vec!["Red", "Green", "Blue"])],
                |t| {
                    let color = t.enum_variant("Color", "Red");
                    let red = t.enum_variant("Color", "Red");
                    t.if_stmt(t.eq(color, red), |t| {
                        t.write("is red");
                    });
                },
            ),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  if (Color::Red == Color::Red) {
                    write("is red")
                  }
                }

                -- after --
                is red
            "#]],
        );
    }

    #[test]
    fn should_evaluate_enum_equality_true() {
        check(
            build_ir_with_enums(
                "Test",
                vec![],
                vec![("Status", vec!["Active", "Inactive", "Pending"])],
                |t| {
                    let active1 = t.enum_variant("Status", "Active");
                    let active2 = t.enum_variant("Status", "Active");
                    t.if_stmt(t.eq(active1, active2), |t| {
                        t.write("equal");
                    });
                },
            ),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  if (Status::Active == Status::Active) {
                    write("equal")
                  }
                }

                -- after --
                equal
            "#]],
        );
    }

    #[test]
    fn should_evaluate_enum_equality_false() {
        check(
            build_ir_with_enums(
                "Test",
                vec![],
                vec![("Status", vec!["Active", "Inactive", "Pending"])],
                |t| {
                    let active = t.enum_variant("Status", "Active");
                    let inactive = t.enum_variant("Status", "Inactive");
                    t.if_else_stmt(
                        t.eq(active, inactive),
                        |t| {
                            t.write("equal");
                        },
                        |t| {
                            t.write("not equal");
                        },
                    );
                },
            ),
            vec![],
            expect![[r#"
                -- before --
                Test() {
                  if (Status::Active == Status::Inactive) {
                    write("equal")
                  } else {
                    write("not equal")
                  }
                }

                -- after --
                not equal
            "#]],
        );
    }
}
