use crate::common::escape_html;
use crate::hop::environment::Environment;
use crate::ir::IrExpr;
use anyhow::{Result, anyhow};
use serde_json::Value;
use std::collections::HashMap;

use super::ast::{BinaryOp, IrEntrypoint, IrExprValue, IrNode, UnaryOp};

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
        IrExprValue::String(s) => Ok(Value::String(s.clone())),
        IrExprValue::Boolean(b) => Ok(Value::Bool(*b)),
        IrExprValue::Number(n) => Ok(Value::Number(
            serde_json::Number::from_f64(*n).unwrap_or_else(|| serde_json::Number::from(0)),
        )),
        IrExprValue::Array(elements) => {
            let mut array = Vec::new();
            for elem in elements {
                array.push(evaluate_ir_expr(elem, env)?);
            }
            Ok(Value::Array(array))
        }
        IrExprValue::Object(properties) => {
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
                BinaryOp::Equal => Ok(Value::Bool(left_val == right_val)),
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

/// Evaluate a slice of IR nodes
pub fn eval_ir(nodes: &[IrNode], env: &mut Environment<Value>, output: &mut String) -> Result<()> {
    for node in nodes {
        eval_node(node, env, output)?;
    }
    Ok(())
}

/// Evaluate a single IR node
fn eval_node(node: &IrNode, env: &mut Environment<Value>, output: &mut String) -> Result<()> {
    match node {
        IrNode::Write { content } => {
            output.push_str(content);
            Ok(())
        }

        IrNode::WriteExpr { expr, escape } => {
            let value = evaluate_ir_expr(expr, env)?;
            let s = value.as_str().unwrap_or("");
            if *escape {
                output.push_str(&escape_html(s));
            } else {
                output.push_str(s);
            }
            Ok(())
        }

        IrNode::If { condition, body } => {
            let cond_value = evaluate_ir_expr(condition, env)?;
            if cond_value.as_bool().unwrap_or(false) {
                eval_ir(body, env, output)?;
            }
            Ok(())
        }

        IrNode::For { var, array, body } => {
            let array_value = evaluate_ir_expr(array, env)?;
            let items = array_value.as_array().cloned().unwrap_or_default();

            for item in items {
                let _ = env.push(var.clone(), item);
                eval_ir(body, env, output)?;
                let _ = env.pop();
            }
            Ok(())
        }

        IrNode::Let { var, value, body } => {
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
    use crate::error_collector::ErrorCollector;
    use crate::hop::module_name::ModuleName;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::typechecker::TypeChecker;
    use crate::ir::Compiler;
    use expect_test::{Expect, expect};
    use serde_json::json;

    fn compile_hop(source: &str) -> IrEntrypoint {
        let mut errors = ErrorCollector::new();
        let module_name = ModuleName::new("test".to_string()).unwrap();
        let tokenizer = Tokenizer::new(source.to_string());
        let ast = parse(module_name.clone(), tokenizer, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Type check
        let mut typechecker = TypeChecker::default();
        typechecker.typecheck(&[&ast]);
        assert!(
            typechecker
                .type_errors
                .get(&module_name)
                .unwrap()
                .is_empty(),
            "Type errors: {:?}",
            typechecker.type_errors
        );

        // Compile to IR
        let mut asts = HashMap::new();
        asts.insert(module_name.clone(), ast);
        let ir_module = Compiler::compile(&asts);

        // Get the first (and only) entrypoint
        ir_module
            .entry_points
            .into_iter()
            .next()
            .expect("Should have an entrypoint")
            .1
    }

    fn check_eval(source: &str, args: HashMap<String, Value>, expected: Expect) {
        let entrypoint = compile_hop(source);
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
        check_eval(
            r#"
            <main-comp entrypoint>
                <div>Hello World</div>
            </main-comp>
            "#,
            HashMap::new(),
            expect!["<div>Hello World</div>"],
        );
    }

    #[test]
    fn test_write_expr() {
        let mut args = HashMap::new();
        args.insert("name".to_string(), json!("Alice"));

        check_eval(
            r#"
            <main-comp entrypoint {name: string}>
                <h1>Hello {name}</h1>
            </main-comp>
            "#,
            args,
            expect!["<h1>Hello Alice</h1>"],
        );
    }

    #[test]
    fn test_escape_html() {
        let mut args = HashMap::new();
        args.insert(
            "content".to_string(),
            json!("<script>alert('xss')</script>"),
        );

        check_eval(
            r#"
            <main-comp entrypoint {content: string}>
                {content}
            </main-comp>
            "#,
            args,
            expect!["&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;"],
        );
    }

    #[test]
    fn test_if_true() {
        let mut args = HashMap::new();
        args.insert("show".to_string(), json!(true));

        check_eval(
            r#"
            <main-comp entrypoint {show: boolean}>
                <if {show}>
                    <div>Visible</div>
                </if>
            </main-comp>
            "#,
            args,
            expect!["<div>Visible</div>"],
        );
    }

    #[test]
    fn test_if_false() {
        let mut args = HashMap::new();
        args.insert("show".to_string(), json!(false));

        check_eval(
            r#"
            <main-comp entrypoint {show: boolean}>
                <if {show}>
                    <div>Hidden</div>
                </if>
            </main-comp>
            "#,
            args,
            expect![""],
        );
    }

    #[test]
    fn test_for_loop() {
        let mut args = HashMap::new();
        args.insert("items".to_string(), json!(["Apple", "Banana", "Cherry"]));

        check_eval(
            r#"
            <main-comp entrypoint {items: array[string]}>
                <for {item in items}>
                    <li>{item}</li>
                </for>
            </main-comp>
            "#,
            args,
            expect![[r#"
                    <li>Apple</li>
                    <li>Banana</li>
                    <li>Cherry</li>"#]],
        );
    }

    #[test]
    fn test_component_with_parameter() {
        // Test that component parameters create Let bindings
        check_eval(
            r#"
            <card-comp {title: string}>
                <p>{title}</p>
            </card-comp>
            
            <main-comp entrypoint>
                <card-comp {title: "Hello World"}/>
            </main-comp>
            "#,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/card-comp">
                <p>Hello World</p>
                </div>"#]],
        );
    }

    #[test]
    fn test_attribute_merging() {
        // Test that class attributes are properly merged
        check_eval(
            r#"
            <button-comp class="btn btn-default">
                Click me
            </button-comp>
            
            <main-comp entrypoint>
                <button-comp class="btn-primary"/>
            </main-comp>
            "#,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/button-comp" class="btn btn-default btn-primary">
                Click me
                </div>"#]],
        );
    }

    #[test]
    fn test_attribute_override() {
        // Test that non-class attributes are overridden, not merged
        check_eval(
            r#"
            <button-comp class="btn" data-id="default">
                Click me
            </button-comp>
            
            <main-comp entrypoint>
                <button-comp data-id="custom"/>
            </main-comp>
            "#,
            HashMap::new(),
            expect![[r#"
                <div data-hop-id="test/button-comp" class="btn" data-id="custom">
                Click me
                </div>"#]],
        );
    }

    #[test]
    fn test_nested_components() {
        // Test nested components with parameters
        check_eval(
            r#"
            <inner-comp {x: string, y: string}>
                {x} {y}
            </inner-comp>
            
            <outer-comp {a: string}>
                <inner-comp {x: a, y: "inner"}/>
            </outer-comp>
            
            <main-comp entrypoint>
                <outer-comp {a: "outer"}/>
            </main-comp>
            "#,
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
