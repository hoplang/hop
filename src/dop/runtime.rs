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
        DopExpr::NumberLiteral(value) => Ok(serde_json::Value::Number(value.clone())),
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

#[cfg(test)]
mod tests {

    use crate::common::Range;
    use crate::dop::parse_expr;

    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_evaluate_expr() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/evaluate_expr.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let env_content = archive
                .get("env")
                .expect("Missing 'env' section in test case")
                .content
                .trim();
            let expr_content = archive
                .get("expr")
                .expect("Missing 'expr' section in test case")
                .content
                .trim();

            println!("Test case {} (line {})", case_num + 1, line_number);

            // Parse the environment JSON
            let env_json: serde_json::Value =
                serde_json::from_str(env_content).unwrap_or_else(|e| {
                    panic!(
                        "Failed to parse environment JSON in test case {} (line {}): {}",
                        case_num + 1,
                        line_number,
                        e
                    );
                });

            // Create environment from JSON
            let mut env = Environment::new();
            if let serde_json::Value::Object(map) = env_json {
                for (key, value) in map {
                    env.push(key, value);
                }
            }

            // Parse the expression
            let expr = parse_expr(expr_content, Range::default()).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse expression '{}' in test case {} (line {}): {:?}",
                    expr_content,
                    case_num + 1,
                    line_number,
                    e
                );
            });

            // Check if this test case expects an error
            if let Some(error_section) = archive.get("error") {
                let expected_error = error_section.content.trim();

                match evaluate_expr(&expr, &mut env) {
                    Ok(result) => {
                        panic!(
                            "Expected error '{}' but got result '{}' in test case {} (line {})",
                            expected_error,
                            result,
                            case_num + 1,
                            line_number
                        );
                    }
                    Err(err) => {
                        let actual_error = err.to_string();
                        assert_eq!(
                            actual_error,
                            expected_error,
                            "Test case {} (line {}): Error message mismatch",
                            case_num + 1,
                            line_number
                        );
                    }
                }
            } else {
                // This test case expects a successful result
                let expected_section = archive
                    .get("out")
                    .expect("Missing 'out' section in test case");
                let expected: serde_json::Value =
                    serde_json::from_str(expected_section.content.trim()).unwrap_or_else(|e| {
                        panic!(
                            "Failed to parse expected result JSON in test case {} (line {}): {}",
                            case_num + 1,
                            line_number,
                            e
                        );
                    });

                let result = evaluate_expr(&expr, &mut env).unwrap_or_else(|e| {
                    panic!(
                        "Failed to evaluate expression '{}' in test case {} (line {}): {}",
                        expr_content,
                        case_num + 1,
                        line_number,
                        e
                    );
                });

                assert_eq!(
                    result,
                    expected,
                    "Test case {} (line {}): Result mismatch",
                    case_num + 1,
                    line_number
                );
            }
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            if line == "## BEGIN" {
                assert!(
                    !in_case,
                    "Found '## BEGIN' at line {} while already inside a test case",
                    line_number
                );
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(
                    in_case,
                    "Found '## END' at line {} without matching '## BEGIN'",
                    line_number
                );
                in_case = false;
                test_cases.push((current_case.clone(), case_start_line));
                current_case.clear();
            } else if in_case {
                current_case.push_str(line);
                current_case.push('\n');
            }
        }

        assert!(
            !in_case,
            "Reached end of file while still inside a test case starting at line {}",
            case_start_line
        );

        test_cases
    }
}
