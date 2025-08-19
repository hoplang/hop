use super::parser::{BinaryOp, DopExpr, UnaryOp};
use super::{DopType, Unifier};
use crate::common::{Environment, Range, RangeError};
use std::collections::BTreeMap;

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
                unifier.new_object(BTreeMap::from([(property.clone(), property_type.clone())]));

            if let Err(_err) = unifier.unify(&base_type, &obj_type) {
                errors.push(RangeError::new(
                    format!("{} can not be used as an object", unifier.query(&base_type)),
                    range,
                ));
            }

            property_type
        }
        DopExpr::BinaryOp(left, BinaryOp::Equal, right) => {
            let left_type =
                typecheck_dop_expression(left, env, unifier, annotations, errors, range);
            let right_type =
                typecheck_dop_expression(right, env, unifier, annotations, errors, range);

            // Both operands should have the same type for equality comparison
            if let Err(_err) = unifier.unify(&left_type, &right_type) {
                errors.push(RangeError::new(
                    format!(
                        "Can not compare {} to {}",
                        unifier.query(&left_type),
                        unifier.query(&right_type)
                    ),
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
            if let Err(_err) = unifier.unify(&expr_type, &DopType::Bool) {
                errors.push(RangeError::new(
                    "Negation operator can only be applied to boolean values".to_string(),
                    range,
                ));
            }

            // The result of ! is always boolean
            DopType::Bool
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::parser::{parse_expr, parse_type_from_string};
    use crate::test_utils::parse_test_cases;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_dop_typechecker() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/typechecker.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let env_section = archive.get("env");
            let expr_content = archive
                .get("expr")
                .expect("Missing 'expr' section in test case")
                .content
                .trim();

            // Parse environment from the env section
            let mut env = Environment::new();
            if let Some(env_section) = env_section {
                for line in env_section.content.lines() {
                    let line = line.trim();
                    if line.is_empty() {
                        continue;
                    }

                    if let Some((var_name, type_str)) = line.split_once(':') {
                        let var_name = var_name.trim();
                        let type_str = type_str.trim();

                        // Parse the type string using the existing parser
                        let var_type = parse_type_from_string(type_str).unwrap_or_else(|e| {
                            panic!(
                                "Failed to parse type '{}' for variable '{}' in test case {} (line {}): {:?}",
                                type_str, var_name, case_num + 1, line_number, e
                            );
                        });

                        env.push(var_name.to_string(), var_type);
                    }
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

            // Run typechecker
            let mut unifier = Unifier::new();
            let mut annotations = Vec::new();
            let mut errors = Vec::new();

            let result_type = typecheck_dop_expression(
                &expr,
                &mut env,
                &mut unifier,
                &mut annotations,
                &mut errors,
                Range::default(),
            );

            // Check if this test case expects an error
            if let Some(error_section) = archive.get("error") {
                let expected_error = error_section.content.trim();

                if errors.is_empty() {
                    panic!(
                        "Expected error '{}' but got successful result '{}' in test case {} (line {})",
                        expected_error,
                        unifier.query(&result_type),
                        case_num + 1,
                        line_number
                    );
                }

                let actual_error = errors[0].message.clone();
                assert_eq!(
                    actual_error,
                    expected_error,
                    "Test case {} (line {}): Error message mismatch",
                    case_num + 1,
                    line_number
                );
            } else {
                // This test case expects a successful result
                if !errors.is_empty() {
                    panic!(
                        "Expected successful result but got error '{}' in test case {} (line {})",
                        errors[0].message,
                        case_num + 1,
                        line_number
                    );
                }

                let expected_section = archive
                    .get("out")
                    .expect("Missing 'out' section in test case");
                let expected = expected_section.content.trim();

                let actual = unifier.query(&result_type);
                assert_eq!(
                    actual.to_string(),
                    expected,
                    "Test case {} (line {}): Type mismatch",
                    case_num + 1,
                    line_number
                );
            }
        }
    }

}
