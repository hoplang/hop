use super::parser::{BinaryOp, DopExpr, UnaryOp};
use super::ConcreteDopType;
use crate::common::{Environment, Range, RangeError};

/// Check if `subtype` is a subtype of `supertype`
pub fn is_subtype(subtype: &ConcreteDopType, supertype: &ConcreteDopType) -> bool {
    match (subtype, supertype) {
        // Any type is compatible with Any
        (_, ConcreteDopType::Any) => true,
        (ConcreteDopType::Any, _) => true,
        
        // Exact matches
        (ConcreteDopType::Bool, ConcreteDopType::Bool) => true,
        (ConcreteDopType::String, ConcreteDopType::String) => true,
        (ConcreteDopType::Number, ConcreteDopType::Number) => true,
        (ConcreteDopType::Void, ConcreteDopType::Void) => true,
        
        // Arrays are covariant in their element type
        (ConcreteDopType::Array(sub_elem), ConcreteDopType::Array(super_elem)) => {
            is_subtype(sub_elem, super_elem)
        }
        
        // Objects: subtype must have all properties of supertype with compatible types
        (ConcreteDopType::Object(sub_props), ConcreteDopType::Object(super_props)) => {
            super_props.iter().all(|(key, super_type)| {
                sub_props.get(key).map_or(false, |sub_type| is_subtype(sub_type, super_type))
            })
        }
        
        // Otherwise, not a subtype
        _ => false,
    }
}

pub fn typecheck_dop_expression(
    expr: &DopExpr,
    env: &mut Environment<ConcreteDopType>,
    annotations: &mut Vec<(crate::common::Range, ConcreteDopType)>,
    errors: &mut Vec<RangeError>,
    range: Range,
) -> ConcreteDopType {
    match expr {
        DopExpr::Variable(name) => {
            if let Some(var_type) = env.lookup(name) {
                var_type.clone()
            } else {
                errors.push(RangeError::undefined_variable(name, range));
                ConcreteDopType::Any
            }
        }
        DopExpr::BooleanLiteral(_) => ConcreteDopType::Bool,
        DopExpr::StringLiteral(_) => ConcreteDopType::String,
        DopExpr::NumberLiteral(_) => ConcreteDopType::Number,
        DopExpr::PropertyAccess(base_expr, property) => {
            let base_type =
                typecheck_dop_expression(base_expr, env, annotations, errors, range);
            
            match &base_type {
                ConcreteDopType::Object(props) => {
                    if let Some(prop_type) = props.get(property) {
                        prop_type.clone()
                    } else {
                        errors.push(RangeError::new(
                            format!("Property {} not found in object", property),
                            range,
                        ));
                        ConcreteDopType::Any
                    }
                }
                ConcreteDopType::Any => ConcreteDopType::Any,
                _ => {
                    errors.push(RangeError::new(
                        format!("{} can not be used as an object", base_type),
                        range,
                    ));
                    ConcreteDopType::Any
                }
            }
        }
        DopExpr::BinaryOp(left, BinaryOp::Equal, right) => {
            let left_type =
                typecheck_dop_expression(left, env, annotations, errors, range);
            let right_type =
                typecheck_dop_expression(right, env, annotations, errors, range);

            // Both operands should have the same type for equality comparison
            if left_type != right_type && left_type != ConcreteDopType::Any && right_type != ConcreteDopType::Any {
                errors.push(RangeError::new(
                    format!("Can not compare {} to {}", left_type, right_type),
                    range,
                ));
            }

            // The result of == is always boolean
            ConcreteDopType::Bool
        }
        DopExpr::UnaryOp(UnaryOp::Not, expr) => {
            let expr_type =
                typecheck_dop_expression(expr, env, annotations, errors, range);

            // Negation only works on boolean expressions
            if !is_subtype(&expr_type, &ConcreteDopType::Bool) {
                errors.push(RangeError::new(
                    "Negation operator can only be applied to boolean values".to_string(),
                    range,
                ));
            }

            // The result of ! is always boolean
            ConcreteDopType::Bool
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::parser::parse_expr;
    use crate::dop::{DopTokenizer, parse_variable_with_type};
    use crate::test_utils::parse_test_cases;
    use pretty_assertions::assert_eq;

    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_dop_typechecker() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/typechecker.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (archive, line_number)) in test_cases.iter().enumerate() {
            let env_section = archive
                .get("env")
                .expect("Missing 'env' section in test case")
                .content
                .trim();

            let mut env = Environment::new();

            for line in env_section.lines() {
                let mut tokenizer = DopTokenizer::new(line, crate::common::Position::new(1, 1))
                    .unwrap_or_else(|e| {
                        panic!(
                            "Failed to create tokenizer for '{}' in test case {} (line {}): {:?}",
                            line,
                            case_num + 1,
                            line_number,
                            e
                        );
                    });
                let (var_name, var_type) =
                    parse_variable_with_type(&mut tokenizer).unwrap_or_else(|e| {
                        panic!(
                            "Parse error in test case {} (line {}): {:?}",
                            case_num + 1,
                            line_number,
                            e
                        );
                    });
                env.push(var_name.value, var_type);
            }

            let expr_content = archive
                .get("expr")
                .expect("Missing 'expr' section in test case")
                .content
                .trim();

            let mut tokenizer = DopTokenizer::new(expr_content, crate::common::Position::new(1, 1))
                .unwrap_or_else(|e| {
                    panic!(
                        "Failed to create tokenizer for '{}' in test case {} (line {}): {:?}",
                        expr_content,
                        case_num + 1,
                        line_number,
                        e
                    );
                });
            let expr = parse_expr(&mut tokenizer).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse expression '{}' in test case {} (line {}): {:?}",
                    expr_content,
                    case_num + 1,
                    line_number,
                    e
                );
            });

            let mut annotations = Vec::new();
            let mut errors = Vec::new();

            let result_type = typecheck_dop_expression(
                &expr,
                &mut env,
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
                        result_type,
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

                let actual = result_type;
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
