use super::parser::{BinaryOp, DopExpr, UnaryOp};
use crate::common::{Environment, Range, RangeError};
use crate::typechecker::TypeAnnotation;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum DopType {
    Object(BTreeMap<String, DopType>),
    Array(Box<DopType>),
    Bool,
    String,
    Number,
    Void,
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopType::Object(properties) => {
                write!(f, "object[")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "]")
            }
            DopType::Array(inner_type) => write!(f, "array[{}]", inner_type),
            DopType::Bool => write!(f, "boolean"),
            DopType::String => write!(f, "string"),
            DopType::Number => write!(f, "number"),
            DopType::Void => write!(f, "void"),
        }
    }
}

/// Check if `subtype` is a subtype of `supertype`
pub fn is_subtype(subtype: &DopType, supertype: &DopType) -> bool {
    match (subtype, supertype) {
        // Exact matches
        (DopType::Bool, DopType::Bool) => true,
        (DopType::String, DopType::String) => true,
        (DopType::Number, DopType::Number) => true,
        (DopType::Void, DopType::Void) => true,

        // Arrays are covariant in their element type
        (DopType::Array(sub_elem), DopType::Array(super_elem)) => is_subtype(sub_elem, super_elem),

        // Objects: subtype must have all properties of supertype with compatible types
        (DopType::Object(sub_props), DopType::Object(super_props)) => {
            super_props.iter().all(|(key, super_type)| {
                sub_props
                    .get(key)
                    .is_some_and(|sub_type| is_subtype(sub_type, super_type))
            })
        }

        // Otherwise, not a subtype
        _ => false,
    }
}

pub fn typecheck_expr(
    expr: &DopExpr,
    env: &mut Environment<DopType>,
    annotations: &mut Vec<TypeAnnotation>,
    errors: &mut Vec<RangeError>,
    range: Range,
) -> Result<DopType, String> {
    match expr {
        DopExpr::Variable(name) => {
            if let Some(var_type) = env.lookup(name) {
                annotations.push(TypeAnnotation {
                    range,
                    typ: var_type.clone(),
                    name: name.clone(),
                });
                Ok(var_type.clone())
            } else {
                Err(format!("Undefined variable: {}", name))
            }
        }
        DopExpr::BooleanLiteral(_) => Ok(DopType::Bool),
        DopExpr::StringLiteral(_) => Ok(DopType::String),
        DopExpr::NumberLiteral(_) => Ok(DopType::Number),
        DopExpr::PropertyAccess(base_expr, property) => {
            let base_type = typecheck_expr(base_expr, env, annotations, errors, range)?;

            match &base_type {
                DopType::Object(props) => {
                    if let Some(prop_type) = props.get(property) {
                        Ok(prop_type.clone())
                    } else {
                        Err(format!("Property {} not found in object", property))
                    }
                }
                _ => Err(format!("{} can not be used as an object", base_type)),
            }
        }
        DopExpr::BinaryOp(left, BinaryOp::Equal, right) => {
            let left_type = typecheck_expr(left, env, annotations, errors, range)?;
            let right_type = typecheck_expr(right, env, annotations, errors, range)?;

            // Both operands should have the same type for equality comparison
            if left_type != right_type {
                return Err(format!("Can not compare {} to {}", left_type, right_type));
            }

            // The result of == is always boolean
            Ok(DopType::Bool)
        }
        DopExpr::UnaryOp(UnaryOp::Not, expr) => {
            let expr_type = typecheck_expr(expr, env, annotations, errors, range)?;

            // Negation only works on boolean expressions
            if !is_subtype(&expr_type, &DopType::Bool) {
                return Err("Negation operator can only be applied to boolean values".to_string());
            }

            // The result of ! is always boolean
            Ok(DopType::Bool)
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

            let result_type = typecheck_expr(
                &expr,
                &mut env,
                &mut annotations,
                &mut errors,
                Range::default(),
            );

            // Check if this test case expects an error
            if let Some(error_section) = archive.get("error") {
                let expected_error = error_section.content.trim();

                match result_type {
                    Err(actual_error) => {
                        assert_eq!(
                            actual_error,
                            expected_error,
                            "Test case {} (line {}): Error message mismatch",
                            case_num + 1,
                            line_number
                        );
                    }
                    Ok(successful_type) => {
                        panic!(
                            "Expected error '{}' but got successful result '{}' in test case {} (line {})",
                            expected_error,
                            successful_type,
                            case_num + 1,
                            line_number
                        );
                    }
                }
            } else {
                // This test case expects a successful result
                match result_type {
                    Ok(actual_type) => {
                        let expected_section = archive
                            .get("out")
                            .expect("Missing 'out' section in test case");
                        let expected = expected_section.content.trim();

                        assert_eq!(
                            actual_type.to_string(),
                            expected,
                            "Test case {} (line {}): Type mismatch",
                            case_num + 1,
                            line_number
                        );
                    }
                    Err(error_message) => {
                        panic!(
                            "Expected successful result but got error '{}' in test case {} (line {})",
                            error_message,
                            case_num + 1,
                            line_number
                        );
                    }
                }
            }
        }
    }
}
