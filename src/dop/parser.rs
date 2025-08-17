use crate::common::{Range, RangeError};
use crate::dop::tokenizer::{DopToken, DopTokenizer};
use crate::dop::{BinaryOp, DopExpr, UnaryOp};

/// A DopVarName represents a validated variable name in dop.
#[derive(Debug, Clone, PartialEq)]
pub struct DopVarName {
    pub value: String,
}

impl DopVarName {
    pub fn new(value: String) -> Option<Self> {
        let mut chars = value.chars();
        if let Some(first_char) = chars.next() {
            if !first_char.is_ascii_lowercase() {
                return None;
            }
            if !chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit()) {
                return None;
            }
        } else {
            return None;
        }
        Some(DopVarName { value })
    }
}

pub fn parse_expr_with_range(expr: &str, full_range: Range) -> Result<DopExpr, RangeError> {
    if expr.trim().is_empty() {
        return Err(RangeError::new("Empty expression".to_string(), full_range));
    }

    let mut tokenizer = DopTokenizer::new(expr, full_range.start)?;
    let result = parse_equality(&mut tokenizer)?;

    // expect Eof
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Unexpected token at end of expression".to_string(),
                *range,
            ));
        }
    }

    Ok(result)
}

pub fn parse_loop_header(
    header: &str,
    full_range: Range,
) -> Result<(DopVarName, DopExpr), RangeError> {
    if header.trim().is_empty() {
        return Err(RangeError::new("Empty loop header".to_string(), full_range));
    }

    let mut tokenizer = DopTokenizer::new(header, full_range.start)?;

    // expect Identifier
    let var_name = match tokenizer.advance()? {
        (DopToken::Identifier(name), range) => match DopVarName::new(name.clone()) {
            Some(var_name) => var_name,
            None => return Err(RangeError::invalid_variable_name(&name, range)),
        },
        (_, range) => {
            return Err(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                range,
            ));
        }
    };

    // expect In
    match tokenizer.advance()? {
        (DopToken::In, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Expected 'in' keyword in <for> tag".to_string(),
                range,
            ));
        }
    }

    let array_expr = parse_equality(&mut tokenizer)?;

    // expect Eof
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new(
                "Unexpected token at end of <for> expression".to_string(),
                *range,
            ));
        }
    }

    Ok((var_name, array_expr))
}

pub fn parse_variable_name(var_expr: &str, range: Range) -> Result<DopVarName, RangeError> {
    if var_expr.trim().is_empty() {
        return Err(RangeError::new("Empty variable name".to_string(), range));
    }

    let mut tokenizer = DopTokenizer::new(var_expr, range.start)?;

    // expect Identifier
    let var_name = match tokenizer.advance()? {
        (DopToken::Identifier(name), range) => match DopVarName::new(name.clone()) {
            Some(var_name) => var_name,
            None => return Err(RangeError::invalid_variable_name(&name, range)),
        },
        (_, range) => {
            return Err(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                range,
            ));
        }
    };

    // expect Eof
    match tokenizer.peek() {
        (DopToken::Eof, _) => {}
        (_, range) => {
            return Err(RangeError::new("Unexpected token".to_string(), *range));
        }
    }

    Ok(var_name)
}

// equality -> unary ( "==" unary )*
fn parse_equality(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    let mut expr = parse_unary(tokenizer)?;

    while matches!(tokenizer.peek(), (DopToken::Equal, _)) {
        tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
    }

    Ok(expr)
}

// unary -> ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.peek() {
        (DopToken::Not, _) => {
            tokenizer.advance()?; // consume !
            let expr = parse_unary(tokenizer)?; // Right associative for multiple !
            Ok(DopExpr::UnaryOp(UnaryOp::Not, Box::new(expr)))
        }
        _ => parse_primary(tokenizer),
    }
}

// primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
fn parse_primary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match tokenizer.peek() {
        (DopToken::Identifier(name), _) => {
            let mut expr = DopExpr::Variable(name.clone());
            tokenizer.advance()?; // consume identifier

            // Handle property access
            while matches!(tokenizer.peek(), (DopToken::Dot, _)) {
                tokenizer.advance()?; // consume .

                match tokenizer.peek() {
                    (DopToken::Identifier(prop), _) => {
                        expr = DopExpr::PropertyAccess(Box::new(expr), prop.clone());
                        tokenizer.advance()?;
                    }
                    (_, range) => {
                        return Err(RangeError::new(
                            "Expected identifier after '.'".to_string(),
                            *range,
                        ));
                    }
                }
            }

            Ok(expr)
        }
        (DopToken::BooleanLiteral(value), _) => {
            let expr = DopExpr::BooleanLiteral(*value);
            tokenizer.advance()?;
            Ok(expr)
        }
        (DopToken::StringLiteral(value), _) => {
            let expr = DopExpr::StringLiteral(value.clone());
            tokenizer.advance()?;
            Ok(expr)
        }
        (DopToken::NumberLiteral(value), _) => {
            let expr = DopExpr::NumberLiteral(value.clone());
            tokenizer.advance()?;
            Ok(expr)
        }
        (DopToken::LeftParen, _) => {
            tokenizer.advance()?; // consume (
            let expr = parse_equality(tokenizer)?;

            match tokenizer.peek() {
                (DopToken::RightParen, _) => {
                    tokenizer.advance()?; // consume )
                    Ok(expr)
                }
                (_, range) => Err(RangeError::new(
                    "Expected closing parenthesis".to_string(),
                    *range,
                )),
            }
        }
        (_, range) => Err(RangeError::new(
            "Expected identifier, string literal, number literal, or opening parenthesis"
                .to_string(),
            *range,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    #[test]
    fn test_parse_expr() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/parse_expr.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let input = archive
                .get("in")
                .expect("Missing 'in' section in test case")
                .content
                .trim();
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();

            println!("Test case {} (line {})", case_num + 1, line_number);

            let result = parse_expr_with_range(input, Range::default()).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse expression '{}' in test case {} (line {}): {:?}",
                    input,
                    case_num + 1,
                    line_number,
                    e
                );
            });

            let actual = format!("{:?}", result);

            assert_eq!(
                actual,
                expected,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
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
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(
            !in_case,
            "Reached end of file while inside a test case (missing '## END')"
        );

        test_cases
    }
}
