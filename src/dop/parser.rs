use crate::common::{DopVarName, Position, Range, RangeError};
use crate::dop::tokenizer::{DopToken, DopTokenizer};
use crate::dop::{BinaryOp, DopExpr, UnaryOp};

pub fn parse_expr(expr: &str) -> Result<DopExpr, RangeError> {
    parse_expr_with_range(expr, Range::new(Position::new(1, 1), Position::new(1, 1)))
}

pub fn parse_expr_with_range(expr: &str, range: Range) -> Result<DopExpr, RangeError> {
    if expr.trim().is_empty() {
        return Err(RangeError::new("Empty expression".to_string(), range));
    }

    let mut tokenizer = DopTokenizer::new_with_offset(expr, range.start)?;
    let result = parse_equality(&mut tokenizer)?;

    // Ensure we've consumed all tokens
    let t = tokenizer.peek();
    if !matches!(t.token, DopToken::Eof) {
        return Err(RangeError::new(
            "Unexpected token at end of expression".to_string(),
            t.range,
        ));
    }

    Ok(result)
}

pub fn parse_loop_header(header: &str, range: Range) -> Result<(DopVarName, DopExpr), RangeError> {
    if header.trim().is_empty() {
        return Err(RangeError::new("Empty loop header".to_string(), range));
    }

    // Create tokenizer for the loop header
    let mut tokenizer = DopTokenizer::new_with_offset(header, range.start)?;

    // Expect: IDENTIFIER "in" expression
    let var_name = match &tokenizer.peek().token {
        DopToken::Identifier(name) => name.clone(),
        _ => {
            return Err(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                tokenizer.peek().range,
            ));
        }
    };

    // Advance past the identifier
    if tokenizer.advance().is_err() {
        return Err(RangeError::new(
            "Invalid expression in <for> tag".to_string(),
            range,
        ));
    }

    // Expect "in" keyword
    if !matches!(tokenizer.peek().token, DopToken::In) {
        return Err(RangeError::new(
            "Expected 'in' keyword in <for> tag".to_string(),
            tokenizer.peek().range,
        ));
    }

    // Advance past "in"
    if tokenizer.advance().is_err() {
        return Err(RangeError::new(
            "Invalid expression in <for> tag".to_string(),
            range,
        ));
    }

    // Parse the array expression
    let array_expr = parse_equality(&mut tokenizer)?;

    // Ensure we've consumed all tokens
    if !matches!(tokenizer.peek().token, DopToken::Eof) {
        return Err(RangeError::new(
            "Unexpected tokens at end of <for> expression".to_string(),
            tokenizer.peek().range,
        ));
    }

    // Validate the variable name
    match DopVarName::new(var_name.clone()) {
        Some(validated_var_name) => Ok((validated_var_name, array_expr)),
        None => Err(RangeError::invalid_variable_name(&var_name, range)),
    }
}

pub fn parse_variable_name(var_expr: &str, range: Range) -> Result<DopVarName, RangeError> {
    let var_expr = var_expr.trim();
    if var_expr.is_empty() {
        return Err(RangeError::new("Empty variable name".to_string(), range));
    }

    // Create tokenizer for the variable expression
    let mut tokenizer = DopTokenizer::new_with_offset(var_expr, range.start)?;

    // Expect: IDENTIFIER (and nothing else)
    let var_name = match &tokenizer.peek().token {
        DopToken::Identifier(name) => name.clone(),
        _ => {
            return Err(RangeError::new(
                "Expected variable name".to_string(),
                tokenizer.peek().range,
            ));
        }
    };

    // Advance past the identifier
    if tokenizer.advance().is_err() {
        return Err(RangeError::new("Invalid expression".to_string(), range));
    }

    // Ensure we've consumed all tokens (should only be a single identifier)
    if !matches!(tokenizer.peek().token, DopToken::Eof) {
        return Err(RangeError::new(
            "Expected only a variable name, found additional tokens".to_string(),
            tokenizer.peek().range,
        ));
    }

    // Validate the variable name
    match DopVarName::new(var_name.clone()) {
        Some(validated_var_name) => Ok(validated_var_name),
        None => Err(RangeError::invalid_variable_name(&var_name, range)),
    }
}

// equality -> unary ( "==" unary )*
fn parse_equality(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    let mut expr = parse_unary(tokenizer)?;

    while matches!(tokenizer.peek().token, DopToken::Equal) {
        tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
    }

    Ok(expr)
}

// unary -> ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    if matches!(tokenizer.peek().token, DopToken::Not) {
        tokenizer.advance()?; // consume !
        let expr = parse_unary(tokenizer)?; // Right associative for multiple !
        Ok(DopExpr::UnaryOp(UnaryOp::Not, Box::new(expr)))
    } else {
        parse_primary(tokenizer)
    }
}

// primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
fn parse_primary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, RangeError> {
    match &tokenizer.peek().token {
        DopToken::Identifier(name) => {
            let mut expr = DopExpr::Variable(name.clone());
            tokenizer.advance()?;

            // Handle property access
            while matches!(tokenizer.peek().token, DopToken::Dot) {
                tokenizer.advance()?; // consume .

                if let DopToken::Identifier(prop) = &tokenizer.peek().token {
                    expr = DopExpr::PropertyAccess(Box::new(expr), prop.clone());
                    tokenizer.advance()?;
                } else {
                    return Err(RangeError::new(
                        "Expected identifier after '.'".to_string(),
                        tokenizer.peek().range,
                    ));
                }
            }

            Ok(expr)
        }
        DopToken::BooleanLiteral(value) => {
            let expr = DopExpr::BooleanLiteral(*value);
            tokenizer.advance()?;
            Ok(expr)
        }
        DopToken::StringLiteral(value) => {
            let expr = DopExpr::StringLiteral(value.clone());
            tokenizer.advance()?;
            Ok(expr)
        }
        DopToken::NumberLiteral(value) => {
            let expr = DopExpr::NumberLiteral(value.clone());
            tokenizer.advance()?;
            Ok(expr)
        }
        DopToken::LeftParen => {
            tokenizer.advance()?; // consume (
            let expr = parse_equality(tokenizer)?;

            if matches!(tokenizer.peek().token, DopToken::RightParen) {
                tokenizer.advance()?; // consume )
                Ok(expr)
            } else {
                Err(RangeError::new(
                    "Expected closing parenthesis".to_string(),
                    tokenizer.peek().range,
                ))
            }
        }
        _ => Err(RangeError::new(
            "Expected identifier, string literal, number literal, or opening parenthesis"
                .to_string(),
            tokenizer.peek().range,
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

            let result = parse_expr(input).unwrap_or_else(|e| {
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
