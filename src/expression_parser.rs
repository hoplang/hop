use crate::common::{BinaryOp, Expression, Range, RangeError, VarName};

#[derive(Debug, Clone, PartialEq)]
enum ExprToken {
    Identifier(String),
    StringLiteral(String),
    Equal,
    Dot,
    LeftParen,
    RightParen,
    In,
    Eof,
}

struct ExprTokenizer {
    input: Vec<char>,
    position: usize,
}

impl ExprTokenizer {
    fn new(input: &str) -> Self {
        ExprTokenizer {
            input: input.chars().collect(),
            position: 0,
        }
    }

    fn peek(&self) -> char {
        if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.peek();
        if ch != '\0' {
            self.position += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_whitespace() {
            self.advance();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        // First character must be letter or underscore
        let first = self.peek();
        if first.is_ascii_alphabetic() || first == '_' {
            result.push(self.advance());
        } else {
            return result;
        }

        // Subsequent characters can be letters, digits, or underscores
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            result.push(self.advance());
        }

        result
    }

    fn read_string_literal(&mut self) -> Result<String, String> {
        let mut result = String::new();

        // Consume opening quote
        if self.peek() != '\'' {
            return Err("Expected opening single quote".to_string());
        }
        self.advance();

        while self.peek() != '\'' && self.peek() != '\0' {
            result.push(self.advance());
        }

        if self.peek() != '\'' {
            return Err("Unterminated string literal".to_string());
        }

        // Consume closing quote
        self.advance();

        Ok(result)
    }

    fn next_token(&mut self) -> Result<ExprToken, String> {
        self.skip_whitespace();

        match self.peek() {
            '\0' => Ok(ExprToken::Eof),
            '.' => {
                self.advance();
                Ok(ExprToken::Dot)
            }
            '(' => {
                self.advance();
                Ok(ExprToken::LeftParen)
            }
            ')' => {
                self.advance();
                Ok(ExprToken::RightParen)
            }
            '=' => {
                self.advance();
                if self.peek() == '=' {
                    self.advance();
                    Ok(ExprToken::Equal)
                } else {
                    Err("Expected '==' but found single '='".to_string())
                }
            }
            '\'' => {
                let string_value = self.read_string_literal()?;
                Ok(ExprToken::StringLiteral(string_value))
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let identifier = self.read_identifier();
                if identifier.is_empty() {
                    Err("Invalid identifier".to_string())
                } else if identifier == "in" {
                    Ok(ExprToken::In)
                } else {
                    Ok(ExprToken::Identifier(identifier))
                }
            }
            ch => Err(format!("Unexpected character: '{}'", ch)),
        }
    }
}

struct ExprParser {
    tokenizer: ExprTokenizer,
    current_token: ExprToken,
}

impl ExprParser {
    fn new(input: &str) -> Result<Self, String> {
        let mut tokenizer = ExprTokenizer::new(input);
        let current_token = tokenizer.next_token()?;
        Ok(ExprParser {
            tokenizer,
            current_token,
        })
    }

    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.tokenizer.next_token()?;
        Ok(())
    }

    fn parse(&mut self) -> Result<Expression, String> {
        self.parse_equality()
    }

    // equality -> primary ( "==" primary )*
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;

        while matches!(self.current_token, ExprToken::Equal) {
            self.advance()?; // consume ==
            let right = self.parse_primary()?;
            expr = Expression::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
        }

        Ok(expr)
    }

    // primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
    fn parse_primary(&mut self) -> Result<Expression, String> {
        match &self.current_token {
            ExprToken::Identifier(name) => {
                let mut expr = Expression::Variable(name.clone());
                self.advance()?;

                // Handle property access
                while matches!(self.current_token, ExprToken::Dot) {
                    self.advance()?; // consume .

                    if let ExprToken::Identifier(prop) = &self.current_token {
                        expr = Expression::PropertyAccess(Box::new(expr), prop.clone());
                        self.advance()?;
                    } else {
                        return Err("Expected identifier after '.'".to_string());
                    }
                }

                Ok(expr)
            }
            ExprToken::StringLiteral(value) => {
                let expr = Expression::StringLiteral(value.clone());
                self.advance()?;
                Ok(expr)
            }
            ExprToken::LeftParen => {
                self.advance()?; // consume (
                let expr = self.parse_equality()?;

                if matches!(self.current_token, ExprToken::RightParen) {
                    self.advance()?; // consume )
                    Ok(expr)
                } else {
                    Err("Expected closing parenthesis".to_string())
                }
            }
            _ => Err("Expected identifier, string literal, or opening parenthesis".to_string()),
        }
    }
}

pub fn parse_expression(expr: &str) -> Result<Expression, String> {
    let expr = expr.trim();
    if expr.is_empty() {
        return Err("Empty expression".to_string());
    }

    let mut parser = ExprParser::new(expr)?;
    let result = parser.parse()?;

    // Ensure we've consumed all tokens
    if !matches!(parser.current_token, ExprToken::Eof) {
        return Err("Unexpected tokens at end of expression".to_string());
    }

    Ok(result)
}

pub fn parse_loop_header(
    header: &str,
    range: Range,
    errors: &mut Vec<RangeError>,
) -> Option<(VarName, Expression)> {
    let header = header.trim();
    if header.is_empty() {
        errors.push(RangeError::new(
            "Empty loop header".to_string(),
            range,
        ));
        return None;
    }

    // Create parser for the loop header
    let mut parser = match ExprParser::new(header) {
        Ok(parser) => parser,
        Err(err) => {
            errors.push(RangeError::new(
                format!("Invalid expression in <for> tag: {}", err),
                range,
            ));
            return None;
        }
    };

    // Expect: IDENTIFIER "in" expression
    let var_name = match &parser.current_token {
        ExprToken::Identifier(name) => name.clone(),
        _ => {
            errors.push(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                range,
            ));
            return None;
        }
    };

    // Advance past the identifier
    if parser.advance().is_err() {
        errors.push(RangeError::new(
            "Invalid expression in <for> tag".to_string(),
            range,
        ));
        return None;
    }

    // Expect "in" keyword
    if !matches!(parser.current_token, ExprToken::In) {
        errors.push(RangeError::new(
            "Expected 'in' keyword in <for> tag".to_string(),
            range,
        ));
        return None;
    }

    // Advance past "in"
    if parser.advance().is_err() {
        errors.push(RangeError::new(
            "Invalid expression in <for> tag".to_string(),
            range,
        ));
        return None;
    }

    // Parse the array expression
    let array_expr = match parser.parse_equality() {
        Ok(expr) => expr,
        Err(err) => {
            errors.push(RangeError::new(
                format!("Invalid array expression in <for> tag: {}", err),
                range,
            ));
            return None;
        }
    };

    // Ensure we've consumed all tokens
    if !matches!(parser.current_token, ExprToken::Eof) {
        errors.push(RangeError::new(
            "Unexpected tokens at end of <for> expression".to_string(),
            range,
        ));
        return None;
    }

    // Validate the variable name
    match VarName::new(var_name.clone()) {
        Some(validated_var_name) => Some((validated_var_name, array_expr)),
        None => {
            errors.push(RangeError::invalid_variable_name(&var_name, range));
            None
        }
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
    fn test_expression_parser() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/expression_parser.cases");

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

            let result = parse_expression(input).unwrap_or_else(|e| {
                panic!(
                    "Failed to parse expression '{}' in test case {} (line {}): {}",
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
