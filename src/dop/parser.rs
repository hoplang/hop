use crate::common::{DopVarName, Range, RangeError};
use crate::dop::{BinaryOp, DopExpr, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
enum DopToken {
    Identifier(String),
    StringLiteral(String),
    BooleanLiteral(bool),
    NumberLiteral(serde_json::Number),
    Equal,
    Not,
    Dot,
    LeftParen,
    RightParen,
    In,
    Eof,
}

struct DopTokenizer {
    input: Vec<char>,
    position: usize,
    current_token: DopToken,
}

impl DopTokenizer {
    fn new(input: &str) -> Result<Self, String> {
        let mut tokenizer = DopTokenizer {
            input: input.chars().collect(),
            position: 0,
            current_token: DopToken::Eof, // temporary value
        };
        tokenizer.current_token = tokenizer.next_token()?;
        Ok(tokenizer)
    }

    fn peek(&self) -> char {
        if self.position >= self.input.len() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance_char(&mut self) -> char {
        let ch = self.peek();
        if ch != '\0' {
            self.position += 1;
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while self.peek().is_whitespace() {
            self.advance_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        // First character must be letter or underscore
        let first = self.peek();
        if first.is_ascii_alphabetic() || first == '_' {
            result.push(self.advance_char());
        } else {
            return result;
        }

        // Subsequent characters can be letters, digits, or underscores
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
            result.push(self.advance_char());
        }

        result
    }

    fn read_string_literal(&mut self) -> Result<String, String> {
        let mut result = String::new();

        // Consume opening quote
        if self.peek() != '\'' {
            return Err("Expected opening single quote".to_string());
        }
        self.advance_char();

        while self.peek() != '\'' && self.peek() != '\0' {
            result.push(self.advance_char());
        }

        if self.peek() != '\'' {
            return Err("Unterminated string literal".to_string());
        }

        // Consume closing quote
        self.advance_char();

        Ok(result)
    }

    fn read_number(&mut self) -> Result<serde_json::Number, String> {
        let mut result = String::new();
        let mut is_float = false;

        // Read integer part
        while self.peek().is_ascii_digit() {
            result.push(self.advance_char());
        }

        // Read decimal part if present
        if self.peek() == '.' {
            is_float = true;
            result.push(self.advance_char()); // consume '.'
            
            if !self.peek().is_ascii_digit() {
                return Err("Expected digit after decimal point".to_string());
            }
            
            while self.peek().is_ascii_digit() {
                result.push(self.advance_char());
            }
        }

        if is_float {
            // Parse as float
            let float_val = result.parse::<f64>()
                .map_err(|_| format!("Invalid float format: {}", result))?;
            serde_json::Number::from_f64(float_val)
                .ok_or_else(|| format!("Invalid float value: {}", float_val))
        } else {
            // Parse as integer
            let int_val = result.parse::<i64>()
                .map_err(|_| format!("Invalid integer format: {}", result))?;
            Ok(serde_json::Number::from(int_val))
        }
    }

    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.next_token()?;
        Ok(())
    }

    fn next_token(&mut self) -> Result<DopToken, String> {
        self.skip_whitespace();

        match self.peek() {
            '\0' => Ok(DopToken::Eof),
            '.' => {
                self.advance_char();
                Ok(DopToken::Dot)
            }
            '(' => {
                self.advance_char();
                Ok(DopToken::LeftParen)
            }
            ')' => {
                self.advance_char();
                Ok(DopToken::RightParen)
            }
            '=' => {
                self.advance_char();
                if self.peek() == '=' {
                    self.advance_char();
                    Ok(DopToken::Equal)
                } else {
                    Err("Expected '==' but found single '='".to_string())
                }
            }
            '!' => {
                self.advance_char();
                Ok(DopToken::Not)
            }
            '\'' => {
                let string_value = self.read_string_literal()?;
                Ok(DopToken::StringLiteral(string_value))
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let identifier = self.read_identifier();
                if identifier.is_empty() {
                    Err("Invalid identifier".to_string())
                } else if identifier == "in" {
                    Ok(DopToken::In)
                } else if identifier == "true" {
                    Ok(DopToken::BooleanLiteral(true))
                } else if identifier == "false" {
                    Ok(DopToken::BooleanLiteral(false))
                } else {
                    Ok(DopToken::Identifier(identifier))
                }
            }
            ch if ch.is_ascii_digit() => {
                let number_value = self.read_number()?;
                Ok(DopToken::NumberLiteral(number_value))
            }
            ch => Err(format!("Unexpected character: '{}'", ch)),
        }
    }
}

// equality -> unary ( "==" unary )*
fn parse_equality(tokenizer: &mut DopTokenizer) -> Result<DopExpr, String> {
    let mut expr = parse_unary(tokenizer)?;

    while matches!(tokenizer.current_token, DopToken::Equal) {
        tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
    }

    Ok(expr)
}

// unary -> ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, String> {
    if matches!(tokenizer.current_token, DopToken::Not) {
        tokenizer.advance()?; // consume !
        let expr = parse_unary(tokenizer)?; // Right associative for multiple !
        Ok(DopExpr::UnaryOp(UnaryOp::Not, Box::new(expr)))
    } else {
        parse_primary(tokenizer)
    }
}

// primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
fn parse_primary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, String> {
    match &tokenizer.current_token {
        DopToken::Identifier(name) => {
            let mut expr = DopExpr::Variable(name.clone());
            tokenizer.advance()?;

            // Handle property access
            while matches!(tokenizer.current_token, DopToken::Dot) {
                tokenizer.advance()?; // consume .

                if let DopToken::Identifier(prop) = &tokenizer.current_token {
                    expr = DopExpr::PropertyAccess(Box::new(expr), prop.clone());
                    tokenizer.advance()?;
                } else {
                    return Err("Expected identifier after '.'".to_string());
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

            if matches!(tokenizer.current_token, DopToken::RightParen) {
                tokenizer.advance()?; // consume )
                Ok(expr)
            } else {
                Err("Expected closing parenthesis".to_string())
            }
        }
        _ => Err("Expected identifier, string literal, number literal, or opening parenthesis".to_string()),
    }
}

pub fn parse_expr(expr: &str) -> Result<DopExpr, String> {
    let expr = expr.trim();
    if expr.is_empty() {
        return Err("Empty expression".to_string());
    }

    let mut tokenizer = DopTokenizer::new(expr)?;
    let result = parse_equality(&mut tokenizer)?;

    // Ensure we've consumed all tokens
    if !matches!(tokenizer.current_token, DopToken::Eof) {
        return Err("Unexpected tokens at end of expression".to_string());
    }

    Ok(result)
}

pub fn parse_loop_header(
    header: &str,
    range: Range,
    errors: &mut Vec<RangeError>,
) -> Option<(DopVarName, DopExpr)> {
    let header = header.trim();
    if header.is_empty() {
        errors.push(RangeError::new("Empty loop header".to_string(), range));
        return None;
    }

    // Create tokenizer for the loop header
    let mut tokenizer = match DopTokenizer::new(header) {
        Ok(tokenizer) => tokenizer,
        Err(err) => {
            errors.push(RangeError::new(
                format!("Invalid expression in <for> tag: {}", err),
                range,
            ));
            return None;
        }
    };

    // Expect: IDENTIFIER "in" expression
    let var_name = match &tokenizer.current_token {
        DopToken::Identifier(name) => name.clone(),
        _ => {
            errors.push(RangeError::new(
                "Expected variable name in <for> tag".to_string(),
                range,
            ));
            return None;
        }
    };

    // Advance past the identifier
    if tokenizer.advance().is_err() {
        errors.push(RangeError::new(
            "Invalid expression in <for> tag".to_string(),
            range,
        ));
        return None;
    }

    // Expect "in" keyword
    if !matches!(tokenizer.current_token, DopToken::In) {
        errors.push(RangeError::new(
            "Expected 'in' keyword in <for> tag".to_string(),
            range,
        ));
        return None;
    }

    // Advance past "in"
    if tokenizer.advance().is_err() {
        errors.push(RangeError::new(
            "Invalid expression in <for> tag".to_string(),
            range,
        ));
        return None;
    }

    // Parse the array expression
    let array_expr = match parse_equality(&mut tokenizer) {
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
    if !matches!(tokenizer.current_token, DopToken::Eof) {
        errors.push(RangeError::new(
            "Unexpected tokens at end of <for> expression".to_string(),
            range,
        ));
        return None;
    }

    // Validate the variable name
    match DopVarName::new(var_name.clone()) {
        Some(validated_var_name) => Some((validated_var_name, array_expr)),
        None => {
            errors.push(RangeError::invalid_variable_name(&var_name, range));
            None
        }
    }
}

pub fn parse_variable_name(
    var_expr: &str,
    range: Range,
    errors: &mut Vec<RangeError>,
) -> Option<DopVarName> {
    let var_expr = var_expr.trim();
    if var_expr.is_empty() {
        errors.push(RangeError::new("Empty variable name".to_string(), range));
        return None;
    }

    // Create tokenizer for the variable expression
    let mut tokenizer = match DopTokenizer::new(var_expr) {
        Ok(tokenizer) => tokenizer,
        Err(err) => {
            errors.push(RangeError::new(
                format!("Invalid expression: {}", err),
                range,
            ));
            return None;
        }
    };

    // Expect: IDENTIFIER (and nothing else)
    let var_name = match &tokenizer.current_token {
        DopToken::Identifier(name) => name.clone(),
        _ => {
            errors.push(RangeError::new("Expected variable name".to_string(), range));
            return None;
        }
    };

    // Advance past the identifier
    if tokenizer.advance().is_err() {
        errors.push(RangeError::new("Invalid expression".to_string(), range));
        return None;
    }

    // Ensure we've consumed all tokens (should only be a single identifier)
    if !matches!(tokenizer.current_token, DopToken::Eof) {
        errors.push(RangeError::new(
            "Expected only a variable name, found additional tokens".to_string(),
            range,
        ));
        return None;
    }

    // Validate the variable name
    match DopVarName::new(var_name.clone()) {
        Some(validated_var_name) => Some(validated_var_name),
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
