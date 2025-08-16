use crate::common::{DopVarName, Position, Range, RangeError};
use crate::dop::{BinaryOp, DopExpr, UnaryOp};
use std::str::FromStr;

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

#[derive(Debug, Clone, PartialEq)]
struct RangedDopToken {
    token: DopToken,
    range: Range,
}

impl RangedDopToken {
    fn new(token: DopToken, range: Range) -> Self {
        RangedDopToken { token, range }
    }
}

struct Cursor {
    input: Vec<char>,
    /// Current position (0-indexed, in characters)
    position: usize,
    /// Current line number (1-indexed)
    line: usize,
    /// Current column index (1-indexed, in bytes)
    column: usize,
}

impl Cursor {
    fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.position]
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.peek();
        if !self.is_at_end() {
            let byte_len = ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += byte_len;
            }
            self.position += 1;
        }
        ch
    }

    fn get_position(&self) -> Position {
        Position::new(self.line, self.column)
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }
}

struct DopTokenizer {
    cursor: Cursor,
    current_token: RangedDopToken,
}

impl DopTokenizer {
    fn new(input: &str) -> Result<Self, String> {
        let mut tokenizer = DopTokenizer {
            cursor: Cursor::new(input),
            current_token: RangedDopToken::new(
                DopToken::Eof,
                Range::new(Position::new(1, 1), Position::new(1, 1)),
            ), // temporary value
        };
        tokenizer.current_token = tokenizer.next_token()?;
        Ok(tokenizer)
    }

    fn read_identifier(&mut self) -> String {
        let mut result = String::new();

        // First character must be letter or underscore
        let first = self.cursor.peek();
        if first.is_ascii_alphabetic() || first == '_' {
            result.push(self.cursor.advance());
        } else {
            return result;
        }

        // Subsequent characters can be letters, digits, or underscores
        while self.cursor.peek().is_ascii_alphanumeric() || self.cursor.peek() == '_' {
            result.push(self.cursor.advance());
        }

        result
    }

    fn read_string_literal(&mut self) -> Result<String, String> {
        let mut result = String::new();

        // Consume opening quote
        if self.cursor.peek() != '\'' {
            return Err("Expected opening single quote".to_string());
        }
        self.cursor.advance();

        while self.cursor.peek() != '\'' && self.cursor.peek() != '\0' {
            result.push(self.cursor.advance());
        }

        if self.cursor.peek() != '\'' {
            return Err("Unterminated string literal".to_string());
        }

        // Consume closing quote
        self.cursor.advance();

        Ok(result)
    }

    fn read_number(&mut self) -> Result<serde_json::Number, String> {
        let mut result = String::new();

        // Read integer part
        while self.cursor.peek().is_ascii_digit() {
            result.push(self.cursor.advance());
        }

        // Read decimal part if present
        if self.cursor.peek() == '.' {
            result.push(self.cursor.advance()); // consume '.'

            if !self.cursor.peek().is_ascii_digit() {
                return Err("Expected digit after decimal point".to_string());
            }

            while self.cursor.peek().is_ascii_digit() {
                result.push(self.cursor.advance());
            }
        }

        serde_json::Number::from_str(&result)
            .map_err(|_| format!("Invalid number format: {}", result))
    }

    fn advance(&mut self) -> Result<(), String> {
        self.current_token = self.next_token()?;
        Ok(())
    }

    fn next_token(&mut self) -> Result<RangedDopToken, String> {
        let start_pos = self.cursor.get_position();

        // Skip whitespace
        while self.cursor.peek().is_whitespace() {
            self.cursor.advance();
        }

        let token = match self.cursor.peek() {
            '\0' => DopToken::Eof,
            '.' => {
                self.cursor.advance();
                DopToken::Dot
            }
            '(' => {
                self.cursor.advance();
                DopToken::LeftParen
            }
            ')' => {
                self.cursor.advance();
                DopToken::RightParen
            }
            '=' => {
                self.cursor.advance();
                if self.cursor.peek() == '=' {
                    self.cursor.advance();
                    DopToken::Equal
                } else {
                    return Err("Expected '==' but found single '='".to_string());
                }
            }
            '!' => {
                self.cursor.advance();
                DopToken::Not
            }
            '\'' => {
                let string_value = self.read_string_literal()?;
                DopToken::StringLiteral(string_value)
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let identifier = self.read_identifier();
                if identifier.is_empty() {
                    return Err("Invalid identifier".to_string());
                } else if identifier == "in" {
                    DopToken::In
                } else if identifier == "true" {
                    DopToken::BooleanLiteral(true)
                } else if identifier == "false" {
                    DopToken::BooleanLiteral(false)
                } else {
                    DopToken::Identifier(identifier)
                }
            }
            ch if ch.is_ascii_digit() => {
                let number_value = self.read_number()?;
                DopToken::NumberLiteral(number_value)
            }
            ch => return Err(format!("Unexpected character: '{}'", ch)),
        };

        let end_pos = self.cursor.get_position();
        Ok(RangedDopToken::new(token, Range::new(start_pos, end_pos)))
    }
}

// equality -> unary ( "==" unary )*
fn parse_equality(tokenizer: &mut DopTokenizer) -> Result<DopExpr, String> {
    let mut expr = parse_unary(tokenizer)?;

    while matches!(tokenizer.current_token.token, DopToken::Equal) {
        tokenizer.advance()?; // consume ==
        let right = parse_unary(tokenizer)?;
        expr = DopExpr::BinaryOp(Box::new(expr), BinaryOp::Equal, Box::new(right));
    }

    Ok(expr)
}

// unary -> ( "!" )* primary
fn parse_unary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, String> {
    if matches!(tokenizer.current_token.token, DopToken::Not) {
        tokenizer.advance()?; // consume !
        let expr = parse_unary(tokenizer)?; // Right associative for multiple !
        Ok(DopExpr::UnaryOp(UnaryOp::Not, Box::new(expr)))
    } else {
        parse_primary(tokenizer)
    }
}

// primary -> IDENTIFIER ( "." IDENTIFIER )* | STRING_LITERAL | "(" equality ")"
fn parse_primary(tokenizer: &mut DopTokenizer) -> Result<DopExpr, String> {
    match &tokenizer.current_token.token {
        DopToken::Identifier(name) => {
            let mut expr = DopExpr::Variable(name.clone());
            tokenizer.advance()?;

            // Handle property access
            while matches!(tokenizer.current_token.token, DopToken::Dot) {
                tokenizer.advance()?; // consume .

                if let DopToken::Identifier(prop) = &tokenizer.current_token.token {
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

            if matches!(tokenizer.current_token.token, DopToken::RightParen) {
                tokenizer.advance()?; // consume )
                Ok(expr)
            } else {
                Err("Expected closing parenthesis".to_string())
            }
        }
        _ => Err(
            "Expected identifier, string literal, number literal, or opening parenthesis"
                .to_string(),
        ),
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
    if !matches!(tokenizer.current_token.token, DopToken::Eof) {
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
    let var_name = match &tokenizer.current_token.token {
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
    if !matches!(tokenizer.current_token.token, DopToken::In) {
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
    if !matches!(tokenizer.current_token.token, DopToken::Eof) {
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
    let var_name = match &tokenizer.current_token.token {
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
    if !matches!(tokenizer.current_token.token, DopToken::Eof) {
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
