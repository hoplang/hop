use crate::common::{Position, Range, RangeError};
use std::{mem, str::FromStr};

#[derive(Debug, Clone, PartialEq)]
pub enum DopToken {
    Identifier(String),
    StringLiteral(String),
    BooleanLiteral(bool),
    NumberLiteral(serde_json::Number),
    Equal,
    Not,
    Dot,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
    In,
    // Type tokens
    TypeString,
    TypeNumber,
    TypeBoolean,
    TypeVoid,
    TypeArray,
    TypeObject,
    Eof,
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
    fn new(input: &str, start_pos: Position) -> Self {
        Self {
            input: input.chars().collect(),
            position: 0,
            line: start_pos.line,
            column: start_pos.column,
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
        if ch != '\0' {
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

pub struct DopTokenizer {
    cursor: Cursor,
    current_token: (DopToken, Range),
}

impl DopTokenizer {
    pub fn new(input: &str, start_pos: Position) -> Result<Self, RangeError> {
        let mut tokenizer = DopTokenizer {
            cursor: Cursor::new(input, start_pos),
            current_token: (DopToken::Eof, Range::new(start_pos, start_pos)), // temporary value
        };
        tokenizer.advance()?;
        Ok(tokenizer)
    }

    pub fn peek(&self) -> &(DopToken, Range) {
        &self.current_token
    }

    pub fn advance(&mut self) -> Result<(DopToken, Range), RangeError> {
        while self.cursor.peek().is_whitespace() {
            self.cursor.advance();
        }

        let start_pos = self.cursor.get_position();

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
            '[' => {
                self.cursor.advance();
                DopToken::LeftBracket
            }
            ']' => {
                self.cursor.advance();
                DopToken::RightBracket
            }
            '{' => {
                self.cursor.advance();
                DopToken::LeftBrace
            }
            '}' => {
                self.cursor.advance();
                DopToken::RightBrace
            }
            ':' => {
                self.cursor.advance();
                DopToken::Colon
            }
            ',' => {
                self.cursor.advance();
                DopToken::Comma
            }
            '=' => {
                self.cursor.advance();
                if self.cursor.peek() == '=' {
                    self.cursor.advance();
                    DopToken::Equal
                } else {
                    let error_pos = self.cursor.get_position();
                    return Err(RangeError::new(
                        "Expected '==' but found single '='".to_string(),
                        Range::new(start_pos, error_pos),
                    ));
                }
            }
            '!' => {
                self.cursor.advance();
                DopToken::Not
            }
            '\'' => {
                let mut result = String::new();
                self.cursor.advance();
                while !matches!(self.cursor.peek(), '\'' | '\0') {
                    result.push(self.cursor.advance());
                }
                if self.cursor.peek() != '\'' {
                    return Err(RangeError::new(
                        "Unterminated string literal".to_string(),
                        Range::new(start_pos, self.cursor.get_position()),
                    ));
                }
                self.cursor.advance(); // consume '
                DopToken::StringLiteral(result)
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut identifier = String::new();

                while matches!(self.cursor.peek(), 'A'..='Z' | 'a'..='z' | '0'..='9' | '_') {
                    identifier.push(self.cursor.advance());
                }

                match identifier.as_str() {
                    "in" => DopToken::In,
                    "true" => DopToken::BooleanLiteral(true),
                    "false" => DopToken::BooleanLiteral(false),
                    "object" => DopToken::TypeObject,
                    // Type keywords
                    "string" => DopToken::TypeString,
                    "number" => DopToken::TypeNumber,
                    "boolean" => DopToken::TypeBoolean,
                    "void" => DopToken::TypeVoid,
                    "array" => DopToken::TypeArray,
                    _ => DopToken::Identifier(identifier),
                }
            }
            '0'..='9' => {
                let mut number_string = String::new();

                while matches!(self.cursor.peek(), '0'..='9') {
                    number_string.push(self.cursor.advance());
                }

                if self.cursor.peek() == '.' {
                    number_string.push(self.cursor.advance()); // consume '.'
                    if !matches!(self.cursor.peek(), '0'..='9') {
                        let error_pos = self.cursor.get_position();
                        return Err(RangeError::new(
                            "Expected digit after decimal point".to_string(),
                            Range::new(start_pos, error_pos),
                        ));
                    }
                    while matches!(self.cursor.peek(), '0'..='9') {
                        number_string.push(self.cursor.advance());
                    }
                }

                let number_value = serde_json::Number::from_str(&number_string).map_err(|_| {
                    let error_pos = self.cursor.get_position();
                    RangeError::new(
                        format!("Invalid number format: {}", number_string),
                        Range::new(start_pos, error_pos),
                    )
                })?;

                DopToken::NumberLiteral(number_value)
            }
            ch => {
                let error_pos = self.cursor.get_position();
                return Err(RangeError::new(
                    format!("Unexpected character: '{}'", ch),
                    Range::new(start_pos, error_pos),
                ));
            }
        };

        let end_pos = self.cursor.get_position();
        Ok(mem::replace(
            &mut self.current_token,
            (token, Range::new(start_pos, end_pos)),
        ))
    }
}

impl Iterator for DopTokenizer {
    type Item = Result<(DopToken, Range), RangeError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.advance() {
            Err(err) => Some(Err(err)),
            Ok((DopToken::Eof, _)) => None,
            Ok((t, range)) => Some(Ok((t, range))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::parse_test_cases;

    use std::fs;
    use std::path::PathBuf;

    fn tokenize_all(input: &str) -> String {
        let mut result = Vec::new();
        let mut tokenizer = match DopTokenizer::new(input, Position::new(1, 1)) {
            Ok(t) => t,
            Err(e) => return format!("ERROR: {:?}", e),
        };

        loop {
            let (token, range) = tokenizer.peek();

            let formatted = match token {
                DopToken::Identifier(s) => format!("Identifier({})", s),
                DopToken::StringLiteral(s) => format!("StringLiteral({})", s),
                DopToken::BooleanLiteral(b) => format!("BooleanLiteral({})", b),
                DopToken::NumberLiteral(n) => {
                    if let Some(i) = n.as_i64() {
                        format!("NumberLiteral({})", i)
                    } else if let Some(f) = n.as_f64() {
                        format!("NumberLiteral({})", f)
                    } else {
                        format!("NumberLiteral({:?})", n)
                    }
                }
                DopToken::Equal => "Equal".to_string(),
                DopToken::Not => "Not".to_string(),
                DopToken::Dot => "Dot".to_string(),
                DopToken::LeftParen => "LeftParen".to_string(),
                DopToken::RightParen => "RightParen".to_string(),
                DopToken::LeftBracket => "LeftBracket".to_string(),
                DopToken::RightBracket => "RightBracket".to_string(),
                DopToken::LeftBrace => "LeftBrace".to_string(),
                DopToken::RightBrace => "RightBrace".to_string(),
                DopToken::Colon => "Colon".to_string(),
                DopToken::Comma => "Comma".to_string(),
                DopToken::In => "In".to_string(),
                // Type tokens
                DopToken::TypeString => "TypeString".to_string(),
                DopToken::TypeNumber => "TypeNumber".to_string(),
                DopToken::TypeBoolean => "TypeBoolean".to_string(),
                DopToken::TypeVoid => "TypeVoid".to_string(),
                DopToken::TypeArray => "TypeArray".to_string(),
                DopToken::TypeObject => "TypeObject".to_string(),
                DopToken::Eof => "Eof".to_string(),
            };

            result.push(format!("{} {}", formatted, range));

            if *token == DopToken::Eof {
                break;
            }

            if tokenizer.advance().is_err() {
                break;
            }
        }

        result.join("\n")
    }

    #[test]
    fn test_tokenize() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/dop/tokenizer.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (archive, line_number)) in test_cases.iter().enumerate() {
            let input = &archive
                .get("in")
                .expect("Missing 'in' section in test case")
                .content;
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();

            println!("Test case {} (line {})", case_num + 1, line_number);

            let actual = tokenize_all(input);

            assert_eq!(
                actual,
                expected,
                "Mismatch in test case {} (line {})",
                case_num + 1,
                line_number
            );
        }
    }
}
