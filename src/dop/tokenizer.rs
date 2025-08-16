use crate::common::{Position, Range};
use std::str::FromStr;

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
    In,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangedDopToken {
    pub token: DopToken,
    pub range: Range,
}

impl RangedDopToken {
    pub fn new(token: DopToken, range: Range) -> Self {
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
    current_token: RangedDopToken,
}

impl DopTokenizer {
    pub fn new(input: &str) -> Result<Self, String> {
        let mut tokenizer = DopTokenizer {
            cursor: Cursor::new(input),
            current_token: RangedDopToken::new(
                DopToken::Eof,
                Range::new(Position::new(1, 1), Position::new(1, 1)),
            ), // temporary value
        };
        tokenizer.advance()?;
        Ok(tokenizer)
    }

    pub fn peek(&self) -> &RangedDopToken {
        &self.current_token
    }

    pub fn advance(&mut self) -> Result<(), String> {
        // Skip whitespace first
        while self.cursor.peek().is_whitespace() {
            self.cursor.advance();
        }

        // Now get the start position after skipping whitespace
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
                let mut result = String::new();
                self.cursor.advance();
                while self.cursor.peek() != '\'' && self.cursor.peek() != '\0' {
                    result.push(self.cursor.advance());
                }
                if self.cursor.peek() != '\'' {
                    return Err("Unterminated string literal".to_string());
                }
                self.cursor.advance(); // consume '
                DopToken::StringLiteral(result)
            }
            ch if ch.is_ascii_alphabetic() || ch == '_' => {
                let mut identifier = String::new();

                identifier.push(self.cursor.advance());

                // Subsequent characters can be letters, digits, or underscores
                while self.cursor.peek().is_ascii_alphanumeric() || self.cursor.peek() == '_' {
                    identifier.push(self.cursor.advance());
                }

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
                let mut number_string = String::new();

                // Read integer part
                while self.cursor.peek().is_ascii_digit() {
                    number_string.push(self.cursor.advance());
                }

                // Read decimal part if present
                if self.cursor.peek() == '.' {
                    number_string.push(self.cursor.advance()); // consume '.'
                    if !self.cursor.peek().is_ascii_digit() {
                        return Err("Expected digit after decimal point".to_string());
                    }
                    while self.cursor.peek().is_ascii_digit() {
                        number_string.push(self.cursor.advance());
                    }
                }

                let number_value = serde_json::Number::from_str(&number_string)
                    .map_err(|_| format!("Invalid number format: {}", number_string))?;

                DopToken::NumberLiteral(number_value)
            }
            ch => return Err(format!("Unexpected character: '{}'", ch)),
        };

        let end_pos = self.cursor.get_position();
        self.current_token = RangedDopToken::new(token, Range::new(start_pos, end_pos));
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    fn tokenize_all(input: &str) -> String {
        let mut result = Vec::new();
        let mut tokenizer = match DopTokenizer::new(input) {
            Ok(t) => t,
            Err(e) => return format!("ERROR: {}", e),
        };

        loop {
            let token = tokenizer.peek();
            let range_str = format!(
                "{}:{}-{}:{}",
                token.range.start.line, token.range.start.column, token.range.end.line, token.range.end.column
            );

            let formatted = match &token.token {
                DopToken::Identifier(s) => format!("Identifier(\"{}\") {}", s, range_str),
                DopToken::StringLiteral(s) => format!("StringLiteral(\"{}\") {}", s, range_str),
                DopToken::BooleanLiteral(b) => format!("BooleanLiteral({}) {}", b, range_str),
                DopToken::NumberLiteral(n) => {
                    if let Some(i) = n.as_i64() {
                        format!("NumberLiteral({}) {}", i, range_str)
                    } else if let Some(f) = n.as_f64() {
                        format!("NumberLiteral({}) {}", f, range_str)
                    } else {
                        format!("NumberLiteral({:?}) {}", n, range_str)
                    }
                }
                DopToken::Equal => format!("Equal {}", range_str),
                DopToken::Not => format!("Not {}", range_str),
                DopToken::Dot => format!("Dot {}", range_str),
                DopToken::LeftParen => format!("LeftParen {}", range_str),
                DopToken::RightParen => format!("RightParen {}", range_str),
                DopToken::In => format!("In {}", range_str),
                DopToken::Eof => format!("Eof {}", range_str),
            };

            result.push(formatted);

            if matches!(token.token, DopToken::Eof) {
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

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

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
