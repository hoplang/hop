use crate::common::{Position, Range, RangeError};
use std::{fmt, mem, str::FromStr};

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

impl fmt::Display for DopToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopToken::Identifier(name) => write!(f, "{}", name),
            DopToken::StringLiteral(s) => write!(f, "'{}'", s),
            DopToken::BooleanLiteral(b) => write!(f, "{}", b),
            DopToken::NumberLiteral(n) => write!(f, "{}", n),
            DopToken::Equal => write!(f, "=="),
            DopToken::Not => write!(f, "!"),
            DopToken::Dot => write!(f, "."),
            DopToken::LeftParen => write!(f, "("),
            DopToken::RightParen => write!(f, ")"),
            DopToken::LeftBracket => write!(f, "["),
            DopToken::RightBracket => write!(f, "]"),
            DopToken::LeftBrace => write!(f, "{{"),
            DopToken::RightBrace => write!(f, "}}"),
            DopToken::Colon => write!(f, ":"),
            DopToken::Comma => write!(f, ","),
            DopToken::In => write!(f, "in"),
            DopToken::TypeString => write!(f, "string"),
            DopToken::TypeNumber => write!(f, "number"),
            DopToken::TypeBoolean => write!(f, "boolean"),
            DopToken::TypeVoid => write!(f, "void"),
            DopToken::TypeArray => write!(f, "array"),
            DopToken::TypeObject => write!(f, "object"),
            DopToken::Eof => write!(f, "EOF"),
        }
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
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let tokenizer =
            DopTokenizer::new(input, Position::new(1, 1)).expect("Failed to create tokenizer");
        let result: Vec<_> = tokenizer.collect();
        let actual = format!("{:#?}", result);
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_tokenize_simple_punctuation() {
        check(
            "( ) . ! ==",
            expect![[r#"
                [
                    Ok(
                        (
                            LeftParen,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 2,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            RightParen,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 3,
                                },
                                end: Position {
                                    line: 1,
                                    column: 4,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Dot,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Not,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 7,
                                },
                                end: Position {
                                    line: 1,
                                    column: 8,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Equal,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 9,
                                },
                                end: Position {
                                    line: 1,
                                    column: 11,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_identifiers_keywords() {
        check(
            "foo in true false _test var123",
            expect![[r#"
                [
                    Ok(
                        (
                            Identifier(
                                "foo",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 4,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            In,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 7,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            BooleanLiteral(
                                true,
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                },
                                end: Position {
                                    line: 1,
                                    column: 12,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            BooleanLiteral(
                                false,
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 13,
                                },
                                end: Position {
                                    line: 1,
                                    column: 18,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "_test",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 19,
                                },
                                end: Position {
                                    line: 1,
                                    column: 24,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "var123",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 25,
                                },
                                end: Position {
                                    line: 1,
                                    column: 31,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_string_literals() {
        check(
            "'hello' 'world with spaces' ''",
            expect![[r#"
                [
                    Ok(
                        (
                            StringLiteral(
                                "hello",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 8,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            StringLiteral(
                                "world with spaces",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 9,
                                },
                                end: Position {
                                    line: 1,
                                    column: 28,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            StringLiteral(
                                "",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 29,
                                },
                                end: Position {
                                    line: 1,
                                    column: 31,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_numbers() {
        check(
            "123 456.789 0 0.5",
            expect![[r#"
                [
                    Ok(
                        (
                            NumberLiteral(
                                Number(123),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 4,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            NumberLiteral(
                                Number(456.789),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 12,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            NumberLiteral(
                                Number(0),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 13,
                                },
                                end: Position {
                                    line: 1,
                                    column: 14,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            NumberLiteral(
                                Number(0.5),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 15,
                                },
                                end: Position {
                                    line: 1,
                                    column: 18,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_multiline() {
        check(
            "foo\nbar",
            expect![[r#"
                [
                    Ok(
                        (
                            Identifier(
                                "foo",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 4,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "bar",
                            ),
                            Range {
                                start: Position {
                                    line: 2,
                                    column: 1,
                                },
                                end: Position {
                                    line: 2,
                                    column: 4,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_property_access() {
        check(
            "user.name",
            expect![[r#"
                [
                    Ok(
                        (
                            Identifier(
                                "user",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 5,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Dot,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "name",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 6,
                                },
                                end: Position {
                                    line: 1,
                                    column: 10,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_whitespace_handling() {
        check(
            "  foo   bar  ",
            expect![[r#"
                [
                    Ok(
                        (
                            Identifier(
                                "foo",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 3,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "bar",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 9,
                                },
                                end: Position {
                                    line: 1,
                                    column: 12,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_complex_expression() {
        check(
            "user.name == 'admin'",
            expect![[r#"
                [
                    Ok(
                        (
                            Identifier(
                                "user",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 5,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Dot,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "name",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 6,
                                },
                                end: Position {
                                    line: 1,
                                    column: 10,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Equal,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 11,
                                },
                                end: Position {
                                    line: 1,
                                    column: 13,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            StringLiteral(
                                "admin",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 14,
                                },
                                end: Position {
                                    line: 1,
                                    column: 21,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_parenthesized_expression() {
        check(
            "!(foo == true)",
            expect![[r#"
                [
                    Ok(
                        (
                            Not,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 2,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            LeftParen,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 3,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "foo",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 3,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Equal,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 7,
                                },
                                end: Position {
                                    line: 1,
                                    column: 9,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            BooleanLiteral(
                                true,
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 10,
                                },
                                end: Position {
                                    line: 1,
                                    column: 14,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            RightParen,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 14,
                                },
                                end: Position {
                                    line: 1,
                                    column: 15,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_array_brackets() {
        check(
            "[1, 2, 3]",
            expect![[r#"
                [
                    Ok(
                        (
                            LeftBracket,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 2,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            NumberLiteral(
                                Number(1),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 3,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Comma,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 3,
                                },
                                end: Position {
                                    line: 1,
                                    column: 4,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            NumberLiteral(
                                Number(2),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 5,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Comma,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 6,
                                },
                                end: Position {
                                    line: 1,
                                    column: 7,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            NumberLiteral(
                                Number(3),
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                },
                                end: Position {
                                    line: 1,
                                    column: 9,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            RightBracket,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 9,
                                },
                                end: Position {
                                    line: 1,
                                    column: 10,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_empty_array() {
        check(
            "[]",
            expect![[r#"
                [
                    Ok(
                        (
                            LeftBracket,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 2,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            RightBracket,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 3,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_object_keyword() {
        check(
            "object",
            expect![[r#"
                [
                    Ok(
                        (
                            TypeObject,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 7,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }

    #[test]
    fn test_tokenize_object_literal_syntax() {
        check(
            "{name: 'John'}",
            expect![[r#"
                [
                    Ok(
                        (
                            LeftBrace,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 1,
                                },
                                end: Position {
                                    line: 1,
                                    column: 2,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Identifier(
                                "name",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 2,
                                },
                                end: Position {
                                    line: 1,
                                    column: 6,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            Colon,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 6,
                                },
                                end: Position {
                                    line: 1,
                                    column: 7,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            StringLiteral(
                                "John",
                            ),
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 8,
                                },
                                end: Position {
                                    line: 1,
                                    column: 14,
                                },
                            },
                        ),
                    ),
                    Ok(
                        (
                            RightBrace,
                            Range {
                                start: Position {
                                    line: 1,
                                    column: 14,
                                },
                                end: Position {
                                    line: 1,
                                    column: 15,
                                },
                            },
                        ),
                    ),
                ]"#]],
        );
    }
}
