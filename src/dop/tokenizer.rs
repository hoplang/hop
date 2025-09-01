use crate::common::{Position, Range};
use std::{fmt, iter::Peekable, str::Chars, str::FromStr};

use super::parser::ParseError;

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
        }
    }
}

struct Cursor<'a> {
    chars: Peekable<Chars<'a>>,
    position: Position,
}

impl<'a> Cursor<'a> {
    fn new(input: &'a str, start_pos: Position) -> Self {
        Self {
            chars: input.chars().peekable(),
            position: start_pos,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            None => None,
            Some(ch) => {
                let byte_len = ch.len_utf8();
                if ch == '\n' {
                    self.position.line += 1;
                    self.position.column = 1;
                } else {
                    self.position.column += byte_len;
                }
                Some(ch)
            }
        }
    }

    fn get_position(&self) -> Position {
        Position::new(self.position.line, self.position.column)
    }
}

pub struct DopTokenizer<'a> {
    cursor: Cursor<'a>,
}

impl<'a> DopTokenizer<'a> {
    pub fn new(input: &'a str, start_pos: Position) -> Self {
        Self {
            cursor: Cursor::new(input, start_pos),
        }
    }
}

impl Iterator for DopTokenizer<'_> {
    type Item = Result<(DopToken, Range), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cursor.peek().is_some_and(|ch| ch.is_whitespace()) {
            self.cursor.advance();
        }

        let start_pos = self.cursor.get_position();

        let token = match self.cursor.peek()? {
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
                if self.cursor.peek().is_some_and(|ch| *ch == '=') {
                    self.cursor.advance();
                    DopToken::Equal
                } else {
                    let error_pos = self.cursor.get_position();
                    return Some(Err(ParseError::new(
                        "Expected '==' but found single '='".to_string(),
                        Range::new(start_pos, error_pos),
                    )));
                }
            }
            '!' => {
                self.cursor.advance();
                DopToken::Not
            }
            '\'' => {
                let mut result = String::new();
                self.cursor.advance();
                while self.cursor.peek().is_some_and(|ch| *ch != '\'') {
                    result.push(self.cursor.advance()?);
                }
                if self.cursor.peek().is_none() {
                    return Some(Err(ParseError::new(
                        "Unterminated string literal".to_string(),
                        Range::new(start_pos, self.cursor.get_position()),
                    )));
                }
                self.cursor.advance(); // consume '
                DopToken::StringLiteral(result)
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut identifier = String::new();

                while matches!(
                    self.cursor.peek(),
                    Some('A'..='Z' | 'a'..='z' | '0'..='9' | '_')
                ) {
                    identifier.push(self.cursor.advance()?);
                }

                match identifier.as_str() {
                    "in" => DopToken::In,
                    "true" => DopToken::BooleanLiteral(true),
                    "false" => DopToken::BooleanLiteral(false),
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

                while matches!(self.cursor.peek(), Some('0'..='9')) {
                    number_string.push(self.cursor.advance()?);
                }

                if self.cursor.peek().is_some_and(|ch| *ch == '.') {
                    number_string.push(self.cursor.advance()?); // consume '.'
                    if !matches!(self.cursor.peek(), Some('0'..='9')) {
                        let error_pos = self.cursor.get_position();
                        return Some(Err(ParseError::new(
                            "Expected digit after decimal point".to_string(),
                            Range::new(start_pos, error_pos),
                        )));
                    }
                    while matches!(self.cursor.peek(), Some('0'..='9')) {
                        number_string.push(self.cursor.advance()?);
                    }
                }

                let number_value = match serde_json::Number::from_str(&number_string) {
                    Ok(n) => n,
                    Err(_) => {
                        let error_pos = self.cursor.get_position();
                        return Some(Err(ParseError::new(
                            format!("Invalid number format: {}", number_string),
                            Range::new(start_pos, error_pos),
                        )));
                    }
                };

                DopToken::NumberLiteral(number_value)
            }
            ch => {
                let result = *ch;
                self.cursor.advance();
                return Some(Err(ParseError::new(
                    format!("Unexpected character: '{}'", result),
                    Range::new(start_pos, self.cursor.get_position()),
                )));
            }
        };

        let end_pos = self.cursor.get_position();
        Some(Ok((token, Range::new(start_pos, end_pos))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let tokenizer = DopTokenizer::new(input, Position::new(1, 1));
        let mut actual = tokenizer
            .map(|t| match t {
                Ok((tok, range)) => {
                    format!("{:<10} {}", range.to_string(), tok)
                }
                Err(ParseError::RangeError { message, range }) => {
                    format!("{:<10} {}", range.to_string(), message)
                }
                Err(_) => {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>()
            .join("\n");
        actual.push('\n');
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_tokenize_unexpected_character() {
        check(
            "~",
            expect![[r#"
                1:1-1:2    Unexpected character: '~'
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unexpected_characters() {
        check(
            "~~ = f __ ~@#!",
            expect![[r#"
                1:1-1:2    Unexpected character: '~'
                1:2-1:3    Unexpected character: '~'
                1:4-1:5    Expected '==' but found single '='
                1:6-1:7    f
                1:8-1:10   __
                1:11-1:12  Unexpected character: '~'
                1:12-1:13  Unexpected character: '@'
                1:13-1:14  Unexpected character: '#'
                1:14-1:15  !
            "#]],
        );
    }

    #[test]
    fn test_tokenize_simple_punctuation() {
        check(
            "( ) . ! ==",
            expect![[r#"
                1:1-1:2    (
                1:3-1:4    )
                1:5-1:6    .
                1:7-1:8    !
                1:9-1:11   ==
            "#]],
        );
    }

    #[test]
    fn test_tokenize_identifiers_keywords() {
        check(
            "foo in true false _test var123",
            expect![[r#"
                1:1-1:4    foo
                1:5-1:7    in
                1:8-1:12   true
                1:13-1:18  false
                1:19-1:24  _test
                1:25-1:31  var123
            "#]],
        );
    }

    #[test]
    fn test_tokenize_string_literals() {
        check(
            "'hello' 'world with spaces' ''",
            expect![[r#"
                1:1-1:8    'hello'
                1:9-1:28   'world with spaces'
                1:29-1:31  ''
            "#]],
        );
    }

    #[test]
    fn test_tokenize_numbers() {
        check(
            "123 456.789 0 0.5",
            expect![[r#"
                1:1-1:4    123
                1:5-1:12   456.789
                1:13-1:14  0
                1:15-1:18  0.5
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiline() {
        check(
            "foo\nbar",
            expect![[r#"
                1:1-1:4    foo
                2:1-2:4    bar
            "#]],
        );
    }

    #[test]
    fn test_tokenize_property_access() {
        check(
            "user.name",
            expect![[r#"
                1:1-1:5    user
                1:5-1:6    .
                1:6-1:10   name
            "#]],
        );
    }

    #[test]
    fn test_tokenize_whitespace_handling() {
        check(
            "  foo   bar  ",
            expect![[r#"
                1:3-1:6    foo
                1:9-1:12   bar
            "#]],
        );
    }

    #[test]
    fn test_tokenize_complex_expression() {
        check(
            "user.name == 'admin'",
            expect![[r#"
                1:1-1:5    user
                1:5-1:6    .
                1:6-1:10   name
                1:11-1:13  ==
                1:14-1:21  'admin'
            "#]],
        );
    }

    #[test]
    fn test_tokenize_parenthesized_expression() {
        check(
            "!(foo == true)",
            expect![[r#"
                1:1-1:2    !
                1:2-1:3    (
                1:3-1:6    foo
                1:7-1:9    ==
                1:10-1:14  true
                1:14-1:15  )
            "#]],
        );
    }

    #[test]
    fn test_tokenize_array_brackets() {
        check(
            "[1, 2, 3]",
            expect![[r#"
                1:1-1:2    [
                1:2-1:3    1
                1:3-1:4    ,
                1:5-1:6    2
                1:6-1:7    ,
                1:8-1:9    3
                1:9-1:10   ]
            "#]],
        );
    }

    #[test]
    fn test_tokenize_empty_array() {
        check(
            "[]",
            expect![[r#"
                1:1-1:2    [
                1:2-1:3    ]
            "#]],
        );
    }

    #[test]
    fn test_tokenize_object_literal_syntax() {
        check(
            "{name: 'John'}",
            expect![[r#"
                1:1-1:2    {
                1:2-1:6    name
                1:6-1:7    :
                1:8-1:14   'John'
                1:14-1:15  }
            "#]],
        );
    }
}
