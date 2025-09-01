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
    chars: Chars<'a>,
    position: Position,
}

impl<'a> Cursor<'a> {
    fn new(input: &'a str, start_pos: Position) -> Self {
        Self {
            chars: input.chars(),
            position: start_pos,
        }
    }
}

impl Iterator for Cursor<'_> {
    type Item = (char, Range);
    fn next(&mut self) -> Option<Self::Item> {
        let was = self.position;
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
                Some((ch, Range::new(was, self.position)))
            }
        }
    }
}

pub struct DopTokenizer<'a> {
    cursor: Peekable<Cursor<'a>>,
}

impl<'a> DopTokenizer<'a> {
    pub fn new(input: &'a str, start_pos: Position) -> Self {
        Self {
            cursor: Cursor::new(input, start_pos).peekable(),
        }
    }
}

impl Iterator for DopTokenizer<'_> {
    type Item = Result<(DopToken, Range), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cursor.peek().is_some_and(|(ch, _)| ch.is_whitespace()) {
            self.cursor.next();
        }

        match self.cursor.next()? {
            ('.', range) => Some(Ok((DopToken::Dot, range))),
            ('(', range) => Some(Ok((DopToken::LeftParen, range))),
            (')', range) => Some(Ok((DopToken::RightParen, range))),
            ('[', range) => Some(Ok((DopToken::LeftBracket, range))),
            (']', range) => Some(Ok((DopToken::RightBracket, range))),
            ('{', range) => Some(Ok((DopToken::LeftBrace, range))),
            ('}', range) => Some(Ok((DopToken::RightBrace, range))),
            (':', range) => Some(Ok((DopToken::Colon, range))),
            (',', range) => Some(Ok((DopToken::Comma, range))),
            ('!', range) => Some(Ok((DopToken::Not, range))),
            ('=', start_range) => {
                if let Some((_, end_range)) = self.cursor.next_if(|(ch, _)| *ch == '=') {
                    Some(Ok((
                        DopToken::Equal,
                        Range::new(start_range.start, end_range.end),
                    )))
                } else {
                    Some(Err(ParseError::new(
                        "Expected '==' but found single '='".to_string(),
                        start_range,
                    )))
                }
            }
            ('\'', start_range) => {
                let mut result = String::new();
                let mut end_range = start_range;
                loop {
                    match self.cursor.next() {
                        Some(('\'', end_range)) => {
                            return Some(Ok((
                                DopToken::StringLiteral(result),
                                Range::new(start_range.start, end_range.end),
                            )));
                        }
                        Some((ch, range)) => {
                            end_range = range;
                            result.push(ch);
                        }
                        None => {
                            return Some(Err(ParseError::new(
                                "Unterminated string literal".to_string(),
                                Range::new(start_range.start, end_range.end),
                            )));
                        }
                    }
                }
            }
            (ch, start_range) if matches!(ch, 'A'..='Z' | 'a'..='z' | '_') => {
                let mut identifier = String::new();

                identifier.push(ch);

                let mut end_range = start_range;

                while let Some((ch, range)) = self
                    .cursor
                    .next_if(|(ch, _)| matches!(ch, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_'))
                {
                    identifier.push(ch);
                    end_range = range;
                }

                let t = match identifier.as_str() {
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
                };
                Some(Ok((t, Range::new(start_range.start, end_range.end))))
            }
            (ch, start_range) if ch.is_ascii_digit() => {
                let mut number_string = String::new();

                number_string.push(ch);

                let mut end_range = start_range;

                while let Some((ch, range)) = self.cursor.next_if(|(ch, _)| ch.is_ascii_digit()) {
                    number_string.push(ch);
                    end_range = range;
                }

                if let Some((_, range)) = self.cursor.next_if(|(ch, _)| *ch == '.') {
                    number_string.push('.');
                    end_range = range;
                    match self.cursor.peek() {
                        None => {
                            return Some(Err(ParseError::new(
                                "Expected digit after decimal point".to_string(),
                                Range::new(start_range.start, end_range.end),
                            )));
                        }
                        Some((ch, _)) if ch.is_ascii_digit() => {
                            while let Some((ch, range)) =
                                self.cursor.next_if(|(ch, _)| ch.is_ascii_digit())
                            {
                                number_string.push(ch);
                                end_range = range;
                            }
                        }
                        Some(_) => {
                            return Some(Err(ParseError::new(
                                "Expected digit after decimal point".to_string(),
                                Range::new(start_range.start, end_range.end),
                            )));
                        }
                    }
                }

                let range = Range::new(start_range.start, end_range.end);
                let parse_result = serde_json::Number::from_str(&number_string);

                match parse_result {
                    Ok(n) => Some(Ok((DopToken::NumberLiteral(n), range))),
                    Err(_) => Some(Err(ParseError::new(
                        format!("Invalid number format: {}", number_string),
                        range,
                    ))),
                }
            }
            (ch, range) => Some(Err(ParseError::new(
                format!("Unexpected character: '{}'", ch),
                range,
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tui::source_annotator::{SimpleAnnotation, SourceAnnotator};

    use super::*;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let tokenizer = DopTokenizer::new(input, Position::default());
        let mut annotations = Vec::new();
        for t in tokenizer {
            match t {
                Ok((tok, range)) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("token: {}", tok),
                        range,
                    });
                }
                Err(ParseError::RangeError { message, range }) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("error: {}", message),
                        range,
                    });
                }
                Err(_) => {
                    unreachable!()
                }
            }
        }
        expected.assert_eq(&SourceAnnotator::new().without_line_numbers().annotate(
            None,
            input,
            &annotations,
        ));
    }

    #[test]
    fn test_tokenize_unexpected_character() {
        check(
            "~",
            expect![[r#"
                error: Unexpected character: '~'
                ~
                ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unexpected_characters() {
        check(
            "~~ = f __ ~@#!",
            expect![[r#"
                error: Unexpected character: '~'
                ~~ = f __ ~@#!
                ^

                error: Unexpected character: '~'
                ~~ = f __ ~@#!
                 ^

                error: Expected '==' but found single '='
                ~~ = f __ ~@#!
                   ^

                token: f
                ~~ = f __ ~@#!
                     ^

                token: __
                ~~ = f __ ~@#!
                       ^^

                error: Unexpected character: '~'
                ~~ = f __ ~@#!
                          ^

                error: Unexpected character: '@'
                ~~ = f __ ~@#!
                           ^

                error: Unexpected character: '#'
                ~~ = f __ ~@#!
                            ^

                token: !
                ~~ = f __ ~@#!
                             ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_invalid_numbers() {
        check(
            "1. 1000. 1.",
            expect![[r#"
                error: Expected digit after decimal point
                1. 1000. 1.
                ^^

                error: Expected digit after decimal point
                1. 1000. 1.
                   ^^^^^

                error: Expected digit after decimal point
                1. 1000. 1.
                         ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_simple_punctuation() {
        check(
            "( ) . ! ==",
            expect![[r#"
                token: (
                ( ) . ! ==
                ^

                token: )
                ( ) . ! ==
                  ^

                token: .
                ( ) . ! ==
                    ^

                token: !
                ( ) . ! ==
                      ^

                token: ==
                ( ) . ! ==
                        ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_identifiers_keywords() {
        check(
            "foo in true false _test var123",
            expect![[r#"
                token: foo
                foo in true false _test var123
                ^^^

                token: in
                foo in true false _test var123
                    ^^

                token: true
                foo in true false _test var123
                       ^^^^

                token: false
                foo in true false _test var123
                            ^^^^^

                token: _test
                foo in true false _test var123
                                  ^^^^^

                token: var123
                foo in true false _test var123
                                        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_string_literals() {
        check(
            "'hello' 'world with spaces' ''",
            expect![[r#"
                token: 'hello'
                'hello' 'world with spaces' ''
                ^^^^^^^

                token: 'world with spaces'
                'hello' 'world with spaces' ''
                        ^^^^^^^^^^^^^^^^^^^

                token: ''
                'hello' 'world with spaces' ''
                                            ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_numbers() {
        check(
            "123 456.789 0 0.5",
            expect![[r#"
                token: 123
                123 456.789 0 0.5
                ^^^

                token: 456.789
                123 456.789 0 0.5
                    ^^^^^^^

                token: 0
                123 456.789 0 0.5
                            ^

                token: 0.5
                123 456.789 0 0.5
                              ^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiline() {
        check(
            "foo\nbar",
            expect![[r#"
                token: foo
                foo
                ^^^

                token: bar
                bar
                ^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_property_access() {
        check(
            "user.name",
            expect![[r#"
                token: user
                user.name
                ^^^^

                token: .
                user.name
                    ^

                token: name
                user.name
                     ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_whitespace_handling() {
        check(
            "  foo   bar  ",
            expect![[r#"
                token: foo
                  foo   bar  
                  ^^^

                token: bar
                  foo   bar  
                        ^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_complex_expression() {
        check(
            "user.name == 'admin'",
            expect![[r#"
                token: user
                user.name == 'admin'
                ^^^^

                token: .
                user.name == 'admin'
                    ^

                token: name
                user.name == 'admin'
                     ^^^^

                token: ==
                user.name == 'admin'
                          ^^

                token: 'admin'
                user.name == 'admin'
                             ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_parenthesized_expression() {
        check(
            "!(foo == true)",
            expect![[r#"
                token: !
                !(foo == true)
                ^

                token: (
                !(foo == true)
                 ^

                token: foo
                !(foo == true)
                  ^^^

                token: ==
                !(foo == true)
                      ^^

                token: true
                !(foo == true)
                         ^^^^

                token: )
                !(foo == true)
                             ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_array_brackets() {
        check(
            "[1, 2, 3]",
            expect![[r#"
                token: [
                [1, 2, 3]
                ^

                token: 1
                [1, 2, 3]
                 ^

                token: ,
                [1, 2, 3]
                  ^

                token: 2
                [1, 2, 3]
                    ^

                token: ,
                [1, 2, 3]
                     ^

                token: 3
                [1, 2, 3]
                       ^

                token: ]
                [1, 2, 3]
                        ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_number_array() {
        check(
            "[1.0, 12.0, 343.0]",
            expect![[r#"
                token: [
                [1.0, 12.0, 343.0]
                ^

                token: 1.0
                [1.0, 12.0, 343.0]
                 ^^^

                token: ,
                [1.0, 12.0, 343.0]
                    ^

                token: 12.0
                [1.0, 12.0, 343.0]
                      ^^^^

                token: ,
                [1.0, 12.0, 343.0]
                          ^

                token: 343.0
                [1.0, 12.0, 343.0]
                            ^^^^^

                token: ]
                [1.0, 12.0, 343.0]
                                 ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_empty_array() {
        check(
            "[]",
            expect![[r#"
                token: [
                []
                ^

                token: ]
                []
                 ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_object_literal_syntax() {
        check(
            "{name: 'John'}",
            expect![[r#"
                token: {
                {name: 'John'}
                ^

                token: name
                {name: 'John'}
                 ^^^^

                token: :
                {name: 'John'}
                     ^

                token: 'John'
                {name: 'John'}
                       ^^^^^^

                token: }
                {name: 'John'}
                             ^
            "#]],
        );
    }
}
