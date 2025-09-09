use itertools::Itertools as _;

use crate::span::string_cursor::{StringCursor, StringSpan};
use std::{fmt, iter::Peekable, str::FromStr};

use super::parse_error::ParseError;

pub struct DopTokenizer {
    iter: Peekable<StringCursor>,
}

impl From<StringCursor> for DopTokenizer {
    fn from(iter: StringCursor) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }
}

impl From<Peekable<StringCursor>> for DopTokenizer {
    fn from(iter: Peekable<StringCursor>) -> Self {
        Self { iter }
    }
}

impl From<String> for DopTokenizer {
    fn from(input: String) -> Self {
        Self {
            iter: StringCursor::new(input).peekable(),
        }
    }
}

impl From<&str> for DopTokenizer {
    fn from(input: &str) -> Self {
        Self::from(input.to_string())
    }
}

#[derive(Debug, Clone)]
pub enum DopToken {
    Identifier(StringSpan),
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

impl PartialEq for DopToken {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DopToken::Identifier(a), DopToken::Identifier(b)) => a.as_str() == b.as_str(),
            (DopToken::StringLiteral(a), DopToken::StringLiteral(b)) => a == b,
            (DopToken::BooleanLiteral(a), DopToken::BooleanLiteral(b)) => a == b,
            (DopToken::NumberLiteral(a), DopToken::NumberLiteral(b)) => a == b,
            (DopToken::Equal, DopToken::Equal) => true,
            (DopToken::Not, DopToken::Not) => true,
            (DopToken::Dot, DopToken::Dot) => true,
            (DopToken::LeftParen, DopToken::LeftParen) => true,
            (DopToken::RightParen, DopToken::RightParen) => true,
            (DopToken::LeftBracket, DopToken::LeftBracket) => true,
            (DopToken::RightBracket, DopToken::RightBracket) => true,
            (DopToken::LeftBrace, DopToken::LeftBrace) => true,
            (DopToken::RightBrace, DopToken::RightBrace) => true,
            (DopToken::Colon, DopToken::Colon) => true,
            (DopToken::Comma, DopToken::Comma) => true,
            (DopToken::In, DopToken::In) => true,
            (DopToken::TypeString, DopToken::TypeString) => true,
            (DopToken::TypeNumber, DopToken::TypeNumber) => true,
            (DopToken::TypeBoolean, DopToken::TypeBoolean) => true,
            (DopToken::TypeVoid, DopToken::TypeVoid) => true,
            (DopToken::TypeArray, DopToken::TypeArray) => true,
            _ => false,
        }
    }
}

impl DopToken {
    /// Returns the matching closing token for opening tokens like LeftBrace, LeftBracket, LeftParen.
    /// Returns None for tokens that don't have a matching pair.
    pub fn matching_closing_token(&self) -> DopToken {
        match self {
            DopToken::LeftBrace => DopToken::RightBrace,
            DopToken::LeftBracket => DopToken::RightBracket,
            DopToken::LeftParen => DopToken::RightParen,
            _ => {
                panic!("matching_closing_token called on {}", self)
            }
        }
    }
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

impl Iterator for DopTokenizer {
    type Item = Result<(DopToken, StringSpan), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            self.iter.next();
        }

        self.iter.next().map(|start| {
            match start.ch() {
                '.' => Ok((DopToken::Dot, start)),
                '(' => Ok((DopToken::LeftParen, start)),
                ')' => Ok((DopToken::RightParen, start)),
                '[' => Ok((DopToken::LeftBracket, start)),
                ']' => Ok((DopToken::RightBracket, start)),
                '{' => Ok((DopToken::LeftBrace, start)),
                '}' => Ok((DopToken::RightBrace, start)),
                ':' => Ok((DopToken::Colon, start)),
                ',' => Ok((DopToken::Comma, start)),
                '!' => Ok((DopToken::Not, start)),
                '=' => {
                    let Some(end) = self.iter.next_if(|s| s.ch() == '=') else {
                        return Err(ParseError::ExpectedDoubleEqButGotSingleEq {
                            span: start.clone(),
                        });
                    };
                    Ok((DopToken::Equal, start.to(end)))
                }
                '\'' => {
                    let mut end_span = start.clone();
                    let mut result = String::new();
                    while let Some(s) = self.iter.next_if(|s| s.ch() != '\'') {
                        result.push(s.ch());
                        end_span = s;
                    }
                    match self.iter.next() {
                        None => Err(ParseError::UnterminatedStringLiteral {
                            span: start.to(end_span),
                        }),
                        Some(end) => Ok((DopToken::StringLiteral(result), start.to(end))),
                    }
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let identifier = start.extend(self.iter.peeking_take_while(
                        |s| matches!(s.ch(), 'A'..='Z' | 'a'..='z' | '0'..='9' | '_'),
                    ));

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
                        _ => DopToken::Identifier(identifier.clone()),
                    };
                    Ok((t, identifier))
                }
                ch if ch.is_ascii_digit() => {
                    let mut number_string =
                        start.extend(self.iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
                    if let Some(dot) = self.iter.next_if(|s| s.ch() == '.') {
                        number_string = number_string.to(dot);
                        number_string = number_string
                            .extend(self.iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
                    }
                    match serde_json::Number::from_str(number_string.as_str()) {
                        Ok(n) => Ok((DopToken::NumberLiteral(n), number_string)),
                        Err(_) => Err(ParseError::InvalidNumberFormat {
                            span: number_string,
                        }),
                    }
                }
                ch => Err(ParseError::UnexpectedCharacter { ch, span: start }),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::span::{SimpleAnnotation, SourceAnnotator, string_cursor::Spanned as _};

    use super::*;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let tokenizer = DopTokenizer::from(input);
        let mut annotations = Vec::new();
        for t in tokenizer {
            match t {
                Ok((tok, span)) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("token: {}", tok),
                        span,
                    });
                }
                Err(err) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("error: {err}"),
                        span: err.span().clone(),
                    });
                }
            }
        }
        expected.assert_eq(
            &SourceAnnotator::new()
                .without_line_numbers()
                .annotate(None, &annotations),
        );
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
    fn test_tokenize_eq_does_not_eat_identifier() {
        check(
            "=foo",
            expect![[r#"
                error: Expected '==' but got '='
                =foo
                ^

                token: foo
                =foo
                 ^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unexpected_characters() {
        check(
            "~ ~ ~@ #",
            expect![[r#"
                error: Unexpected character: '~'
                ~ ~ ~@ #
                ^

                error: Unexpected character: '~'
                ~ ~ ~@ #
                  ^

                error: Unexpected character: '~'
                ~ ~ ~@ #
                    ^

                error: Unexpected character: '@'
                ~ ~ ~@ #
                     ^

                error: Unexpected character: '#'
                ~ ~ ~@ #
                       ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_valid_numbers() {
        check(
            "1.0 0.0 0.0000 1000000 0.0000 0.1010",
            expect![[r#"
                token: 1.0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                ^^^

                token: 0.0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                    ^^^

                token: 0.0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                        ^^^^^^

                token: 1000000
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                               ^^^^^^^

                token: 0.0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                                       ^^^^^^

                token: 0.101
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                                              ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_invalid_numbers() {
        check(
            "1. 1000. 1. 000 0. 0123 01010",
            expect![[r#"
                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                ^^

                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                   ^^^^^

                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                         ^^

                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                            ^^^

                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                                ^^

                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                                   ^^^^

                error: Invalid number format
                1. 1000. 1. 000 0. 0123 01010
                                        ^^^^^
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
