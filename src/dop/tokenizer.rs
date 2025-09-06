use crate::range::{
    Ranged,
    string_cursor::{StringCursor, StringSpan},
};
use std::{fmt, iter::Peekable, str::FromStr};

use super::errors::ParseError;

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

impl From<&str> for DopTokenizer {
    fn from(input: &str) -> Self {
        Self {
            iter: StringCursor::new(input).peekable(),
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
                    if let Some(end) = self.iter.next_if(|s| s.ch() == '=') {
                        return Ok((DopToken::Equal, start.to(end)));
                    }
                    Err(ParseError::new(
                        "Expected '==' but found single '='".to_string(),
                        start.clone(),
                    ))
                }
                '\'' => {
                    let mut end_span = start.clone();
                    let mut result = String::new();
                    while let Some(s) = self.iter.next_if(|s| s.ch() != '\'') {
                        result.push(s.ch());
                        end_span = s;
                    }
                    match self.iter.next() {
                        None => Err(ParseError::unterminated_string_literal(
                            start.to(end_span),
                        )),
                        Some(end) => Ok((DopToken::StringLiteral(result), start.to(end))),
                    }
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let mut identifier = start;

                    while let Some(s) = self
                        .iter
                        .next_if(|s| matches!(s.ch(), 'A'..='Z' | 'a'..='z' | '0'..='9' | '_'))
                    {
                        identifier = identifier.to(s);
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
                        _ => DopToken::Identifier(identifier.clone()),
                    };
                    Ok((t, identifier))
                }
                ch if ch.is_ascii_digit() => {
                    let mut number_string = start;
                    while let Some(digit) = self.iter.next_if(|s| s.ch().is_ascii_digit()) {
                        number_string = number_string.to(digit);
                    }
                    if let Some(dot) = self.iter.next_if(|s| s.ch() == '.') {
                        number_string = number_string.to(dot);
                        if !self.iter.peek().is_some_and(|s| s.ch().is_ascii_digit()) {
                            return Err(ParseError::expected_digit_after_decimal_point(
                                number_string.clone(),
                            ));
                        }
                        while let Some(digit) = self.iter.next_if(|s| s.ch().is_ascii_digit()) {
                            number_string = number_string.to(digit);
                        }
                    }
                    match serde_json::Number::from_str(number_string.as_str()) {
                        Ok(n) => Ok((DopToken::NumberLiteral(n), number_string)),
                        Err(_) => Err(ParseError::new(
                            format!("Invalid number format: {}", number_string.as_str()),
                            number_string.clone(),
                        )),
                    }
                }
                ch => Err(ParseError::new(
                    format!("Unexpected character: '{}'", ch),
                    start.clone(),
                )),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::range::{SimpleAnnotation, SourceAnnotator};

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
                        range: span.range(),
                    });
                }
                Err(ParseError::Ranged { message, span }) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("error: {}", message),
                        range: span.range(),
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
    fn test_tokenize_eq_does_not_eat_identifier() {
        check(
            "=foo",
            expect![[r#"
                error: Expected '==' but found single '='
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
                error: Expected digit after decimal point
                1. 1000. 1. 000 0. 0123 01010
                ^^

                error: Expected digit after decimal point
                1. 1000. 1. 000 0. 0123 01010
                   ^^^^^

                error: Expected digit after decimal point
                1. 1000. 1. 000 0. 0123 01010
                         ^^

                error: Invalid number format: 000
                1. 1000. 1. 000 0. 0123 01010
                            ^^^

                error: Expected digit after decimal point
                1. 1000. 1. 000 0. 0123 01010
                                ^^

                error: Invalid number format: 0123
                1. 1000. 1. 000 0. 0123 01010
                                   ^^^^

                error: Invalid number format: 01010
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
