use itertools::Itertools as _;

use crate::document::document_cursor::{DocumentCursor, DocumentRange};
use std::{iter::Peekable, str::FromStr};

use super::{parse_error::ParseError, token::Token};

pub struct Tokenizer {
    iter: Peekable<DocumentCursor>,
}

impl From<DocumentCursor> for Tokenizer {
    fn from(iter: DocumentCursor) -> Self {
        Self {
            iter: iter.peekable(),
        }
    }
}

impl From<Peekable<DocumentCursor>> for Tokenizer {
    fn from(iter: Peekable<DocumentCursor>) -> Self {
        Self { iter }
    }
}

impl From<String> for Tokenizer {
    fn from(input: String) -> Self {
        Self {
            iter: DocumentCursor::new(input).peekable(),
        }
    }
}

impl From<&str> for Tokenizer {
    fn from(input: &str) -> Self {
        Self::from(input.to_string())
    }
}

impl Iterator for Tokenizer {
    type Item = Result<(Token, DocumentRange), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            self.iter.next();
        }

        self.iter.next().map(|start| {
            match start.ch() {
                '.' => Ok((Token::Dot, start)),
                '(' => Ok((Token::LeftParen, start)),
                ')' => Ok((Token::RightParen, start)),
                '[' => Ok((Token::LeftBracket, start)),
                ']' => Ok((Token::RightBracket, start)),
                '{' => Ok((Token::LeftBrace, start)),
                '}' => Ok((Token::RightBrace, start)),
                ':' => Ok((Token::Colon, start)),
                ',' => Ok((Token::Comma, start)),
                '+' => Ok((Token::Plus, start)),
                '-' => Ok((Token::Minus, start)),
                '*' => Ok((Token::Multiply, start)),
                '&' => match self.iter.next_if(|s| s.ch() == '&') {
                    Some(end) => Ok((Token::LogicalAnd, start.to(end))),
                    None => Err(ParseError::UnexpectedCharacter {
                        ch: '&',
                        range: start,
                    }),
                },
                '|' => match self.iter.next_if(|s| s.ch() == '|') {
                    Some(end) => Ok((Token::LogicalOr, start.to(end))),
                    None => Err(ParseError::UnexpectedCharacter {
                        ch: '|',
                        range: start,
                    }),
                },
                '!' => match self.iter.next_if(|s| s.ch() == '=') {
                    Some(end) => Ok((Token::NotEq, start.to(end))),
                    None => Ok((Token::Not, start)),
                },
                '<' => match self.iter.next_if(|s| s.ch() == '=') {
                    Some(end) => Ok((Token::LessThanOrEqual, start.to(end))),
                    None => Ok((Token::LessThan, start)),
                },
                '>' => match self.iter.next_if(|s| s.ch() == '=') {
                    Some(end) => Ok((Token::GreaterThanOrEqual, start.to(end))),
                    None => Ok((Token::GreaterThan, start)),
                },
                '=' => match self.iter.next_if(|s| s.ch() == '=') {
                    Some(end) => Ok((Token::Eq, start.to(end))),
                    None => Err(ParseError::ExpectedDoubleEqButGotSingleEq { range: start }),
                },
                '"' => {
                    let mut end_range = start.clone();
                    let mut result = String::new();
                    while let Some(s) = self.iter.next_if(|s| s.ch() != '"') {
                        result.push(s.ch());
                        end_range = s;
                    }
                    match self.iter.next() {
                        None => Err(ParseError::UnterminatedStringLiteral {
                            range: start.to(end_range),
                        }),
                        Some(end) => Ok((Token::StringLiteral(result), start.to(end))),
                    }
                }
                'A'..='Z' | 'a'..='z' | '_' => {
                    let identifier = start.extend(self.iter.peeking_take_while(
                        |s| matches!(s.ch(), 'A'..='Z' | 'a'..='z' | '0'..='9' | '_'),
                    ));
                    let t = match identifier.as_str() {
                        "in" => Token::In,
                        "true" => Token::BooleanLiteral(true),
                        "false" => Token::BooleanLiteral(false),
                        "record" => Token::KeywordRecord,
                        // Type keywords
                        "String" => Token::TypeString,
                        "Int" => Token::TypeInt,
                        "Float" => Token::TypeFloat,
                        "Bool" => Token::TypeBoolean,
                        "TrustedHTML" => Token::TypeTrustedHTML,
                        "Array" => Token::TypeArray,
                        _ => Token::Identifier(identifier.clone()),
                    };
                    Ok((t, identifier))
                }
                ch if ch.is_ascii_digit() => {
                    let mut number_string =
                        start.extend(self.iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
                    let has_decimal = if let Some(dot) = self.iter.next_if(|s| s.ch() == '.') {
                        number_string = number_string.to(dot);
                        number_string = number_string
                            .extend(self.iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
                        true
                    } else {
                        false
                    };

                    match serde_json::Number::from_str(number_string.as_str()) {
                        Ok(n) => {
                            if has_decimal {
                                // If original string had a decimal point, treat as float
                                let f = n.as_f64().unwrap();
                                Ok((Token::FloatLiteral(f), number_string))
                            } else if let Some(i) = n.as_i64() {
                                Ok((Token::IntLiteral(i), number_string))
                            } else {
                                // Convert serde_json::Number to f64 for FloatLiteral
                                let f = n.as_f64().unwrap();
                                Ok((Token::FloatLiteral(f), number_string))
                            }
                        }
                        Err(_) => Err(ParseError::InvalidNumberFormat {
                            range: number_string,
                        }),
                    }
                }
                ch => Err(ParseError::UnexpectedCharacter { ch, range: start }),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::document::{DocumentAnnotator, SimpleAnnotation, document_cursor::Ranged as _};

    use super::*;
    use expect_test::{Expect, expect};

    fn check(input: &str, expected: Expect) {
        let tokenizer = Tokenizer::from(input);
        let mut annotations = Vec::new();
        for t in tokenizer {
            match t {
                Ok((tok, range)) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("token: {}", tok),
                        range,
                    });
                }
                Err(err) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("error: {err}"),
                        range: err.range().clone(),
                    });
                }
            }
        }
        expected.assert_eq(
            &DocumentAnnotator::new()
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
                token: 1
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                ^^^

                token: 0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                    ^^^

                token: 0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                        ^^^^^^

                token: 1000000
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                               ^^^^^^^

                token: 0
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                                       ^^^^^^

                token: 0.101
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                                              ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_integers() {
        check(
            "42 0 123 999",
            expect![[r#"
                token: 42
                42 0 123 999
                ^^

                token: 0
                42 0 123 999
                   ^

                token: 123
                42 0 123 999
                     ^^^

                token: 999
                42 0 123 999
                         ^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_mixed_integers_and_floats() {
        check(
            "42 3.14 0 0.0 123 99.99",
            expect![[r#"
                token: 42
                42 3.14 0 0.0 123 99.99
                ^^

                token: 3.14
                42 3.14 0 0.0 123 99.99
                   ^^^^

                token: 0
                42 3.14 0 0.0 123 99.99
                        ^

                token: 0
                42 3.14 0 0.0 123 99.99
                          ^^^

                token: 123
                42 3.14 0 0.0 123 99.99
                              ^^^

                token: 99.99
                42 3.14 0 0.0 123 99.99
                                  ^^^^^
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
            "( ) . ! == < >",
            expect![[r#"
                token: (
                ( ) . ! == < >
                ^

                token: )
                ( ) . ! == < >
                  ^

                token: .
                ( ) . ! == < >
                    ^

                token: !
                ( ) . ! == < >
                      ^

                token: ==
                ( ) . ! == < >
                        ^^

                token: <
                ( ) . ! == < >
                           ^

                token: >
                ( ) . ! == < >
                             ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_not_equals() {
        check(
            "!=",
            expect![[r#"
                token: !=
                !=
                ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_not_equals_in_expression() {
        check(
            "x != y",
            expect![[r#"
                token: x
                x != y
                ^

                token: !=
                x != y
                  ^^

                token: y
                x != y
                     ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_less_than_or_equal() {
        check(
            "<=",
            expect![[r#"
                token: <=
                <=
                ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_less_than_or_equal_in_expression() {
        check(
            "x <= y",
            expect![[r#"
                token: x
                x <= y
                ^

                token: <=
                x <= y
                  ^^

                token: y
                x <= y
                     ^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_greater_than_or_equal() {
        check(
            ">=",
            expect![[r#"
                token: >=
                >=
                ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_greater_than_or_equal_in_expression() {
        check(
            "x >= y",
            expect![[r#"
                token: x
                x >= y
                ^

                token: >=
                x >= y
                  ^^

                token: y
                x >= y
                     ^
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
    fn test_tokenize_type_keywords() {
        check(
            "String Int Float Bool TrustedHTML Array",
            expect![[r#"
                token: String
                String Int Float Bool TrustedHTML Array
                ^^^^^^

                token: Int
                String Int Float Bool TrustedHTML Array
                       ^^^

                token: Float
                String Int Float Bool TrustedHTML Array
                           ^^^^^

                token: Bool
                String Int Float Bool TrustedHTML Array
                                 ^^^^

                token: TrustedHTML
                String Int Float Bool TrustedHTML Array
                                      ^^^^^^^^^^^

                token: Array
                String Int Float Bool TrustedHTML Array
                                                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_string_literals() {
        check(
            r#""hello" "world with spaces" """#,
            expect![[r#"
                token: "hello"
                "hello" "world with spaces" ""
                ^^^^^^^

                token: "world with spaces"
                "hello" "world with spaces" ""
                        ^^^^^^^^^^^^^^^^^^^

                token: ""
                "hello" "world with spaces" ""
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
            r#"user.name == "admin""#,
            expect![[r#"
                token: user
                user.name == "admin"
                ^^^^

                token: .
                user.name == "admin"
                    ^

                token: name
                user.name == "admin"
                     ^^^^

                token: ==
                user.name == "admin"
                          ^^

                token: "admin"
                user.name == "admin"
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

                token: 1
                [1.0, 12.0, 343.0]
                 ^^^

                token: ,
                [1.0, 12.0, 343.0]
                    ^

                token: 12
                [1.0, 12.0, 343.0]
                      ^^^^

                token: ,
                [1.0, 12.0, 343.0]
                          ^

                token: 343
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
            r#"{name: "John"}"#,
            expect![[r#"
                token: {
                {name: "John"}
                ^

                token: name
                {name: "John"}
                 ^^^^

                token: :
                {name: "John"}
                     ^

                token: "John"
                {name: "John"}
                       ^^^^^^

                token: }
                {name: "John"}
                             ^
            "#]],
        );
    }
}
