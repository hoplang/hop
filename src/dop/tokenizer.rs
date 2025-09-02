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
        let start = self.position;
        self.chars.next().map(|ch| {
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += ch.len_utf8();
            }
            (ch, Range::new(start, self.position))
        })
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

        self.cursor.next().map(|(t, start_range)| {
            match t {
                '.' => Ok((DopToken::Dot, start_range)),
                '(' => Ok((DopToken::LeftParen, start_range)),
                ')' => Ok((DopToken::RightParen, start_range)),
                '[' => Ok((DopToken::LeftBracket, start_range)),
                ']' => Ok((DopToken::RightBracket, start_range)),
                '{' => Ok((DopToken::LeftBrace, start_range)),
                '}' => Ok((DopToken::RightBrace, start_range)),
                ':' => Ok((DopToken::Colon, start_range)),
                ',' => Ok((DopToken::Comma, start_range)),
                '!' => Ok((DopToken::Not, start_range)),
                '=' => {
                    if let Some((_, end_range)) = self.cursor.next_if(|(ch, _)| *ch == '=') {
                        return Ok((DopToken::Equal, start_range.extend_to(end_range)));
                    }
                    Err(ParseError::new(
                        "Expected '==' but found single '='".to_string(),
                        start_range,
                    ))
                }
                '\'' => {
                    let mut end_range = start_range;
                    let mut result = String::new();
                    while let Some((ch, range)) = self.cursor.next_if(|(ch, _)| *ch != '\'') {
                        end_range = range;
                        result.push(ch);
                    }
                    match self.cursor.next() {
                        None => Err(ParseError::unterminated_string_literal(
                            start_range.extend_to(end_range),
                        )),
                        Some(('\'', end_range)) => Ok((
                            DopToken::StringLiteral(result),
                            start_range.extend_to(end_range),
                        )),
                        _ => unreachable!(),
                    }
                }
                ch @ ('A'..='Z' | 'a'..='z' | '_') => {
                    let mut end_range = start_range;
                    let mut identifier = String::from(ch);

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
                    Ok((t, start_range.extend_to(end_range)))
                }
                ch if ch.is_ascii_digit() => {
                    let mut end_range = start_range;

                    let mut number_string = String::from(ch);

                    while let Some((ch, range)) = self.cursor.next_if(|(ch, _)| ch.is_ascii_digit())
                    {
                        number_string.push(ch);
                        end_range = range;
                    }

                    if let Some((_, range)) = self.cursor.next_if(|(ch, _)| *ch == '.') {
                        number_string.push('.');
                        end_range = range;

                        if !self
                            .cursor
                            .peek()
                            .is_some_and(|(ch, _)| ch.is_ascii_digit())
                        {
                            return Err(ParseError::expected_digit_after_decimal_point(
                                start_range.extend_to(end_range),
                            ));
                        }

                        while let Some((ch, range)) =
                            self.cursor.next_if(|(ch, _)| ch.is_ascii_digit())
                        {
                            number_string.push(ch);
                            end_range = range;
                        }
                    }

                    let parse_result = serde_json::Number::from_str(&number_string);

                    match parse_result {
                        Ok(n) => Ok((DopToken::NumberLiteral(n), start_range.extend_to(end_range))),
                        Err(_) => Err(ParseError::new(
                            format!("Invalid number format: {}", number_string),
                            start_range.extend_to(end_range),
                        )),
                    }
                }
                ch => Err(ParseError::new(
                    format!("Unexpected character: '{}'", ch),
                    start_range,
                )),
            }
        })
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
                Err(ParseError::Ranged { message, range }) => {
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
