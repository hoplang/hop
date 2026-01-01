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
                ':' => match self.iter.next_if(|s| s.ch() == ':') {
                    Some(end) => Ok((Token::ColonColon, start.to(end))),
                    None => Ok((Token::Colon, start)),
                },
                ',' => Ok((Token::Comma, start)),
                '+' => Ok((Token::Plus, start)),
                '-' => match self.iter.next_if(|s| s.ch() == '>') {
                    Some(end) => Ok((Token::Arrow, start.to(end))),
                    None => Ok((Token::Minus, start)),
                },
                '*' => Ok((Token::Asterisk, start)),
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
                '=' => match self.iter.peek().map(|s| s.ch()) {
                    Some('=') => {
                        let end = self.iter.next().unwrap();
                        Ok((Token::Eq, start.to(end)))
                    }
                    Some('>') => {
                        let end = self.iter.next().unwrap();
                        Ok((Token::FatArrow, start.to(end)))
                    }
                    _ => Ok((Token::Assign, start)),
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
                        // Keywords
                        "in" => Token::In,
                        "import" => Token::Import,
                        "true" => Token::True,
                        "false" => Token::False,
                        "record" => Token::Record,
                        "match" => Token::Match,
                        "enum" => Token::Enum,
                        "Some" => Token::Some,
                        "None" => Token::None,
                        // Types
                        "String" => Token::TypeString,
                        "Int" => Token::TypeInt,
                        "Float" => Token::TypeFloat,
                        "Bool" => Token::TypeBoolean,
                        "TrustedHTML" => Token::TypeTrustedHTML,
                        "Array" => Token::TypeArray,
                        "Option" => Token::TypeOption,
                        _ => {
                            let first_char = identifier.as_str().chars().next().unwrap();
                            if first_char.is_ascii_uppercase() {
                                Token::TypeName(identifier.to_string_span())
                            } else {
                                Token::Identifier(identifier.to_string_span())
                            }
                        }
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
    fn should_skip_whitespace_between_tokens() {
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
    fn should_accept_multiline_input() {
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
    fn should_accept_valid_float_numbers() {
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
    fn should_reject_for_invalid_number_formats() {
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
    fn should_accept_single_equals_as_assign_token() {
        check(
            "=foo",
            expect![[r#"
                token: =
                =foo
                ^

                token: foo
                =foo
                 ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unexpected_character() {
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
    fn should_accept_integer_literals() {
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
    fn should_accept_mixed_integers_and_floats() {
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
    fn should_accept_simple_punctuation() {
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
    fn should_accept_not_equals_operator() {
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
    fn should_accept_not_equals_in_expression() {
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
    fn should_accept_less_than_or_equal_operator() {
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
    fn should_accept_less_than_or_equal_in_expression() {
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
    fn should_accept_greater_than_or_equal_operator() {
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
    fn should_accept_greater_than_or_equal_in_expression() {
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
    fn should_accept_identifiers_and_keywords() {
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
    fn should_accept_type_keywords() {
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
    fn should_accept_custom_type_names() {
        check(
            "User Person MyType CustomRecord",
            expect![[r#"
                token: User
                User Person MyType CustomRecord
                ^^^^

                token: Person
                User Person MyType CustomRecord
                     ^^^^^^

                token: MyType
                User Person MyType CustomRecord
                            ^^^^^^

                token: CustomRecord
                User Person MyType CustomRecord
                                   ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_distinguish_identifiers_from_type_names() {
        check(
            "foo Foo bar Bar _test Test",
            expect![[r#"
                token: foo
                foo Foo bar Bar _test Test
                ^^^

                token: Foo
                foo Foo bar Bar _test Test
                    ^^^

                token: bar
                foo Foo bar Bar _test Test
                        ^^^

                token: Bar
                foo Foo bar Bar _test Test
                            ^^^

                token: _test
                foo Foo bar Bar _test Test
                                ^^^^^

                token: Test
                foo Foo bar Bar _test Test
                                      ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_string_literals() {
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
    fn should_accept_field_access_expression() {
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
    fn should_accept_complex_expression() {
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
    fn should_accept_parenthesized_expression() {
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
    fn should_accept_array_with_brackets() {
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
    fn should_accept_array_of_floats() {
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
    fn should_accept_empty_array() {
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
    fn should_accept_import_statement() {
        check(
            r#"import user_list::UserList"#,
            expect![[r#"
                token: import
                import user_list::UserList
                ^^^^^^

                token: user_list
                import user_list::UserList
                       ^^^^^^^^^

                token: ::
                import user_list::UserList
                                ^^

                token: UserList
                import user_list::UserList
                                  ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_record_statement() {
        check(
            "record User {name: String, age: Int}",
            expect![[r#"
                token: record
                record User {name: String, age: Int}
                ^^^^^^

                token: User
                record User {name: String, age: Int}
                       ^^^^

                token: {
                record User {name: String, age: Int}
                            ^

                token: name
                record User {name: String, age: Int}
                             ^^^^

                token: :
                record User {name: String, age: Int}
                                 ^

                token: String
                record User {name: String, age: Int}
                                   ^^^^^^

                token: ,
                record User {name: String, age: Int}
                                         ^

                token: age
                record User {name: String, age: Int}
                                           ^^^

                token: :
                record User {name: String, age: Int}
                                              ^

                token: Int
                record User {name: String, age: Int}
                                                ^^^

                token: }
                record User {name: String, age: Int}
                                                   ^
            "#]],
        );
    }

    #[test]
    fn should_accept_record_with_array_field() {
        check(
            "record UserList {users: Array[User]}",
            expect![[r#"
                token: record
                record UserList {users: Array[User]}
                ^^^^^^

                token: UserList
                record UserList {users: Array[User]}
                       ^^^^^^^^

                token: {
                record UserList {users: Array[User]}
                                ^

                token: users
                record UserList {users: Array[User]}
                                 ^^^^^

                token: :
                record UserList {users: Array[User]}
                                      ^

                token: Array
                record UserList {users: Array[User]}
                                        ^^^^^

                token: [
                record UserList {users: Array[User]}
                                             ^

                token: User
                record UserList {users: Array[User]}
                                              ^^^^

                token: ]
                record UserList {users: Array[User]}
                                                  ^

                token: }
                record UserList {users: Array[User]}
                                                   ^
            "#]],
        );
    }

    #[test]
    fn should_accept_multiline_record() {
        check(
            "record User {\n    name: String,\n    age: Int,\n}",
            expect![[r#"
                token: record
                record User {
                ^^^^^^

                token: User
                record User {
                       ^^^^

                token: {
                record User {
                            ^

                token: name
                    name: String,
                    ^^^^

                token: :
                    name: String,
                        ^

                token: String
                    name: String,
                          ^^^^^^

                token: ,
                    name: String,
                                ^

                token: age
                    age: Int,
                    ^^^

                token: :
                    age: Int,
                       ^

                token: Int
                    age: Int,
                         ^^^

                token: ,
                    age: Int,
                            ^

                token: }
                }
                ^
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_import_declarations() {
        check(
            "import foo::Foo\nimport bar::Bar",
            expect![[r#"
                token: import
                import foo::Foo
                ^^^^^^

                token: foo
                import foo::Foo
                       ^^^

                token: ::
                import foo::Foo
                          ^^

                token: Foo
                import foo::Foo
                            ^^^

                token: import
                import bar::Bar
                ^^^^^^

                token: bar
                import bar::Bar
                       ^^^

                token: ::
                import bar::Bar
                          ^^

                token: Bar
                import bar::Bar
                            ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_import_followed_by_record() {
        check(
            "import foo::Foo\nrecord Bar {name: String}",
            expect![[r#"
                token: import
                import foo::Foo
                ^^^^^^

                token: foo
                import foo::Foo
                       ^^^

                token: ::
                import foo::Foo
                          ^^

                token: Foo
                import foo::Foo
                            ^^^

                token: record
                record Bar {name: String}
                ^^^^^^

                token: Bar
                record Bar {name: String}
                       ^^^

                token: {
                record Bar {name: String}
                           ^

                token: name
                record Bar {name: String}
                            ^^^^

                token: :
                record Bar {name: String}
                                ^

                token: String
                record Bar {name: String}
                                  ^^^^^^

                token: }
                record Bar {name: String}
                                        ^
            "#]],
        );
    }

    #[test]
    fn should_accept_varying_whitespace_between_tokens() {
        check(
            "record  User  {  name :  String  }",
            expect![[r#"
                token: record
                record  User  {  name :  String  }
                ^^^^^^

                token: User
                record  User  {  name :  String  }
                        ^^^^

                token: {
                record  User  {  name :  String  }
                              ^

                token: name
                record  User  {  name :  String  }
                                 ^^^^

                token: :
                record  User  {  name :  String  }
                                      ^

                token: String
                record  User  {  name :  String  }
                                         ^^^^^^

                token: }
                record  User  {  name :  String  }
                                                 ^
            "#]],
        );
    }

    #[test]
    fn should_accept_record_with_nested_array_types() {
        check(
            "record Data {matrix: Array[Array[Int]]}",
            expect![[r#"
                token: record
                record Data {matrix: Array[Array[Int]]}
                ^^^^^^

                token: Data
                record Data {matrix: Array[Array[Int]]}
                       ^^^^

                token: {
                record Data {matrix: Array[Array[Int]]}
                            ^

                token: matrix
                record Data {matrix: Array[Array[Int]]}
                             ^^^^^^

                token: :
                record Data {matrix: Array[Array[Int]]}
                                   ^

                token: Array
                record Data {matrix: Array[Array[Int]]}
                                     ^^^^^

                token: [
                record Data {matrix: Array[Array[Int]]}
                                          ^

                token: Array
                record Data {matrix: Array[Array[Int]]}
                                           ^^^^^

                token: [
                record Data {matrix: Array[Array[Int]]}
                                                ^

                token: Int
                record Data {matrix: Array[Array[Int]]}
                                                 ^^^

                token: ]
                record Data {matrix: Array[Array[Int]]}
                                                    ^

                token: ]
                record Data {matrix: Array[Array[Int]]}
                                                     ^

                token: }
                record Data {matrix: Array[Array[Int]]}
                                                      ^
            "#]],
        );
    }

    #[test]
    fn should_accept_identifiers_containing_underscores() {
        check(
            "record my_record {field_name: String}",
            expect![[r#"
                token: record
                record my_record {field_name: String}
                ^^^^^^

                token: my_record
                record my_record {field_name: String}
                       ^^^^^^^^^

                token: {
                record my_record {field_name: String}
                                 ^

                token: field_name
                record my_record {field_name: String}
                                  ^^^^^^^^^^

                token: :
                record my_record {field_name: String}
                                            ^

                token: String
                record my_record {field_name: String}
                                              ^^^^^^

                token: }
                record my_record {field_name: String}
                                                    ^
            "#]],
        );
    }

    #[test]
    fn should_accept_match_keyword() {
        check(
            "match foo",
            expect![[r#"
                token: match
                match foo
                ^^^^^

                token: foo
                match foo
                      ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_keyword() {
        check(
            "enum Color",
            expect![[r#"
                token: enum
                enum Color
                ^^^^

                token: Color
                enum Color
                     ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_some_keyword() {
        check(
            "Some(x)",
            expect![[r#"
                token: Some
                Some(x)
                ^^^^

                token: (
                Some(x)
                    ^

                token: x
                Some(x)
                     ^

                token: )
                Some(x)
                      ^
            "#]],
        );
    }

    #[test]
    fn should_accept_none_keyword() {
        check(
            "None",
            expect![[r#"
                token: None
                None
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_option_type() {
        check(
            "Option[String]",
            expect![[r#"
                token: Option
                Option[String]
                ^^^^^^

                token: [
                Option[String]
                      ^

                token: String
                Option[String]
                       ^^^^^^

                token: ]
                Option[String]
                             ^
            "#]],
        );
    }

    #[test]
    fn should_accept_colon_colon_operator() {
        check(
            "foo::bar::Baz",
            expect![[r#"
                token: foo
                foo::bar::Baz
                ^^^

                token: ::
                foo::bar::Baz
                   ^^

                token: bar
                foo::bar::Baz
                     ^^^

                token: ::
                foo::bar::Baz
                        ^^

                token: Baz
                foo::bar::Baz
                          ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_import_keyword() {
        check(
            "import foo",
            expect![[r#"
                token: import
                import foo
                ^^^^^^

                token: foo
                import foo
                       ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_arrow_operator() {
        check(
            "->",
            expect![[r#"
                token: ->
                ->
                ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_fat_arrow_operator() {
        check(
            "=>",
            expect![[r#"
                token: =>
                =>
                ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_fat_arrow_in_expression() {
        check(
            "x => y",
            expect![[r#"
                token: x
                x => y
                ^

                token: =>
                x => y
                  ^^

                token: y
                x => y
                     ^
            "#]],
        );
    }

    #[test]
    fn should_distinguish_minus_from_arrow() {
        check(
            "x - y x -> y",
            expect![[r#"
                token: x
                x - y x -> y
                ^

                token: -
                x - y x -> y
                  ^

                token: y
                x - y x -> y
                    ^

                token: x
                x - y x -> y
                      ^

                token: ->
                x - y x -> y
                        ^^

                token: y
                x - y x -> y
                           ^
            "#]],
        );
    }
}
