use itertools::Itertools as _;
use std::iter::Peekable;

use crate::document::document_cursor::{DocumentCursor, DocumentRange};
use crate::hop::parse_error::ParseError;

/// Tokens produced by the declaration tokenizer.
#[derive(Debug, Clone)]
pub enum Token {
    /// "import" keyword
    Import,
    /// "record" keyword
    Record,
    /// An identifier (type name, record name, field name)
    Identifier(DocumentRange),
    /// A quoted string literal (the path in import statements)
    String(DocumentRange),
    /// {
    LeftBrace,
    /// }
    RightBrace,
    /// :
    Colon,
    /// ::
    ColonColon,
    /// ,
    Comma,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Token::Import, Token::Import)
                | (Token::Record, Token::Record)
                | (Token::Identifier(_), Token::Identifier(_))
                | (Token::String(_), Token::String(_))
                | (Token::LeftBrace, Token::LeftBrace)
                | (Token::RightBrace, Token::RightBrace)
                | (Token::Colon, Token::Colon)
                | (Token::ColonColon, Token::ColonColon)
                | (Token::Comma, Token::Comma)
                | (Token::LeftBracket, Token::LeftBracket)
                | (Token::RightBracket, Token::RightBracket)
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Import => write!(f, "import"),
            Token::Record => write!(f, "record"),
            Token::Identifier(range) => write!(f, "{}", range.as_str()),
            Token::String(range) => write!(f, "\"{}\"", range.as_str()),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Colon => write!(f, ":"),
            Token::ColonColon => write!(f, "::"),
            Token::Comma => write!(f, ","),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
        }
    }
}

/// Tokenizer for declaration syntax.
pub struct Tokenizer {
    iter: Peekable<DocumentCursor>,
}

impl From<DocumentRange> for Tokenizer {
    fn from(range: DocumentRange) -> Self {
        Self {
            iter: range.cursor().peekable(),
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Result<(Token, DocumentRange), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace
        while self.iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            self.iter.next();
        }

        self.iter.next().map(|start| {
            match start.ch() {
                '{' => Ok((Token::LeftBrace, start)),
                '}' => Ok((Token::RightBrace, start)),
                ':' => {
                    // Check for ::
                    if self.iter.peek().is_some_and(|s| s.ch() == ':') {
                        let end = self.iter.next().unwrap();
                        Ok((Token::ColonColon, start.to(end)))
                    } else {
                        Ok((Token::Colon, start))
                    }
                }
                ',' => Ok((Token::Comma, start)),
                '[' => Ok((Token::LeftBracket, start)),
                ']' => Ok((Token::RightBracket, start)),
                '"' => {
                    let Some(content_start) = self.iter.next_if(|s| s.ch() != '"') else {
                        // Empty string or unterminated
                        return match self.iter.next() {
                            Some(end) => Err(ParseError::GenericError {
                                message: "Empty string literal".to_string(),
                                range: start.to(end),
                            }),
                            None => Err(ParseError::UnmatchedCharacter {
                                ch: '"',
                                range: start,
                            }),
                        };
                    };
                    let content =
                        content_start.extend(self.iter.peeking_take_while(|s| s.ch() != '"'));
                    match self.iter.next() {
                        Some(end) => Ok((Token::String(content), start.to(end))),
                        None => Err(ParseError::UnmatchedCharacter {
                            ch: '"',
                            range: start,
                        }),
                    }
                }
                ch if ch.is_ascii_alphabetic() || ch == '_' => self.tokenize_identifier(start),
                ch => Err(ParseError::GenericError {
                    message: format!("Unexpected character: '{}'", ch),
                    range: start,
                }),
            }
        })
    }
}

impl Tokenizer {
    /// Tokenize an identifier or keyword.
    fn tokenize_identifier(
        &mut self,
        start: DocumentRange,
    ) -> Result<(Token, DocumentRange), ParseError> {
        let identifier = start.extend(
            self.iter
                .peeking_take_while(|s| s.ch().is_ascii_alphanumeric() || s.ch() == '_'),
        );

        let token = match identifier.as_str() {
            "import" => Token::Import,
            "record" => Token::Record,
            _ => Token::Identifier(identifier.clone()),
        };

        Ok((token, identifier))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::document_cursor::Ranged as _;
    use crate::document::{DocumentAnnotator, SimpleAnnotation};
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let range = DocumentCursor::new(input.to_string()).range();
        let tokenizer = Tokenizer::from(range);
        let mut annotations = Vec::new();

        for t in tokenizer {
            match t {
                Ok((token, range)) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("token: {}", token),
                        range,
                    });
                }
                Err(err) => {
                    annotations.push(SimpleAnnotation {
                        message: format!("error: {}", err),
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
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
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
    fn should_reject_when_containing_unexpected_character() {
        check(
            "record User @",
            expect![[r#"
                token: record
                record User @
                ^^^^^^

                token: User
                record User @
                       ^^^^

                error: Unexpected character: '@'
                record User @
                            ^
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_import_declarations() {
        check(
            indoc! {r#"
                import foo::Foo
                import bar::Bar
            "#},
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
            indoc! {r#"
                import foo::Foo
                record Bar {name: String}
            "#},
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
}
