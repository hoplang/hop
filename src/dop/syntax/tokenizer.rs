use std::collections::VecDeque;
use std::iter::Peekable;
use std::str::FromStr;

use itertools::Itertools as _;

use crate::document::{CheapString, DocumentCursor, DocumentRange};
use crate::error_collector::ErrorCollector;

use super::token::Token;
use crate::parse_error::ParseError;

pub fn next(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<(Token, DocumentRange)> {
    // Skip whitespace
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }

    let start = iter.next()?;

    let result = match start.ch() {
        '.' => {
            // Check for ..=
            let mut lookahead = iter.clone();
            if lookahead.next_if(|s| s.ch() == '.').is_some()
                && lookahead.peek().map(|s| s.ch()) == Some('=')
            {
                iter.next(); // consume second dot
                let eq = iter.next().unwrap();
                (Token::DotDotEq, start.to(eq))
            } else {
                (Token::Dot, start)
            }
        }
        '(' => (Token::LeftParen, start),
        ')' => (Token::RightParen, start),
        '[' => (Token::LeftBracket, start),
        ']' => (Token::RightBracket, start),
        '{' => (Token::LeftBrace, start),
        '}' => (Token::RightBrace, start),
        ':' => match iter.next_if(|s| s.ch() == ':') {
            Some(end) => (Token::ColonColon, start.to(end)),
            None => (Token::Colon, start),
        },
        ',' => (Token::Comma, start),
        '+' => (Token::Plus, start),
        '-' => match iter.next_if(|s| s.ch() == '>') {
            Some(end) => (Token::Arrow, start.to(end)),
            None => (Token::Minus, start),
        },
        '*' => (Token::Asterisk, start),
        '&' => match iter.next_if(|s| s.ch() == '&') {
            Some(end) => (Token::LogicalAnd, start.to(end)),
            None => {
                errors.push(ParseError::UnexpectedCharacter {
                    ch: '&',
                    range: start,
                });
                return next(iter, errors); // Continue tokenization after error
            }
        },
        '|' => match iter.next_if(|s| s.ch() == '|') {
            Some(end) => (Token::LogicalOr, start.to(end)),
            None => {
                errors.push(ParseError::UnexpectedCharacter {
                    ch: '|',
                    range: start,
                });
                return next(iter, errors); // Continue tokenization after error
            }
        },
        '/' => match iter.next_if(|s| s.ch() == '/') {
            Some(second_slash) => {
                let comment = start
                    .to(second_slash)
                    .extend(iter.peeking_take_while(|s| s.ch() != '\n'));
                let text = comment.to_cheap_string();
                (Token::Comment(text), comment)
            }
            None => {
                errors.push(ParseError::UnexpectedCharacter {
                    ch: '/',
                    range: start,
                });
                return next(iter, errors); // Continue tokenization after error
            }
        },
        '!' => match iter.next_if(|s| s.ch() == '=') {
            Some(end) => (Token::NotEq, start.to(end)),
            None => (Token::Not, start),
        },
        '<' => match iter.next_if(|s| s.ch() == '=') {
            Some(end) => (Token::LessThanOrEqual, start.to(end)),
            None => (Token::LessThan, start),
        },
        '>' => match iter.next_if(|s| s.ch() == '=') {
            Some(end) => (Token::GreaterThanOrEqual, start.to(end)),
            None => (Token::GreaterThan, start),
        },
        '=' => match iter.peek().map(|s| s.ch()) {
            Some('=') => {
                let end = iter.next().unwrap();
                (Token::Eq, start.to(end))
            }
            Some('>') => {
                let end = iter.next().unwrap();
                (Token::FatArrow, start.to(end))
            }
            _ => (Token::Assign, start),
        },
        '"' => {
            // Parse string with escape sequence validation
            let mut content: Option<DocumentRange> = None;
            loop {
                match iter.peek().map(|s| s.ch()) {
                    None => {
                        // Unterminated string
                        errors.push(ParseError::UnterminatedStringLiteral {
                            range: content
                                .map(|c| start.clone().to(c))
                                .unwrap_or(start.clone()),
                        });
                        return None;
                    }
                    Some('"') => {
                        // End of string
                        let end = iter.next().unwrap();
                        let value = content
                            .map(|c| c.to_cheap_string())
                            .unwrap_or_else(|| CheapString::new(String::new()));
                        break (Token::StringLiteral(value), start.to(end));
                    }
                    Some('\\') => {
                        // Escape sequence - consume backslash
                        let backslash = iter.next().unwrap();
                        content = Some(
                            content
                                .map(|c| c.to(backslash.clone()))
                                .unwrap_or(backslash.clone()),
                        );

                        // Check what follows the backslash
                        match iter.peek().map(|s| s.ch()) {
                            None => {
                                // Backslash at end of input
                                errors.push(ParseError::InvalidEscapeSequenceAtEndOfString {
                                    range: backslash.clone(),
                                });
                                errors.push(ParseError::UnterminatedStringLiteral {
                                    range: start.clone().to(backslash),
                                });
                                return None;
                            }
                            Some(ch @ ('n' | 't' | 'r' | '\\' | '"')) => {
                                // Valid escape sequence - consume the character
                                let escape_char = iter.next().unwrap();
                                content = Some(
                                    content
                                        .map(|c| c.to(escape_char.clone()))
                                        .unwrap_or(escape_char),
                                );
                                // Continue parsing - the raw escape sequence is kept
                                let _ = ch; // silence unused warning
                            }
                            Some(ch) => {
                                // Invalid escape sequence - report error but continue
                                let escape_char = iter.next().unwrap();
                                errors.push(ParseError::InvalidEscapeSequence {
                                    ch,
                                    range: backslash.to(escape_char.clone()),
                                });
                                content = Some(
                                    content
                                        .map(|c| c.to(escape_char.clone()))
                                        .unwrap_or(escape_char),
                                );
                            }
                        }
                    }
                    Some(_) => {
                        // Regular character - consume it
                        let ch = iter.next().unwrap();
                        content = Some(content.map(|c| c.to(ch.clone())).unwrap_or(ch));
                    }
                }
            }
        }
        'A'..='Z' | 'a'..='z' | '_' => {
            let identifier =
                start.extend(iter.peeking_take_while(
                    |s| matches!(s.ch(), 'A'..='Z' | 'a'..='z' | '0'..='9' | '_'),
                ));
            let t = match identifier.as_str() {
                // Wildcard
                "_" => Token::Underscore,
                // Keywords
                "in" => Token::In,
                "import" => Token::Import,
                "true" => Token::True,
                "false" => Token::False,
                "record" => Token::Record,
                "match" => Token::Match,
                "enum" => Token::Enum,
                "entrypoint" => Token::Entrypoint,
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
                // Reserved keywords
                "let" | "fn" | "func" | "if" | "else" | "return" | "struct" | "var" | "const"
                | "assert" | "comp" | "component" | "and" | "or" | "not" | "of" | "while"
                | "loop" | "break" | "continue" | "case" | "default" | "try" | "catch"
                | "throw" | "finally" | "async" | "await" | "yield" | "pub" | "private"
                | "mut" | "impl" | "trait" | "interface" | "as" | "is" | "where" | "self"
                | "this" | "super" | "use" | "from" | "export" | "mod" | "null" | "nil"
                | "new" | "static" | "defer" | "extends" | "implements" | "namespace"
                | "include" | "newtype" | "package" | "internal" | "undefined" | "void"
                | "final" | "when" | "out" | "priv" | "public" | "val" | "elif" | "readonly"
                | "get" | "set" | "auto" | "constructor" | "alias"
                // Uppercase reserved keywords
                | "Any" | "Arr" | "Async" | "Auto" | "Box" | "CSS" | "Class" | "Classes"
                | "Client" | "Comp" | "Computed" | "Dyn" | "Dynamic" | "Enum" | "Err"
                | "Error" | "Fn" | "Func" | "Function" | "Future" | "HTML" | "IO" | "List"
                | "Map" | "Never" | "Object" | "Ok" | "Promise" | "Rec" | "Record" | "Result"
                | "Runtime" | "Safe" | "Scope" | "Scoped" | "Self" | "Set" | "Static"
                | "Struct" | "Task" | "Trusted" | "Tuple" | "Type" | "Union" | "Unknown"
                | "Void" => Token::Reserved(identifier.to_cheap_string()),
                _ => {
                    let first_char = identifier.as_str().chars().next().unwrap();
                    if first_char.is_ascii_uppercase() {
                        Token::TypeName(identifier.to_cheap_string())
                    } else {
                        Token::Identifier(identifier.to_cheap_string())
                    }
                }
            };
            (t, identifier)
        }
        ch if ch.is_ascii_digit() => {
            let mut number_string =
                start.extend(iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
            // Only consume '.' as decimal if followed by a digit
            let has_decimal = if iter.peek().map(|s| s.ch()) == Some('.') {
                let mut lookahead = iter.clone();
                lookahead.next(); // consume dot in lookahead
                if lookahead.peek().is_some_and(|s| s.ch().is_ascii_digit()) {
                    let dot = iter.next().unwrap();
                    number_string = number_string.to(dot);
                    number_string =
                        number_string.extend(iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
                    true
                } else {
                    false
                }
            } else {
                false
            };

            match serde_json::Number::from_str(number_string.as_str()) {
                Ok(n) => {
                    if has_decimal {
                        // If original string had a decimal point, treat as float
                        let f = n.as_f64().unwrap();
                        (Token::FloatLiteral(f), number_string)
                    } else if let Some(i) = n.as_i64() {
                        (Token::IntLiteral(i), number_string)
                    } else {
                        // Convert serde_json::Number to f64 for FloatLiteral
                        let f = n.as_f64().unwrap();
                        (Token::FloatLiteral(f), number_string)
                    }
                }
                Err(_) => {
                    errors.push(ParseError::InvalidNumberFormat {
                        range: number_string,
                    });
                    return next(iter, errors); // Continue tokenization after error
                }
            }
        }
        ch => {
            errors.push(ParseError::UnexpectedCharacter { ch, range: start });
            return next(iter, errors); // Continue tokenization after error
        }
    };

    Some(result)
}

/// Returns the next non-comment token, collecting any comment tokens into the provided deque.
pub fn next_collecting_comments(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<(Token, DocumentRange)> {
    loop {
        match next(iter, errors) {
            Some((Token::Comment(_), range)) => {
                comments.push_back(range);
                continue;
            }
            other => return other,
        }
    }
}

/// Peeks at the next token without consuming it (includes comments).
/// Uses a temporary error collector that is discarded.
pub fn peek(iter: &Peekable<DocumentCursor>) -> Option<(Token, DocumentRange)> {
    let mut cloned = iter.clone();
    let mut discarded_errors = ErrorCollector::new();
    next(&mut cloned, &mut discarded_errors)
}

/// Peeks at the next non-comment token without consuming it.
/// Uses a temporary error collector that is discarded.
pub fn peek_past_comments(iter: &Peekable<DocumentCursor>) -> Option<(Token, DocumentRange)> {
    let mut cloned = iter.clone();
    let mut discarded_comments = VecDeque::new();
    let mut discarded_errors = ErrorCollector::new();
    next_collecting_comments(&mut cloned, &mut discarded_comments, &mut discarded_errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::{DocumentAnnotator, Ranged as _, SimpleAnnotation};
    use crate::hop::symbols::module_id::ModuleId;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let mut cursor = DocumentCursor::new(ModuleId::test(), input.to_string()).peekable();
        let mut errors = ErrorCollector::new();
        let mut annotations = Vec::new();
        while let Some((tok, range)) = next(&mut cursor, &mut errors) {
            annotations.push(SimpleAnnotation {
                message: format!("token: {:?}", tok),
                range,
            });
        }
        for err in &errors {
            annotations.push(SimpleAnnotation {
                message: format!("error: {err}"),
                range: err.range().clone(),
            });
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
                token: Identifier("foo")
                  foo   bar  
                  ^^^

                token: Identifier("bar")
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
                token: Identifier("foo")
                foo
                ^^^

                token: Identifier("bar")
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
                token: FloatLiteral(1.0)
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                ^^^

                token: FloatLiteral(0.0)
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                    ^^^

                token: FloatLiteral(0.0)
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                        ^^^^^^

                token: IntLiteral(1000000)
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                               ^^^^^^^

                token: FloatLiteral(0.0)
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                                       ^^^^^^

                token: FloatLiteral(0.101)
                1.0 0.0 0.0000 1000000 0.0000 0.1010
                                              ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_for_invalid_number_formats() {
        // Numbers with leading zeros are invalid
        check(
            "000 0123 01010",
            expect![[r#"
                error: Invalid number format
                000 0123 01010
                ^^^

                error: Invalid number format
                000 0123 01010
                    ^^^^

                error: Invalid number format
                000 0123 01010
                         ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_parse_trailing_dot_as_separate_token() {
        // `1.` is parsed as integer `1` followed by `.` (not an invalid float)
        // This allows range expressions like `0..=10`
        check(
            "1. 1000.",
            expect![[r#"
                token: IntLiteral(1)
                1. 1000.
                ^

                token: Dot
                1. 1000.
                 ^

                token: IntLiteral(1000)
                1. 1000.
                   ^^^^

                token: Dot
                1. 1000.
                       ^
            "#]],
        );
    }

    #[test]
    fn should_accept_single_equals_as_assign_token() {
        check(
            "=foo",
            expect![[r#"
                token: Assign
                =foo
                ^

                token: Identifier("foo")
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
                token: IntLiteral(42)
                42 0 123 999
                ^^

                token: IntLiteral(0)
                42 0 123 999
                   ^

                token: IntLiteral(123)
                42 0 123 999
                     ^^^

                token: IntLiteral(999)
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
                token: IntLiteral(42)
                42 3.14 0 0.0 123 99.99
                ^^

                token: FloatLiteral(3.14)
                42 3.14 0 0.0 123 99.99
                   ^^^^

                token: IntLiteral(0)
                42 3.14 0 0.0 123 99.99
                        ^

                token: FloatLiteral(0.0)
                42 3.14 0 0.0 123 99.99
                          ^^^

                token: IntLiteral(123)
                42 3.14 0 0.0 123 99.99
                              ^^^

                token: FloatLiteral(99.99)
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
                token: LeftParen
                ( ) . ! == < >
                ^

                token: RightParen
                ( ) . ! == < >
                  ^

                token: Dot
                ( ) . ! == < >
                    ^

                token: Not
                ( ) . ! == < >
                      ^

                token: Eq
                ( ) . ! == < >
                        ^^

                token: LessThan
                ( ) . ! == < >
                           ^

                token: GreaterThan
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
                token: NotEq
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
                token: Identifier("x")
                x != y
                ^

                token: NotEq
                x != y
                  ^^

                token: Identifier("y")
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
                token: LessThanOrEqual
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
                token: Identifier("x")
                x <= y
                ^

                token: LessThanOrEqual
                x <= y
                  ^^

                token: Identifier("y")
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
                token: GreaterThanOrEqual
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
                token: Identifier("x")
                x >= y
                ^

                token: GreaterThanOrEqual
                x >= y
                  ^^

                token: Identifier("y")
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
                token: Identifier("foo")
                foo in true false _test var123
                ^^^

                token: In
                foo in true false _test var123
                    ^^

                token: True
                foo in true false _test var123
                       ^^^^

                token: False
                foo in true false _test var123
                            ^^^^^

                token: Identifier("_test")
                foo in true false _test var123
                                  ^^^^^

                token: Identifier("var123")
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
                token: TypeString
                String Int Float Bool TrustedHTML Array
                ^^^^^^

                token: TypeInt
                String Int Float Bool TrustedHTML Array
                       ^^^

                token: TypeFloat
                String Int Float Bool TrustedHTML Array
                           ^^^^^

                token: TypeBoolean
                String Int Float Bool TrustedHTML Array
                                 ^^^^

                token: TypeTrustedHTML
                String Int Float Bool TrustedHTML Array
                                      ^^^^^^^^^^^

                token: TypeArray
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
                token: TypeName("User")
                User Person MyType CustomRecord
                ^^^^

                token: TypeName("Person")
                User Person MyType CustomRecord
                     ^^^^^^

                token: TypeName("MyType")
                User Person MyType CustomRecord
                            ^^^^^^

                token: TypeName("CustomRecord")
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
                token: Identifier("foo")
                foo Foo bar Bar _test Test
                ^^^

                token: TypeName("Foo")
                foo Foo bar Bar _test Test
                    ^^^

                token: Identifier("bar")
                foo Foo bar Bar _test Test
                        ^^^

                token: TypeName("Bar")
                foo Foo bar Bar _test Test
                            ^^^

                token: Identifier("_test")
                foo Foo bar Bar _test Test
                                ^^^^^

                token: TypeName("Test")
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
                token: StringLiteral("hello")
                "hello" "world with spaces" ""
                ^^^^^^^

                token: StringLiteral("world with spaces")
                "hello" "world with spaces" ""
                        ^^^^^^^^^^^^^^^^^^^

                token: StringLiteral("")
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
                token: Identifier("user")
                user.name
                ^^^^

                token: Dot
                user.name
                    ^

                token: Identifier("name")
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
                token: Identifier("user")
                user.name == "admin"
                ^^^^

                token: Dot
                user.name == "admin"
                    ^

                token: Identifier("name")
                user.name == "admin"
                     ^^^^

                token: Eq
                user.name == "admin"
                          ^^

                token: StringLiteral("admin")
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
                token: Not
                !(foo == true)
                ^

                token: LeftParen
                !(foo == true)
                 ^

                token: Identifier("foo")
                !(foo == true)
                  ^^^

                token: Eq
                !(foo == true)
                      ^^

                token: True
                !(foo == true)
                         ^^^^

                token: RightParen
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
                token: LeftBracket
                [1, 2, 3]
                ^

                token: IntLiteral(1)
                [1, 2, 3]
                 ^

                token: Comma
                [1, 2, 3]
                  ^

                token: IntLiteral(2)
                [1, 2, 3]
                    ^

                token: Comma
                [1, 2, 3]
                     ^

                token: IntLiteral(3)
                [1, 2, 3]
                       ^

                token: RightBracket
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
                token: LeftBracket
                [1.0, 12.0, 343.0]
                ^

                token: FloatLiteral(1.0)
                [1.0, 12.0, 343.0]
                 ^^^

                token: Comma
                [1.0, 12.0, 343.0]
                    ^

                token: FloatLiteral(12.0)
                [1.0, 12.0, 343.0]
                      ^^^^

                token: Comma
                [1.0, 12.0, 343.0]
                          ^

                token: FloatLiteral(343.0)
                [1.0, 12.0, 343.0]
                            ^^^^^

                token: RightBracket
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
                token: LeftBracket
                []
                ^

                token: RightBracket
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
                token: Import
                import user_list::UserList
                ^^^^^^

                token: Identifier("user_list")
                import user_list::UserList
                       ^^^^^^^^^

                token: ColonColon
                import user_list::UserList
                                ^^

                token: TypeName("UserList")
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
                token: Record
                record User {name: String, age: Int}
                ^^^^^^

                token: TypeName("User")
                record User {name: String, age: Int}
                       ^^^^

                token: LeftBrace
                record User {name: String, age: Int}
                            ^

                token: Identifier("name")
                record User {name: String, age: Int}
                             ^^^^

                token: Colon
                record User {name: String, age: Int}
                                 ^

                token: TypeString
                record User {name: String, age: Int}
                                   ^^^^^^

                token: Comma
                record User {name: String, age: Int}
                                         ^

                token: Identifier("age")
                record User {name: String, age: Int}
                                           ^^^

                token: Colon
                record User {name: String, age: Int}
                                              ^

                token: TypeInt
                record User {name: String, age: Int}
                                                ^^^

                token: RightBrace
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
                token: Record
                record UserList {users: Array[User]}
                ^^^^^^

                token: TypeName("UserList")
                record UserList {users: Array[User]}
                       ^^^^^^^^

                token: LeftBrace
                record UserList {users: Array[User]}
                                ^

                token: Identifier("users")
                record UserList {users: Array[User]}
                                 ^^^^^

                token: Colon
                record UserList {users: Array[User]}
                                      ^

                token: TypeArray
                record UserList {users: Array[User]}
                                        ^^^^^

                token: LeftBracket
                record UserList {users: Array[User]}
                                             ^

                token: TypeName("User")
                record UserList {users: Array[User]}
                                              ^^^^

                token: RightBracket
                record UserList {users: Array[User]}
                                                  ^

                token: RightBrace
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
                token: Record
                record User {
                ^^^^^^

                token: TypeName("User")
                record User {
                       ^^^^

                token: LeftBrace
                record User {
                            ^

                token: Identifier("name")
                    name: String,
                    ^^^^

                token: Colon
                    name: String,
                        ^

                token: TypeString
                    name: String,
                          ^^^^^^

                token: Comma
                    name: String,
                                ^

                token: Identifier("age")
                    age: Int,
                    ^^^

                token: Colon
                    age: Int,
                       ^

                token: TypeInt
                    age: Int,
                         ^^^

                token: Comma
                    age: Int,
                            ^

                token: RightBrace
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
                token: Import
                import foo::Foo
                ^^^^^^

                token: Identifier("foo")
                import foo::Foo
                       ^^^

                token: ColonColon
                import foo::Foo
                          ^^

                token: TypeName("Foo")
                import foo::Foo
                            ^^^

                token: Import
                import bar::Bar
                ^^^^^^

                token: Identifier("bar")
                import bar::Bar
                       ^^^

                token: ColonColon
                import bar::Bar
                          ^^

                token: TypeName("Bar")
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
                token: Import
                import foo::Foo
                ^^^^^^

                token: Identifier("foo")
                import foo::Foo
                       ^^^

                token: ColonColon
                import foo::Foo
                          ^^

                token: TypeName("Foo")
                import foo::Foo
                            ^^^

                token: Record
                record Bar {name: String}
                ^^^^^^

                token: TypeName("Bar")
                record Bar {name: String}
                       ^^^

                token: LeftBrace
                record Bar {name: String}
                           ^

                token: Identifier("name")
                record Bar {name: String}
                            ^^^^

                token: Colon
                record Bar {name: String}
                                ^

                token: TypeString
                record Bar {name: String}
                                  ^^^^^^

                token: RightBrace
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
                token: Record
                record  User  {  name :  String  }
                ^^^^^^

                token: TypeName("User")
                record  User  {  name :  String  }
                        ^^^^

                token: LeftBrace
                record  User  {  name :  String  }
                              ^

                token: Identifier("name")
                record  User  {  name :  String  }
                                 ^^^^

                token: Colon
                record  User  {  name :  String  }
                                      ^

                token: TypeString
                record  User  {  name :  String  }
                                         ^^^^^^

                token: RightBrace
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
                token: Record
                record Data {matrix: Array[Array[Int]]}
                ^^^^^^

                token: TypeName("Data")
                record Data {matrix: Array[Array[Int]]}
                       ^^^^

                token: LeftBrace
                record Data {matrix: Array[Array[Int]]}
                            ^

                token: Identifier("matrix")
                record Data {matrix: Array[Array[Int]]}
                             ^^^^^^

                token: Colon
                record Data {matrix: Array[Array[Int]]}
                                   ^

                token: TypeArray
                record Data {matrix: Array[Array[Int]]}
                                     ^^^^^

                token: LeftBracket
                record Data {matrix: Array[Array[Int]]}
                                          ^

                token: TypeArray
                record Data {matrix: Array[Array[Int]]}
                                           ^^^^^

                token: LeftBracket
                record Data {matrix: Array[Array[Int]]}
                                                ^

                token: TypeInt
                record Data {matrix: Array[Array[Int]]}
                                                 ^^^

                token: RightBracket
                record Data {matrix: Array[Array[Int]]}
                                                    ^

                token: RightBracket
                record Data {matrix: Array[Array[Int]]}
                                                     ^

                token: RightBrace
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
                token: Record
                record my_record {field_name: String}
                ^^^^^^

                token: Identifier("my_record")
                record my_record {field_name: String}
                       ^^^^^^^^^

                token: LeftBrace
                record my_record {field_name: String}
                                 ^

                token: Identifier("field_name")
                record my_record {field_name: String}
                                  ^^^^^^^^^^

                token: Colon
                record my_record {field_name: String}
                                            ^

                token: TypeString
                record my_record {field_name: String}
                                              ^^^^^^

                token: RightBrace
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
                token: Match
                match foo
                ^^^^^

                token: Identifier("foo")
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
                token: Enum
                enum Color
                ^^^^

                token: TypeName("Color")
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

                token: LeftParen
                Some(x)
                    ^

                token: Identifier("x")
                Some(x)
                     ^

                token: RightParen
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
                token: TypeOption
                Option[String]
                ^^^^^^

                token: LeftBracket
                Option[String]
                      ^

                token: TypeString
                Option[String]
                       ^^^^^^

                token: RightBracket
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
                token: Identifier("foo")
                foo::bar::Baz
                ^^^

                token: ColonColon
                foo::bar::Baz
                   ^^

                token: Identifier("bar")
                foo::bar::Baz
                     ^^^

                token: ColonColon
                foo::bar::Baz
                        ^^

                token: TypeName("Baz")
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
                token: Import
                import foo
                ^^^^^^

                token: Identifier("foo")
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
                token: Arrow
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
                token: FatArrow
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
                token: Identifier("x")
                x => y
                ^

                token: FatArrow
                x => y
                  ^^

                token: Identifier("y")
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
                token: Identifier("x")
                x - y x -> y
                ^

                token: Minus
                x - y x -> y
                  ^

                token: Identifier("y")
                x - y x -> y
                    ^

                token: Identifier("x")
                x - y x -> y
                      ^

                token: Arrow
                x - y x -> y
                        ^^

                token: Identifier("y")
                x - y x -> y
                           ^
            "#]],
        );
    }

    #[test]
    fn should_accept_underscore_as_wildcard() {
        check(
            "_ _test __",
            expect![[r#"
                token: Underscore
                _ _test __
                ^

                token: Identifier("_test")
                _ _test __
                  ^^^^^

                token: Identifier("__")
                _ _test __
                        ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_line_comment() {
        check(
            "// this is a comment",
            expect![[r#"
                token: Comment("// this is a comment")
                // this is a comment
                ^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_comment_after_code() {
        check(
            "foo // comment",
            expect![[r#"
                token: Identifier("foo")
                foo // comment
                ^^^

                token: Comment("// comment")
                foo // comment
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_code_after_comment_on_next_line() {
        check(
            "// comment\nfoo",
            expect![[r#"
                token: Comment("// comment")
                // comment
                ^^^^^^^^^^

                token: Identifier("foo")
                foo
                ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_empty_comment() {
        check(
            "//",
            expect![[r#"
                token: Comment("//")
                //
                ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_comments() {
        check(
            "// first\n// second",
            expect![[r#"
                token: Comment("// first")
                // first
                ^^^^^^^^

                token: Comment("// second")
                // second
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_single_slash() {
        check(
            "foo / bar",
            expect![[r#"
                token: Identifier("foo")
                foo / bar
                ^^^

                token: Identifier("bar")
                foo / bar
                      ^^^

                error: Unexpected character: '/'
                foo / bar
                    ^
            "#]],
        );
    }

    #[test]
    fn should_accept_inclusive_range_operator() {
        check(
            "..=",
            expect![[r#"
                token: DotDotEq
                ..=
                ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_inclusive_range_in_expression() {
        check(
            "0..=10",
            expect![[r#"
                token: IntLiteral(0)
                0..=10
                ^

                token: DotDotEq
                0..=10
                 ^^^

                token: IntLiteral(10)
                0..=10
                    ^^
            "#]],
        );
    }

    #[test]
    fn should_tokenize_double_dot_as_two_dots() {
        check(
            "..",
            expect![[r#"
                token: Dot
                ..
                ^

                token: Dot
                ..
                 ^
            "#]],
        );
    }

    #[test]
    fn should_accept_reserved_keywords() {
        check(
            indoc! {"
                alias
                and
                as
                assert
                async
                auto
                await
                break
                case
                catch
                comp
                component
                const
                constructor
                continue
                default
                defer
                elif
                else
                export
                extends
                final
                finally
                fn
                from
                func
                get
                if
                impl
                implements
                include
                interface
                internal
                is
                let
                loop
                mod
                mut
                namespace
                new
                newtype
                nil
                not
                null
                of
                or
                out
                package
                priv
                private
                pub
                public
                readonly
                return
                self
                set
                static
                struct
                super
                this
                throw
                trait
                try
                undefined
                use
                val
                var
                void
                when
                where
                while
                yield
            "},
            expect![[r#"
                token: Reserved("alias")
                alias
                ^^^^^

                token: Reserved("and")
                and
                ^^^

                token: Reserved("as")
                as
                ^^

                token: Reserved("assert")
                assert
                ^^^^^^

                token: Reserved("async")
                async
                ^^^^^

                token: Reserved("auto")
                auto
                ^^^^

                token: Reserved("await")
                await
                ^^^^^

                token: Reserved("break")
                break
                ^^^^^

                token: Reserved("case")
                case
                ^^^^

                token: Reserved("catch")
                catch
                ^^^^^

                token: Reserved("comp")
                comp
                ^^^^

                token: Reserved("component")
                component
                ^^^^^^^^^

                token: Reserved("const")
                const
                ^^^^^

                token: Reserved("constructor")
                constructor
                ^^^^^^^^^^^

                token: Reserved("continue")
                continue
                ^^^^^^^^

                token: Reserved("default")
                default
                ^^^^^^^

                token: Reserved("defer")
                defer
                ^^^^^

                token: Reserved("elif")
                elif
                ^^^^

                token: Reserved("else")
                else
                ^^^^

                token: Reserved("export")
                export
                ^^^^^^

                token: Reserved("extends")
                extends
                ^^^^^^^

                token: Reserved("final")
                final
                ^^^^^

                token: Reserved("finally")
                finally
                ^^^^^^^

                token: Reserved("fn")
                fn
                ^^

                token: Reserved("from")
                from
                ^^^^

                token: Reserved("func")
                func
                ^^^^

                token: Reserved("get")
                get
                ^^^

                token: Reserved("if")
                if
                ^^

                token: Reserved("impl")
                impl
                ^^^^

                token: Reserved("implements")
                implements
                ^^^^^^^^^^

                token: Reserved("include")
                include
                ^^^^^^^

                token: Reserved("interface")
                interface
                ^^^^^^^^^

                token: Reserved("internal")
                internal
                ^^^^^^^^

                token: Reserved("is")
                is
                ^^

                token: Reserved("let")
                let
                ^^^

                token: Reserved("loop")
                loop
                ^^^^

                token: Reserved("mod")
                mod
                ^^^

                token: Reserved("mut")
                mut
                ^^^

                token: Reserved("namespace")
                namespace
                ^^^^^^^^^

                token: Reserved("new")
                new
                ^^^

                token: Reserved("newtype")
                newtype
                ^^^^^^^

                token: Reserved("nil")
                nil
                ^^^

                token: Reserved("not")
                not
                ^^^

                token: Reserved("null")
                null
                ^^^^

                token: Reserved("of")
                of
                ^^

                token: Reserved("or")
                or
                ^^

                token: Reserved("out")
                out
                ^^^

                token: Reserved("package")
                package
                ^^^^^^^

                token: Reserved("priv")
                priv
                ^^^^

                token: Reserved("private")
                private
                ^^^^^^^

                token: Reserved("pub")
                pub
                ^^^

                token: Reserved("public")
                public
                ^^^^^^

                token: Reserved("readonly")
                readonly
                ^^^^^^^^

                token: Reserved("return")
                return
                ^^^^^^

                token: Reserved("self")
                self
                ^^^^

                token: Reserved("set")
                set
                ^^^

                token: Reserved("static")
                static
                ^^^^^^

                token: Reserved("struct")
                struct
                ^^^^^^

                token: Reserved("super")
                super
                ^^^^^

                token: Reserved("this")
                this
                ^^^^

                token: Reserved("throw")
                throw
                ^^^^^

                token: Reserved("trait")
                trait
                ^^^^^

                token: Reserved("try")
                try
                ^^^

                token: Reserved("undefined")
                undefined
                ^^^^^^^^^

                token: Reserved("use")
                use
                ^^^

                token: Reserved("val")
                val
                ^^^

                token: Reserved("var")
                var
                ^^^

                token: Reserved("void")
                void
                ^^^^

                token: Reserved("when")
                when
                ^^^^

                token: Reserved("where")
                where
                ^^^^^

                token: Reserved("while")
                while
                ^^^^^

                token: Reserved("yield")
                yield
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_valid_escape_sequences_in_strings() {
        // Note: The expected output uses Debug format, so backslashes appear doubled
        check(
            r#""hello\nworld" "tab\there" "quote\"here" "back\\slash" "cr\rhere""#,
            expect![[r#"
                token: StringLiteral("hello\\nworld")
                "hello\nworld" "tab\there" "quote\"here" "back\\slash" "cr\rhere"
                ^^^^^^^^^^^^^^

                token: StringLiteral("tab\\there")
                "hello\nworld" "tab\there" "quote\"here" "back\\slash" "cr\rhere"
                               ^^^^^^^^^^^

                token: StringLiteral("quote\\\"here")
                "hello\nworld" "tab\there" "quote\"here" "back\\slash" "cr\rhere"
                                           ^^^^^^^^^^^^^

                token: StringLiteral("back\\\\slash")
                "hello\nworld" "tab\there" "quote\"here" "back\\slash" "cr\rhere"
                                                         ^^^^^^^^^^^^^

                token: StringLiteral("cr\\rhere")
                "hello\nworld" "tab\there" "quote\"here" "back\\slash" "cr\rhere"
                                                                       ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_invalid_escape_sequences_in_strings() {
        // Note: tokens appear first, then errors at the end
        check(
            r#""invalid\q" "also\xinvalid""#,
            expect![[r#"
                token: StringLiteral("invalid\\q")
                "invalid\q" "also\xinvalid"
                ^^^^^^^^^^^

                token: StringLiteral("also\\xinvalid")
                "invalid\q" "also\xinvalid"
                            ^^^^^^^^^^^^^^^

                error: Invalid escape sequence '\q'
                "invalid\q" "also\xinvalid"
                        ^^

                error: Invalid escape sequence '\x'
                "invalid\q" "also\xinvalid"
                                 ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_trailing_backslash_in_string() {
        check(
            r#""trailing\"#,
            expect![[r#"
                error: Invalid escape sequence at end of string
                "trailing\
                         ^

                error: Unterminated string literal
                "trailing\
                ^^^^^^^^^^
            "#]],
        );
    }
}
