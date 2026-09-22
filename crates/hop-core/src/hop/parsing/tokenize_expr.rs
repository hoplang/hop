use crate::document::{DocumentCursor, DocumentRange};

use super::token::LangToken;
use crate::hop::uncooked_string::UncookedString;
use crate::parse_error::{Emit, ParseError, ParseErrorKind};

/// A single outcome of advancing the tokenizer.
pub enum LexStep {
    Token(LangToken, DocumentRange),
    Comment(DocumentRange),
    Error(ParseErrorKind, DocumentRange),
}

/// Advances the cursor past one token, comment, or lexical error.
///
/// Returns `None` if the cursor is exhausted or only has whitespace left.
pub fn step(iter: &mut DocumentCursor) -> Option<LexStep> {
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }

    let start = iter.next()?;

    Some(match start.ch() {
        '.' => {
            if let Some(third_dot) = iter.speculate(|iter| {
                iter.next_if(|s| s.ch() == '.')?;
                iter.next_if(|s| s.ch() == '.')
            }) {
                LexStep::Token(LangToken::DotDotDot, start.to(third_dot))
            } else if let Some(eq) = iter.speculate(|iter| {
                iter.next_if(|s| s.ch() == '.')?;
                iter.next_if(|s| s.ch() == '=')
            }) {
                LexStep::Token(LangToken::DotDotEq, start.to(eq))
            } else {
                LexStep::Token(LangToken::Dot, start)
            }
        }
        '(' => LexStep::Token(LangToken::LeftParen, start),
        ')' => LexStep::Token(LangToken::RightParen, start),
        '[' => LexStep::Token(LangToken::LeftBracket, start),
        ']' => LexStep::Token(LangToken::RightBracket, start),
        '{' => LexStep::Token(LangToken::LeftBrace, start),
        '}' => LexStep::Token(LangToken::RightBrace, start),
        ':' => match iter.next_if(|s| s.ch() == ':') {
            Some(end) => LexStep::Token(LangToken::ColonColon, start.to(end)),
            None => LexStep::Token(LangToken::Colon, start),
        },
        ',' => LexStep::Token(LangToken::Comma, start),
        ';' => LexStep::Token(LangToken::Semicolon, start),
        '#' => match iter.next_if(|s| s.ch() == '[') {
            Some(end) => LexStep::Token(LangToken::HashBracket, start.to(end)),
            None => LexStep::Error(ParseErrorKind::UnexpectedCharacter { ch: '#' }, start),
        },
        '+' => LexStep::Token(LangToken::Plus, start),
        '-' => match iter.next_if(|s| s.ch() == '>') {
            Some(end) => LexStep::Token(LangToken::Arrow, start.to(end)),
            None => LexStep::Token(LangToken::Minus, start),
        },
        '*' => LexStep::Token(LangToken::Asterisk, start),
        '&' => match iter.next_if(|s| s.ch() == '&') {
            Some(end) => LexStep::Token(LangToken::LogicalAnd, start.to(end)),
            None => LexStep::Error(ParseErrorKind::UnexpectedCharacter { ch: '&' }, start),
        },
        '|' => match iter.next_if(|s| s.ch() == '|') {
            Some(end) => LexStep::Token(LangToken::LogicalOr, start.to(end)),
            None => LexStep::Error(ParseErrorKind::UnexpectedCharacter { ch: '|' }, start),
        },
        '/' => match iter.next_if(|s| s.ch() == '/') {
            Some(second_slash) => LexStep::Comment(
                start
                    .to(second_slash)
                    .extend(iter.peeking_take_while(|s| s.ch() != '\n')),
            ),
            None => LexStep::Error(ParseErrorKind::UnexpectedCharacter { ch: '/' }, start),
        },
        '!' => match iter.next_if(|s| s.ch() == '=') {
            Some(end) => LexStep::Token(LangToken::NotEq, start.to(end)),
            None => LexStep::Token(LangToken::Not, start),
        },
        '<' => match iter.next_if(|s| s.ch() == '=') {
            Some(end) => LexStep::Token(LangToken::LessThanOrEqual, start.to(end)),
            None => LexStep::Token(LangToken::LessThan, start),
        },
        '>' => match iter.next_if(|s| s.ch() == '=') {
            Some(end) => LexStep::Token(LangToken::GreaterThanOrEqual, start.to(end)),
            None => LexStep::Token(LangToken::GreaterThan, start),
        },
        '=' => {
            if let Some(end) = iter.next_if(|s| s.ch() == '=') {
                LexStep::Token(LangToken::Eq, start.to(end))
            } else if let Some(end) = iter.next_if(|s| s.ch() == '>') {
                LexStep::Token(LangToken::FatArrow, start.to(end))
            } else {
                LexStep::Token(LangToken::Assign, start)
            }
        }
        '"' => {
            let mut content: Option<DocumentRange> = None;
            loop {
                let Some(ch) = iter.next() else {
                    return Some(LexStep::Error(
                        ParseErrorKind::UnterminatedStringLiteral {},
                        content.map(|c| start.clone().to(c)).unwrap_or(start),
                    ));
                };
                match ch.ch() {
                    '"' => {
                        break LexStep::Token(
                            LangToken::StringLiteral(UncookedString::new(content)),
                            start.to(ch),
                        );
                    }
                    '\\' => {
                        let backslash = ch;
                        let Some(escaped) = iter.next() else {
                            return Some(LexStep::Error(
                                ParseErrorKind::UnterminatedStringLiteral {},
                                start.to(backslash),
                            ));
                        };
                        content = content.into_iter().chain([backslash, escaped]).collect();
                    }
                    _ => content = content.into_iter().chain(Some(ch)).collect(),
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
                "_" => LangToken::Underscore,
                // Keywords
                "enum" => LangToken::Enum,
                "false" => LangToken::False,
                "fn" => LangToken::Fn,
                "for" => LangToken::For,
                "import" => LangToken::Import,
                "in" => LangToken::In,
                "let" => LangToken::Let,
                "match" => LangToken::Match,
                "page" => LangToken::Page,
                "pub" => LangToken::Pub,
                "record" => LangToken::Record,
                "true" => LangToken::True,
                // Constructors
                "None" => LangToken::None,
                "Some" => LangToken::Some,
                // Types
                "Array" => LangToken::TypeArray,
                "Bool" => LangToken::TypeBoolean,
                "Float" => LangToken::TypeFloat,
                "Html" => LangToken::TypeHtml,
                "Int" => LangToken::TypeInt,
                "Option" => LangToken::TypeOption,
                "String" => LangToken::TypeString,
                _ => LangToken::Identifier(identifier.to_cheap_string()),
            };
            LexStep::Token(t, identifier)
        }
        ch if ch.is_ascii_digit() => {
            let mut number_string =
                start.extend(iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
            // A '.' is part of the number only if a digit follows it.
            let fraction = iter.speculate(|iter| {
                iter.next_if(|s| s.ch() == '.')?;
                iter.next_if(|s| s.ch().is_ascii_digit())
            });
            let has_decimal = fraction.is_some();
            if let Some(first_digit) = fraction {
                number_string = number_string
                    .to(first_digit)
                    .extend(iter.peeking_take_while(|s| s.ch().is_ascii_digit()));
            }

            // Reject leading zeros (e.g. 000, 0123) but allow bare 0 and 0.x
            let s = number_string.as_str();
            let has_leading_zero = s.starts_with('0') && s.len() > 1 && !s.starts_with("0.");

            if has_leading_zero {
                LexStep::Error(ParseErrorKind::InvalidNumberFormat {}, number_string)
            } else if has_decimal {
                match number_string.as_str().parse::<f64>() {
                    Ok(f) => LexStep::Token(LangToken::FloatLiteral(f), number_string),
                    Err(_) => LexStep::Error(ParseErrorKind::InvalidNumberFormat {}, number_string),
                }
            } else {
                match number_string.as_str().parse::<i32>() {
                    Ok(i) => LexStep::Token(LangToken::IntLiteral(i), number_string),
                    Err(_) => {
                        LexStep::Error(ParseErrorKind::IntLiteralOutOfRange {}, number_string)
                    }
                }
            }
        }
        ch => LexStep::Error(ParseErrorKind::UnexpectedCharacter { ch }, start),
    })
}

/// Peeks at the next token without consuming it.
pub fn peek(iter: &DocumentCursor) -> Option<(LangToken, DocumentRange)> {
    next_ignoring_trivia(&mut iter.clone())
}

pub fn peek2(iter: &DocumentCursor) -> Option<(LangToken, DocumentRange)> {
    let mut cloned = iter.clone();
    next_ignoring_trivia(&mut cloned)?;
    next_ignoring_trivia(&mut cloned)
}

pub fn peek3(iter: &DocumentCursor) -> Option<(LangToken, DocumentRange)> {
    let mut cloned = iter.clone();
    next_ignoring_trivia(&mut cloned)?;
    next_ignoring_trivia(&mut cloned)?;
    next_ignoring_trivia(&mut cloned)
}

fn next_ignoring_trivia(iter: &mut DocumentCursor) -> Option<(LangToken, DocumentRange)> {
    loop {
        match step(iter)? {
            LexStep::Token(token, range) => return Some((token, range)),
            LexStep::Comment(_) | LexStep::Error(..) => {}
        }
    }
}

/// Returns the next token, collecting any comments encountered along the way
/// into the provided vec.
pub fn next(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<(LangToken, DocumentRange)> {
    loop {
        match step(iter)? {
            LexStep::Token(token, range) => return Some((token, range)),
            LexStep::Comment(range) => comments.push(range),
            LexStep::Error(kind, range) => {
                let _ = errors.emit(kind, range);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostic;
    use crate::diagnostic_severity::DiagnosticSeverity;
    use crate::{document_annotator::DocumentAnnotator, document_id::DocumentId};
    use expect_test::{Expect, expect};

    fn run_tokenizer(input: &str) -> (String, bool) {
        let mut cursor =
            DocumentCursor::new(DocumentId::new("test.hop").unwrap(), input.to_string());
        let mut errors = Vec::new();
        let mut comments = Vec::new();
        let mut annotations = Vec::new();
        while let Some((tok, range)) = next(&mut cursor, &mut comments, &mut errors) {
            annotations.push(Diagnostic::new(
                format!("token: {:?}", tok),
                range,
                DiagnosticSeverity::Error,
            ));
        }
        for range in comments {
            annotations.push(Diagnostic::new(
                format!("comment: {}", range.as_str()),
                range,
                DiagnosticSeverity::Error,
            ));
        }
        for err in &errors {
            let diagnostic = err.to_diagnostic();
            annotations.push(Diagnostic::new(
                format!("error: {}", diagnostic.message()),
                diagnostic.range().clone(),
                diagnostic.severity(),
            ));
        }
        let actual = DocumentAnnotator::new()
            .without_line_numbers()
            .annotate(annotations)
            .render();
        (actual, !errors.is_empty())
    }

    fn accept(input: &str, expected: Expect) {
        let (actual, has_errors) = run_tokenizer(input);
        if has_errors {
            panic!("expected no tokenizer errors, got:\n{actual}");
        }
        expected.assert_eq(&actual);
    }

    fn reject(input: &str, expected: Expect) {
        let (actual, has_errors) = run_tokenizer(input);
        if !has_errors {
            panic!("expected tokenizer errors but got none");
        }
        expected.assert_eq(&actual);
    }

    #[test]
    fn accepts_whitespace_between_tokens() {
        accept(
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
    fn accepts_multiline_input() {
        accept(
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
    fn rejects_integer_literal_beyond_i32_range() {
        reject(
            "2147483647 2147483648",
            expect![[r#"
                token: IntLiteral(2147483647)
                2147483647 2147483648
                ^^^^^^^^^^

                error: Integer literal is too large for Int (maximum is 2147483647)
                2147483647 2147483648
                           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_valid_float_numbers() {
        accept(
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
    fn rejects_for_invalid_number_formats() {
        // Numbers with leading zeros are invalid
        reject(
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
    fn accepts_trailing_dot_as_separate_token() {
        // `1.` is parsed as integer `1` followed by `.` (not an invalid float)
        // This allows range expressions like `0..=10`
        accept(
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
    fn accepts_single_equals_as_assign_token() {
        accept(
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
    fn rejects_unexpected_character() {
        reject(
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
    fn accepts_integer_literals() {
        accept(
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
    fn accepts_mixed_integers_and_floats() {
        accept(
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
    fn accepts_let_keyword_and_semicolon() {
        accept(
            "let letter = 1;",
            expect![[r#"
                token: Let
                let letter = 1;
                ^^^

                token: Identifier("letter")
                let letter = 1;
                    ^^^^^^

                token: Assign
                let letter = 1;
                           ^

                token: IntLiteral(1)
                let letter = 1;
                             ^

                token: Semicolon
                let letter = 1;
                              ^
            "#]],
        );
    }

    #[test]
    fn accepts_for_keyword() {
        accept(
            "for x in xs {}",
            expect![[r#"
                token: For
                for x in xs {}
                ^^^

                token: Identifier("x")
                for x in xs {}
                    ^

                token: In
                for x in xs {}
                      ^^

                token: Identifier("xs")
                for x in xs {}
                         ^^

                token: LeftBrace
                for x in xs {}
                            ^

                token: RightBrace
                for x in xs {}
                             ^
            "#]],
        );
    }

    #[test]
    fn accepts_simple_punctuation() {
        accept(
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
    fn accepts_not_equals_operator() {
        accept(
            "!=",
            expect![[r#"
                token: NotEq
                !=
                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_not_equals_in_expression() {
        accept(
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
    fn accepts_less_than_or_equal_operator() {
        accept(
            "<=",
            expect![[r#"
                token: LessThanOrEqual
                <=
                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_less_than_or_equal_in_expression() {
        accept(
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
    fn accepts_greater_than_or_equal_operator() {
        accept(
            ">=",
            expect![[r#"
                token: GreaterThanOrEqual
                >=
                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_greater_than_or_equal_in_expression() {
        accept(
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
    fn accepts_identifiers_and_keywords() {
        accept(
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
    fn accepts_type_keywords() {
        accept(
            "String Int Float Bool Html Array",
            expect![[r#"
                token: TypeString
                String Int Float Bool Html Array
                ^^^^^^

                token: TypeInt
                String Int Float Bool Html Array
                       ^^^

                token: TypeFloat
                String Int Float Bool Html Array
                           ^^^^^

                token: TypeBoolean
                String Int Float Bool Html Array
                                 ^^^^

                token: TypeHtml
                String Int Float Bool Html Array
                                      ^^^^

                token: TypeArray
                String Int Float Bool Html Array
                                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_custom_type_names() {
        accept(
            "User Person MyType CustomRecord",
            expect![[r#"
                token: Identifier("User")
                User Person MyType CustomRecord
                ^^^^

                token: Identifier("Person")
                User Person MyType CustomRecord
                     ^^^^^^

                token: Identifier("MyType")
                User Person MyType CustomRecord
                            ^^^^^^

                token: Identifier("CustomRecord")
                User Person MyType CustomRecord
                                   ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_identifiers_regardless_of_case() {
        accept(
            "foo Foo bar Bar _test Test",
            expect![[r#"
                token: Identifier("foo")
                foo Foo bar Bar _test Test
                ^^^

                token: Identifier("Foo")
                foo Foo bar Bar _test Test
                    ^^^

                token: Identifier("bar")
                foo Foo bar Bar _test Test
                        ^^^

                token: Identifier("Bar")
                foo Foo bar Bar _test Test
                            ^^^

                token: Identifier("_test")
                foo Foo bar Bar _test Test
                                ^^^^^

                token: Identifier("Test")
                foo Foo bar Bar _test Test
                                      ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_string_literals() {
        accept(
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
    fn accepts_field_access_expression() {
        accept(
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
    fn accepts_complex_expression() {
        accept(
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
    fn accepts_parenthesized_expression() {
        accept(
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
    fn accepts_array_with_brackets() {
        accept(
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
    fn accepts_array_of_floats() {
        accept(
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
    fn accepts_empty_array() {
        accept(
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
    fn accepts_import_statement() {
        accept(
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

                token: Identifier("UserList")
                import user_list::UserList
                                  ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_record_statement() {
        accept(
            "record User {name: String, age: Int}",
            expect![[r#"
                token: Record
                record User {name: String, age: Int}
                ^^^^^^

                token: Identifier("User")
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
    fn accepts_record_with_array_field() {
        accept(
            "record UserList {users: Array[User]}",
            expect![[r#"
                token: Record
                record UserList {users: Array[User]}
                ^^^^^^

                token: Identifier("UserList")
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

                token: Identifier("User")
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
    fn accepts_multiline_record() {
        accept(
            "record User {\n    name: String,\n    age: Int,\n}",
            expect![[r#"
                token: Record
                record User {
                ^^^^^^

                token: Identifier("User")
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
    fn accepts_multiple_import_declarations() {
        accept(
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

                token: Identifier("Foo")
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

                token: Identifier("Bar")
                import bar::Bar
                            ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_import_followed_by_record() {
        accept(
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

                token: Identifier("Foo")
                import foo::Foo
                            ^^^

                token: Record
                record Bar {name: String}
                ^^^^^^

                token: Identifier("Bar")
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
    fn accepts_varying_whitespace_between_tokens() {
        accept(
            "record  User  {  name :  String  }",
            expect![[r#"
                token: Record
                record  User  {  name :  String  }
                ^^^^^^

                token: Identifier("User")
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
    fn accepts_record_with_nested_array_types() {
        accept(
            "record Data {matrix: Array[Array[Int]]}",
            expect![[r#"
                token: Record
                record Data {matrix: Array[Array[Int]]}
                ^^^^^^

                token: Identifier("Data")
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
    fn accepts_identifiers_containing_underscores() {
        accept(
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
    fn accepts_match_keyword() {
        accept(
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
    fn accepts_fn_keyword() {
        accept(
            "fn foo",
            expect![[r#"
                token: Fn
                fn foo
                ^^

                token: Identifier("foo")
                fn foo
                   ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_keyword() {
        accept(
            "enum Color",
            expect![[r#"
                token: Enum
                enum Color
                ^^^^

                token: Identifier("Color")
                enum Color
                     ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_some_keyword() {
        accept(
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
    fn accepts_none_keyword() {
        accept(
            "None",
            expect![[r#"
                token: None
                None
                ^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_option_type() {
        accept(
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
    fn accepts_colon_colon_operator() {
        accept(
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

                token: Identifier("Baz")
                foo::bar::Baz
                          ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_import_keyword() {
        accept(
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
    fn accepts_arrow_operator() {
        accept(
            "->",
            expect![[r#"
                token: Arrow
                ->
                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_fat_arrow_operator() {
        accept(
            "=>",
            expect![[r#"
                token: FatArrow
                =>
                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_fat_arrow_in_expression() {
        accept(
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
    fn accepts_minus_distinct_from_arrow() {
        accept(
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
    fn accepts_underscore_as_wildcard() {
        accept(
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
    fn accepts_line_comment() {
        accept(
            "// this is a comment",
            expect![[r#"
                comment: // this is a comment
                // this is a comment
                ^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_comment_after_code() {
        accept(
            "foo // comment",
            expect![[r#"
                token: Identifier("foo")
                foo // comment
                ^^^

                comment: // comment
                foo // comment
                    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_code_after_comment_on_next_line() {
        accept(
            "// comment\nfoo",
            expect![[r#"
                comment: // comment
                // comment
                ^^^^^^^^^^

                token: Identifier("foo")
                foo
                ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_empty_comment() {
        accept(
            "//",
            expect![[r#"
                comment: //
                //
                ^^
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_comments() {
        accept(
            "// first\n// second",
            expect![[r#"
                comment: // first
                // first
                ^^^^^^^^

                comment: // second
                // second
                ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_single_slash() {
        reject(
            "foo / bar",
            expect![[r#"
                token: Identifier("foo")
                foo / bar
                ^^^

                error: Unexpected character: '/'
                foo / bar
                    ^

                token: Identifier("bar")
                foo / bar
                      ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_inclusive_range_operator() {
        accept(
            "..=",
            expect![[r#"
                token: DotDotEq
                ..=
                ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_inclusive_range_in_expression() {
        accept(
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
    fn accepts_double_dot_as_two_dots() {
        accept(
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
    fn accepts_valid_escape_sequences_in_strings() {
        // Note: The expected output uses Debug format, so backslashes appear doubled
        accept(
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
    fn accepts_unknown_escape_sequences() {
        accept(
            r#""invalid\q" "also\xinvalid""#,
            expect![[r#"
                token: StringLiteral("invalid\\q")
                "invalid\q" "also\xinvalid"
                ^^^^^^^^^^^

                token: StringLiteral("also\\xinvalid")
                "invalid\q" "also\xinvalid"
                            ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_trailing_backslash_in_string() {
        reject(
            r#""trailing\"#,
            expect![[r#"
                error: Unterminated string literal
                "trailing\
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_triple_dot() {
        accept(
            "...rest",
            expect![[r#"
                token: DotDotDot
                ...rest
                ^^^

                token: Identifier("rest")
                ...rest
                   ^^^^
            "#]],
        );
    }
}
