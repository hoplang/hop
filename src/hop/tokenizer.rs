use std::fmt::{self, Display};
use std::iter::Peekable;
use std::mem;

use itertools::Itertools as _;

use crate::document::document_cursor::{DocumentCursor, DocumentRange, Ranged};
use crate::dop::DopTokenizer;
use crate::dop::tokenizer::DopToken;
use crate::hop::ast::Attribute;
use crate::hop::parse_error::ParseError;

#[derive(Debug)]
pub enum Token {
    Doctype {
        range: DocumentRange,
    },
    Comment {
        range: DocumentRange,
    },
    Expression {
        expression: DocumentRange,
        range: DocumentRange,
    },
    OpeningTag {
        tag_name: DocumentRange,
        attributes: Vec<Attribute>,
        expression: Option<DocumentRange>,
        self_closing: bool,
        range: DocumentRange,
    },
    ClosingTag {
        tag_name: DocumentRange,
        range: DocumentRange,
    },
    Text {
        range: DocumentRange,
    },
}

impl Token {
    pub fn get_attribute_value(&self, name: &str) -> Option<DocumentRange> {
        match self {
            Token::OpeningTag { attributes, .. } => attributes
                .iter()
                .find(|attr| attr.name.as_str() == name)
                .and_then(|attr| attr.value.clone()),
            _ => None,
        }
    }
}

impl Ranged for Token {
    fn range(&self) -> &DocumentRange {
        match self {
            Token::Doctype { range }
            | Token::Comment { range }
            | Token::Expression { range, .. }
            | Token::OpeningTag { range, .. }
            | Token::ClosingTag { range, .. }
            | Token::Text { range } => range,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Text { range: value } => {
                write!(
                    f,
                    "Text [{} byte, {:#?}]",
                    value.as_str().len(),
                    value.to_string()
                )
            }
            Token::Doctype { .. } => {
                write!(f, "Doctype")
            }
            Token::ClosingTag { tag_name, .. } => {
                write!(f, "ClosingTag </{}>", tag_name)
            }
            Token::OpeningTag {
                tag_name,
                attributes,
                expression,
                self_closing,
                ..
            } => {
                write!(f, "OpeningTag <{}", tag_name)?;

                if !attributes.is_empty() {
                    write!(f, " ")?;
                    let attr_strs: Vec<String> = attributes
                        .iter()
                        .map(|attr| {
                            if let Some(val) = &attr.value {
                                format!("{}={:#?}", attr.name, val.to_string())
                            } else {
                                attr.name.to_string()
                            }
                        })
                        .collect();
                    write!(f, "{}", attr_strs.join(" "))?;
                }

                if let Some(expr) = expression {
                    write!(f, " expr={:#?}", expr.to_string())?;
                }

                if *self_closing {
                    write!(f, "/")?;
                }
                write!(f, ">")
            }
            Token::Comment { .. } => {
                write!(f, "Comment")
            }
            Token::Expression { expression, .. } => {
                write!(f, "Expression {:#?}", expression.to_string())
            }
        }
    }
}

pub struct Tokenizer {
    /// The string cursor for the document we're tokenizing.
    iter: Peekable<DocumentCursor>,
    /// The error vector contains the errors that occured during tokenization
    /// of the current token. It is returned together for the iterator.
    errors: Vec<ParseError>,
    /// The current raw text closing tag we're looking for, if any.
    /// E.g. </script>
    raw_text_closing_tag: Option<DocumentRange>,
}

impl Tokenizer {
    pub fn new(input: String) -> Self {
        Self {
            iter: DocumentCursor::new(input).peekable(),
            errors: Vec::new(),
            raw_text_closing_tag: None,
        }
    }

    /// Find the end of an expression using the dop tokenizer.
    ///
    /// Expects the current char iterator be on the first character
    /// of a dop expression.
    ///
    /// E.g. {x + 2}
    ///       ^
    ///
    /// Returns None if we reached EOF before finding the closing '}'.
    fn find_expression_end(iter: Peekable<DocumentCursor>) -> Option<DocumentRange> {
        let mut dop_tokenizer = DopTokenizer::from(iter);
        let mut open_braces = 1;
        loop {
            let token = dop_tokenizer.next()?;
            match token {
                Ok((DopToken::LeftBrace, _)) => {
                    open_braces += 1;
                }
                Ok((DopToken::RightBrace, range)) => {
                    open_braces -= 1;
                    if open_braces == 0 {
                        return Some(range);
                    }
                }
                _ => {}
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while self.iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            self.iter.next();
        }
    }

    // Parse a comment.
    //
    // E.g. <!-- hello -->
    //        ^^^^^^^^^^^^
    // Expects that the iterator points to the initial '-'.
    //
    fn parse_comment(&mut self, left_angle_to_bang: DocumentRange) -> Option<Token> {
        let Some(first_dash) = self.iter.next_if(|s| s.ch() == '-') else {
            panic!(
                "Expected '-' in parse_comment but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };
        let Some(second_dash) = self.iter.next_if(|s| s.ch() == '-') else {
            self.errors.push(ParseError::new(
                "Invalid markup declaration".to_string(),
                left_angle_to_bang.to(first_dash),
            ));
            return None;
        };
        // Count the number of seen '-' to find the end of the comment
        let mut count = 0;
        loop {
            match self.iter.next() {
                Some(s) if s.ch() == '-' => {
                    count += 1;
                }
                Some(s) if s.ch() == '>' => {
                    if count >= 2 {
                        return Some(Token::Comment {
                            range: left_angle_to_bang.to(s),
                        });
                    } else {
                        count = 0;
                    }
                }
                Some(_) => {
                    count = 0;
                }
                None => {
                    self.errors.push(ParseError::new(
                        "Unterminated comment".to_string(),
                        left_angle_to_bang.to(second_dash),
                    ));
                    return None;
                }
            }
        }
    }

    fn parse_doctype(&mut self, left_angle_to_bang: DocumentRange) -> Option<Token> {
        let doctype = self
            .iter
            .clone()
            .map(|s| s.ch())
            .take(7)
            .collect::<String>()
            .to_lowercase();
        if doctype != "doctype" {
            self.errors.push(ParseError::new(
                "Invalid markup declaration".to_string(),
                left_angle_to_bang,
            ));
            return None;
        }
        while self.iter.next_if(|s| s.ch() != '>').is_some() {}
        let Some(right_angle) = self.iter.next_if(|s| s.ch() == '>') else {
            self.errors.push(ParseError::new(
                "Invalid markup declaration".to_string(),
                left_angle_to_bang,
            ));
            return None;
        };
        Some(Token::Doctype {
            range: left_angle_to_bang.to(right_angle),
        })
    }

    // Parse a markup declaration from the iterator.
    //
    // E.g. <!-- hello -->
    //       ^^^^^^^^^^^^^
    // Expects that the iterator points to the initial '!'.
    //
    fn parse_markup_declaration(&mut self, left_angle: DocumentRange) -> Option<Token> {
        let Some(bang) = self.iter.next_if(|s| s.ch() == '!') else {
            panic!(
                "Expected '!' in parse_markup_declaration but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };
        match self.iter.peek().map(|s| s.ch()) {
            Some('-') => self.parse_comment(left_angle.to(bang)),
            Some('D' | 'd') => self.parse_doctype(left_angle.to(bang)),
            _ => {
                self.errors.push(ParseError::new(
                    "Invalid markup declaration".to_string(),
                    left_angle.to(bang),
                ));
                None
            }
        }
    }

    // Parse an attribute from the iterator.
    //
    // E.g. <div foo="bar">
    //           ^^^^^^^^^
    // Expects that the iterator points to the initial alphabetic char.
    //
    // Returns None if a valid attribute could not be parsed from the iterator.
    fn parse_attribute(&mut self) -> Option<Attribute> {
        // consume: [a-zA-Z]
        let Some(initial) = self.iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
            panic!(
                "Expected [a-zA-Z] in parse_attribute but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };

        // consume: ('-' | [a-zA-Z0-9])*
        let attr_name = initial.extend(
            self.iter
                .peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()),
        );

        // consume: whitespace
        self.skip_whitespace();

        // consume: '='
        let Some(eq) = self.iter.next_if(|s| s.ch() == '=') else {
            return Some(Attribute {
                name: attr_name.clone(),
                value: None,
                range: attr_name,
            });
        };

        // consume: whitespace
        self.skip_whitespace();

        // consume: " or '
        let Some(open_quote) = self.iter.next_if(|s| s.ch() == '"' || s.ch() == '\'') else {
            self.errors.push(ParseError::new(
                "Expected quoted attribute value".to_string(),
                attr_name.to(eq),
            ));
            return None;
        };

        // consume: attribute value
        let attr_value = self
            .iter
            .peeking_take_while(|s| s.ch() != open_quote.ch())
            .collect();

        // consume: " or '
        let Some(close_quote) = self.iter.next_if(|s| s.ch() == open_quote.ch()) else {
            self.errors
                .push(ParseError::UnmatchedCharacter { ch: open_quote });
            return None;
        };

        Some(Attribute {
            name: attr_name.clone(),
            value: attr_value,
            range: attr_name.to(close_quote),
        })
    }

    /// Parse an expression.
    ///
    /// E.g. <div foo="bar" {x: string}>
    ///                     ^^^^^^^^^^^
    /// Expects that the iterator points to the initial '{'.
    ///
    /// Returns None if we reached EOF or if the expression was empty.
    /// Returns Some((expr,range)) if we managed to parse the expression
    /// where expr is the inner range for the expression and range is the
    /// outer (containing the braces).
    fn parse_expression(&mut self) -> Option<(DocumentRange, DocumentRange)> {
        // consume '{'
        let Some(left_brace) = self.iter.next_if(|s| s.ch() == '{') else {
            panic!(
                "Expected '{{' in parse_expression but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };
        let clone = self.iter.clone();
        let Some(found_right_brace) = Self::find_expression_end(clone) else {
            self.errors
                .push(ParseError::UnmatchedCharacter { ch: left_brace });
            return None;
        };
        // handle empty expression
        if left_brace.end() == found_right_brace.start() {
            let Some(right_brace) = self.iter.next_if(|s| s.ch() == '}') else {
                panic!(
                    "Expected '}}' in parse_expression but got {:?}",
                    self.iter.next().map(|s| s.ch())
                );
            };
            self.errors.push(ParseError::new(
                "Empty expression".to_string(),
                left_brace.to(right_brace),
            ));
            return None;
        }
        let mut expr = self.iter.next().unwrap();
        while self.iter.peek().unwrap().start() != found_right_brace.start() {
            expr = expr.to(self.iter.next()?);
        }
        // consume: '}'
        let Some(right_brace) = self.iter.next_if(|s| s.ch() == '}') else {
            panic!(
                "Expected '}}' in parse_expression but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };
        Some((expr, left_brace.to(right_brace)))
    }

    /// Parse tag content (attributes and expressions).
    ///
    /// E.g. <div foo="bar" {x: string}>
    ///           ^^^^^^^^^^^^^^^^^^^^^
    fn parse_tag_content(&mut self) -> (Vec<Attribute>, Option<DocumentRange>) {
        let mut attributes: Vec<Attribute> = Vec::new();
        let mut expression: Option<DocumentRange> = None;
        loop {
            self.skip_whitespace();
            match self.iter.peek().map(|s| s.ch()) {
                Some('{') => {
                    let Some((expr, _)) = self.parse_expression() else {
                        continue;
                    };
                    expression = Some(expr);
                }
                Some(ch) if ch.is_ascii_alphabetic() => {
                    let Some(attr) = self.parse_attribute() else {
                        continue;
                    };
                    let exists = attributes
                        .iter()
                        .any(|a| a.name.as_str() == attr.name.as_str());
                    if exists {
                        self.errors.push(ParseError::DuplicateAttribute {
                            name: attr.name.to_string(),
                            range: attr.name.clone(),
                        });
                    } else {
                        attributes.push(attr);
                    }
                }
                _ => {
                    return (attributes, expression);
                }
            }
        }
    }

    /// Parse an opening tag.
    ///
    /// E.g. <div foo="bar" {x: string}>
    ///       ^^^^^^^^^^^^^^^^^^^^^^^^^^
    /// Expects that the iterator points to the initial alphabetic char.
    ///
    fn parse_opening_tag(&mut self, left_angle: DocumentRange) -> Option<Token> {
        // consume: [a-zA-Z]
        let Some(initial) = self.iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
            panic!(
                "Expected [a-zA-Z] in parse_opening_tag but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };

        // consume: ('-' | [a-zA-Z0-9])*
        let tag_name = initial.extend(
            self.iter
                .peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()),
        );
        // consume: tag content
        let (attributes, expression) = self.parse_tag_content();

        // consume: whitespace
        self.skip_whitespace();

        // consume: '/'
        let self_closing = self.iter.next_if(|s| s.ch() == '/').is_some();

        // consume: '>'
        let Some(right_angle) = self.iter.next_if(|s| s.ch() == '>') else {
            self.errors.push(ParseError::new(
                "Unterminated opening tag".to_string(),
                tag_name.clone(),
            ));
            return None;
        };

        if is_tag_name_with_raw_content(tag_name.as_str()) {
            self.raw_text_closing_tag = Some(tag_name.clone());
        }

        Some(Token::OpeningTag {
            self_closing,
            tag_name,
            attributes,
            expression,
            range: left_angle.to(right_angle),
        })
    }

    /// Parse a closing tag.
    ///
    /// E.g. <div></div>
    ///            ^^^^^
    /// Expects that the iterator points to the initial '/'.
    ///
    fn parse_closing_tag(&mut self, left_angle: DocumentRange) -> Option<Token> {
        let Some(slash) = self.iter.next_if(|s| s.ch() == '/') else {
            panic!(
                "Expected '/' in parse_closing_tag but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };

        // consume: whitespace
        self.skip_whitespace();

        // consume: [a-zA-Z]
        let Some(initial) = self.iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
            self.errors.push(ParseError::new(
                "Unterminated closing tag".to_string(),
                left_angle.to(slash),
            ));
            return None;
        };
        // consume: ('-' | [a-zA-Z0-9])*
        let tag_name = initial.extend(
            self.iter
                .peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()),
        );

        // consume: whitespace
        self.skip_whitespace();

        // consume: '>'
        let Some(right_angle) = self.iter.next_if(|s| s.ch() == '>') else {
            self.errors.push(ParseError::new(
                "Unterminated closing tag".to_string(),
                tag_name,
            ));
            return None;
        };
        Some(Token::ClosingTag {
            tag_name,
            range: left_angle.to(right_angle),
        })
    }

    fn iter_peek_rawtext_closing_tag(&self, tag_name: &DocumentRange) -> bool {
        let mut iter = self.iter.clone();
        // consume: '<'
        if iter.next().is_none_or(|s| s.ch() != '<') {
            return false;
        }

        // consume: '/'
        if iter.next().is_none_or(|s| s.ch() != '/') {
            return false;
        }

        // consume: whitespace
        while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            iter.next();
        }

        // consume: tag name
        for ch in tag_name.as_str().chars() {
            if iter.next().is_none_or(|s| s.ch() != ch) {
                return false;
            }
        }

        // consume: whitespace
        while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
            iter.next();
        }

        // consume: '>'
        if iter.next().is_none_or(|s| s.ch() != '>') {
            return false;
        }
        true
    }

    fn parse_rawtext(&mut self, tag_name: DocumentRange) -> Option<Token> {
        let mut raw_text: Option<DocumentRange> = None;
        loop {
            if self.iter.peek().is_none() || self.iter_peek_rawtext_closing_tag(&tag_name) {
                return raw_text.map(|s| Token::Text { range: s });
            }
            if let Some(ch) = self.iter.next() {
                raw_text = raw_text.into_iter().chain(Some(ch)).collect();
            }
        }
    }

    /// Parse a text expression.
    ///
    /// E.g.
    /// hello {name}!
    ///       ^^^^^^
    /// Expects that the iterator points to the initial '{'.
    ///
    fn parse_text_expression(&mut self) -> Option<Token> {
        self.parse_expression()
            .map(|(expression, range)| Token::Expression { expression, range })
    }

    /// Parse a text token.
    ///
    /// E.g. <div>hello</div>
    ///           ^^^^^
    /// Expects that the iterator points to the initial char.
    ///
    fn parse_text(&mut self) -> Option<Token> {
        let Some(initial) = self.iter.next() else {
            panic!("Expected an initial char in parse_text but got None");
        };
        Some(Token::Text {
            range: initial.extend(
                self.iter
                    .peeking_take_while(|s| s.ch() != '{' && s.ch() != '<'),
            ),
        })
    }

    fn parse_tag(&mut self) -> Option<Token> {
        let Some(left_angle) = self.iter.next() else {
            panic!(
                "Expected '<' in parse_tag but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };

        match self.iter.peek().map(|s| s.ch()) {
            Some('!') => self.parse_markup_declaration(left_angle),
            Some('/') => self.parse_closing_tag(left_angle),
            Some(ch) if ch.is_ascii_alphabetic() => self.parse_opening_tag(left_angle),
            _ => {
                self.errors.push(ParseError::new(
                    "Unterminated tag start".to_string(),
                    left_angle,
                ));
                None
            }
        }
    }

    fn step(&mut self) -> Option<Token> {
        // if we have a raw text closing tag stored
        // we need to parse all content as raw text
        // until we find a matching closing tag
        if let Some(tag_name) = self.raw_text_closing_tag.take() {
            // note that we should only return if parse_raw_text
            // returns an actual token
            if let Some(token) = self.parse_rawtext(tag_name) {
                return Some(token);
            }
        }

        match self.iter.peek().map(|s| s.ch()) {
            Some('<') => self.parse_tag(),
            Some('{') => self.parse_text_expression(),
            Some(_) => self.parse_text(),
            None => None,
        }
    }
}

impl Iterator for Tokenizer {
    type Item = (Option<Token>, Vec<ParseError>);

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.step();
        let errors = mem::take(&mut self.errors);
        if token.is_none() && errors.is_empty() {
            None
        } else {
            Some((token, errors))
        }
    }
}

fn is_tag_name_with_raw_content(name: &str) -> bool {
    matches!(name, "script" | "style" | "hop-x-raw")
}

#[cfg(test)]
mod tests {
    use crate::document::{DocumentAnnotator, SimpleAnnotation};

    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let tokenizer = Tokenizer::new(input.to_string());
        let result: Vec<_> = tokenizer.collect();
        let mut annotations = Vec::new();
        for (token, errors) in result {
            if let Some(t) = token {
                annotations.push(SimpleAnnotation {
                    message: t.to_string(),
                    range: t.range().clone(),
                });
            }
            for err in errors {
                annotations.push(SimpleAnnotation {
                    message: err.to_string(),
                    range: err.range().clone(),
                });
            }
        }

        expected.assert_eq(&DocumentAnnotator::new().annotate(None, annotations));
    }

    #[test]
    fn test_tokenize_empty() {
        check("", expect![""]);
    }

    #[test]
    fn test_tokenize_input_with_attributes() {
        check(
            r#"<input type="" value="" disabled="">"#,
            expect![[r#"
                OpeningTag <input type value disabled>
                1 | <input type="" value="" disabled="">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_raw_text_tricky() {
        check(
            r#"<script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>"#,
            expect![[r#"
                OpeningTag <script>
                1 | <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                  | ^^^^^^^^

                Text [62 byte, "</scri</<<</div></div><</scrip</scrip></cript></script</script"]
                1 | <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </script>
                1 | <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                  |                                                                       ^^^^^^^^^

                OpeningTag <div>
                1 | <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                  |                                                                                ^^^^^

                Text [6 byte, "works!"]
                1 | <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                  |                                                                                     ^^^^^^

                OpeningTag <div>
                1 | <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                  |                                                                                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_attributes_without_spaces() {
        check(
            r#"<h1 foo="bar"x="y">"#,
            expect![[r#"
                OpeningTag <h1 foo="bar" x="y">
                1 | <h1 foo="bar"x="y">
                  | ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_over_multiple_lines() {
        check(
            "this\ntext\nspans\nmultiple lines",
            expect![[r#"
                Text [30 byte, "this\ntext\nspans\nmultiple lines"]
                1 | this
                  | ^^^^
                2 | text
                  | ^^^^
                3 | spans
                  | ^^^^^
                4 | multiple lines
                  | ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_script_with_src() {
        check(
            r#"<script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>"#,
            expect![[r#"
                OpeningTag <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4">
                1 | <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </script>
                1 | <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                  |                                                                   ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_src() {
        check(
            indoc! {r#"
                <not-found-error entrypoint {path: string, available_routes: array[string]}>
                    <!DOCTYPE html>
                    <html>
                    <head>
                        <title>404 Not Found</title>
                        <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                        <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
                        <style>
                          body { font-family: "JetBrains Mono"; }
                        </style>
                    </head>
                    <body>
                        <page-container>
                            <error-not-found-error {requested_path: path, available_routes: available_routes} />
                        </page-container>
                    </body>
                    </html>
                </not-found-error>
            "#},
            expect![[r#"
                OpeningTag <not-found-error entrypoint expr="path: string, available_routes: array[string]">
                 1 | <not-found-error entrypoint {path: string, available_routes: array[string]}>
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [5 byte, "\n    "]
                 1 | <not-found-error entrypoint {path: string, available_routes: array[string]}>
                 2 |     <!DOCTYPE html>
                   | ^^^^

                Doctype
                 2 |     <!DOCTYPE html>
                   |     ^^^^^^^^^^^^^^^

                Text [5 byte, "\n    "]
                 2 |     <!DOCTYPE html>
                 3 |     <html>
                   | ^^^^

                OpeningTag <html>
                 3 |     <html>
                   |     ^^^^^^

                Text [5 byte, "\n    "]
                 3 |     <html>
                 4 |     <head>
                   | ^^^^

                OpeningTag <head>
                 4 |     <head>
                   |     ^^^^^^

                Text [9 byte, "\n        "]
                 4 |     <head>
                 5 |         <title>404 Not Found</title>
                   | ^^^^^^^^

                OpeningTag <title>
                 5 |         <title>404 Not Found</title>
                   |         ^^^^^^^

                Text [13 byte, "404 Not Found"]
                 5 |         <title>404 Not Found</title>
                   |                ^^^^^^^^^^^^^

                ClosingTag </title>
                 5 |         <title>404 Not Found</title>
                   |                             ^^^^^^^^

                Text [9 byte, "\n        "]
                 5 |         <title>404 Not Found</title>
                 6 |         <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                   | ^^^^^^^^

                OpeningTag <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4">
                 6 |         <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                   |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </script>
                 6 |         <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                   |                                                                           ^^^^^^^^^

                Text [9 byte, "\n        "]
                 6 |         <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                 7 |         <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
                   | ^^^^^^^^

                OpeningTag <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
                 7 |         <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
                   |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [9 byte, "\n        "]
                 7 |         <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
                 8 |         <style>
                   | ^^^^^^^^

                OpeningTag <style>
                 8 |         <style>
                   |         ^^^^^^^

                Text [59 byte, "\n          body { font-family: \"JetBrains Mono\"; }\n        "]
                 8 |         <style>
                 9 |           body { font-family: "JetBrains Mono"; }
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                10 |         </style>
                   | ^^^^^^^^

                ClosingTag </style>
                10 |         </style>
                   |         ^^^^^^^^

                Text [5 byte, "\n    "]
                10 |         </style>
                11 |     </head>
                   | ^^^^

                ClosingTag </head>
                11 |     </head>
                   |     ^^^^^^^

                Text [5 byte, "\n    "]
                11 |     </head>
                12 |     <body>
                   | ^^^^

                OpeningTag <body>
                12 |     <body>
                   |     ^^^^^^

                Text [9 byte, "\n        "]
                12 |     <body>
                13 |         <page-container>
                   | ^^^^^^^^

                OpeningTag <page-container>
                13 |         <page-container>
                   |         ^^^^^^^^^^^^^^^^

                Text [13 byte, "\n            "]
                13 |         <page-container>
                14 |             <error-not-found-error {requested_path: path, available_routes: available_routes} />
                   | ^^^^^^^^^^^^

                OpeningTag <error-not-found-error expr="requested_path: path, available_routes: available_routes"/>
                14 |             <error-not-found-error {requested_path: path, available_routes: available_routes} />
                   |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [9 byte, "\n        "]
                14 |             <error-not-found-error {requested_path: path, available_routes: available_routes} />
                15 |         </page-container>
                   | ^^^^^^^^

                ClosingTag </page-container>
                15 |         </page-container>
                   |         ^^^^^^^^^^^^^^^^^

                Text [5 byte, "\n    "]
                15 |         </page-container>
                16 |     </body>
                   | ^^^^

                ClosingTag </body>
                16 |     </body>
                   |     ^^^^^^^

                Text [5 byte, "\n    "]
                16 |     </body>
                17 |     </html>
                   | ^^^^

                ClosingTag </html>
                17 |     </html>
                   |     ^^^^^^^

                Text [1 byte, "\n"]
                17 |     </html>
                18 | </not-found-error>

                ClosingTag </not-found-error>
                18 | </not-found-error>
                   | ^^^^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                18 | </not-found-error>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_invalid_char_in_middle_of_tag() {
        check(
            "<div!>",
            expect![[r#"
                Unterminated opening tag
                1 | <div!>
                  |  ^^^

                Text [2 byte, "!>"]
                1 | <div!>
                  |     ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_with_attributes() {
        check(
            "<h1 foo bar/>",
            expect![[r#"
                OpeningTag <h1 foo bar/>
                1 | <h1 foo bar/>
                  | ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_with_expr_before_attributes() {
        check(
            "<h1 {foo: {k: string}} foo bar/>",
            expect![[r#"
                OpeningTag <h1 foo bar expr="foo: {k: string}"/>
                1 | <h1 {foo: {k: string}} foo bar/>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing() {
        check(
            "<h1 foo/>",
            expect![[r#"
                OpeningTag <h1 foo/>
                1 | <h1 foo/>
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_with_text() {
        check(
            indoc! {"
                <p><!-- -->
                </p>
            "},
            expect![[r#"
                OpeningTag <p>
                1 | <p><!-- -->
                  | ^^^

                Comment
                1 | <p><!-- -->
                  |    ^^^^^^^^

                Text [1 byte, "\n"]
                1 | <p><!-- -->
                2 | </p>

                ClosingTag </p>
                2 | </p>
                  | ^^^^

                Text [1 byte, "\n"]
                2 | </p>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_tricky() {
        check(
            indoc! {"
                <!-- ---><!-- ----><!----><!-----><!-- ---->
            "},
            expect![[r#"
                Comment
                1 | <!-- ---><!-- ----><!----><!-----><!-- ---->
                  | ^^^^^^^^^

                Comment
                1 | <!-- ---><!-- ----><!----><!-----><!-- ---->
                  |          ^^^^^^^^^^

                Comment
                1 | <!-- ---><!-- ----><!----><!-----><!-- ---->
                  |                    ^^^^^^^

                Comment
                1 | <!-- ---><!-- ----><!----><!-----><!-- ---->
                  |                           ^^^^^^^^

                Comment
                1 | <!-- ---><!-- ----><!----><!-----><!-- ---->
                  |                                   ^^^^^^^^^^

                Text [1 byte, "\n"]
                1 | <!-- ---><!-- ----><!----><!-----><!-- ---->
            "#]],
        );
    }

    #[test]
    fn test_tokenize_textarea_with_content() {
        check(
            indoc! {"
                <textarea>
                	<div></div>
                </textarea>
            "},
            expect![[r#"
                OpeningTag <textarea>
                1 | <textarea>
                  | ^^^^^^^^^^

                Text [2 byte, "\n\t"]
                1 | <textarea>
                2 |     <div></div>
                  | ^^^^

                OpeningTag <div>
                2 |     <div></div>
                  |     ^^^^^

                ClosingTag </div>
                2 |     <div></div>
                  |          ^^^^^^

                Text [1 byte, "\n"]
                2 |     <div></div>
                3 | </textarea>

                ClosingTag </textarea>
                3 | </textarea>
                  | ^^^^^^^^^^^

                Text [1 byte, "\n"]
                3 | </textarea>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_title_with_self_closing() {
        check(
            "<title><slot-title/></title>",
            expect![[r#"
                OpeningTag <title>
                1 | <title><slot-title/></title>
                  | ^^^^^^^

                OpeningTag <slot-title/>
                1 | <title><slot-title/></title>
                  |        ^^^^^^^^^^^^^

                ClosingTag </title>
                1 | <title><slot-title/></title>
                  |                     ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_simple() {
        check(
            "<p><!-- --></p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p><!-- --></p>
                  | ^^^

                Comment
                1 | <p><!-- --></p>
                  |    ^^^^^^^^

                ClosingTag </p>
                1 | <p><!-- --></p>
                  |            ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_standalone() {
        check(
            "<!-- Comment with -- dashes -- inside -->",
            expect![[r#"
                Comment
                1 | <!-- Comment with -- dashes -- inside -->
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiline_comment() {
        check(
            indoc! {"
                <p><!--
                A comment
                that stretches
                over several
                lines --></p>
            "},
            expect![[r#"
                OpeningTag <p>
                1 | <p><!--
                  | ^^^

                Comment
                1 | <p><!--
                  |    ^^^^
                2 | A comment
                  | ^^^^^^^^^
                3 | that stretches
                  | ^^^^^^^^^^^^^^
                4 | over several
                  | ^^^^^^^^^^^^
                5 | lines --></p>
                  | ^^^^^^^^^

                ClosingTag </p>
                5 | lines --></p>
                  |          ^^^^

                Text [1 byte, "\n"]
                5 | lines --></p>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_comment_with_quotes() {
        check(
            r#"<!-- This comment has <tags> and "quotes" and 'apostrophes' -->"#,
            expect![[r#"
                Comment
                1 | <!-- This comment has <tags> and "quotes" and 'apostrophes' -->
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_doctype() {
        check(
            "<!DOCTYPE   html>",
            expect![[r#"
                Doctype
                1 | <!DOCTYPE   html>
                  | ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_end_tag_with_space() {
        check(
            "</div >",
            expect![[r#"
                ClosingTag </div>
                1 | </div >
                  | ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_simple() {
        check(
            "<h1/>",
            expect![[r#"
                OpeningTag <h1/>
                1 | <h1/>
                  | ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_self_closing_with_space() {
        check(
            "<h1 />",
            expect![[r#"
                OpeningTag <h1/>
                1 | <h1 />
                  | ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_style_with_content() {
        check(
            indoc! {"
                <style>
                  body { color: red; }
                  .class { font-size: 12px; }
                </style>
            "},
            expect![[r#"
                OpeningTag <style>
                1 | <style>
                  | ^^^^^^^

                Text [54 byte, "\n  body { color: red; }\n  .class { font-size: 12px; }\n"]
                1 | <style>
                2 |   body { color: red; }
                  | ^^^^^^^^^^^^^^^^^^^^^^
                3 |   .class { font-size: 12px; }
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                4 | </style>

                ClosingTag </style>
                4 | </style>
                  | ^^^^^^^^

                Text [1 byte, "\n"]
                4 | </style>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiple_headings() {
        check(
            indoc! {"
                <h1></h1>
                <h2></h2>
                <h3></h3>
                <h4></h4>
                <h5></h5>
                <h6></h6>
            "},
            expect![[r#"
                OpeningTag <h1>
                1 | <h1></h1>
                  | ^^^^

                ClosingTag </h1>
                1 | <h1></h1>
                  |     ^^^^^

                Text [1 byte, "\n"]
                1 | <h1></h1>
                2 | <h2></h2>

                OpeningTag <h2>
                2 | <h2></h2>
                  | ^^^^

                ClosingTag </h2>
                2 | <h2></h2>
                  |     ^^^^^

                Text [1 byte, "\n"]
                2 | <h2></h2>
                3 | <h3></h3>

                OpeningTag <h3>
                3 | <h3></h3>
                  | ^^^^

                ClosingTag </h3>
                3 | <h3></h3>
                  |     ^^^^^

                Text [1 byte, "\n"]
                3 | <h3></h3>
                4 | <h4></h4>

                OpeningTag <h4>
                4 | <h4></h4>
                  | ^^^^

                ClosingTag </h4>
                4 | <h4></h4>
                  |     ^^^^^

                Text [1 byte, "\n"]
                4 | <h4></h4>
                5 | <h5></h5>

                OpeningTag <h5>
                5 | <h5></h5>
                  | ^^^^

                ClosingTag </h5>
                5 | <h5></h5>
                  |     ^^^^^

                Text [1 byte, "\n"]
                5 | <h5></h5>
                6 | <h6></h6>

                OpeningTag <h6>
                6 | <h6></h6>
                  | ^^^^

                ClosingTag </h6>
                6 | <h6></h6>
                  |     ^^^^^

                Text [1 byte, "\n"]
                6 | <h6></h6>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_multiline_attributes() {
        check(
            indoc! {r#"
                <div
                  class="multiline"
                  id="test"
                  data-value="something">
                </div>
            "#},
            expect![[r#"
                OpeningTag <div class="multiline" id="test" data-value="something">
                1 | <div
                  | ^^^^
                2 |   class="multiline"
                  | ^^^^^^^^^^^^^^^^^^^
                3 |   id="test"
                  | ^^^^^^^^^^^
                4 |   data-value="something">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                4 |   data-value="something">
                5 | </div>

                ClosingTag </div>
                5 | </div>
                  | ^^^^^^

                Text [1 byte, "\n"]
                5 | </div>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_script_with_content() {
        check(
            indoc! {r#"
                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </script>
            "#},
            expect![[r#"
                OpeningTag <script>
                1 | <script>
                  | ^^^^^^^^

                Text [65 byte, "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n"]
                1 | <script>
                2 |   const html = "<title>Nested</title>";
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                3 |   document.write(html);
                  | ^^^^^^^^^^^^^^^^^^^^^^^
                4 | </script>

                ClosingTag </script>
                4 | </script>
                  | ^^^^^^^^^

                Text [1 byte, "\n"]
                4 | </script>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_script_with_content_and_white_space_in_closing_tag() {
        check(
            indoc! {r#"
                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </ script>

                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </script  >

                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </ script  >
            "#},
            expect![[r#"
                OpeningTag <script>
                 1 | <script>
                   | ^^^^^^^^

                Text [65 byte, "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n"]
                 1 | <script>
                 2 |   const html = "<title>Nested</title>";
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 3 |   document.write(html);
                   | ^^^^^^^^^^^^^^^^^^^^^^^
                 4 | </ script>

                ClosingTag </script>
                 4 | </ script>
                   | ^^^^^^^^^^

                Text [2 byte, "\n\n"]
                 4 | </ script>
                 5 | 
                 6 | <script>

                OpeningTag <script>
                 6 | <script>
                   | ^^^^^^^^

                Text [65 byte, "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n"]
                 6 | <script>
                 7 |   const html = "<title>Nested</title>";
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 8 |   document.write(html);
                   | ^^^^^^^^^^^^^^^^^^^^^^^
                 9 | </script  >

                ClosingTag </script>
                 9 | </script  >
                   | ^^^^^^^^^^^

                Text [2 byte, "\n\n"]
                 9 | </script  >
                10 | 
                11 | <script>

                OpeningTag <script>
                11 | <script>
                   | ^^^^^^^^

                Text [65 byte, "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n"]
                11 | <script>
                12 |   const html = "<title>Nested</title>";
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                13 |   document.write(html);
                   | ^^^^^^^^^^^^^^^^^^^^^^^
                14 | </ script  >

                ClosingTag </script>
                14 | </ script  >
                   | ^^^^^^^^^^^^

                Text [1 byte, "\n"]
                14 | </ script  >
            "#]],
        );
    }

    #[test]
    fn test_tokenize_hop_x_raw_simple() {
        check(
            "<hop-x-raw>foo bar</hop-x-raw>",
            expect![[r#"
                OpeningTag <hop-x-raw>
                1 | <hop-x-raw>foo bar</hop-x-raw>
                  | ^^^^^^^^^^^

                Text [7 byte, "foo bar"]
                1 | <hop-x-raw>foo bar</hop-x-raw>
                  |            ^^^^^^^

                ClosingTag </hop-x-raw>
                1 | <hop-x-raw>foo bar</hop-x-raw>
                  |                   ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_hop_x_raw_with_html() {
        check(
            indoc! {"
                <hop-x-raw>
                  <div>some html</div>
                </hop-x-raw>
            "},
            expect![[r#"
                OpeningTag <hop-x-raw>
                1 | <hop-x-raw>
                  | ^^^^^^^^^^^

                Text [24 byte, "\n  <div>some html</div>\n"]
                1 | <hop-x-raw>
                2 |   <div>some html</div>
                  | ^^^^^^^^^^^^^^^^^^^^^^
                3 | </hop-x-raw>

                ClosingTag </hop-x-raw>
                3 | </hop-x-raw>
                  | ^^^^^^^^^^^^

                Text [1 byte, "\n"]
                3 | </hop-x-raw>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_adjacent_elements() {
        check(
            "<p></p><p></p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p></p><p></p>
                  | ^^^

                ClosingTag </p>
                1 | <p></p><p></p>
                  |    ^^^^

                OpeningTag <p>
                1 | <p></p><p></p>
                  |        ^^^

                ClosingTag </p>
                1 | <p></p><p></p>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_expression() {
        check(
            indoc! {r#"
                <main-comp {foo}>
                	<script>
                		const x = "<div></div>";
                	</script>
                </main-comp>
            "#},
            expect![[r#"
                OpeningTag <main-comp expr="foo">
                1 | <main-comp {foo}>
                  | ^^^^^^^^^^^^^^^^^

                Text [2 byte, "\n\t"]
                1 | <main-comp {foo}>
                2 |     <script>
                  | ^^^^

                OpeningTag <script>
                2 |     <script>
                  |     ^^^^^^^^

                Text [29 byte, "\n\t\tconst x = \"<div></div>\";\n\t"]
                2 |     <script>
                3 |         const x = "<div></div>";
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                4 |     </script>
                  | ^^^^

                ClosingTag </script>
                4 |     </script>
                  |     ^^^^^^^^^

                Text [1 byte, "\n"]
                4 |     </script>
                5 | </main-comp>

                ClosingTag </main-comp>
                5 | </main-comp>
                  | ^^^^^^^^^^^^

                Text [1 byte, "\n"]
                5 | </main-comp>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_full_html_document() {
        check(
            indoc! {r#"
                <!DOCTYPE html>
                <html>
                <head>
                  <title>My Page</title>
                </head>
                <body>
                  <div class="container">
                    Hello, world!
                  </div>
                </body>
                </html>
            "#},
            expect![[r#"
                Doctype
                 1 | <!DOCTYPE html>
                   | ^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                 1 | <!DOCTYPE html>
                 2 | <html>

                OpeningTag <html>
                 2 | <html>
                   | ^^^^^^

                Text [1 byte, "\n"]
                 2 | <html>
                 3 | <head>

                OpeningTag <head>
                 3 | <head>
                   | ^^^^^^

                Text [3 byte, "\n  "]
                 3 | <head>
                 4 |   <title>My Page</title>
                   | ^^

                OpeningTag <title>
                 4 |   <title>My Page</title>
                   |   ^^^^^^^

                Text [7 byte, "My Page"]
                 4 |   <title>My Page</title>
                   |          ^^^^^^^

                ClosingTag </title>
                 4 |   <title>My Page</title>
                   |                 ^^^^^^^^

                Text [1 byte, "\n"]
                 4 |   <title>My Page</title>
                 5 | </head>

                ClosingTag </head>
                 5 | </head>
                   | ^^^^^^^

                Text [1 byte, "\n"]
                 5 | </head>
                 6 | <body>

                OpeningTag <body>
                 6 | <body>
                   | ^^^^^^

                Text [3 byte, "\n  "]
                 6 | <body>
                 7 |   <div class="container">
                   | ^^

                OpeningTag <div class="container">
                 7 |   <div class="container">
                   |   ^^^^^^^^^^^^^^^^^^^^^^^

                Text [21 byte, "\n    Hello, world!\n  "]
                 7 |   <div class="container">
                 8 |     Hello, world!
                   | ^^^^^^^^^^^^^^^^^
                 9 |   </div>
                   | ^^

                ClosingTag </div>
                 9 |   </div>
                   |   ^^^^^^

                Text [1 byte, "\n"]
                 9 |   </div>
                10 | </body>

                ClosingTag </body>
                10 | </body>
                   | ^^^^^^^

                Text [1 byte, "\n"]
                10 | </body>
                11 | </html>

                ClosingTag </html>
                11 | </html>
                   | ^^^^^^^

                Text [1 byte, "\n"]
                11 | </html>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_svg_with_many_attributes() {
        check(
            indoc! {r#"
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                <line x1="12" y1="22.08" x2="12" y2="12"></line>
                </svg>
            "#},
            expect![[r#"
                OpeningTag <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>

                OpeningTag <line x1="16.5" y1="9.4" x2="7.5" y2="4.21">
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </line>
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                  |                                             ^^^^^^^

                Text [1 byte, "\n"]
                2 | <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>

                OpeningTag <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z">
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                  |                                                                                                                                     ^^^^^^^

                Text [1 byte, "\n"]
                3 | <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>

                OpeningTag <polyline points="3.27 6.96 12 12.01 20.73 6.96">
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </polyline>
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                  |                                                  ^^^^^^^^^^^

                Text [1 byte, "\n"]
                4 | <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>

                OpeningTag <line x1="12" y1="22.08" x2="12" y2="12">
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </line>
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>
                  |                                          ^^^^^^^

                Text [1 byte, "\n"]
                5 | <line x1="12" y1="22.08" x2="12" y2="12"></line>
                6 | </svg>

                ClosingTag </svg>
                6 | </svg>
                  | ^^^^^^

                Text [1 byte, "\n"]
                6 | </svg>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_nested_svg() {
        check(
            indoc! {r#"
                <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                	<g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                		<path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                		<path d="M17.54 47.09v48l35.099 12.775"></path>
                		<path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                	</g>
                </svg>
            "#},
            expect![[r#"
                OpeningTag <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [2 byte, "\n\t"]
                1 | <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                2 |     <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                  | ^^^^

                OpeningTag <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                2 |     <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [3 byte, "\n\t\t"]
                2 |     <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                  | ^^^^^^^^

                OpeningTag <path d="M20.04 38 64 22l43.96 16L64 54Z">
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                  |                                                   ^^^^^^^

                Text [3 byte, "\n\t\t"]
                3 |         <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                  | ^^^^^^^^

                OpeningTag <path d="M17.54 47.09v48l35.099 12.775">
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                  |                                                 ^^^^^^^

                Text [3 byte, "\n\t\t"]
                4 |         <path d="M17.54 47.09v48l35.099 12.775"></path>
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                  | ^^^^^^^^

                OpeningTag <path d="M64 112V64l46.46-16.91v48L77.988 106.91">
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </path>
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                  |                                                           ^^^^^^^

                Text [2 byte, "\n\t"]
                5 |         <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                6 |     </g>
                  | ^^^^

                ClosingTag </g>
                6 |     </g>
                  |     ^^^^

                Text [1 byte, "\n"]
                6 |     </g>
                7 | </svg>

                ClosingTag </svg>
                7 | </svg>
                  | ^^^^^^

                Text [1 byte, "\n"]
                7 | </svg>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_form_with_input() {
        check(
            indoc! {r#"
                <main-comp>
                	<form id="form">
                		<input type="text" required>
                		<button type="submit">Send</button>
                	</form>
                </main-comp>
            "#},
            expect![[r#"
                OpeningTag <main-comp>
                1 | <main-comp>
                  | ^^^^^^^^^^^

                Text [2 byte, "\n\t"]
                1 | <main-comp>
                2 |     <form id="form">
                  | ^^^^

                OpeningTag <form id="form">
                2 |     <form id="form">
                  |     ^^^^^^^^^^^^^^^^

                Text [3 byte, "\n\t\t"]
                2 |     <form id="form">
                3 |         <input type="text" required>
                  | ^^^^^^^^

                OpeningTag <input type="text" required>
                3 |         <input type="text" required>
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [3 byte, "\n\t\t"]
                3 |         <input type="text" required>
                4 |         <button type="submit">Send</button>
                  | ^^^^^^^^

                OpeningTag <button type="submit">
                4 |         <button type="submit">Send</button>
                  |         ^^^^^^^^^^^^^^^^^^^^^^

                Text [4 byte, "Send"]
                4 |         <button type="submit">Send</button>
                  |                               ^^^^

                ClosingTag </button>
                4 |         <button type="submit">Send</button>
                  |                                   ^^^^^^^^^

                Text [2 byte, "\n\t"]
                4 |         <button type="submit">Send</button>
                5 |     </form>
                  | ^^^^

                ClosingTag </form>
                5 |     </form>
                  |     ^^^^^^^

                Text [1 byte, "\n"]
                5 |     </form>
                6 | </main-comp>

                ClosingTag </main-comp>
                6 | </main-comp>
                  | ^^^^^^^^^^^^

                Text [1 byte, "\n"]
                6 | </main-comp>
            "#]],
        );
    }

    #[test]
    fn test_tokenize_if_with_expression() {
        check(
            "<if {foo}>",
            expect![[r#"
                OpeningTag <if expr="foo">
                1 | <if {foo}>
                  | ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_class_and_expression() {
        check(
            "<div class=\"test\" {bar}>",
            expect![[r#"
                OpeningTag <div class="test" expr="bar">
                1 | <div class="test" {bar}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_if_with_equality_expression() {
        check(
            "<if {user.name == 'John'}>",
            expect![[r#"
                OpeningTag <if expr="user.name == 'John'">
                1 | <if {user.name == 'John'}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_component_with_property_access() {
        check(
            "<component {obj.prop.subprop}>",
            expect![[r#"
                OpeningTag <component expr="obj.prop.subprop">
                1 | <component {obj.prop.subprop}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_button_with_attribute_and_expression() {
        check(
            "<button disabled {enabled == 'yes'}>",
            expect![[r#"
                OpeningTag <button disabled expr="enabled == 'yes'">
                1 | <button disabled {enabled == 'yes'}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_input_with_variable_name() {
        check(
            "<input {variable_name_123}>",
            expect![[r#"
                OpeningTag <input expr="variable_name_123">
                1 | <input {variable_name_123}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_spaced_expression() {
        check(
            "<div class=\"test\" {  user.name  }>",
            expect![[r#"
                OpeningTag <div class="test" expr="  user.name  ">
                1 | <div class="test" {  user.name  }>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_span_with_string_expression() {
        check(
            "<span {'hello world'}>",
            expect![[r#"
                OpeningTag <span expr="'hello world'">
                1 | <span {'hello world'}>
                  | ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_form_with_parenthesized_expression() {
        check(
            "<form {(user.role == 'admin')}>",
            expect![[r#"
                OpeningTag <form expr="(user.role == 'admin')">
                1 | <form {(user.role == 'admin')}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_section_with_chained_equality() {
        check(
            "<section {a == b == c}>",
            expect![[r#"
                OpeningTag <section expr="a == b == c">
                1 | <section {a == b == c}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_for_with_in_expression() {
        check(
            "<for {user in users}>",
            expect![[r#"
                OpeningTag <for expr="user in users">
                1 | <for {user in users}>
                  | ^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_for_with_property_access() {
        check(
            "<for {item in user.items}>",
            expect![[r#"
                OpeningTag <for expr="item in user.items">
                1 | <for {item in user.items}>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_div_with_in_expression() {
        check(
            "<div {foo in bars}>",
            expect![[r#"
                OpeningTag <div expr="foo in bars">
                1 | <div {foo in bars}>
                  | ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_single_expression() {
        check(
            "<h1>Hello {name}!</h1>",
            expect![[r#"
                OpeningTag <h1>
                1 | <h1>Hello {name}!</h1>
                  | ^^^^

                Text [6 byte, "Hello "]
                1 | <h1>Hello {name}!</h1>
                  |     ^^^^^^

                Expression "name"
                1 | <h1>Hello {name}!</h1>
                  |           ^^^^^^

                Text [1 byte, "!"]
                1 | <h1>Hello {name}!</h1>
                  |                 ^

                ClosingTag </h1>
                1 | <h1>Hello {name}!</h1>
                  |                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_unrecognized_expressions() {
        check(
            "{ ~ } {{ ~ }} {{{{   }}}{}{{}}}",
            expect![[r#"
                Expression " ~ "
                1 | { ~ } {{ ~ }} {{{{   }}}{}{{}}}
                  | ^^^^^

                Text [1 byte, " "]
                1 | { ~ } {{ ~ }} {{{{   }}}{}{{}}}
                  |      ^

                Expression "{ ~ }"
                1 | { ~ } {{ ~ }} {{{{   }}}{}{{}}}
                  |       ^^^^^^^

                Text [1 byte, " "]
                1 | { ~ } {{ ~ }} {{{{   }}}{}{{}}}
                  |              ^

                Expression "{{{   }}}{}{{}}"
                1 | { ~ } {{ ~ }} {{{{   }}}{}{{}}}
                  |               ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_multiple_expressions() {
        check(
            "<p>User {user.name} has {user.count} items</p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p>User {user.name} has {user.count} items</p>
                  | ^^^

                Text [5 byte, "User "]
                1 | <p>User {user.name} has {user.count} items</p>
                  |    ^^^^^

                Expression "user.name"
                1 | <p>User {user.name} has {user.count} items</p>
                  |         ^^^^^^^^^^^

                Text [5 byte, " has "]
                1 | <p>User {user.name} has {user.count} items</p>
                  |                    ^^^^^

                Expression "user.count"
                1 | <p>User {user.name} has {user.count} items</p>
                  |                         ^^^^^^^^^^^^

                Text [6 byte, " items"]
                1 | <p>User {user.name} has {user.count} items</p>
                  |                                     ^^^^^^

                ClosingTag </p>
                1 | <p>User {user.name} has {user.count} items</p>
                  |                                           ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_at_start() {
        check(
            "<span>{greeting} world!</span>",
            expect![[r#"
                OpeningTag <span>
                1 | <span>{greeting} world!</span>
                  | ^^^^^^

                Expression "greeting"
                1 | <span>{greeting} world!</span>
                  |       ^^^^^^^^^^

                Text [7 byte, " world!"]
                1 | <span>{greeting} world!</span>
                  |                 ^^^^^^^

                ClosingTag </span>
                1 | <span>{greeting} world!</span>
                  |                        ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_at_end() {
        check(
            "<div>Price: {price}</div>",
            expect![[r#"
                OpeningTag <div>
                1 | <div>Price: {price}</div>
                  | ^^^^^

                Text [7 byte, "Price: "]
                1 | <div>Price: {price}</div>
                  |      ^^^^^^^

                Expression "price"
                1 | <div>Price: {price}</div>
                  |             ^^^^^^^

                ClosingTag </div>
                1 | <div>Price: {price}</div>
                  |                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_expression_with_braces() {
        check(
            "<div>Price: {{k: v}}</div>",
            expect![[r#"
                OpeningTag <div>
                1 | <div>Price: {{k: v}}</div>
                  | ^^^^^

                Text [7 byte, "Price: "]
                1 | <div>Price: {{k: v}}</div>
                  |      ^^^^^^^

                Expression "{k: v}"
                1 | <div>Price: {{k: v}}</div>
                  |             ^^^^^^^^

                ClosingTag </div>
                1 | <div>Price: {{k: v}}</div>
                  |                     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_text_with_only_expression() {
        check(
            "<h2>{title}</h2>",
            expect![[r#"
                OpeningTag <h2>
                1 | <h2>{title}</h2>
                  | ^^^^

                Expression "title"
                1 | <h2>{title}</h2>
                  |     ^^^^^^^

                ClosingTag </h2>
                1 | <h2>{title}</h2>
                  |            ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_complex_expression_in_text() {
        check(
            "<p>Status: {user.profile.status == 'active'}</p>",
            expect![[r#"
                OpeningTag <p>
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  | ^^^

                Text [8 byte, "Status: "]
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  |    ^^^^^^^^

                Expression "user.profile.status == 'active'"
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  |            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag </p>
                1 | <p>Status: {user.profile.status == 'active'}</p>
                  |                                             ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_expression_with_property_access() {
        check(
            "<span>Item: {item.title}</span>",
            expect![[r#"
                OpeningTag <span>
                1 | <span>Item: {item.title}</span>
                  | ^^^^^^

                Text [6 byte, "Item: "]
                1 | <span>Item: {item.title}</span>
                  |       ^^^^^^

                Expression "item.title"
                1 | <span>Item: {item.title}</span>
                  |             ^^^^^^^^^^^^

                ClosingTag </span>
                1 | <span>Item: {item.title}</span>
                  |                         ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_mixed_tag_and_text_expressions() {
        check(
            "<div {className}>Content: {content}</div>",
            expect![[r#"
                OpeningTag <div expr="className">
                1 | <div {className}>Content: {content}</div>
                  | ^^^^^^^^^^^^^^^^^

                Text [9 byte, "Content: "]
                1 | <div {className}>Content: {content}</div>
                  |                  ^^^^^^^^^

                Expression "content"
                1 | <div {className}>Content: {content}</div>
                  |                           ^^^^^^^^^

                ClosingTag </div>
                1 | <div {className}>Content: {content}</div>
                  |                                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_error() {
        check(
            r#"<div class="foo" class="bar"></div>"#,
            expect![[r#"
                OpeningTag <div class="foo">
                1 | <div class="foo" class="bar"></div>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Duplicate attribute 'class'
                1 | <div class="foo" class="bar"></div>
                  |                  ^^^^^

                ClosingTag </div>
                1 | <div class="foo" class="bar"></div>
                  |                              ^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_different_quotes() {
        check(
            r#"<input type="text" type='number'/>"#,
            expect![[r#"
                OpeningTag <input type="text"/>
                1 | <input type="text" type='number'/>
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Duplicate attribute 'type'
                1 | <input type="text" type='number'/>
                  |                    ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_duplicate_attribute_no_value() {
        check(
            r#"<input required required />"#,
            expect![[r#"
                OpeningTag <input required/>
                1 | <input required required />
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Duplicate attribute 'required'
                1 | <input required required />
                  |                 ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_empty_expression() {
        check(
            r#"{}"#,
            expect![[r#"
                Empty expression
                1 | {}
                  | ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unterminated_expression() {
        check(
            r#"{"#,
            expect![[r#"
            Unmatched {
            1 | {
              | ^
        "#]],
        );
        check(
            r#"hello {"#,
            expect![[r#"
            Text [6 byte, "hello "]
            1 | hello {
              | ^^^^^^

            Unmatched {
            1 | hello {
              |       ^
        "#]],
        );
        check(
            r#"hello {foo"#,
            expect![[r#"
                Text [6 byte, "hello "]
                1 | hello {foo
                  | ^^^^^^

                Unmatched {
                1 | hello {foo
                  |       ^

                Text [3 byte, "foo"]
                1 | hello {foo
                  |        ^^^
            "#]],
        );
        check(
            r#"hello {{{foo}}"#,
            expect![[r#"
                Text [6 byte, "hello "]
                1 | hello {{{foo}}
                  | ^^^^^^

                Unmatched {
                1 | hello {{{foo}}
                  |       ^

                Expression "{foo}"
                1 | hello {{{foo}}
                  |        ^^^^^^^
            "#]],
        );
        check(
            r#"<div {>"#,
            expect![[r#"
                OpeningTag <div>
                1 | <div {>
                  | ^^^^^^^

                Unmatched {
                1 | <div {>
                  |      ^
            "#]],
        );
        check(
            r#"{{{}}{{{}}}{{{}}}{}"#,
            expect![[r#"
                Unmatched {
                1 | {{{}}{{{}}}{{{}}}{}
                  | ^

                Expression "{}"
                1 | {{{}}{{{}}}{{{}}}{}
                  |  ^^^^

                Expression "{{}}"
                1 | {{{}}{{{}}}{{{}}}{}
                  |      ^^^^^^

                Expression "{{}}"
                1 | {{{}}{{{}}}{{{}}}{}
                  |            ^^^^^^

                Empty expression
                1 | {{{}}{{{}}}{{{}}}{}
                  |                  ^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unterminated_comments() {
        check(
            r#"<!--"#,
            expect![[r#"
                Unterminated comment
                1 | <!--
                  | ^^^^
            "#]],
        );
        check(
            r#"<!-- foo bar"#,
            expect![[r#"
                Unterminated comment
                1 | <!-- foo bar
                  | ^^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unterminated_opening_tags() {
        check(
            r#"<div <div>"#,
            expect![[r#"
                Unterminated opening tag
                1 | <div <div>
                  |  ^^^

                OpeningTag <div>
                1 | <div <div>
                  |      ^^^^^
            "#]],
        );
        check(
            r#"<div class="foo" <div>"#,
            expect![[r#"
                Unterminated opening tag
                1 | <div class="foo" <div>
                  |  ^^^

                OpeningTag <div>
                1 | <div class="foo" <div>
                  |                  ^^^^^
            "#]],
        );
        check(
            r#"<div"#,
            expect![[r#"
            Unterminated opening tag
            1 | <div
              |  ^^^
        "#]],
        );
        check(
            r#"<div foo"#,
            expect![[r#"
            Unterminated opening tag
            1 | <div foo
              |  ^^^
        "#]],
        );
        check(
            r#"<div foo="#,
            expect![[r#"
                Expected quoted attribute value
                1 | <div foo=
                  |      ^^^^

                Unterminated opening tag
                1 | <div foo=
                  |  ^^^
            "#]],
        );
        check(
            r#"<div foo="""#,
            expect![[r#"
                Unterminated opening tag
                1 | <div foo=""
                  |  ^^^
            "#]],
        );
        check(
            r#"<div foo=""#,
            expect![[r#"
                Unmatched "
                1 | <div foo="
                  |          ^

                Unterminated opening tag
                1 | <div foo="
                  |  ^^^
            "#]],
        );
        check(
            r#"<div foo="bar"#,
            expect![[r#"
                Unmatched "
                1 | <div foo="bar
                  |          ^

                Unterminated opening tag
                1 | <div foo="bar
                  |  ^^^
            "#]],
        );
    }

    #[test]
    fn test_tokenize_unterminated_closing_tags() {
        check(
            r#"</div </div>"#,
            expect![[r#"
                Unterminated closing tag
                1 | </div </div>
                  |   ^^^

                ClosingTag </div>
                1 | </div </div>
                  |       ^^^^^^
            "#]],
        );
        check(
            r#"</div> </div"#,
            expect![[r#"
                ClosingTag </div>
                1 | </div> </div
                  | ^^^^^^

                Text [1 byte, " "]
                1 | </div> </div
                  |       ^

                Unterminated closing tag
                1 | </div> </div
                  |          ^^^
            "#]],
        );
        check(
            r#"</di<div>"#,
            expect![[r#"
                Unterminated closing tag
                1 | </di<div>
                  |   ^^

                OpeningTag <div>
                1 | </di<div>
                  |     ^^^^^
            "#]],
        );
        check(
            r#"</</div"#,
            expect![[r#"
                Unterminated closing tag
                1 | </</div
                  | ^^

                Unterminated closing tag
                1 | </</div
                  |     ^^^
            "#]],
        );
        check(
            r#"<div foo="#,
            expect![[r#"
                Expected quoted attribute value
                1 | <div foo=
                  |      ^^^^

                Unterminated opening tag
                1 | <div foo=
                  |  ^^^
            "#]],
        );
        check(
            r#"<div foo="""#,
            expect![[r#"
                Unterminated opening tag
                1 | <div foo=""
                  |  ^^^
            "#]],
        );
        check(
            r#"<div foo=""#,
            expect![[r#"
                Unmatched "
                1 | <div foo="
                  |          ^

                Unterminated opening tag
                1 | <div foo="
                  |  ^^^
            "#]],
        );
        check(
            r#"<div foo="bar"#,
            expect![[r#"
                Unmatched "
                1 | <div foo="bar
                  |          ^

                Unterminated opening tag
                1 | <div foo="bar
                  |  ^^^
            "#]],
        );
    }
}
