use std::fmt::{self, Display};
use std::iter::Peekable;
use std::mem;

use itertools::Itertools as _;

use super::parse_error::ParseError;
use crate::document::{DocumentCursor, DocumentRange, Ranged};
use crate::dop;

#[derive(Debug, Clone)]
pub enum TokenizedAttributeValue {
    /// A quoted string attribute value. Content is None for empty strings like `a=""`
    String {
        content: Option<DocumentRange>,
    },
    Expression(DocumentRange),
}

#[derive(Debug, Clone)]
pub struct TokenizedAttribute {
    pub name: DocumentRange,
    pub value: Option<TokenizedAttributeValue>,

    /// This is the range for the whole attribute,
    /// including possible quotes.
    ///
    /// E.g. <div foo="bar">
    ///           ^^^^^^^^^
    pub range: DocumentRange,
}

#[derive(Debug)]
pub enum Token {
    Doctype {
        range: DocumentRange,
    },
    Comment {
        range: DocumentRange,
    },
    OpeningTag {
        tag_name: DocumentRange,
        attributes: Vec<TokenizedAttribute>,
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

impl Ranged for Token {
    fn range(&self) -> &DocumentRange {
        match self {
            Token::Doctype { range }
            | Token::Comment { range }
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
                writeln!(f, "ClosingTag(")?;
                writeln!(f, "  tag_name: {:?},", tag_name.to_string())?;
                write!(f, ")")
            }
            Token::OpeningTag {
                tag_name,
                attributes,
                expression,
                self_closing,
                ..
            } => {
                writeln!(f, "OpeningTag(")?;
                writeln!(f, "  tag_name: {:?},", tag_name.to_string())?;
                write!(f, "  attributes: {{")?;
                if attributes.is_empty() {
                    writeln!(f, "}},")?;
                } else {
                    writeln!(f)?;
                    for attr in attributes {
                        let value_str = match &attr.value {
                            Some(TokenizedAttributeValue::String { content }) => {
                                let val =
                                    content.as_ref().map(|r| r.to_string()).unwrap_or_default();
                                format!("String({:?})", val)
                            }
                            Some(TokenizedAttributeValue::Expression(val)) => {
                                format!("Expression({:?})", val.to_string())
                            }
                            None => "None".to_string(),
                        };
                        writeln!(f, "    {}: {},", attr.name, value_str)?;
                    }
                    writeln!(f, "  }},")?;
                }
                let expr_str = match expression {
                    Some(expr) => format!("Some({:?})", expr.to_string()),
                    None => "None".to_string(),
                };
                writeln!(f, "  expression: {},", expr_str)?;
                writeln!(f, "  self_closing: {},", self_closing)?;
                write!(f, ")")
            }
            Token::Comment { .. } => {
                write!(f, "Comment")
            }
        }
    }
}

pub struct Tokenizer {
    /// The string cursor for the document we're tokenizing.
    iter: Peekable<DocumentCursor>,
    /// The error vector contains the errors that occured during tokenization
    /// of the current token. It is returned together with the token.
    errors: Vec<ParseError>,
    /// The current raw text closing tag we're looking for, if any.
    /// E.g. </script>
    raw_text_closing_tag: Option<DocumentRange>,
}

impl Tokenizer {
    pub fn new(input: DocumentCursor) -> Self {
        Self {
            iter: input.peekable(),
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
    fn find_expression_end(mut iter: Peekable<DocumentCursor>) -> Option<DocumentRange> {
        let mut open_braces = 1;
        loop {
            let token = dop::tokenizer::next(&mut iter)?;
            match token {
                Ok((dop::Token::LeftBrace, _)) => {
                    open_braces += 1;
                }
                Ok((dop::Token::RightBrace, range)) => {
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
            self.errors.push(ParseError::InvalidMarkupDeclaration {
                range: left_angle_to_bang.to(first_dash),
            });
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
                    self.errors.push(ParseError::UnterminatedComment {
                        range: left_angle_to_bang.to(second_dash),
                    });
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
            self.errors.push(ParseError::InvalidMarkupDeclaration {
                range: left_angle_to_bang,
            });
            return None;
        }
        while self.iter.next_if(|s| s.ch() != '>').is_some() {}
        let Some(right_angle) = self.iter.next_if(|s| s.ch() == '>') else {
            self.errors.push(ParseError::InvalidMarkupDeclaration {
                range: left_angle_to_bang,
            });
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
                self.errors.push(ParseError::InvalidMarkupDeclaration {
                    range: left_angle.to(bang),
                });
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
    fn parse_attribute(&mut self) -> Option<TokenizedAttribute> {
        // consume: [a-zA-Z]
        let Some(initial) = self.iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
            panic!(
                "Expected [a-zA-Z] in parse_attribute but got {:?}",
                self.iter.next().map(|s| s.ch())
            );
        };

        // consume: ('-' | '_' | [a-zA-Z0-9])*
        let attr_name = initial.extend(self.iter.peeking_take_while(|s| {
            s.ch() == '-' || s.ch() == '_' || s.ch().is_ascii_alphanumeric()
        }));

        // consume: whitespace
        self.skip_whitespace();

        // consume: '='
        let Some(eq) = self.iter.next_if(|s| s.ch() == '=') else {
            return Some(TokenizedAttribute {
                name: attr_name.clone(),
                value: None,
                range: attr_name,
            });
        };

        // consume: whitespace
        self.skip_whitespace();

        // Check if value is an expression
        if self.iter.peek().map(|s| s.ch()) == Some('{') {
            // Parse expression value
            let (expr, expr_range) = self.parse_expression()?;
            return Some(TokenizedAttribute {
                name: attr_name.clone(),
                value: Some(TokenizedAttributeValue::Expression(expr)),
                range: attr_name.to(expr_range),
            });
        }

        // Parse string value (quoted)
        let Some(open_quote) = self.iter.next_if(|s| s.ch() == '"' || s.ch() == '\'') else {
            self.errors.push(ParseError::ExpectedQuotedAttributeValue {
                range: attr_name.to(eq),
            });
            return None;
        };

        // consume: attribute value
        let attr_value: Option<DocumentRange> = self
            .iter
            .peeking_take_while(|s| s.ch() != open_quote.ch())
            .collect();

        // consume: " or '
        let Some(close_quote) = self.iter.next_if(|s| s.ch() == open_quote.ch()) else {
            self.errors.push(ParseError::UnmatchedCharacter {
                ch: open_quote.ch(),
                range: open_quote,
            });
            return None;
        };

        // For empty strings like a="", we still want to return Some(String(...))
        // to distinguish from valueless attributes like `disabled`
        let value = Some(TokenizedAttributeValue::String {
            content: attr_value,
        });

        Some(TokenizedAttribute {
            name: attr_name.clone(),
            value,
            range: attr_name.to(close_quote),
        })
    }

    /// Parse an expression.
    ///
    /// E.g. <div foo="bar" {x: String}>
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
            self.errors.push(ParseError::UnmatchedCharacter {
                ch: left_brace.ch(),
                range: left_brace,
            });
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
            self.errors.push(ParseError::EmptyExpression {
                range: left_brace.to(right_brace),
            });
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
    /// E.g. <div foo="bar" {x: String}>
    ///           ^^^^^^^^^^^^^^^^^^^^^
    fn parse_tag_content(&mut self) -> (Vec<TokenizedAttribute>, Option<DocumentRange>) {
        let mut attributes: Vec<TokenizedAttribute> = Vec::new();
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
                    if attributes
                        .iter()
                        .any(|a| a.name.as_str() == attr.name.as_str())
                    {
                        self.errors.push(ParseError::DuplicateAttribute {
                            name: attr.name.to_cheap_string(),
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
    /// E.g. <div foo="bar" {x: String}>
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
            self.errors.push(ParseError::UnterminatedOpeningTag {
                range: tag_name.clone(),
            });
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
            self.errors.push(ParseError::UnterminatedClosingTag {
                range: left_angle.to(slash),
            });
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
            self.errors
                .push(ParseError::UnterminatedClosingTag { range: tag_name });
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

    /// Parse a text token.
    ///
    /// E.g. <div>hello</div>
    ///           ^^^^^
    /// Expects that the iterator points to the initial char.
    ///
    /// Note: Text tokens may contain `{...}` expressions. The parser is
    /// responsible for splitting these into separate Text and TextExpression
    /// nodes.
    fn parse_text(&mut self) -> Option<Token> {
        let Some(initial) = self.iter.next() else {
            panic!("Expected an initial char in parse_text but got None");
        };
        Some(Token::Text {
            range: initial.extend(self.iter.peeking_take_while(|s| s.ch() != '<')),
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
                self.errors
                    .push(ParseError::UnterminatedTagStart { range: left_angle });
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
    matches!(name, "script" | "style")
}

#[cfg(test)]
mod tests {
    use crate::document::{DocumentAnnotator, SimpleAnnotation};

    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let tokenizer = Tokenizer::new(DocumentCursor::new(input.to_string()));
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

        expected.assert_eq(
            &DocumentAnnotator::new()
                .without_line_numbers()
                .annotate(None, annotations),
        );
    }

    #[test]
    fn should_accept_empty_input() {
        check("", expect![""]);
    }

    #[test]
    fn should_accept_if_with_expression() {
        check(
            "<if {foo}>",
            expect![[r#"
                OpeningTag(
                  tag_name: "if",
                  attributes: {},
                  expression: Some("foo"),
                  self_closing: false,
                )
                <if {foo}>
                ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_expression_in_attribute() {
        check(
            r#"<div class={user.theme}>"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "div",
                  attributes: {
                    class: Expression("user.theme"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <div class={user.theme}>
                ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_mixed_attributes() {
        check(
            r#"<a href={user.url} target="_blank">"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "a",
                  attributes: {
                    href: Expression("user.url"),
                    target: String("_blank"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <a href={user.url} target="_blank">
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_input_with_attributes() {
        check(
            r#"<input type="" value="" disabled="">"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "input",
                  attributes: {
                    type: String(""),
                    value: String(""),
                    disabled: String(""),
                  },
                  expression: None,
                  self_closing: false,
                )
                <input type="" value="" disabled="">
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_attributes_without_spaces() {
        check(
            r#"<h1 foo="bar"x="y">"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {
                    foo: String("bar"),
                    x: String("y"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <h1 foo="bar"x="y">
                ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_over_multiple_lines() {
        check(
            "this\ntext\nspans\nmultiple lines",
            expect![[r#"
                Text [30 byte, "this\ntext\nspans\nmultiple lines"]
                this
                ^^^^
                text
                ^^^^
                spans
                ^^^^^
                multiple lines
                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_invalid_char_in_middle_of_tag() {
        check(
            "<div!>",
            expect![[r#"
                Unterminated opening tag
                <div!>
                 ^^^

                Text [2 byte, "!>"]
                <div!>
                    ^^
            "#]],
        );
    }

    #[test]
    fn should_accept_script_with_src() {
        check(
            r#"<script src="https://example.com/script.js"></script>"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "script",
                  attributes: {
                    src: String("https://example.com/script.js"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <script src="https://example.com/script.js"></script>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "script",
                )
                <script src="https://example.com/script.js"></script>
                                                            ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_self_closing_tag() {
        check(
            "<h1 />",
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {},
                  expression: None,
                  self_closing: true,
                )
                <h1 />
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_self_closing_tag_with_attributes() {
        check(
            "<h1 foo bar/>",
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {
                    foo: None,
                    bar: None,
                  },
                  expression: None,
                  self_closing: true,
                )
                <h1 foo bar/>
                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_before_attributes() {
        check(
            "<h1 {foo: String} foo bar/>",
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {
                    foo: None,
                    bar: None,
                  },
                  expression: Some("foo: String"),
                  self_closing: true,
                )
                <h1 {foo: String} foo bar/>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_simple_comment() {
        check(
            "<p><!-- --></p>",
            expect![[r#"
                OpeningTag(
                  tag_name: "p",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <p><!-- --></p>
                ^^^

                Comment
                <p><!-- --></p>
                   ^^^^^^^^

                ClosingTag(
                  tag_name: "p",
                )
                <p><!-- --></p>
                           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_succeeded_by_text() {
        check(
            indoc! {"
                <p><!-- -->
                </p>
            "},
            expect![[r#"
                OpeningTag(
                  tag_name: "p",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <p><!-- -->
                ^^^

                Comment
                <p><!-- -->
                   ^^^^^^^^

                Text [1 byte, "\n"]
                <p><!-- -->
                </p>

                ClosingTag(
                  tag_name: "p",
                )
                </p>
                ^^^^

                Text [1 byte, "\n"]
                </p>
            "#]],
        );
    }

    #[test]
    fn should_accept_comment_with_dashes_inside() {
        check(
            "<!-- Comment with -- dashes -- inside -->",
            expect![[r#"
                Comment
                <!-- Comment with -- dashes -- inside -->
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_multiline_comment() {
        check(
            indoc! {"
                <p><!--
                A comment
                that stretches
                over several
                lines --></p>
            "},
            expect![[r#"
                OpeningTag(
                  tag_name: "p",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <p><!--
                ^^^

                Comment
                <p><!--
                   ^^^^
                A comment
                ^^^^^^^^^
                that stretches
                ^^^^^^^^^^^^^^
                over several
                ^^^^^^^^^^^^
                lines --></p>
                ^^^^^^^^^

                ClosingTag(
                  tag_name: "p",
                )
                lines --></p>
                         ^^^^

                Text [1 byte, "\n"]
                lines --></p>
            "#]],
        );
    }

    #[test]
    fn should_accept_comment_with_quotes() {
        check(
            r#"<!-- This comment has <tags> and "quotes" and 'apostrophes' -->"#,
            expect![[r#"
                Comment
                <!-- This comment has <tags> and "quotes" and 'apostrophes' -->
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_tricky_comments() {
        check(
            indoc! {"
                <!-- ---><!-- ----><!----><!-----><!-- ---->
            "},
            expect![[r#"
                Comment
                <!-- ---><!-- ----><!----><!-----><!-- ---->
                ^^^^^^^^^

                Comment
                <!-- ---><!-- ----><!----><!-----><!-- ---->
                         ^^^^^^^^^^

                Comment
                <!-- ---><!-- ----><!----><!-----><!-- ---->
                                   ^^^^^^^

                Comment
                <!-- ---><!-- ----><!----><!-----><!-- ---->
                                          ^^^^^^^^

                Comment
                <!-- ---><!-- ----><!----><!-----><!-- ---->
                                                  ^^^^^^^^^^

                Text [1 byte, "\n"]
                <!-- ---><!-- ----><!----><!-----><!-- ---->
            "#]],
        );
    }

    #[test]
    fn should_accept_textarea_with_content() {
        check(
            indoc! {"
                <textarea>
                	<div></div>
                </textarea>
            "},
            expect![[r#"
                OpeningTag(
                  tag_name: "textarea",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <textarea>
                ^^^^^^^^^^

                Text [2 byte, "\n\t"]
                <textarea>
                    <div></div>
                ^^^^

                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                    <div></div>
                    ^^^^^

                ClosingTag(
                  tag_name: "div",
                )
                    <div></div>
                         ^^^^^^

                Text [1 byte, "\n"]
                    <div></div>
                </textarea>

                ClosingTag(
                  tag_name: "textarea",
                )
                </textarea>
                ^^^^^^^^^^^

                Text [1 byte, "\n"]
                </textarea>
            "#]],
        );
    }

    #[test]
    fn should_accept_doctype() {
        check(
            "<!DOCTYPE   html>",
            expect![[r#"
                Doctype
                <!DOCTYPE   html>
                ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_end_tag_with_space() {
        check(
            "</div >",
            expect![[r#"
                ClosingTag(
                  tag_name: "div",
                )
                </div >
                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_simple_self_closing_tag() {
        check(
            "<h1/>",
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {},
                  expression: None,
                  self_closing: true,
                )
                <h1/>
                ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_self_closing_tag_with_space() {
        check(
            "<h1 />",
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {},
                  expression: None,
                  self_closing: true,
                )
                <h1 />
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_style_with_content() {
        check(
            indoc! {"
                <style>
                  body { color: red; }
                  .class { font-size: 12px; }
                </style>
            "},
            expect![[r#"
                OpeningTag(
                  tag_name: "style",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <style>
                ^^^^^^^

                Text [54 byte, "\n  body { color: red; }\n  .class { font-size: 12px; }\n"]
                <style>
                  body { color: red; }
                ^^^^^^^^^^^^^^^^^^^^^^
                  .class { font-size: 12px; }
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                </style>

                ClosingTag(
                  tag_name: "style",
                )
                </style>
                ^^^^^^^^

                Text [1 byte, "\n"]
                </style>
            "#]],
        );
    }

    #[test]
    fn should_accept_attributes_on_multiple_lines() {
        check(
            indoc! {r#"
                <div
                  class="multiline"
                  id="test"
                  data-value="something">
            "#},
            expect![[r#"
                OpeningTag(
                  tag_name: "div",
                  attributes: {
                    class: String("multiline"),
                    id: String("test"),
                    data-value: String("something"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <div
                ^^^^
                  class="multiline"
                ^^^^^^^^^^^^^^^^^^^
                  id="test"
                ^^^^^^^^^^^
                  data-value="something">
                ^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                  data-value="something">
            "#]],
        );
    }

    #[test]
    fn should_accept_script_with_html_content() {
        check(
            indoc! {r#"
                <script>
                  const html = "<title>Nested</title>";
                  document.write(html);
                </script>
            "#},
            expect![[r#"
                OpeningTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script>
                ^^^^^^^^

                Text [65 byte, "\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n"]
                <script>
                  const html = "<title>Nested</title>";
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  document.write(html);
                ^^^^^^^^^^^^^^^^^^^^^^^
                </script>

                ClosingTag(
                  tag_name: "script",
                )
                </script>
                ^^^^^^^^^

                Text [1 byte, "\n"]
                </script>
            "#]],
        );
    }

    #[test]
    fn should_accept_script_with_whitespace_in_closing_tag() {
        check(
            indoc! {r#"
                <script>
                  const html = "";
                </ script>

                <script>
                  const html = "";
                </script  >

                <script>
                  const html = "";
                </ script  >
            "#},
            expect![[r#"
                OpeningTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script>
                ^^^^^^^^

                Text [20 byte, "\n  const html = \"\";\n"]
                <script>
                  const html = "";
                ^^^^^^^^^^^^^^^^^^
                </ script>

                ClosingTag(
                  tag_name: "script",
                )
                </ script>
                ^^^^^^^^^^

                Text [2 byte, "\n\n"]
                </ script>

                <script>

                OpeningTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script>
                ^^^^^^^^

                Text [20 byte, "\n  const html = \"\";\n"]
                <script>
                  const html = "";
                ^^^^^^^^^^^^^^^^^^
                </script  >

                ClosingTag(
                  tag_name: "script",
                )
                </script  >
                ^^^^^^^^^^^

                Text [2 byte, "\n\n"]
                </script  >

                <script>

                OpeningTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script>
                ^^^^^^^^

                Text [20 byte, "\n  const html = \"\";\n"]
                <script>
                  const html = "";
                ^^^^^^^^^^^^^^^^^^
                </ script  >

                ClosingTag(
                  tag_name: "script",
                )
                </ script  >
                ^^^^^^^^^^^^

                Text [1 byte, "\n"]
                </ script  >
            "#]],
        );
    }

    #[test]
    fn should_accept_svg_with_many_attributes() {
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
                OpeningTag(
                  tag_name: "svg",
                  attributes: {
                    xmlns: String("http://www.w3.org/2000/svg"),
                    width: String("24"),
                    height: String("24"),
                    viewBox: String("0 0 24 24"),
                    fill: String("none"),
                    stroke: String("currentColor"),
                    stroke-width: String("2"),
                    stroke-linecap: String("round"),
                    stroke-linejoin: String("round"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>

                OpeningTag(
                  tag_name: "line",
                  attributes: {
                    x1: String("16.5"),
                    y1: String("9.4"),
                    x2: String("7.5"),
                    y2: String("4.21"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "line",
                )
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                                                            ^^^^^^^

                Text [1 byte, "\n"]
                <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"></line>
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>

                OpeningTag(
                  tag_name: "path",
                  attributes: {
                    d: String("M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "path",
                )
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                                                                                                                                                    ^^^^^^^

                Text [1 byte, "\n"]
                <path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>

                OpeningTag(
                  tag_name: "polyline",
                  attributes: {
                    points: String("3.27 6.96 12 12.01 20.73 6.96"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "polyline",
                )
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                                                                 ^^^^^^^^^^^

                Text [1 byte, "\n"]
                <polyline points="3.27 6.96 12 12.01 20.73 6.96"></polyline>
                <line x1="12" y1="22.08" x2="12" y2="12"></line>

                OpeningTag(
                  tag_name: "line",
                  attributes: {
                    x1: String("12"),
                    y1: String("22.08"),
                    x2: String("12"),
                    y2: String("12"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <line x1="12" y1="22.08" x2="12" y2="12"></line>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "line",
                )
                <line x1="12" y1="22.08" x2="12" y2="12"></line>
                                                         ^^^^^^^

                Text [1 byte, "\n"]
                <line x1="12" y1="22.08" x2="12" y2="12"></line>
                </svg>

                ClosingTag(
                  tag_name: "svg",
                )
                </svg>
                ^^^^^^

                Text [1 byte, "\n"]
                </svg>
            "#]],
        );
    }

    #[test]
    fn should_accept_div_with_class_and_expression() {
        check(
            indoc! {r#"
                <div class="test" {bar}>
            "#},
            expect![[r#"
                OpeningTag(
                  tag_name: "div",
                  attributes: {
                    class: String("test"),
                  },
                  expression: Some("bar"),
                  self_closing: false,
                )
                <div class="test" {bar}>
                ^^^^^^^^^^^^^^^^^^^^^^^^

                Text [1 byte, "\n"]
                <div class="test" {bar}>
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_single_expression() {
        check(
            "<h1>Hello {name}!</h1>",
            expect![[r#"
                OpeningTag(
                  tag_name: "h1",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <h1>Hello {name}!</h1>
                ^^^^

                Text [13 byte, "Hello {name}!"]
                <h1>Hello {name}!</h1>
                    ^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "h1",
                )
                <h1>Hello {name}!</h1>
                                 ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_multiple_expressions() {
        check(
            "<p>User {user.name} has {user.count} items</p>",
            expect![[r#"
                OpeningTag(
                  tag_name: "p",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <p>User {user.name} has {user.count} items</p>
                ^^^

                Text [39 byte, "User {user.name} has {user.count} items"]
                <p>User {user.name} has {user.count} items</p>
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "p",
                )
                <p>User {user.name} has {user.count} items</p>
                                                          ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_expression_at_start() {
        check(
            "<span>{greeting} world!</span>",
            expect![[r#"
                OpeningTag(
                  tag_name: "span",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <span>{greeting} world!</span>
                ^^^^^^

                Text [17 byte, "{greeting} world!"]
                <span>{greeting} world!</span>
                      ^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "span",
                )
                <span>{greeting} world!</span>
                                       ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_expression_at_end() {
        check(
            "<div>Price: {price}</div>",
            expect![[r#"
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>Price: {price}</div>
                ^^^^^

                Text [14 byte, "Price: {price}"]
                <div>Price: {price}</div>
                     ^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "div",
                )
                <div>Price: {price}</div>
                                   ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_expression_containing_braces() {
        check(
            "<div>Price: {{k: v}}</div>",
            expect![[r#"
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>Price: {{k: v}}</div>
                ^^^^^

                Text [15 byte, "Price: {{k: v}}"]
                <div>Price: {{k: v}}</div>
                     ^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "div",
                )
                <div>Price: {{k: v}}</div>
                                    ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_only_expression() {
        check(
            "<h2>{title}</h2>",
            expect![[r#"
                OpeningTag(
                  tag_name: "h2",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <h2>{title}</h2>
                ^^^^

                Text [7 byte, "{title}"]
                <h2>{title}</h2>
                    ^^^^^^^

                ClosingTag(
                  tag_name: "h2",
                )
                <h2>{title}</h2>
                           ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_duplicate_attribute() {
        check(
            r#"<div class="foo" class="bar"></div>"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "div",
                  attributes: {
                    class: String("foo"),
                  },
                  expression: None,
                  self_closing: false,
                )
                <div class="foo" class="bar"></div>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Duplicate attribute 'class'
                <div class="foo" class="bar"></div>
                                 ^^^^^

                ClosingTag(
                  tag_name: "div",
                )
                <div class="foo" class="bar"></div>
                                             ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_duplicate_attribute_with_different_quotes() {
        check(
            r#"<input type="text" type='number'/>"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "input",
                  attributes: {
                    type: String("text"),
                  },
                  expression: None,
                  self_closing: true,
                )
                <input type="text" type='number'/>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Duplicate attribute 'type'
                <input type="text" type='number'/>
                                   ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_duplicate_attribute_without_value() {
        check(
            r#"<input required required />"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "input",
                  attributes: {
                    required: None,
                  },
                  expression: None,
                  self_closing: true,
                )
                <input required required />
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Duplicate attribute 'required'
                <input required required />
                                ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unterminated_comments() {
        check(
            r#"<!--"#,
            expect![[r#"
                Unterminated comment
                <!--
                ^^^^
            "#]],
        );
        check(
            r#"<!-- foo bar"#,
            expect![[r#"
                Unterminated comment
                <!-- foo bar
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unterminated_opening_tags() {
        check(
            r#"<div <div>"#,
            expect![[r#"
                Unterminated opening tag
                <div <div>
                 ^^^

                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div <div>
                     ^^^^^
            "#]],
        );
        check(
            r#"<div class="foo" <div>"#,
            expect![[r#"
                Unterminated opening tag
                <div class="foo" <div>
                 ^^^

                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div class="foo" <div>
                                 ^^^^^
            "#]],
        );
        check(
            r#"<div"#,
            expect![[r#"
                Unterminated opening tag
                <div
                 ^^^
            "#]],
        );
        check(
            r#"<div foo"#,
            expect![[r#"
                Unterminated opening tag
                <div foo
                 ^^^
            "#]],
        );
        check(
            r#"<div foo="#,
            expect![[r#"
                Expected quoted attribute value or expression
                <div foo=
                     ^^^^

                Unterminated opening tag
                <div foo=
                 ^^^
            "#]],
        );
        check(
            r#"<div foo="""#,
            expect![[r#"
                Unterminated opening tag
                <div foo=""
                 ^^^
            "#]],
        );
        check(
            r#"<div foo=""#,
            expect![[r#"
                Unmatched "
                <div foo="
                         ^

                Unterminated opening tag
                <div foo="
                 ^^^
            "#]],
        );
        check(
            r#"<div foo="bar"#,
            expect![[r#"
                Unmatched "
                <div foo="bar
                         ^

                Unterminated opening tag
                <div foo="bar
                 ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unterminated_closing_tags() {
        check(
            r#"</div </div>"#,
            expect![[r#"
                Unterminated closing tag
                </div </div>
                  ^^^

                ClosingTag(
                  tag_name: "div",
                )
                </div </div>
                      ^^^^^^
            "#]],
        );
        check(
            r#"</div> </div"#,
            expect![[r#"
                ClosingTag(
                  tag_name: "div",
                )
                </div> </div
                ^^^^^^

                Text [1 byte, " "]
                </div> </div
                      ^

                Unterminated closing tag
                </div> </div
                         ^^^
            "#]],
        );
        check(
            r#"</di<div>"#,
            expect![[r#"
                Unterminated closing tag
                </di<div>
                  ^^

                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                </di<div>
                    ^^^^^
            "#]],
        );
        check(
            r#"</</div"#,
            expect![[r#"
                Unterminated closing tag
                </</div
                ^^

                Unterminated closing tag
                </</div
                    ^^^
            "#]],
        );
        check(
            r#"<div foo="#,
            expect![[r#"
                Expected quoted attribute value or expression
                <div foo=
                     ^^^^

                Unterminated opening tag
                <div foo=
                 ^^^
            "#]],
        );
        check(
            r#"<div foo="""#,
            expect![[r#"
                Unterminated opening tag
                <div foo=""
                 ^^^
            "#]],
        );
        check(
            r#"<div foo=""#,
            expect![[r#"
                Unmatched "
                <div foo="
                         ^

                Unterminated opening tag
                <div foo="
                 ^^^
            "#]],
        );
        check(
            r#"<div foo="bar"#,
            expect![[r#"
                Unmatched "
                <div foo="bar
                         ^

                Unterminated opening tag
                <div foo="bar
                 ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_raw_text_with_tricky_content() {
        check(
            r#"<script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>"#,
            expect![[r#"
                OpeningTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                ^^^^^^^^

                Text [62 byte, "</scri</<<</div></div><</scrip</scrip></cript></script</script"]
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "script",
                )
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                                                                                      ^^^^^^^^^

                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                                                                                               ^^^^^

                Text [6 byte, "works!"]
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                                                                                                    ^^^^^^

                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                                                                                                          ^^^^^
            "#]],
        );
    }
}
