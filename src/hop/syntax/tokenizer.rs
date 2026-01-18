use std::fmt::{self, Display};
use std::iter::Peekable;

use itertools::Itertools as _;

use super::parse_error::ParseError;
use crate::common::is_void_element;
use crate::document::{DocumentCursor, DocumentRange, Ranged};
use crate::dop;
use crate::error_collector::ErrorCollector;

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
    /// A newline character in text position.
    /// This is emitted separately from Text tokens to allow the parser
    /// to handle whitespace normalization more precisely.
    Newline {
        range: DocumentRange,
    },
    TextExpression {
        content: DocumentRange,
        range: DocumentRange,
    },
    RawTextTag {
        tag_name: DocumentRange,
        attributes: Vec<TokenizedAttribute>,
        expression: Option<DocumentRange>,
        content: Option<DocumentRange>,
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
            | Token::Text { range }
            | Token::Newline { range }
            | Token::TextExpression { range, .. }
            | Token::RawTextTag { range, .. } => range,
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
            Token::Newline { .. } => {
                write!(f, "Newline")
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
            Token::TextExpression { content, .. } => {
                write!(f, "TextExpression({:?})", content.to_string())
            }
            Token::RawTextTag {
                tag_name,
                attributes,
                expression,
                content,
                ..
            } => {
                writeln!(f, "RawTextTag(")?;
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
                let content_str = match content {
                    Some(c) => format!("Some({:?})", c.to_string()),
                    None => "None".to_string(),
                };
                writeln!(f, "  content: {},", content_str)?;
                write!(f, ")")
            }
        }
    }
}

/// Parse the next token from the input.
///
/// Returns `Some(token)` if a token was parsed, `None` at end of input.
/// Errors are collected in the `errors` collector.
fn step(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<Token> {
    loop {
        match iter.peek().map(|s| s.ch()) {
            Some('<') => {
                if let Some(token) = parse_tag(iter, errors) {
                    return Some(token);
                } else {
                    continue;
                }
            }
            Some('{') => {
                if let Some(token) = parse_text_expression(iter, errors) {
                    return Some(token);
                } else {
                    continue;
                }
            }
            Some('\n') => {
                let newline = iter.next().unwrap();
                return Some(Token::Newline { range: newline });
            }
            Some(_) => return Some(parse_text(iter)),
            None => return None,
        }
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

fn skip_whitespace(iter: &mut Peekable<DocumentCursor>) {
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }
}

// Parse a comment.
//
// E.g. <!-- hello -->
//        ^^^^^^^^^^^^
// Expects that the iterator points to the initial '-'.
//
fn parse_comment(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
    left_angle_to_bang: DocumentRange,
) -> Option<Token> {
    let Some(first_dash) = iter.next_if(|s| s.ch() == '-') else {
        panic!(
            "Expected '-' in parse_comment but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    let Some(second_dash) = iter.next_if(|s| s.ch() == '-') else {
        errors.push(ParseError::InvalidMarkupDeclaration {
            range: left_angle_to_bang.to(first_dash),
        });
        return None;
    };
    // Count the number of seen '-' to find the end of the comment
    let mut count = 0;
    loop {
        match iter.next() {
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
                errors.push(ParseError::UnterminatedComment {
                    range: left_angle_to_bang.to(second_dash),
                });
                return None;
            }
        }
    }
}

fn parse_doctype(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
    left_angle_to_bang: DocumentRange,
) -> Option<Token> {
    let doctype = iter
        .clone()
        .map(|s| s.ch())
        .take(7)
        .collect::<String>()
        .to_lowercase();
    if doctype != "doctype" {
        errors.push(ParseError::InvalidMarkupDeclaration {
            range: left_angle_to_bang,
        });
        return None;
    }
    while iter.next_if(|s| s.ch() != '>').is_some() {}
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        errors.push(ParseError::InvalidMarkupDeclaration {
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
fn parse_markup_declaration(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
    left_angle: DocumentRange,
) -> Option<Token> {
    let Some(bang) = iter.next_if(|s| s.ch() == '!') else {
        panic!(
            "Expected '!' in parse_markup_declaration but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    match iter.peek().map(|s| s.ch()) {
        Some('-') => parse_comment(iter, errors, left_angle.to(bang)),
        Some('D' | 'd') => parse_doctype(iter, errors, left_angle.to(bang)),
        _ => {
            errors.push(ParseError::InvalidMarkupDeclaration {
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
fn parse_attribute(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<TokenizedAttribute> {
    // consume: [a-zA-Z]
    let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
        panic!(
            "Expected [a-zA-Z] in parse_attribute but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };

    // consume: ('-' | '_' | [a-zA-Z0-9])*
    let attr_name =
        initial.extend(iter.peeking_take_while(|s| {
            s.ch() == '-' || s.ch() == '_' || s.ch().is_ascii_alphanumeric()
        }));

    // consume: whitespace
    skip_whitespace(iter);

    // consume: '='
    let Some(eq) = iter.next_if(|s| s.ch() == '=') else {
        return Some(TokenizedAttribute {
            name: attr_name.clone(),
            value: None,
            range: attr_name,
        });
    };

    // consume: whitespace
    skip_whitespace(iter);

    // Check if value is an expression
    if iter.peek().map(|s| s.ch()) == Some('{') {
        // Parse expression value
        let (expr, expr_range) = parse_expression(iter, errors)?;
        return Some(TokenizedAttribute {
            name: attr_name.clone(),
            value: Some(TokenizedAttributeValue::Expression(expr)),
            range: attr_name.to(expr_range),
        });
    }

    // Parse string value (quoted)
    let Some(open_quote) = iter.next_if(|s| s.ch() == '"' || s.ch() == '\'') else {
        errors.push(ParseError::ExpectedQuotedAttributeValue {
            range: attr_name.to(eq),
        });
        return None;
    };

    // consume: attribute value
    let attr_value: Option<DocumentRange> = iter
        .peeking_take_while(|s| s.ch() != open_quote.ch())
        .collect();

    // consume: " or '
    let Some(close_quote) = iter.next_if(|s| s.ch() == open_quote.ch()) else {
        errors.push(ParseError::UnmatchedCharacter {
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
fn parse_expression(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<(DocumentRange, DocumentRange)> {
    // consume '{'
    let Some(left_brace) = iter.next_if(|s| s.ch() == '{') else {
        panic!(
            "Expected '{{' in parse_expression but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    let clone = iter.clone();
    let Some(found_right_brace) = find_expression_end(clone) else {
        errors.push(ParseError::UnmatchedCharacter {
            ch: left_brace.ch(),
            range: left_brace,
        });
        return None;
    };
    // handle empty expression
    if left_brace.end() == found_right_brace.start() {
        let Some(right_brace) = iter.next_if(|s| s.ch() == '}') else {
            panic!(
                "Expected '}}' in parse_expression but got {:?}",
                iter.next().map(|s| s.ch())
            );
        };
        errors.push(ParseError::EmptyExpression {
            range: left_brace.to(right_brace),
        });
        return None;
    }
    let mut expr = iter.next().unwrap();
    while iter.peek().unwrap().start() != found_right_brace.start() {
        expr = expr.to(iter.next()?);
    }
    // consume: '}'
    let Some(right_brace) = iter.next_if(|s| s.ch() == '}') else {
        panic!(
            "Expected '}}' in parse_expression but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    Some((expr, left_brace.to(right_brace)))
}

/// Parse a text expression.
///
/// E.g. <div>Hello {name}!</div>
///                 ^^^^^^
/// Expects that the iterator points to the initial '{'.
fn parse_text_expression(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<Token> {
    let (content, range) = parse_expression(iter, errors)?;
    Some(Token::TextExpression { content, range })
}

/// Parse tag content (attributes and expressions).
///
/// E.g. <div foo="bar" {x: String}>
///           ^^^^^^^^^^^^^^^^^^^^^
fn parse_tag_content(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> (Vec<TokenizedAttribute>, Option<DocumentRange>) {
    let mut attributes: Vec<TokenizedAttribute> = Vec::new();
    let mut expression: Option<DocumentRange> = None;
    loop {
        skip_whitespace(iter);
        match iter.peek().map(|s| s.ch()) {
            Some('{') => {
                let Some((expr, _)) = parse_expression(iter, errors) else {
                    continue;
                };
                expression = Some(expr);
            }
            Some(ch) if ch.is_ascii_alphabetic() => {
                let Some(attr) = parse_attribute(iter, errors) else {
                    continue;
                };
                if attributes
                    .iter()
                    .any(|a| a.name.as_str() == attr.name.as_str())
                {
                    errors.push(ParseError::DuplicateAttribute {
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
fn parse_opening_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
    left_angle: DocumentRange,
) -> Option<Token> {
    // consume: [a-zA-Z]
    let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
        panic!(
            "Expected [a-zA-Z] in parse_opening_tag but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };

    // consume: ('-' | [a-zA-Z0-9])*
    let tag_name = initial
        .extend(iter.peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()));
    // consume: tag content
    let (attributes, expression) = parse_tag_content(iter, errors);

    // consume: whitespace
    skip_whitespace(iter);

    // consume: '/'
    let self_closing = iter.next_if(|s| s.ch() == '/').is_some();

    // consume: '>'
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        errors.push(ParseError::UnterminatedOpeningTag {
            range: tag_name.clone(),
        });
        return None;
    };

    // For raw text tags (script, style), parse the entire element including content and closing tag
    if is_tag_name_with_raw_content(tag_name.as_str()) && !self_closing {
        return parse_raw_text_element(iter, errors, left_angle, tag_name, attributes, expression);
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
fn parse_closing_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
    left_angle: DocumentRange,
) -> Option<Token> {
    let Some(slash) = iter.next_if(|s| s.ch() == '/') else {
        panic!(
            "Expected '/' in parse_closing_tag but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };

    // consume: whitespace
    skip_whitespace(iter);

    // consume: [a-zA-Z]
    let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
        errors.push(ParseError::UnterminatedClosingTag {
            range: left_angle.to(slash),
        });
        return None;
    };
    // consume: ('-' | [a-zA-Z0-9])*
    let tag_name = initial
        .extend(iter.peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()));

    // consume: whitespace
    skip_whitespace(iter);

    // consume: '>'
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        errors.push(ParseError::UnterminatedClosingTag { range: tag_name });
        return None;
    };
    Some(Token::ClosingTag {
        tag_name,
        range: left_angle.to(right_angle),
    })
}

fn peek_rawtext_closing_tag(iter: &Peekable<DocumentCursor>, tag_name: &DocumentRange) -> bool {
    let mut iter = iter.clone();
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

fn parse_rawtext_content(
    iter: &mut Peekable<DocumentCursor>,
    tag_name: &DocumentRange,
) -> Option<DocumentRange> {
    let mut raw_text: Option<DocumentRange> = None;
    loop {
        if iter.peek().is_none() || peek_rawtext_closing_tag(iter, tag_name) {
            return raw_text;
        }
        if let Some(ch) = iter.next() {
            raw_text = raw_text.into_iter().chain(Some(ch)).collect();
        }
    }
}

/// Consume the closing tag for a raw text element.
///
/// E.g. </script>
///      ^^^^^^^^^
/// Expects that peek_rawtext_closing_tag returned true.
/// Returns the range of the '>' character.
fn consume_rawtext_closing_tag(
    iter: &mut Peekable<DocumentCursor>,
    tag_name: &DocumentRange,
) -> DocumentRange {
    // consume: '<'
    iter.next();
    // consume: '/'
    iter.next();
    // consume: whitespace
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }
    // consume: tag name
    for _ in tag_name.as_str().chars() {
        iter.next();
    }
    // consume: whitespace
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }
    // consume: '>'
    iter.next()
        .expect("Expected '>' in consume_rawtext_closing_tag")
}

/// Parse a raw text element (script or style).
///
/// E.g. <script>alert(1)</script>
///      ^^^^^^^^^^^^^^^^^^^^^^^^^
/// This method is called after the opening tag has been parsed up to '>'.
/// It parses the content and closing tag, returning a RawTextTag token.
fn parse_raw_text_element(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
    left_angle: DocumentRange,
    tag_name: DocumentRange,
    attributes: Vec<TokenizedAttribute>,
    expression: Option<DocumentRange>,
) -> Option<Token> {
    // Parse content until closing tag
    let content = parse_rawtext_content(iter, &tag_name);

    // Check for EOF
    if iter.peek().is_none() {
        errors.push(ParseError::UnterminatedOpeningTag {
            range: tag_name.clone(),
        });
        return None;
    }

    // Consume the closing tag
    let closing_end = consume_rawtext_closing_tag(iter, &tag_name);

    Some(Token::RawTextTag {
        tag_name,
        attributes,
        expression,
        content,
        range: left_angle.to(closing_end),
    })
}

/// Parse a text token.
///
/// E.g. <div>hello</div>
///           ^^^^^
/// Expects that the iterator points to the initial char.
/// Stops at '<', '{', or '\n' (newlines are emitted as separate tokens).
fn parse_text(iter: &mut Peekable<DocumentCursor>) -> Token {
    let Some(initial) = iter.next() else {
        panic!("Expected an initial char in parse_text but got None");
    };
    Token::Text {
        range: initial
            .extend(iter.peeking_take_while(|s| s.ch() != '<' && s.ch() != '{' && s.ch() != '\n')),
    }
}

/// Tokenizer state for whitespace normalization.
///
/// This wraps the raw `step()` function to handle two concerns:
///
/// ## 1. Whitespace Trimming
///
/// - Trim leading whitespace after OpeningTag or Newline
/// - Trim trailing whitespace before Newline or ClosingTag
/// - Skip Text tokens that become empty after trimming
///
/// ## 2. Newline-to-Space Conversion
///
/// Selectively emit Newline tokens:
/// - Emit Newline only between Text/TextExpression tokens
/// - Discard Newline before any tag (it's either a container boundary or
///   the user can add explicit space on the same line if needed)
///
pub struct Tokenizer {
    /// Trim leading whitespace from the next Text token.
    /// Set after OpeningTag (container start) or Newline.
    trim_next_start: bool,
    /// Previous emitted token was Text or TextExpression.
    /// Used to decide if a Newline should be recorded as pending.
    prev_was_inline: bool,
    /// Pending Newline to emit before the next Text/TextExpression.
    /// Discarded if the next token is a tag.
    pending_newline: Option<DocumentRange>,
    /// Token buffered to return after emitting a pending Newline.
    buffered_token: Option<Token>,
}

impl Tokenizer {
    pub fn new() -> Self {
        Self {
            trim_next_start: true,
            prev_was_inline: false,
            pending_newline: None,
            buffered_token: None,
        }
    }

    /// If there's a pending newline that should be emitted before the given token,
    /// buffer the token and return the newline. Otherwise, return the token directly.
    fn maybe_emit_pending_newline(&mut self, token: Token) -> Option<Token> {
        if let Some(range) = self.pending_newline.take() {
            self.buffered_token = Some(token);
            Some(Token::Newline { range })
        } else {
            Some(token)
        }
    }

    pub fn next(
        &mut self,
        iter: &mut Peekable<DocumentCursor>,
        errors: &mut ErrorCollector<ParseError>,
    ) -> Option<Token> {
        // First, return any buffered token from a previous call
        if let Some(token) = self.buffered_token.take() {
            return Some(token);
        }

        loop {
            let token = step(iter, errors)?;

            match token {
                Token::Text { range } => {
                    let trim_start = self.trim_next_start;
                    let trim_end = {
                        let mut peek_iter = iter.clone();
                        match peek_iter.next().map(|s| s.ch()) {
                            Some('\n') => true,
                            Some('<') => peek_iter.next().is_some_and(|s| s.ch() == '/'),
                            _ => false,
                        }
                    };

                    let range = match (trim_start, trim_end) {
                        (true, true) => range.trim(),
                        (true, false) => range.trim_start(),
                        (false, true) => range.trim_end(),
                        (false, false) => range,
                    };

                    self.trim_next_start = false;

                    if range.as_str().is_empty() {
                        // Skip empty text tokens, get next token
                        continue;
                    }

                    self.prev_was_inline = true;
                    return self.maybe_emit_pending_newline(Token::Text { range });
                }
                Token::Newline { range } => {
                    // Only record a pending newline if previous token was inline content
                    if self.prev_was_inline {
                        self.pending_newline = Some(range);
                    }
                    self.trim_next_start = true;
                    // Don't emit the newline yet; continue to get the next token
                    continue;
                }
                Token::TextExpression { content, range } => {
                    self.trim_next_start = false;
                    self.prev_was_inline = true;
                    return self
                        .maybe_emit_pending_newline(Token::TextExpression { content, range });
                }
                Token::OpeningTag {
                    tag_name,
                    attributes,
                    expression,
                    self_closing,
                    range,
                } => {
                    // Don't emit Newline before opening tag; only between text/expression
                    self.pending_newline = None;
                    // After a non-void, non-self-closing opening tag, the next text
                    // is at the start of the container's children
                    self.trim_next_start = !self_closing && !is_void_element(tag_name.as_str());
                    self.prev_was_inline = false;
                    return Some(Token::OpeningTag {
                        tag_name,
                        attributes,
                        expression,
                        self_closing,
                        range,
                    });
                }
                Token::ClosingTag { tag_name, range } => {
                    // Don't emit Newline before closing tag (container end)
                    self.pending_newline = None;
                    self.trim_next_start = false;
                    // Only Text/TextExpression are inline content; tags are not
                    self.prev_was_inline = false;
                    return Some(Token::ClosingTag { tag_name, range });
                }
                Token::RawTextTag {
                    tag_name,
                    attributes,
                    expression,
                    content,
                    range,
                } => {
                    // Don't emit Newline before tags; only between text/expression
                    self.pending_newline = None;
                    self.trim_next_start = false;
                    self.prev_was_inline = false;
                    return Some(Token::RawTextTag {
                        tag_name,
                        attributes,
                        expression,
                        content,
                        range,
                    });
                }
                Token::Comment { range } => {
                    // Don't emit Newline before comments
                    self.pending_newline = None;
                    self.trim_next_start = false;
                    self.prev_was_inline = false;
                    return Some(Token::Comment { range });
                }
                Token::Doctype { range } => {
                    // Don't emit Newline before doctype
                    self.pending_newline = None;
                    self.trim_next_start = false;
                    self.prev_was_inline = false;
                    return Some(Token::Doctype { range });
                }
            }
        }
    }
}

fn parse_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<Token> {
    let Some(left_angle) = iter.next() else {
        panic!(
            "Expected '<' in parse_tag but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };

    match iter.peek().map(|s| s.ch()) {
        Some('!') => parse_markup_declaration(iter, errors, left_angle),
        Some('/') => parse_closing_tag(iter, errors, left_angle),
        Some(ch) if ch.is_ascii_alphabetic() => parse_opening_tag(iter, errors, left_angle),
        _ => {
            errors.push(ParseError::UnterminatedTagStart { range: left_angle });
            None
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
        let mut iter = DocumentCursor::new(input.to_string()).peekable();
        let mut token_annotations = Vec::new();
        let mut error_annotations = Vec::new();
        let mut errors = ErrorCollector::new();
        let mut tokenizer = Tokenizer::new();

        while let Some(token) = tokenizer.next(&mut iter, &mut errors) {
            token_annotations.push(SimpleAnnotation {
                message: token.to_string(),
                range: token.range().clone(),
            });
            for err in &errors {
                error_annotations.push(SimpleAnnotation {
                    message: err.to_string(),
                    range: err.range().clone(),
                });
            }
            errors.clear();
        }
        for err in &errors {
            error_annotations.push(SimpleAnnotation {
                message: err.to_string(),
                range: err.range().clone(),
            });
        }

        let annotator = DocumentAnnotator::new().without_line_numbers();
        let mut output = String::new();

        if !error_annotations.is_empty() {
            output.push_str("-- errors --\n");
            output.push_str(&annotator.annotate(None, error_annotations));
        }
        if !token_annotations.is_empty() {
            output.push_str("-- tokens --\n");
            output.push_str(&annotator.annotate(None, token_annotations));
        }

        expected.assert_eq(&output);
    }

    #[test]
    fn should_accept_empty_input() {
        check("", expect![""]);
    }

    #[test]
    fn should_accept_text_on_separate_line() {
        check(
            indoc! {"
                <div>
                  hello world
                </div>
            "},
            expect![[r#"
                -- tokens --
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>
                ^^^^^

                Text [11 byte, "hello world"]
                  hello world
                  ^^^^^^^^^^^

                ClosingTag(
                  tag_name: "div",
                )
                </div>
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_text_on_multiple_lines() {
        check(
            indoc! {"
                <div>
                  hello
                  world
                </div>
            "},
            expect![[r#"
                -- tokens --
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>
                ^^^^^

                Text [5 byte, "hello"]
                  hello
                  ^^^^^

                Newline
                  hello
                  world

                Text [5 byte, "world"]
                  world
                  ^^^^^

                ClosingTag(
                  tag_name: "div",
                )
                </div>
                ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_inline_text_with_links() {
        check(
            indoc! {r##"
                <div>
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                </div>
            "##},
            expect![[r##"
                -- tokens --
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>
                ^^^^^

                Text [39 byte, "By clicking continue, you agree to our "]
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                OpeningTag(
                  tag_name: "a",
                  attributes: {
                    href: String("#"),
                  },
                  expression: None,
                  self_closing: false,
                )
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                         ^^^^^^^^^^^^

                Text [16 byte, "Terms of Service"]
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                     ^^^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "a",
                )
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                                     ^^^^

                Text [5 byte, " and "]
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                                         ^^^^^

                OpeningTag(
                  tag_name: "a",
                  attributes: {
                    href: String("#"),
                  },
                  expression: None,
                  self_closing: false,
                )
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                                              ^^^^^^^^^^^^

                Text [14 byte, "Privacy Policy"]
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                                                          ^^^^^^^^^^^^^^

                ClosingTag(
                  tag_name: "a",
                )
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                                                                        ^^^^

                Text [1 byte, "."]
                  By clicking continue, you agree to our <a href="#">Terms of Service</a> and <a href="#">Privacy Policy</a>.
                                                                                                                            ^

                ClosingTag(
                  tag_name: "div",
                )
                </div>
                ^^^^^^
            "##]],
        );
    }

    #[test]
    fn should_accept_if_with_expression() {
        check(
            "<if {foo}>",
            expect![[r#"
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
                Text [4 byte, "this"]
                this
                ^^^^

                Newline
                this
                text

                Text [4 byte, "text"]
                text
                ^^^^

                Newline
                text
                spans

                Text [5 byte, "spans"]
                spans
                ^^^^^

                Newline
                spans
                multiple lines

                Text [14 byte, "multiple lines"]
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
                -- errors --
                Unterminated opening tag
                <div!>
                 ^^^
                -- tokens --
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
                -- tokens --
                RawTextTag(
                  tag_name: "script",
                  attributes: {
                    src: String("https://example.com/script.js"),
                  },
                  expression: None,
                  content: None,
                )
                <script src="https://example.com/script.js"></script>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_self_closing_tag() {
        check(
            "<h1 />",
            expect![[r#"
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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

                ClosingTag(
                  tag_name: "p",
                )
                </p>
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_comment_with_dashes_inside() {
        check(
            "<!-- Comment with -- dashes -- inside -->",
            expect![[r#"
                -- tokens --
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
                -- tokens --
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
            "#]],
        );
    }

    #[test]
    fn should_accept_comment_with_quotes() {
        check(
            r#"<!-- This comment has <tags> and "quotes" and 'apostrophes' -->"#,
            expect![[r#"
                -- tokens --
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
                -- tokens --
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
                -- tokens --
                OpeningTag(
                  tag_name: "textarea",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <textarea>
                ^^^^^^^^^^

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

                ClosingTag(
                  tag_name: "textarea",
                )
                </textarea>
                ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_doctype() {
        check(
            "<!DOCTYPE   html>",
            expect![[r#"
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
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
                -- tokens --
                RawTextTag(
                  tag_name: "style",
                  attributes: {},
                  expression: None,
                  content: Some("\n  body { color: red; }\n  .class { font-size: 12px; }\n"),
                )
                <style>
                ^^^^^^^
                  body { color: red; }
                ^^^^^^^^^^^^^^^^^^^^^^
                  .class { font-size: 12px; }
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                </style>
                ^^^^^^^^
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
                -- tokens --
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
                -- tokens --
                RawTextTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  content: Some("\n  const html = \"<title>Nested</title>\";\n  document.write(html);\n"),
                )
                <script>
                ^^^^^^^^
                  const html = "<title>Nested</title>";
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                  document.write(html);
                ^^^^^^^^^^^^^^^^^^^^^^^
                </script>
                ^^^^^^^^^
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
                -- tokens --
                RawTextTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  content: Some("\n  const html = \"\";\n"),
                )
                <script>
                ^^^^^^^^
                  const html = "";
                ^^^^^^^^^^^^^^^^^^
                </ script>
                ^^^^^^^^^^

                RawTextTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  content: Some("\n  const html = \"\";\n"),
                )
                <script>
                ^^^^^^^^
                  const html = "";
                ^^^^^^^^^^^^^^^^^^
                </script  >
                ^^^^^^^^^^^

                RawTextTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  content: Some("\n  const html = \"\";\n"),
                )
                <script>
                ^^^^^^^^
                  const html = "";
                ^^^^^^^^^^^^^^^^^^
                </ script  >
                ^^^^^^^^^^^^
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
                -- tokens --
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

                ClosingTag(
                  tag_name: "svg",
                )
                </svg>
                ^^^^^^
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
                -- tokens --
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
            "#]],
        );
    }

    #[test]
    fn should_accept_text_with_single_expression() {
        check(
            "<h1>Hello {name}!</h1>",
            expect![[r#"
                -- tokens --
                OpeningTag(
                  tag_name: "h1",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <h1>Hello {name}!</h1>
                ^^^^

                Text [6 byte, "Hello "]
                <h1>Hello {name}!</h1>
                    ^^^^^^

                TextExpression("name")
                <h1>Hello {name}!</h1>
                          ^^^^^^

                Text [1 byte, "!"]
                <h1>Hello {name}!</h1>
                                ^

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
                -- tokens --
                OpeningTag(
                  tag_name: "p",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <p>User {user.name} has {user.count} items</p>
                ^^^

                Text [5 byte, "User "]
                <p>User {user.name} has {user.count} items</p>
                   ^^^^^

                TextExpression("user.name")
                <p>User {user.name} has {user.count} items</p>
                        ^^^^^^^^^^^

                Text [5 byte, " has "]
                <p>User {user.name} has {user.count} items</p>
                                   ^^^^^

                TextExpression("user.count")
                <p>User {user.name} has {user.count} items</p>
                                        ^^^^^^^^^^^^

                Text [6 byte, " items"]
                <p>User {user.name} has {user.count} items</p>
                                                    ^^^^^^

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
                -- tokens --
                OpeningTag(
                  tag_name: "span",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <span>{greeting} world!</span>
                ^^^^^^

                TextExpression("greeting")
                <span>{greeting} world!</span>
                      ^^^^^^^^^^

                Text [7 byte, " world!"]
                <span>{greeting} world!</span>
                                ^^^^^^^

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
                -- tokens --
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>Price: {price}</div>
                ^^^^^

                Text [7 byte, "Price: "]
                <div>Price: {price}</div>
                     ^^^^^^^

                TextExpression("price")
                <div>Price: {price}</div>
                            ^^^^^^^

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
                -- tokens --
                OpeningTag(
                  tag_name: "div",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <div>Price: {{k: v}}</div>
                ^^^^^

                Text [7 byte, "Price: "]
                <div>Price: {{k: v}}</div>
                     ^^^^^^^

                TextExpression("{k: v}")
                <div>Price: {{k: v}}</div>
                            ^^^^^^^^

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
                -- tokens --
                OpeningTag(
                  tag_name: "h2",
                  attributes: {},
                  expression: None,
                  self_closing: false,
                )
                <h2>{title}</h2>
                ^^^^

                TextExpression("title")
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
                -- errors --
                Duplicate attribute 'class'
                <div class="foo" class="bar"></div>
                                 ^^^^^
                -- tokens --
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
                -- errors --
                Duplicate attribute 'type'
                <input type="text" type='number'/>
                                   ^^^^
                -- tokens --
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
            "#]],
        );
    }

    #[test]
    fn should_reject_duplicate_attribute_without_value() {
        check(
            r#"<input required required />"#,
            expect![[r#"
                -- errors --
                Duplicate attribute 'required'
                <input required required />
                                ^^^^^^^^
                -- tokens --
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
            "#]],
        );
    }

    #[test]
    fn should_reject_unterminated_comments() {
        check(
            r#"<!--"#,
            expect![[r#"
                -- errors --
                Unterminated comment
                <!--
                ^^^^
            "#]],
        );
        check(
            r#"<!-- foo bar"#,
            expect![[r#"
                -- errors --
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
                -- errors --
                Unterminated opening tag
                <div <div>
                 ^^^
                -- tokens --
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
                -- errors --
                Unterminated opening tag
                <div class="foo" <div>
                 ^^^
                -- tokens --
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
                -- errors --
                Unterminated opening tag
                <div
                 ^^^
            "#]],
        );
        check(
            r#"<div foo"#,
            expect![[r#"
                -- errors --
                Unterminated opening tag
                <div foo
                 ^^^
            "#]],
        );
        check(
            r#"<div foo="#,
            expect![[r#"
                -- errors --
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
                -- errors --
                Unterminated opening tag
                <div foo=""
                 ^^^
            "#]],
        );
        check(
            r#"<div foo=""#,
            expect![[r#"
                -- errors --
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
                -- errors --
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
                -- errors --
                Unterminated closing tag
                </div </div>
                  ^^^
                -- tokens --
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
                -- errors --
                Unterminated closing tag
                </div> </div
                         ^^^
                -- tokens --
                ClosingTag(
                  tag_name: "div",
                )
                </div> </div
                ^^^^^^
            "#]],
        );
        check(
            r#"</di<div>"#,
            expect![[r#"
                -- errors --
                Unterminated closing tag
                </di<div>
                  ^^
                -- tokens --
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
                -- errors --
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
                -- errors --
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
                -- errors --
                Unterminated opening tag
                <div foo=""
                 ^^^
            "#]],
        );
        check(
            r#"<div foo=""#,
            expect![[r#"
                -- errors --
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
                -- errors --
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
                -- tokens --
                RawTextTag(
                  tag_name: "script",
                  attributes: {},
                  expression: None,
                  content: Some("</scri</<<</div></div><</scrip</scrip></cript></script</script"),
                )
                <script></scri</<<</div></div><</scrip</scrip></cript></script</script</script><div>works!<div>
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
