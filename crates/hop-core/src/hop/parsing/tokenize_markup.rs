use std::iter::Peekable;

use crate::hop::parsing::token::{AttributeString, MarkupToken, RawTextToken, TagToken};
use crate::itertools::PeekingExt as _;

use crate::document::{DocumentCursor, DocumentRange};
use crate::parse_error::{ParseError, ParseErrorKind};

/// Lex the next token from the input.
///
/// Returns `Some(token)` if a token was lexed, `None` at end of input or
/// when the input reaches a '}', which closes the enclosing block.
/// Errors are collected in the `errors` collector.
pub fn next(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
) -> Option<MarkupToken> {
    loop {
        match iter.peek().map(|s| s.ch()) {
            Some('<') => {
                if let Some(token) = lex_tag(iter, errors) {
                    return Some(token);
                } else {
                    continue;
                }
            }
            Some('{') => {
                let left_brace = iter.next().unwrap();
                return Some(MarkupToken::ExpressionStart { left_brace });
            }
            Some('\n') => {
                let newline = iter.next().unwrap();
                return Some(MarkupToken::Newline { range: newline });
            }
            Some('}') => return None,
            Some(_) => return Some(lex_text(iter)),
            None => return None,
        }
    }
}

/// Lex the next thing inside an opening tag.
///
/// E.g.
/// ```text
/// <div foo="bar" {x}>
///      ^^^^^^^^^
/// ```
/// Returns None when the tag does not end the way it should, leaving the
/// caller to report it against the tag name. An attribute or spread that
/// could not be read is reported here and skipped over, so that the rest of
/// the tag is still read.
pub fn next_tag_token(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
) -> Option<TagToken> {
    loop {
        skip_whitespace(iter);
        // consume: '/'
        if let Some(slash) = iter.next_if(|s| s.ch() == '/') {
            // consume: '>'
            let right_angle = iter.next_if(|s| s.ch() == '>')?;
            return Some(TagToken::SelfClosingEnd {
                range: slash.to(right_angle),
            });
        }
        // consume: '>'
        if let Some(right_angle) = iter.next_if(|s| s.ch() == '>') {
            return Some(TagToken::End { range: right_angle });
        }
        // consume: '{'
        if let Some(left_brace) = iter.next_if(|s| s.ch() == '{') {
            return Some(TagToken::ExpressionStart { left_brace });
        }
        // peek: "..."
        let is_spread = {
            let mut ahead = iter.clone();
            (0..3).all(|_| ahead.next().is_some_and(|s| s.ch() == '.'))
        };
        if is_spread {
            // consume: '.'
            let first_dot = iter.next().unwrap();
            // consume: '.'
            iter.next();
            // consume: '.'
            let last_dot = iter.next().unwrap();
            // consume: [a-zA-Z_]
            let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic() || s.ch() == '_')
            else {
                // Report the dots and carry on with the tag.
                errors.push(ParseError::new(
                    ParseErrorKind::MissingVariableNameForSpread {},
                    first_dot.to(last_dot),
                ));
                continue;
            };
            // consume: [a-zA-Z_]*
            let name = initial.extend(
                iter.peeking_take_while(|s| s.ch().is_ascii_alphanumeric() || s.ch() == '_'),
            );
            return Some(TagToken::Spread {
                range: first_dot.to(name.clone()),
                name,
            });
        }
        // peek: [a-zA-Z]
        if iter.peek().is_some_and(|s| s.ch().is_ascii_alphabetic()) {
            match lex_attribute(iter, errors) {
                Some(part) => return Some(part),
                // The attribute was reported; carry on with the tag.
                None => continue,
            }
        }
        // Nothing here belongs in a tag. Skip past the end of it, so that the
        // parse resumes after the tag rather than inside it.
        while iter.next_if(|s| s.ch() != '>').is_some() {}
        iter.next();
        return None;
    }
}

/// Lex a raw text element's content and the closing tag that ends it.
///
/// E.g.
/// ```text
/// <script>alert(1)</script>
///         ^^^^^^^^^^^^^^^^^
/// ```
/// Returns None at end of input if the closing tag is never found.
pub fn next_raw_text_token(
    iter: &mut Peekable<DocumentCursor>,
    tag_name: &DocumentRange,
) -> Option<RawTextToken> {
    /// Whether the iterator is on the element's closing tag.
    fn peek_closing_tag(iter: &Peekable<DocumentCursor>, tag_name: &DocumentRange) -> bool {
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
        iter.next().is_some_and(|s| s.ch() == '>')
    }
    let mut content: Option<DocumentRange> = None;
    while !peek_closing_tag(iter, tag_name) {
        let ch = iter.next()?;
        content = content.into_iter().chain(Some(ch)).collect();
    }
    // consume: '<'
    iter.next();
    // consume: '/'
    iter.next();
    skip_whitespace(iter);
    // consume: tag name
    for _ in tag_name.as_str().chars() {
        iter.next();
    }
    skip_whitespace(iter);
    // consume: '>'
    let closing_tag_end = iter.next().unwrap();
    Some(RawTextToken {
        content,
        closing_tag_end,
    })
}

fn skip_whitespace(iter: &mut Peekable<DocumentCursor>) {
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }
}

fn lex_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
) -> Option<MarkupToken> {
    let Some(left_angle) = iter.next() else {
        panic!(
            "Expected '<' in lex_tag but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    match iter.peek().map(|s| s.ch()) {
        Some('!') => lex_markup_declaration(iter, errors, left_angle),
        Some('/') => lex_closing_tag(iter, errors, left_angle),
        Some('>') => {
            // consume: >
            let right_angle = iter.next().unwrap();
            Some(MarkupToken::FragmentStart {
                range: left_angle.to(right_angle),
            })
        }
        Some(ch) if ch.is_ascii_alphabetic() => Some(lex_opening_tag_start(iter, left_angle)),
        _ => {
            errors.push(ParseError::new(
                ParseErrorKind::UnterminatedTagStart {},
                left_angle,
            ));
            None
        }
    }
}

/// Lex a markup declaration from the iterator.
///
/// E.g.
/// ```text
/// <!-- hello -->
///  ^^^^^^^^^^^^^
/// ```
/// Expects that the iterator points to the initial '!'.
fn lex_markup_declaration(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
    left_angle: DocumentRange,
) -> Option<MarkupToken> {
    let Some(bang) = iter.next_if(|s| s.ch() == '!') else {
        panic!(
            "Expected '!' in lex_markup_declaration but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    match iter.peek().map(|s| s.ch()) {
        Some('-') => lex_comment(iter, errors, left_angle.to(bang)),
        Some('D' | 'd') => lex_doctype(iter, errors, left_angle.to(bang)),
        _ => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidMarkupDeclaration {},
                left_angle.to(bang),
            ));
            None
        }
    }
}

/// Lex a comment.
///
/// E.g.
/// ```text
/// <!-- hello -->
///   ^^^^^^^^^^^^
/// ```
/// Expects that the iterator points to the initial '-'.
fn lex_comment(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
    left_angle_to_bang: DocumentRange,
) -> Option<MarkupToken> {
    let Some(first_dash) = iter.next_if(|s| s.ch() == '-') else {
        panic!(
            "Expected '-' in lex_comment but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    let Some(second_dash) = iter.next_if(|s| s.ch() == '-') else {
        errors.push(ParseError::new(
            ParseErrorKind::InvalidMarkupDeclaration {},
            left_angle_to_bang.to(first_dash),
        ));
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
                    return Some(MarkupToken::Comment {
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
                errors.push(ParseError::new(
                    ParseErrorKind::UnterminatedComment {},
                    left_angle_to_bang.to(second_dash),
                ));
                return None;
            }
        }
    }
}

/// Lex a doctype declaration.
///
/// E.g.
/// ```text
/// <!doctype html>
///   ^^^^^^^^^^^^^
/// ```
/// Expects that the iterator points to the initial 'd'.
/// Always returns None: a doctype is reported, since one is inserted for
/// every page, and anything else after the '!' is not a declaration we know.
fn lex_doctype(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
    left_angle_to_bang: DocumentRange,
) -> Option<MarkupToken> {
    let doctype = iter
        .clone()
        .map(|s| s.ch())
        .take(7)
        .collect::<String>()
        .to_lowercase();
    if doctype != "doctype" {
        errors.push(ParseError::new(
            ParseErrorKind::InvalidMarkupDeclaration {},
            left_angle_to_bang,
        ));
        return None;
    }
    while iter.next_if(|s| s.ch() != '>').is_some() {}
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        errors.push(ParseError::new(
            ParseErrorKind::InvalidMarkupDeclaration {},
            left_angle_to_bang,
        ));
        return None;
    };
    errors.push(ParseError::new(
        ParseErrorKind::DoctypeNotAllowed {},
        left_angle_to_bang.to(right_angle),
    ));
    None
}

/// Lex the start of an opening tag.
///
/// E.g.
/// ```text
/// <div foo="bar">
///  ^^^
/// ```
/// Expects that the iterator points to the initial alphabetic char.
fn lex_opening_tag_start(
    iter: &mut Peekable<DocumentCursor>,
    left_angle: DocumentRange,
) -> MarkupToken {
    let initial = iter.next_if(|s| s.ch().is_ascii_alphabetic()).unwrap();
    let tag_name = initial
        .extend(iter.peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()));
    MarkupToken::OpeningTagStart {
        range: left_angle.to(tag_name.clone()),
        tag_name,
    }
}

/// Lex a closing tag.
///
/// E.g.
/// ```text
/// <div></div>
///       ^^^^^
/// ```
/// Expects that the iterator points to the initial '/'.
fn lex_closing_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
    left_angle: DocumentRange,
) -> Option<MarkupToken> {
    let Some(slash) = iter.next_if(|s| s.ch() == '/') else {
        panic!(
            "Expected '/' in lex_closing_tag but got {:?}",
            iter.next().map(|s| s.ch())
        );
    };
    // consume: whitespace
    skip_whitespace(iter);
    // consume: '>'
    if let Some(right_angle) = iter.next_if(|s| s.ch() == '>') {
        return Some(MarkupToken::FragmentEnd {
            range: left_angle.to(right_angle),
        });
    }
    // consume: [a-zA-Z]
    let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
        errors.push(ParseError::new(
            ParseErrorKind::UnterminatedClosingTag {},
            left_angle.to(slash),
        ));
        return None;
    };
    // consume: ('-' | [a-zA-Z0-9])*
    let tag_name = initial
        .extend(iter.peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()));
    // consume: whitespace
    skip_whitespace(iter);
    // consume: '>'
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        errors.push(ParseError::new(
            ParseErrorKind::UnterminatedClosingTag {},
            tag_name,
        ));
        return None;
    };
    Some(MarkupToken::ClosingTag {
        tag_name,
        range: left_angle.to(right_angle),
    })
}

/// Lex a text token.
///
/// E.g.
/// ```text
/// <div>hello</div>
///      ^^^^^
/// ```
/// Expects that the iterator points to the initial char.
/// Stops at '<', '{', or '\n' (newlines are emitted as separate tokens).
fn lex_text(iter: &mut Peekable<DocumentCursor>) -> MarkupToken {
    let Some(initial) = iter.next() else {
        panic!("Expected an initial char in lex_text but got None");
    };
    MarkupToken::Text {
        range: initial.extend(iter.peeking_take_while(|s| {
            s.ch() != '<' && s.ch() != '{' && s.ch() != '\n' && s.ch() != '}'
        })),
    }
}

/// Lex an attribute.
///
/// E.g.
/// ```text
/// <div foo="bar">
///      ^^^^^^^^^
/// ```
/// Expects that the iterator points to the initial alphabetic char.
/// Returns None if a valid attribute could not be lexed from the iterator.
fn lex_attribute(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut Vec<ParseError>,
) -> Option<TagToken> {
    let initial = iter.next_if(|s| s.ch().is_ascii_alphabetic()).unwrap();
    let name = initial.extend(iter.peeking_take_while(|s| {
        matches!(s.ch(), '-' | '_' | ':' | '.') || s.ch().is_ascii_alphanumeric()
    }));
    skip_whitespace(iter);
    // consume: '='
    let Some(eq) = iter.next_if(|s| s.ch() == '=') else {
        return Some(TagToken::Attribute { name, value: None });
    };
    skip_whitespace(iter);
    // consume: '{'
    if let Some(left_brace) = iter.next_if(|s| s.ch() == '{') {
        return Some(TagToken::AttributeExpressionStart { name, left_brace });
    }
    // consume: '\''
    if let Some(single_open) = iter.next_if(|s| s.ch() == '\'') {
        // Only double quotes are allowed, report error.
        let _value: Option<DocumentRange> = iter.peeking_take_while(|s| s.ch() != '\'').collect();
        let range = match iter.next_if(|s| s.ch() == '\'') {
            Some(single_close) => single_open.to(single_close),
            None => single_open,
        };
        errors.push(ParseError::new(
            ParseErrorKind::SingleQuotedAttributeValue {},
            range,
        ));
        return None;
    }
    // consume: '"'
    let Some(open_quote) = iter.next_if(|s| s.ch() == '"') else {
        errors.push(ParseError::new(
            ParseErrorKind::ExpectedQuotedAttributeValue {},
            name.to(eq),
        ));
        return None;
    };
    // consume: [^"]*
    let content: Option<DocumentRange> = iter.peeking_take_while(|s| s.ch() != '"').collect();
    let Some(close_quote) = iter.next_if(|s| s.ch() == '"') else {
        errors.push(ParseError::new(
            ParseErrorKind::UnmatchedCharacter {
                ch: open_quote.ch(),
            },
            open_quote,
        ));
        return None;
    };

    // `a=""` keeps a value, to tell it from a valueless `a`.
    Some(TagToken::Attribute {
        name,
        value: Some(AttributeString {
            content_range: content,
            quoted_range: open_quote.to(close_quote),
        }),
    })
}
