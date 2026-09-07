use std::iter::Peekable;

use crate::hop::parsing::token::{AttributeString, MarkupToken, RawTextToken, TagToken};
use crate::itertools::PeekingExt as _;

use crate::document::{DocumentCursor, DocumentRange};
use crate::parse_error::{ErrorEmitted, ParseErrorKind, ParseErrors};

/// Lex the next token in text position.
///
/// Returns `None` at end of input or at a `}`, which is left for the
/// enclosing expression to close. A `<` that starts nothing is reported and
/// skipped over.
pub fn next(iter: &mut Peekable<DocumentCursor>, errors: &mut ParseErrors) -> Option<MarkupToken> {
    loop {
        if let Some(left_angle) = iter.next_if(|s| s.ch() == '<') {
            match lex_tag(iter, errors, left_angle) {
                Ok(token) => return Some(token),
                Err(_) => continue,
            }
        }
        if let Some(left_brace) = iter.next_if(|s| s.ch() == '{') {
            return Some(MarkupToken::ExpressionStart { left_brace });
        }
        if let Some(newline) = iter.next_if(|s| s.ch() == '\n') {
            return Some(MarkupToken::Newline { range: newline });
        }
        if iter.peek().is_some_and(|s| s.ch() == '}') {
            return None;
        }
        let initial = iter.next()?;
        return Some(lex_text(iter, initial));
    }
}

/// Lex the next thing inside an opening tag, from the name the caller has
/// already consumed.
///
/// E.g.
/// ```text
/// <div foo="bar" {x}>
///      ^^^^^^^^^
/// ```
/// Fails, after reporting against the tag name, when the tag ends without a
/// `>`: at end of input, or at a `<`, which is left to start the next tag.
/// Anything else that does not belong in a tag is reported and skipped over,
/// so that the rest of the tag is still read.
pub fn next_tag_token(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    tag_name: &DocumentRange,
) -> Result<TagToken, ErrorEmitted> {
    loop {
        skip_whitespace(iter);
        // consume: "/>"
        if let Some(range) = iter.speculate(|iter| {
            let slash = iter.next_if(|s| s.ch() == '/')?;
            let right_angle = iter.next_if(|s| s.ch() == '>')?;
            Some(slash.to(right_angle))
        }) {
            return Ok(TagToken::SelfClosingEnd { range });
        }
        // consume: '>'
        if let Some(right_angle) = iter.next_if(|s| s.ch() == '>') {
            return Ok(TagToken::End { range: right_angle });
        }
        // consume: '{'
        if let Some(left_brace) = iter.next_if(|s| s.ch() == '{') {
            return Ok(TagToken::ExpressionStart { left_brace });
        }
        // consume: "..."
        if let Some(dots) = iter.speculate(|iter| {
            let first_dot = iter.next_if(|s| s.ch() == '.')?;
            iter.next_if(|s| s.ch() == '.')?;
            let last_dot = iter.next_if(|s| s.ch() == '.')?;
            Some(first_dot.to(last_dot))
        }) {
            // consume: [a-zA-Z_]
            let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic() || s.ch() == '_')
            else {
                // Report the dots and carry on with the tag.
                let _ = errors.emit(ParseErrorKind::MissingVariableNameForSpread {}, dots);
                continue;
            };
            // consume: [a-zA-Z_]*
            let name = initial.extend(
                iter.peeking_take_while(|s| s.ch().is_ascii_alphanumeric() || s.ch() == '_'),
            );
            return Ok(TagToken::Spread {
                range: dots.to(name.clone()),
                name,
            });
        }
        // consume: [a-zA-Z]
        if let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) {
            match lex_attribute(iter, errors, initial) {
                Ok(part) => return Ok(part),
                // The attribute was reported; carry on with the tag.
                Err(_) => continue,
            }
        }
        // Nothing else belongs in a tag. Report it and carry on with the tag.
        if let Some(unexpected) = iter.next_if(|s| s.ch() != '<') {
            let _ = errors.emit(
                ParseErrorKind::UnexpectedCharacter {
                    ch: unexpected.ch(),
                },
                unexpected,
            );
            continue;
        }
        // The tag ends without a `>`: at end of input, or at the `<` that
        // starts the next tag, which is left for text position.
        return Err(errors.emit(ParseErrorKind::UnterminatedOpeningTag {}, tag_name.clone()));
    }
}

/// Lex a raw text element's content and the closing tag that ends it.
///
/// E.g.
/// ```text
/// <script>alert(1)</script>
///         ^^^^^^^^^^^^^^^^^
/// ```
/// The closing tag is `None` when the input ends before one is found; the
/// content then runs to the end of input.
pub fn next_raw_text_token(
    iter: &mut Peekable<DocumentCursor>,
    tag_name: &DocumentRange,
) -> RawTextToken {
    let mut content: Option<DocumentRange> = None;
    loop {
        // Consume the closing tag if the input is on it, keeping the '>'.
        let closing_tag_end = iter.speculate(|iter| {
            // consume: '<'
            iter.next_if(|s| s.ch() == '<')?;
            // consume: '/'
            iter.next_if(|s| s.ch() == '/')?;
            skip_whitespace(iter);
            // consume: tag name
            for ch in tag_name.as_str().chars() {
                iter.next_if(|s| s.ch() == ch)?;
            }
            skip_whitespace(iter);
            // consume: '>'
            iter.next_if(|s| s.ch() == '>')
        });
        if closing_tag_end.is_some() {
            return RawTextToken {
                content,
                closing_tag_end,
            };
        }
        match iter.next() {
            Some(ch) => content = content.into_iter().chain(Some(ch)).collect(),
            None => {
                return RawTextToken {
                    content,
                    closing_tag_end: None,
                };
            }
        }
    }
}

fn skip_whitespace(iter: &mut Peekable<DocumentCursor>) {
    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
        iter.next();
    }
}

/// Lex a tag from the `<` the caller has already consumed.
///
/// E.g.
/// ```text
/// <div foo="bar">
/// ^^^^
/// ```
pub fn lex_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    left_angle: DocumentRange,
) -> Result<MarkupToken, ErrorEmitted> {
    // consume: '!'
    if let Some(bang) = iter.next_if(|s| s.ch() == '!') {
        return lex_markup_declaration(iter, errors, left_angle.to(bang));
    }
    // consume: '/'
    if let Some(slash) = iter.next_if(|s| s.ch() == '/') {
        return lex_closing_tag(iter, errors, left_angle.to(slash));
    }
    // consume: '>'
    if let Some(right_angle) = iter.next_if(|s| s.ch() == '>') {
        return Ok(MarkupToken::FragmentStart {
            range: left_angle.to(right_angle),
        });
    }
    // consume: [a-zA-Z]
    if let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) {
        return Ok(lex_opening_tag_start(iter, left_angle, initial));
    }
    Err(errors.emit(ParseErrorKind::UnterminatedTagStart {}, left_angle))
}

/// Lex a markup declaration from the `<!` the caller has already consumed.
///
/// E.g.
/// ```text
/// <!-- hello -->
/// ^^^^^^^^^^^^^^
/// ```
fn lex_markup_declaration(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    left_angle_to_bang: DocumentRange,
) -> Result<MarkupToken, ErrorEmitted> {
    // consume: '-'
    if let Some(first_dash) = iter.next_if(|s| s.ch() == '-') {
        return lex_comment(iter, errors, left_angle_to_bang.to(first_dash));
    }
    Err(lex_doctype(iter, errors, left_angle_to_bang))
}

/// Lex a comment from the `<!-` the caller has already consumed.
///
/// E.g.
/// ```text
/// <!-- hello -->
/// ^^^^^^^^^^^^^^
/// ```
fn lex_comment(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    left_angle_to_first_dash: DocumentRange,
) -> Result<MarkupToken, ErrorEmitted> {
    // consume: '-'
    let Some(second_dash) = iter.next_if(|s| s.ch() == '-') else {
        return Err(errors.emit(
            ParseErrorKind::InvalidMarkupDeclaration {},
            left_angle_to_first_dash,
        ));
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
                    return Ok(MarkupToken::Comment {
                        range: left_angle_to_first_dash.to(s),
                    });
                } else {
                    count = 0;
                }
            }
            Some(_) => {
                count = 0;
            }
            None => {
                return Err(errors.emit(
                    ParseErrorKind::UnterminatedComment {},
                    left_angle_to_first_dash.to(second_dash),
                ));
            }
        }
    }
}

/// Lex a doctype declaration from the `<!` the caller has already consumed.
///
/// E.g.
/// ```text
/// <!doctype html>
/// ^^^^^^^^^^^^^^^
/// ```
/// Always fails: a doctype is reported, since one is inserted for every
/// page, and anything else after the `<!` is not a declaration we know.
fn lex_doctype(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    left_angle_to_bang: DocumentRange,
) -> ErrorEmitted {
    let doctype = iter.speculate(|iter| {
        "doctype".chars().try_for_each(|expected| {
            iter.next_if(|s| s.ch().eq_ignore_ascii_case(&expected))
                .map(drop)
        })
    });
    if doctype.is_none() {
        return errors.emit(
            ParseErrorKind::InvalidMarkupDeclaration {},
            left_angle_to_bang,
        );
    }
    while iter.next_if(|s| s.ch() != '>').is_some() {}
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        return errors.emit(
            ParseErrorKind::InvalidMarkupDeclaration {},
            left_angle_to_bang,
        );
    };
    errors.emit(
        ParseErrorKind::DoctypeNotAllowed {},
        left_angle_to_bang.to(right_angle),
    )
}

/// Lex the start of an opening tag from the `<` and the first letter of the
/// name, which the caller has already consumed.
///
/// E.g.
/// ```text
/// <div foo="bar">
/// ^^^^
/// ```
fn lex_opening_tag_start(
    iter: &mut Peekable<DocumentCursor>,
    left_angle: DocumentRange,
    initial: DocumentRange,
) -> MarkupToken {
    // consume: ('-' | [a-zA-Z0-9])*
    let tag_name = initial
        .extend(iter.peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()));
    MarkupToken::OpeningTagStart {
        range: left_angle.to(tag_name.clone()),
        tag_name,
    }
}

/// Lex a closing tag from the `</` the caller has already consumed.
///
/// E.g.
/// ```text
/// <div></div>
///      ^^^^^^
/// ```
fn lex_closing_tag(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    left_angle_to_slash: DocumentRange,
) -> Result<MarkupToken, ErrorEmitted> {
    // consume: whitespace
    skip_whitespace(iter);
    // consume: '>'
    if let Some(right_angle) = iter.next_if(|s| s.ch() == '>') {
        return Ok(MarkupToken::FragmentEnd {
            range: left_angle_to_slash.to(right_angle),
        });
    }
    // consume: [a-zA-Z]
    let Some(initial) = iter.next_if(|s| s.ch().is_ascii_alphabetic()) else {
        return Err(errors.emit(
            ParseErrorKind::UnterminatedClosingTag {},
            left_angle_to_slash,
        ));
    };
    // consume: ('-' | [a-zA-Z0-9])*
    let tag_name = initial
        .extend(iter.peeking_take_while(|s| s.ch() == '-' || s.ch().is_ascii_alphanumeric()));
    // consume: whitespace
    skip_whitespace(iter);
    // consume: '>'
    let Some(right_angle) = iter.next_if(|s| s.ch() == '>') else {
        return Err(errors.emit(ParseErrorKind::UnterminatedClosingTag {}, tag_name));
    };
    Ok(MarkupToken::ClosingTag {
        tag_name,
        range: left_angle_to_slash.to(right_angle),
    })
}

/// Lex a text token from its first character, which the caller has already
/// consumed.
///
/// E.g.
/// ```text
/// <div>hello</div>
///      ^^^^^
/// ```
/// Stops at '<', '{', '}', or '\n' (newlines are emitted as separate tokens).
fn lex_text(iter: &mut Peekable<DocumentCursor>, initial: DocumentRange) -> MarkupToken {
    MarkupToken::Text {
        range: initial.extend(iter.peeking_take_while(|s| {
            s.ch() != '<' && s.ch() != '{' && s.ch() != '\n' && s.ch() != '}'
        })),
    }
}

/// Lex an attribute from the first letter of its name, which the caller has
/// already consumed.
///
/// E.g.
/// ```text
/// <div foo="bar">
///      ^^^^^^^^^
/// ```
/// Fails, after reporting, if what follows the name is not a value.
fn lex_attribute(
    iter: &mut Peekable<DocumentCursor>,
    errors: &mut ParseErrors,
    initial: DocumentRange,
) -> Result<TagToken, ErrorEmitted> {
    // consume: ('-' | '_' | ':' | '.' | [a-zA-Z0-9])*
    let name = initial.extend(iter.peeking_take_while(|s| {
        matches!(s.ch(), '-' | '_' | ':' | '.') || s.ch().is_ascii_alphanumeric()
    }));
    skip_whitespace(iter);
    // consume: '='
    let Some(eq) = iter.next_if(|s| s.ch() == '=') else {
        return Ok(TagToken::Attribute { name, value: None });
    };
    skip_whitespace(iter);
    // consume: '{'
    if let Some(left_brace) = iter.next_if(|s| s.ch() == '{') {
        return Ok(TagToken::AttributeExpressionStart { name, left_brace });
    }
    // consume: '\''
    if let Some(single_open) = iter.next_if(|s| s.ch() == '\'') {
        // Only double quotes are allowed, report error.
        let _value: Option<DocumentRange> = iter.peeking_take_while(|s| s.ch() != '\'').collect();
        let range = match iter.next_if(|s| s.ch() == '\'') {
            Some(single_close) => single_open.to(single_close),
            None => single_open,
        };
        return Err(errors.emit(ParseErrorKind::SingleQuotedAttributeValue {}, range));
    }
    // consume: '"'
    let Some(open_quote) = iter.next_if(|s| s.ch() == '"') else {
        return Err(errors.emit(ParseErrorKind::ExpectedQuotedAttributeValue {}, name.to(eq)));
    };
    // consume: [^"]*
    let content: Option<DocumentRange> = iter.peeking_take_while(|s| s.ch() != '"').collect();
    let Some(close_quote) = iter.next_if(|s| s.ch() == '"') else {
        return Err(errors.emit(
            ParseErrorKind::UnmatchedCharacter {
                ch: open_quote.ch(),
            },
            open_quote,
        ));
    };

    // `a=""` keeps a value, to tell it from a valueless `a`.
    Ok(TagToken::Attribute {
        name,
        value: Some(AttributeString {
            content_range: content,
            quoted_range: open_quote.to(close_quote),
        }),
    })
}
