use std::collections::VecDeque;
use std::iter::Peekable;

use crate::document::{CheapString, DocumentCursor, DocumentRange};
use crate::hop::parsing::token::LangTokenPair;

use super::token::LangToken;
use super::tokenize_expr::{next, peek};
use crate::parse_error::{ErrorEmitted, ParseErrorKind, ParseErrors};

/// The tokens that start a declaration, at which skipping over unexpected
/// tokens at the top level stops.
pub const DECLARATION_KEYWORDS: &[LangToken] = &[
    LangToken::Pub,
    LangToken::Import,
    LangToken::Record,
    LangToken::Enum,
    LangToken::Page,
    LangToken::Fn,
];

const RIGHT_DELIMITERS: &[LangToken] = &[
    LangToken::RightParen,
    LangToken::RightBracket,
    LangToken::RightBrace,
];

pub fn advance_if(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    token: LangToken,
) -> Option<DocumentRange> {
    match peek(iter) {
        Some((t, _)) if t == token => {
            let (_, range) = next(iter, comments, errors)?;
            Some(range)
        }
        _ => None,
    }
}

/// Skip tokens until one that satisfies `stop`, or end of input. Takes the
/// proof that whatever made the tokens unparseable was reported, and lexes
/// into a discard buffer, so that skipping reports nothing itself.
pub fn skip_to(
    iter: &mut Peekable<DocumentCursor>,
    _reported: ErrorEmitted,
    stop: impl Fn(&LangToken) -> bool,
) {
    let mut comments = VecDeque::new();
    let mut errors = ParseErrors::new();
    while let Some((token, _)) = peek(iter) {
        if stop(&token) {
            break;
        }
        next(iter, &mut comments, &mut errors);
    }
}

/// Consume the next token if `map` accepts it, returning what it mapped to
/// along with the token's range.
pub fn next_if_map<T>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    map: impl FnOnce(LangToken) -> Option<T>,
) -> Option<(T, DocumentRange)> {
    let (token, range) = peek(iter)?;
    let mapped = map(token)?;
    next(iter, comments, errors);
    Some((mapped, range))
}

pub fn expect_token(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    expected: &LangToken,
) -> Result<DocumentRange, ErrorEmitted> {
    if let Some(token_range) = advance_if(iter, comments, errors, expected.clone()) {
        return Ok(token_range);
    }
    Err(match peek(iter) {
        Some((actual, token_range)) => errors.emit(
            ParseErrorKind::ExpectedTokenButGot {
                expected: expected.clone(),
                actual,
            },
            token_range,
        ),
        None => errors.emit(
            ParseErrorKind::ExpectedTokenButGotEof {
                expected: expected.clone(),
            },
            eof_range.clone(),
        ),
    })
}
pub fn expect_right_delimiter(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    pair: LangTokenPair,
    left_delimiter_range: &DocumentRange,
) -> Result<DocumentRange, ErrorEmitted> {
    let expected = pair.right_delimiter();
    if let Some(actual_range) = advance_if(iter, comments, errors, expected.clone()) {
        return Ok(actual_range);
    }
    Err(match peek(iter) {
        Some((actual, actual_range)) => errors.emit(
            ParseErrorKind::ExpectedTokenButGot { expected, actual },
            actual_range,
        ),
        None => errors.emit(
            ParseErrorKind::UnmatchedToken {
                token: pair.left_delimiter(),
            },
            left_delimiter_range.clone(),
        ),
    })
}

pub fn expect_identifier(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<(CheapString, DocumentRange), ErrorEmitted> {
    match peek(iter) {
        Some((LangToken::Identifier(name), name_range)) => {
            next(iter, comments, errors);
            Ok((name, name_range))
        }
        Some((actual, actual_range)) => Err(errors.emit(
            ParseErrorKind::ExpectedIdentifierButGot { actual },
            actual_range,
        )),
        None => Err(errors.emit(ParseErrorKind::UnexpectedEof {}, eof_range.clone())),
    }
}

/// Parse one item after a left delimiter the caller has already consumed,
/// then the right delimiter that matches it. Returns the item with the range
/// from the left delimiter through the right.
pub fn parse_delimited<T, F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    pair: LangTokenPair,
    left_delimiter_range: &DocumentRange,
    parse: F,
) -> Result<(T, DocumentRange), ErrorEmitted>
where
    F: FnOnce(
        &mut Peekable<DocumentCursor>,
        &mut VecDeque<DocumentRange>,
        &mut ParseErrors,
        &DocumentRange,
    ) -> Result<T, ErrorEmitted>,
{
    let delimited = parse(iter, comments, errors, eof_range).and_then(|item| {
        let closing_range =
            expect_right_delimiter(iter, comments, errors, pair, left_delimiter_range)?;
        Ok((item, left_delimiter_range.clone().to(closing_range)))
    });
    if let Err(reported) = delimited {
        // Skip whatever could not be parsed, and the right delimiter with
        // it, so that the caller resumes after the delimited part.
        skip_to(iter, reported, |token| {
            RIGHT_DELIMITERS.contains(token) || DECLARATION_KEYWORDS.contains(token)
        });
        advance_if(iter, comments, errors, pair.right_delimiter());
    }
    delimited
}

/// Parse comma-separated items after a left delimiter the caller has already
/// consumed, then the right delimiter that matches it. Returns the items with
/// the range from the left delimiter through the right.
///
/// An item that fails to parse is dropped, and the tokens up to the next
/// comma are skipped with it. The list ends at any right delimiter, at any
/// declaration keyword, and at any token in `stops`, which names what may
/// follow the list, so that a missing right delimiter never swallows what
/// comes after.
pub fn parse_delimited_list<T, F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    pair: LangTokenPair,
    left_delimiter_range: &DocumentRange,
    stops: &[LangToken],
    mut parse: F,
) -> Result<(Vec<T>, DocumentRange), ErrorEmitted>
where
    F: FnMut(
        &mut Peekable<DocumentCursor>,
        &mut VecDeque<DocumentRange>,
        &mut ParseErrors,
        &DocumentRange,
    ) -> Result<T, ErrorEmitted>,
{
    let ends_list = |token: &LangToken| {
        RIGHT_DELIMITERS.contains(token)
            || DECLARATION_KEYWORDS.contains(token)
            || stops.contains(token)
    };
    let mut items = Vec::new();
    // The list ended on a token that cannot close it, and the failure that
    // got us there is already reported: propagate it rather than report the
    // stray token again here and in every list around this one.
    let mut failed_on_end = None;
    // Every pass either breaks or consumes at least one token: a comma, or
    // the unexpected token that stands where a comma should be.
    loop {
        if peek(iter).is_none_or(|(token, _)| ends_list(&token)) {
            break;
        }
        let failed = match parse(iter, comments, errors, eof_range) {
            Ok(item) => {
                items.push(item);
                None
            }
            Err(reported) => Some(reported),
        };
        if advance_if(iter, comments, errors, LangToken::Comma).is_some() {
            continue;
        }
        let Some((actual, actual_range)) = peek(iter) else {
            break;
        };
        if ends_list(&actual) {
            failed_on_end = failed.filter(|_| actual != pair.right_delimiter());
            break;
        }
        // Unexpected token follows the item. The item reported it if it failed
        // on it, otherwise it is a missing separator.
        let reported = failed.unwrap_or_else(|| {
            errors.emit(
                ParseErrorKind::ExpectedTokenButGot {
                    expected: LangToken::Comma,
                    actual,
                },
                actual_range,
            )
        });
        skip_to(iter, reported, |token| {
            *token == LangToken::Comma || ends_list(token)
        });
        if advance_if(iter, comments, errors, LangToken::Comma).is_some() {
            continue;
        }
        // Skipping stopped on a token that ends the list rather than on a
        // comma.
        if peek(iter).is_some_and(|(token, _)| token != pair.right_delimiter()) {
            failed_on_end = Some(reported);
        }
        break;
    }
    if let Some(reported) = failed_on_end {
        return Err(reported);
    }
    let closing_range = expect_right_delimiter(iter, comments, errors, pair, left_delimiter_range)?;
    Ok((items, left_delimiter_range.clone().to(closing_range)))
}
