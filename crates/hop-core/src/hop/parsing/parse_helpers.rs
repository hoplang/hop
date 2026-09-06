use std::collections::VecDeque;
use std::iter::Peekable;

use crate::document::{DocumentCursor, DocumentRange};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

use super::token::LangToken;
use super::tokenize_expr::{next, peek};
use crate::parse_error::{ParseError, ParseErrorKind};

pub fn advance_if(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
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

pub fn next_if<F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    predicate: F,
) -> Option<(LangToken, DocumentRange)>
where
    F: FnOnce(&(LangToken, DocumentRange)) -> bool,
{
    match peek(iter) {
        Some(ref result) if predicate(result) => next(iter, comments, errors),
        _ => None,
    }
}

pub fn expect_token(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    expected: &LangToken,
) -> Option<DocumentRange> {
    match next(iter, comments, errors) {
        Some((token, token_range)) if token == *expected => Some(token_range),
        Some((actual, token_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTokenButGot {
                    expected: expected.clone(),
                    actual,
                },
                token_range,
            ));
            None
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTokenButGotEof {
                    expected: expected.clone(),
                },
                range.clone(),
            ));
            None
        }
    }
}

pub fn expect_opposite(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    token: &LangToken,
    token_range: &DocumentRange,
) -> Option<DocumentRange> {
    let expected = token.opposite_token();
    match next(iter, comments, errors) {
        Some((actual, actual_range)) if actual == expected => Some(actual_range),
        Some((actual, actual_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTokenButGot { expected, actual },
                actual_range,
            ));
            None
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::UnmatchedToken {
                    token: token.clone(),
                },
                token_range.clone(),
            ));
            None
        }
    }
}

pub fn expect_variable_name(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<(VarName, DocumentRange)> {
    match next(iter, comments, errors) {
        Some((LangToken::Identifier(name), name_range)) => {
            match VarName::from_cheap_string(name.clone()) {
                Ok(var_name) => Some((var_name, name_range)),
                Err(error) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::InvalidVariableName { name, error },
                        name_range,
                    ));
                    None
                }
            }
        }
        Some((actual, actual_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedVariableNameButGot { actual },
                actual_range,
            ));
            None
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::UnexpectedEof {},
                range.clone(),
            ));
            None
        }
    }
}

pub fn expect_field_name(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<(FieldName, DocumentRange)> {
    match next(iter, comments, errors) {
        Some((LangToken::Identifier(name), name_range)) => {
            match FieldName::from_cheap_string(name.clone()) {
                Ok(prop_name) => Some((prop_name, name_range)),
                Err(error) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::InvalidFieldName { name, error },
                        name_range,
                    ));
                    None
                }
            }
        }
        Some((token, token_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedFieldNameButGot { actual: token },
                token_range,
            ));
            None
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::UnexpectedEof {},
                range.clone(),
            ));
            None
        }
    }
}

pub fn expect_type_name(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<(TypeName, DocumentRange)> {
    match next(iter, comments, errors) {
        Some((LangToken::TypeName(name), name_range)) => match TypeName::from_cheap_string(name) {
            Ok(type_name) => Some((type_name, name_range)),
            Err(error) => {
                errors.push(ParseError::new(
                    ParseErrorKind::InvalidTypeName { error },
                    name_range,
                ));
                None
            }
        },
        Some((actual, actual_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTypeNameButGot { actual },
                actual_range,
            ));
            None
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTypeNameButGotEof {},
                range.clone(),
            ));
            None
        }
    }
}

pub fn parse_comma_separated<T, F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    mut parse: F,
    end_token: Option<&LangToken>,
) -> Option<Vec<T>>
where
    F: FnMut(
        &mut Peekable<DocumentCursor>,
        &mut VecDeque<DocumentRange>,
        &mut Vec<ParseError>,
        &DocumentRange,
    ) -> Option<T>,
{
    let mut items = Vec::new();
    items.push(parse(iter, comments, errors, range)?);
    while advance_if(iter, comments, errors, LangToken::Comma).is_some() {
        let next_token = peek(iter).map(|(t, _)| t);
        if next_token.as_ref() == end_token || (end_token.is_some() && next_token.is_none()) {
            break;
        }
        items.push(parse(iter, comments, errors, range)?);
    }

    Some(items)
}

pub fn parse_delimited_list<T, F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    opening_token: &LangToken,
    opening_range: &DocumentRange,
    parse: F,
) -> Option<(Vec<T>, DocumentRange)>
where
    F: FnMut(
        &mut Peekable<DocumentCursor>,
        &mut VecDeque<DocumentRange>,
        &mut Vec<ParseError>,
        &DocumentRange,
    ) -> Option<T>,
{
    let closing_token = opening_token.opposite_token();
    if let Some(closing_range) = advance_if(iter, comments, errors, closing_token.clone()) {
        return Some((Vec::new(), closing_range));
    }
    let items = parse_comma_separated(iter, comments, errors, range, parse, Some(&closing_token))?;
    let closing_range = expect_opposite(iter, comments, errors, opening_token, opening_range)?;
    Some((items, closing_range))
}
