use super::parse_helpers::{expect_token, next_if_eq, parse_delimited, parse_delimited_list};
use super::tokenize_expr::{next, peek};

use super::parsed_type::{ParsedModifier, ParsedType};
use super::token::LangToken;
use crate::document::{DocumentCursor, DocumentRange};
use crate::hop::parsing::parse_error::{Emit, ErrorEmitted, OrEmit, ParseError, ParseErrorKind};
use crate::hop::parsing::token::LangTokenPair;
use crate::symbols::type_name::TypeName;

pub fn parse_type(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Result<ParsedType, ErrorEmitted> {
    if let Some(type_range) = next_if_eq(iter, comments, errors, LangToken::TypeString) {
        return Ok(ParsedType::String { range: type_range });
    }
    if let Some(type_range) = next_if_eq(iter, comments, errors, LangToken::TypeInt) {
        return Ok(ParsedType::Int { range: type_range });
    }
    if let Some(type_range) = next_if_eq(iter, comments, errors, LangToken::TypeFloat) {
        return Ok(ParsedType::Float { range: type_range });
    }
    if let Some(type_range) = next_if_eq(iter, comments, errors, LangToken::TypeBool) {
        return Ok(ParsedType::Bool { range: type_range });
    }
    if let Some(type_range) = next_if_eq(iter, comments, errors, LangToken::TypeHtml) {
        return Ok(ParsedType::Html { range: type_range });
    }
    if let Some(type_array) = next_if_eq(iter, comments, errors, LangToken::TypeArray) {
        let left_bracket = expect_token(iter, comments, errors, &LangToken::LeftBracket)?;
        let (element, brackets) = parse_delimited(
            iter,
            comments,
            errors,
            LangTokenPair::Brackets,
            &left_bracket,
            parse_type,
        )?;
        return Ok(ParsedType::Array {
            element: Box::new(element),
            range: type_array.to(brackets),
        });
    }
    if let Some(type_option) = next_if_eq(iter, comments, errors, LangToken::TypeOption) {
        let left_bracket = expect_token(iter, comments, errors, &LangToken::LeftBracket)?;
        let (element, brackets) = parse_delimited(
            iter,
            comments,
            errors,
            LangTokenPair::Brackets,
            &left_bracket,
            parse_type,
        )?;
        return Ok(ParsedType::Option {
            element: Box::new(element),
            range: type_option.to(brackets),
        });
    }
    let modifier = match next_if_eq(iter, comments, errors, LangToken::Mut) {
        Some(range) => Some((ParsedModifier::Mut, range)),
        None => match next_if_eq(iter, comments, errors, LangToken::Read) {
            Some(range) => Some((ParsedModifier::Read, range)),
            None => next_if_eq(iter, comments, errors, LangToken::Render)
                .map(|range| (ParsedModifier::Render, range)),
        },
    };
    let fn_keyword = match &modifier {
        Some(_) => Some(expect_token(iter, comments, errors, &LangToken::Fn)?),
        None => next_if_eq(iter, comments, errors, LangToken::Fn),
    };
    if let Some(fn_keyword) = fn_keyword {
        let left_paren = expect_token(iter, comments, errors, &LangToken::LeftParen)?;
        let (parameters, _) = parse_delimited_list(
            iter,
            comments,
            errors,
            LangTokenPair::Parens,
            &left_paren,
            &[LangToken::Arrow],
            parse_type,
        )?;
        expect_token(iter, comments, errors, &LangToken::Arrow)?;
        let result = parse_type(iter, comments, errors)?;
        return Ok(ParsedType::Function {
            range: (modifier
                .as_ref()
                .map_or(fn_keyword, |(_, range)| range.clone()))
            .to(result.range().clone()),
            modifier,
            parameters,
            result: Box::new(result),
        });
    }
    if let Some(type_tuple) = next_if_eq(iter, comments, errors, LangToken::TypeTuple) {
        let left_bracket = expect_token(iter, comments, errors, &LangToken::LeftBracket)?;
        let (elements, brackets) = parse_delimited_list(
            iter,
            comments,
            errors,
            LangTokenPair::Brackets,
            &left_bracket,
            &[],
            parse_type,
        )?;
        return Ok(ParsedType::Tuple {
            elements,
            range: type_tuple.to(brackets),
        });
    }
    match peek(iter) {
        Some((LangToken::Identifier(name), type_range)) => {
            next(iter, comments, errors);
            Ok(ParsedType::Named {
                name: TypeName::new(name).or_emit(errors, &type_range)?,
                range: type_range,
            })
        }
        Some((actual, actual_range)) => Err(errors.emit(
            ParseErrorKind::ExpectedTypeNameButGot { actual },
            actual_range,
        )),
        None => Err(errors.emit(
            ParseErrorKind::ExpectedTypeNameButGotEof {},
            iter.eof_range(),
        )),
    }
}
