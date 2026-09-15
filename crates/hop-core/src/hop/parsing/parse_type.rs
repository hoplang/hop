use std::{collections::VecDeque, iter::Peekable};

use super::parse_helpers::{advance_if, expect_token, parse_delimited, parse_delimited_list};
use super::tokenize_expr::{next, peek, peek2};

use super::parsed_type::ParsedType;
use super::token::LangToken;
use crate::document::{DocumentCursor, DocumentRange};
use crate::hop::parsing::token::LangTokenPair;
use crate::parse_error::{ErrorEmitted, OrEmit, ParseErrorKind, ParseErrors};
use crate::symbols::type_name::TypeName;

pub fn parse_type(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    range: &DocumentRange,
) -> Result<ParsedType, ErrorEmitted> {
    if let Some(type_range) = advance_if(iter, comments, errors, LangToken::TypeString) {
        return Ok(ParsedType::String { range: type_range });
    }
    if let Some(type_range) = advance_if(iter, comments, errors, LangToken::TypeInt) {
        return Ok(ParsedType::Int { range: type_range });
    }
    if let Some(type_range) = advance_if(iter, comments, errors, LangToken::TypeFloat) {
        return Ok(ParsedType::Float { range: type_range });
    }
    if let Some(type_range) = advance_if(iter, comments, errors, LangToken::TypeBoolean) {
        return Ok(ParsedType::Bool { range: type_range });
    }
    if let Some(type_range) = advance_if(iter, comments, errors, LangToken::TypeHtml) {
        return Ok(ParsedType::Html { range: type_range });
    }
    if let Some(type_array) = advance_if(iter, comments, errors, LangToken::TypeArray) {
        let left_bracket = expect_token(iter, comments, errors, range, &LangToken::LeftBracket)?;
        let (element, brackets) = parse_delimited(
            iter,
            comments,
            errors,
            range,
            LangTokenPair::Brackets,
            &left_bracket,
            parse_type,
        )?;
        return Ok(ParsedType::Array {
            element: Box::new(element),
            range: type_array.to(brackets),
        });
    }
    if let Some(type_option) = advance_if(iter, comments, errors, LangToken::TypeOption) {
        let left_bracket = expect_token(iter, comments, errors, range, &LangToken::LeftBracket)?;
        let (element, brackets) = parse_delimited(
            iter,
            comments,
            errors,
            range,
            LangTokenPair::Brackets,
            &left_bracket,
            parse_type,
        )?;
        return Ok(ParsedType::Option {
            element: Box::new(element),
            range: type_option.to(brackets),
        });
    }
    if let Some(left_paren) = advance_if(iter, comments, errors, LangToken::LeftParen) {
        let mut trailing_comma = false;
        let (elements, parens) = parse_delimited_list(
            iter,
            comments,
            errors,
            range,
            LangTokenPair::Parens,
            &left_paren,
            &[],
            |iter, comments, errors, range| {
                let element = parse_type(iter, comments, errors, range)?;
                trailing_comma = matches!(peek(iter), Some((LangToken::Comma, _)))
                    && matches!(peek2(iter), Some((LangToken::RightParen, _)));
                Ok(element)
            },
        )?;
        let tuple_range = left_paren.to(parens);
        let mut elements = elements;
        if elements.len() == 1 && !trailing_comma {
            return Ok(elements.remove(0));
        }
        return Ok(ParsedType::Tuple {
            elements,
            range: tuple_range,
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
        None => Err(errors.emit(ParseErrorKind::ExpectedTypeNameButGotEof {}, range.clone())),
    }
}
