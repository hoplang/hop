use std::{collections::VecDeque, iter::Peekable};

use super::tokenize_expr::{expect_opposite, expect_token, next_collecting_comments as next};

use super::parsed_type::ParsedType;
use super::token::LangToken;
use crate::document::{DocumentCursor, DocumentRange};
use crate::parse_error::{ParseError, ParseErrorKind};
use crate::symbols::type_name::TypeName;

pub fn parse_type(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedType> {
    match next(iter, comments, errors) {
        Some((LangToken::TypeString, type_range)) => Some(ParsedType::String { range: type_range }),
        Some((LangToken::TypeInt, type_range)) => Some(ParsedType::Int { range: type_range }),
        Some((LangToken::TypeFloat, type_range)) => Some(ParsedType::Float { range: type_range }),
        Some((LangToken::TypeBoolean, type_range)) => Some(ParsedType::Bool { range: type_range }),
        Some((LangToken::TypeFragment, type_range)) => {
            Some(ParsedType::Fragment { range: type_range })
        }
        Some((LangToken::TypeArray, type_array)) => {
            let left_bracket =
                expect_token(iter, comments, errors, range, &LangToken::LeftBracket)?;
            let element = parse_type(iter, comments, errors, range)?;
            let right_bracket = expect_opposite(
                iter,
                comments,
                errors,
                &LangToken::LeftBracket,
                &left_bracket,
            )?;
            Some(ParsedType::Array {
                element: Box::new(element),
                range: type_array.to(right_bracket),
            })
        }
        Some((LangToken::TypeOption, type_option)) => {
            let left_bracket =
                expect_token(iter, comments, errors, range, &LangToken::LeftBracket)?;
            let element = parse_type(iter, comments, errors, range)?;
            let right_bracket = expect_opposite(
                iter,
                comments,
                errors,
                &LangToken::LeftBracket,
                &left_bracket,
            )?;
            Some(ParsedType::Option {
                element: Box::new(element),
                range: type_option.to(right_bracket),
            })
        }
        Some((LangToken::TypeName(name), type_range)) => match TypeName::from_cheap_string(name) {
            Ok(type_name) => Some(ParsedType::Named {
                name: type_name,
                range: type_range,
            }),
            Err(error) => {
                errors.push(ParseError::new(
                    ParseErrorKind::InvalidTypeName { error },
                    type_range,
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
