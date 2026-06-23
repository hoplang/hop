use std::{collections::VecDeque, iter::Peekable};

use super::tokenizer::{expect_opposite, expect_token, next_collecting_comments as next};

use crate::document::{DocumentCursor, DocumentRange};
use crate::expr::{Token, parsing::ParsedType};
use crate::parse_error::ParseError;
use crate::symbols::type_name::TypeName;

pub fn parse_type(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedType> {
    match next(iter, comments, errors) {
        Some((Token::TypeString, type_range)) => Some(ParsedType::String { range: type_range }),
        Some((Token::TypeInt, type_range)) => Some(ParsedType::Int { range: type_range }),
        Some((Token::TypeFloat, type_range)) => Some(ParsedType::Float { range: type_range }),
        Some((Token::TypeBoolean, type_range)) => Some(ParsedType::Bool { range: type_range }),
        Some((Token::TypeFragment, type_range)) => Some(ParsedType::Fragment { range: type_range }),
        Some((Token::TypeArray, type_array)) => {
            let left_bracket = expect_token(iter, comments, errors, range, &Token::LeftBracket)?;
            let element = parse_type(iter, comments, errors, range)?;
            let right_bracket =
                expect_opposite(iter, comments, errors, &Token::LeftBracket, &left_bracket)?;
            Some(ParsedType::Array {
                element: Box::new(element),
                range: type_array.to(right_bracket),
            })
        }
        Some((Token::TypeOption, type_option)) => {
            let left_bracket = expect_token(iter, comments, errors, range, &Token::LeftBracket)?;
            let element = parse_type(iter, comments, errors, range)?;
            let right_bracket =
                expect_opposite(iter, comments, errors, &Token::LeftBracket, &left_bracket)?;
            Some(ParsedType::Option {
                element: Box::new(element),
                range: type_option.to(right_bracket),
            })
        }
        Some((Token::TypeName(name), type_range)) => match TypeName::from_cheap_string(name) {
            Ok(type_name) => Some(ParsedType::Named {
                name: type_name,
                range: type_range,
            }),
            Err(error) => {
                errors.push(ParseError::InvalidTypeName {
                    error,
                    range: type_range,
                });
                None
            }
        },
        Some((actual, actual_range)) => {
            errors.push(ParseError::ExpectedTypeNameButGot {
                actual,
                range: actual_range,
            });
            None
        }
        None => {
            errors.push(ParseError::ExpectedTypeNameButGotEof {
                range: range.clone(),
            });
            None
        }
    }
}
