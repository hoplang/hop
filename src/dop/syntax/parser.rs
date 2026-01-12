use std::collections::{HashSet, VecDeque};
use std::iter::Peekable;

use crate::document::document_cursor::{DocumentCursor, DocumentRange, CheapString};
use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::symbols::var_name::VarName;
use crate::error_collector::ErrorCollector;
use crate::hop::symbols::module_name::ModuleName;

use super::parse_error::ParseError;
use super::parsed::{
    Constructor, ParsedBinaryOp, ParsedDeclaration, ParsedExpr, ParsedMatchArm, ParsedMatchPattern,
    ParsedType,
};
use super::token::Token;
use super::tokenizer;

fn next(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
) -> Option<Result<(Token, DocumentRange), ParseError>> {
    tokenizer::next_collecting_comments(iter, comments)
}

fn peek(iter: &Peekable<DocumentCursor>) -> Option<Result<(Token, DocumentRange), ParseError>> {
    tokenizer::peek_past_comments(iter)
}

fn next_if<F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    predicate: F,
) -> Option<Result<(Token, DocumentRange), ParseError>>
where
    F: FnOnce(&Result<(Token, DocumentRange), ParseError>) -> bool,
{
    match peek(iter) {
        Some(ref result) if predicate(result) => next(iter, comments),
        _ => None,
    }
}

fn advance_if(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    token: Token,
) -> Option<DocumentRange> {
    match next_if(iter, comments, |res| {
        res.as_ref().is_ok_and(|(t, _)| *t == token)
    }) {
        Some(Ok((_, range))) => Some(range),
        _ => None,
    }
}

fn expect_token(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    expected: &Token,
) -> Result<DocumentRange, ParseError> {
    match next(iter, comments).transpose()? {
        Some((token, token_range)) if token == *expected => Ok(token_range),
        Some((actual, token_range)) => Err(ParseError::ExpectedTokenButGot {
            expected: expected.clone(),
            actual,
            range: token_range,
        }),
        None => Err(ParseError::ExpectedTokenButGotEof {
            expected: expected.clone(),
            range: range.clone(),
        }),
    }
}

fn expect_opposite(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    token: &Token,
    token_range: &DocumentRange,
) -> Result<DocumentRange, ParseError> {
    let expected = token.opposite_token();

    match next(iter, comments).transpose()? {
        Some((actual, actual_range)) if actual == expected => Ok(actual_range),
        Some((actual, actual_range)) => Err(ParseError::ExpectedTokenButGot {
            expected,
            actual,
            range: actual_range,
        }),
        None => Err(ParseError::UnmatchedToken {
            token: token.clone(),
            range: token_range.clone(),
        }),
    }
}

fn expect_variable_name(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<(VarName, DocumentRange), ParseError> {
    match next(iter, comments).transpose()? {
        Some((Token::Identifier(name), name_range)) => {
            let var_name =
                VarName::new(name.as_str()).map_err(|error| ParseError::InvalidVariableName {
                    name,
                    error,
                    range: name_range.clone(),
                })?;
            Ok((var_name, name_range))
        }
        Some((actual, actual_range)) => Err(ParseError::ExpectedVariableNameButGot {
            actual,
            range: actual_range,
        }),
        None => Err(ParseError::UnexpectedEof {
            range: range.clone(),
        }),
    }
}

fn expect_field_name(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<(FieldName, DocumentRange), ParseError> {
    match next(iter, comments).transpose()? {
        Some((Token::Identifier(name), name_range)) => {
            let prop_name =
                FieldName::new(name.as_str()).map_err(|error| ParseError::InvalidFieldName {
                    name,
                    error,
                    range: name_range.clone(),
                })?;
            Ok((prop_name, name_range))
        }
        Some((token, token_range)) => Err(ParseError::ExpectedFieldNameButGot {
            actual: token,
            range: token_range,
        }),
        None => Err(ParseError::UnexpectedEof {
            range: range.clone(),
        }),
    }
}

fn expect_type_name(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<(TypeName, DocumentRange), ParseError> {
    match next(iter, comments).transpose()? {
        Some((Token::TypeName(name), name_range)) => {
            let type_name =
                TypeName::new(name.as_str()).map_err(|error| ParseError::InvalidTypeName {
                    error,
                    range: name_range.clone(),
                })?;
            Ok((type_name, name_range))
        }
        Some((actual, actual_range)) => Err(ParseError::ExpectedTypeNameButGot {
            actual,
            range: actual_range,
        }),
        None => Err(ParseError::ExpectedTypeNameButGotEof {
            range: range.clone(),
        }),
    }
}

fn expect_eof(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
) -> Result<(), ParseError> {
    match next(iter, comments).transpose()? {
        None => Ok(()),
        Some((token, token_range)) => Err(ParseError::UnexpectedToken {
            token,
            range: token_range,
        }),
    }
}

fn parse_comma_separated<F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    mut parse: F,
    end_token: Option<&Token>,
) -> Result<(), ParseError>
where
    F: FnMut(
        &mut Peekable<DocumentCursor>,
        &mut VecDeque<(CheapString, DocumentRange)>,
        &DocumentRange,
    ) -> Result<(), ParseError>,
{
    parse(iter, comments, range)?;
    while advance_if(iter, comments, Token::Comma).is_some() {
        let at_end = peek(iter).and_then(|r| r.ok()).map(|(t, _)| t).as_ref() == end_token;
        if at_end {
            break;
        }
        parse(iter, comments, range)?;
    }

    Ok(())
}

fn parse_delimited_list<F>(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    opening_token: &Token,
    opening_range: &DocumentRange,
    parse: F,
) -> Result<DocumentRange, ParseError>
where
    F: FnMut(
        &mut Peekable<DocumentCursor>,
        &mut VecDeque<(CheapString, DocumentRange)>,
        &DocumentRange,
    ) -> Result<(), ParseError>,
{
    let closing_token = opening_token.opposite_token();
    if let Some(closing_range) = advance_if(iter, comments, closing_token.clone()) {
        return Ok(closing_range);
    }
    parse_comma_separated(iter, comments, range, parse, Some(&closing_token))?;
    expect_opposite(iter, comments, opening_token, opening_range)
}

pub fn parse_expr(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let result = parse_logical(iter, comments, range)?;
    expect_eof(iter, comments)?;
    Ok(result)
}

pub fn parse_loop_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<(VarName, DocumentRange, ParsedExpr), ParseError> {
    let (var_name, var_name_range) = expect_variable_name(iter, comments, range)?;
    expect_token(iter, comments, range, &Token::In)?;
    let array_expr = parse_logical(iter, comments, range)?;
    expect_eof(iter, comments)?;
    Ok((var_name, var_name_range, array_expr))
}

pub fn parse_let_bindings(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<Vec<(VarName, DocumentRange, ParsedType, ParsedExpr)>, ParseError> {
    let mut bindings = Vec::new();
    parse_comma_separated(
        iter,
        comments,
        range,
        |iter, comments, range| {
            let (var_name, var_name_range) = expect_variable_name(iter, comments, range)?;
            expect_token(iter, comments, range, &Token::Colon)?;
            let var_type = parse_type(iter, comments, range)?;
            expect_token(iter, comments, range, &Token::Assign)?;
            let value_expr = parse_logical(iter, comments, range)?;
            bindings.push((var_name, var_name_range, var_type, value_expr));
            Ok(())
        },
        None,
    )?;
    expect_eof(iter, comments)?;
    Ok(bindings)
}

fn parse_parameter(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<((VarName, DocumentRange), ParsedType, Option<ParsedExpr>), ParseError> {
    let (var_name, var_name_range) = expect_variable_name(iter, comments, range)?;
    expect_token(iter, comments, range, &Token::Colon)?;
    let var_type = parse_type(iter, comments, range)?;
    let default_value = if advance_if(iter, comments, Token::Assign).is_some() {
        Some(parse_primary(iter, comments, range)?)
    } else {
        None
    };
    Ok(((var_name, var_name_range), var_type, default_value))
}

pub fn parse_parameters(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<Vec<((VarName, DocumentRange), ParsedType, Option<ParsedExpr>)>, ParseError> {
    let mut params = Vec::new();
    let mut seen_names = HashSet::new();
    parse_comma_separated(
        iter,
        comments,
        range,
        |iter, comments, range| {
            let param = parse_parameter(iter, comments, range)?;
            let (var_name, var_name_range) = &param.0;
            if !seen_names.insert(var_name.as_str().to_string()) {
                return Err(ParseError::DuplicateParameter {
                    name: var_name_range.to_cheap_string(),
                    range: var_name_range.clone(),
                });
            }
            params.push(param);
            Ok(())
        },
        None,
    )?;
    expect_eof(iter, comments)?;
    Ok(params)
}

pub fn parse_type(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedType, ParseError> {
    match next(iter, comments).transpose()? {
        Some((Token::TypeString, type_range)) => Ok(ParsedType::String { range: type_range }),
        Some((Token::TypeInt, type_range)) => Ok(ParsedType::Int { range: type_range }),
        Some((Token::TypeFloat, type_range)) => Ok(ParsedType::Float { range: type_range }),
        Some((Token::TypeBoolean, type_range)) => Ok(ParsedType::Bool { range: type_range }),
        Some((Token::TypeTrustedHTML, type_range)) => {
            Ok(ParsedType::TrustedHTML { range: type_range })
        }
        Some((Token::TypeArray, type_array)) => {
            let left_bracket = expect_token(iter, comments, range, &Token::LeftBracket)?;
            let element = parse_type(iter, comments, range)?;
            let right_bracket =
                expect_opposite(iter, comments, &Token::LeftBracket, &left_bracket)?;
            Ok(ParsedType::Array {
                element: Box::new(element),
                range: type_array.to(right_bracket),
            })
        }
        Some((Token::TypeOption, type_option)) => {
            let left_bracket = expect_token(iter, comments, range, &Token::LeftBracket)?;
            let element = parse_type(iter, comments, range)?;
            let right_bracket =
                expect_opposite(iter, comments, &Token::LeftBracket, &left_bracket)?;
            Ok(ParsedType::Option {
                element: Box::new(element),
                range: type_option.to(right_bracket),
            })
        }
        Some((Token::TypeName(name), type_range)) => Ok(ParsedType::Named {
            name,
            range: type_range,
        }),
        Some((actual, actual_range)) => Err(ParseError::ExpectedTypeNameButGot {
            actual,
            range: actual_range,
        }),
        None => Err(ParseError::ExpectedTypeNameButGotEof {
            range: range.clone(),
        }),
    }
}

fn parse_logical(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut expr = parse_logical_and(iter, comments, range)?;
    while advance_if(iter, comments, Token::LogicalOr).is_some() {
        let right = parse_logical_and(iter, comments, range)?;
        expr = ParsedExpr::BinaryOp {
            range: expr.range().clone().to(right.range().clone()),
            left: Box::new(expr),
            operator: ParsedBinaryOp::LogicalOr,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn parse_logical_and(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut expr = parse_equality(iter, comments, range)?;
    while advance_if(iter, comments, Token::LogicalAnd).is_some() {
        let right = parse_equality(iter, comments, range)?;
        expr = ParsedExpr::BinaryOp {
            range: expr.range().clone().to(right.range().clone()),
            left: Box::new(expr),
            operator: ParsedBinaryOp::LogicalAnd,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn parse_equality(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut expr = parse_relational(iter, comments, range)?;
    loop {
        if advance_if(iter, comments, Token::Eq).is_some() {
            let right = parse_relational(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Eq,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, Token::NotEq).is_some() {
            let right = parse_relational(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::NotEq,
                right: Box::new(right),
            };
        } else {
            break;
        }
    }
    Ok(expr)
}

fn parse_relational(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut expr = parse_additive(iter, comments, range)?;
    loop {
        if advance_if(iter, comments, Token::LessThan).is_some() {
            let right = parse_additive(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::LessThan,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, Token::GreaterThan).is_some() {
            let right = parse_additive(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::GreaterThan,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, Token::LessThanOrEqual).is_some() {
            let right = parse_additive(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::LessThanOrEqual,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, Token::GreaterThanOrEqual).is_some() {
            let right = parse_additive(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::GreaterThanOrEqual,
                right: Box::new(right),
            };
        } else {
            break;
        }
    }
    Ok(expr)
}

fn parse_additive(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut expr = parse_multiplicative(iter, comments, range)?;
    loop {
        if advance_if(iter, comments, Token::Plus).is_some() {
            let right = parse_multiplicative(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Plus,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, Token::Minus).is_some() {
            let right = parse_multiplicative(iter, comments, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Minus,
                right: Box::new(right),
            };
        } else {
            break;
        }
    }
    Ok(expr)
}

fn parse_multiplicative(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut expr = parse_unary(iter, comments, range)?;
    while advance_if(iter, comments, Token::Asterisk).is_some() {
        let right = parse_unary(iter, comments, range)?;
        expr = ParsedExpr::BinaryOp {
            range: expr.range().clone().to(right.range().clone()),
            left: Box::new(expr),
            operator: ParsedBinaryOp::Multiply,
            right: Box::new(right),
        };
    }
    Ok(expr)
}

fn parse_unary(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    if let Some(operator_range) = advance_if(iter, comments, Token::Not) {
        let expr = parse_unary(iter, comments, range)?; // Right associative for multiple !
        Ok(ParsedExpr::Negation {
            range: operator_range.to(expr.range().clone()),
            operand: Box::new(expr),
        })
    } else {
        parse_primary(iter, comments, range)
    }
}

fn parse_array_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    left_bracket: DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let mut elements = Vec::new();
    let right_bracket = parse_delimited_list(
        iter,
        comments,
        range,
        &Token::LeftBracket,
        &left_bracket,
        |iter, comments, range| {
            elements.push(parse_logical(iter, comments, range)?);
            Ok(())
        },
    )?;
    Ok(ParsedExpr::ArrayLiteral {
        elements,
        range: left_bracket.to(right_bracket),
    })
}

fn parse_field_access(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    _range: &DocumentRange,
    identifier: CheapString,
    id_range: DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let var_name =
        VarName::new(identifier.as_str()).map_err(|error| ParseError::InvalidVariableName {
            name: identifier,
            error,
            range: id_range.clone(),
        })?;
    let mut expr = ParsedExpr::Var {
        range: id_range,
        value: var_name,
    };

    while let Some(dot) = advance_if(iter, comments, Token::Dot) {
        match next(iter, comments).transpose()? {
            Some((Token::Identifier(field_ident), field_range)) => {
                // Validate field name
                let field_name = FieldName::new(field_ident.as_str()).map_err(|error| {
                    ParseError::InvalidFieldName {
                        name: field_ident,
                        error,
                        range: field_range.clone(),
                    }
                })?;
                let new_range = expr.range().clone().to(field_range);
                expr = ParsedExpr::FieldAccess {
                    record: Box::new(expr),
                    field: field_name,
                    range: new_range,
                };
            }
            Some((_, token_range)) => {
                return Err(ParseError::ExpectedIdentifierAfterDot { range: token_range });
            }
            None => {
                return Err(ParseError::UnexpectedEndOfFieldAccess {
                    range: expr.range().clone().to(dot),
                });
            }
        }
    }
    Ok(expr)
}

fn parse_primary(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    match next(iter, comments).transpose()? {
        Some((Token::Identifier(name), name_range)) => {
            // Check for macro invocation: identifier!
            if advance_if(iter, comments, Token::Not).is_some() {
                return parse_macro_invocation(iter, comments, range, name, name_range);
            }
            parse_field_access(iter, comments, range, name, name_range)
        }
        Some((Token::TypeName(name), name_range)) => {
            let is_enum_variant =
                peek(iter).and_then(|r| r.ok()).map(|(t, _)| t) == Some(Token::ColonColon);
            if is_enum_variant {
                parse_enum_literal(iter, comments, range, name, name_range)
            } else {
                parse_record_literal(iter, comments, range, name, name_range)
            }
        }
        Some((Token::StringLiteral(value), lit_range)) => Ok(ParsedExpr::StringLiteral {
            value,
            range: lit_range,
        }),
        Some((Token::True, lit_range)) => Ok(ParsedExpr::BooleanLiteral {
            value: true,
            range: lit_range,
        }),
        Some((Token::False, lit_range)) => Ok(ParsedExpr::BooleanLiteral {
            value: false,
            range: lit_range,
        }),
        Some((Token::IntLiteral(value), lit_range)) => Ok(ParsedExpr::IntLiteral {
            value,
            range: lit_range,
        }),
        Some((Token::FloatLiteral(value), lit_range)) => Ok(ParsedExpr::FloatLiteral {
            value,
            range: lit_range,
        }),
        Some((Token::LeftBracket, left_bracket)) => {
            parse_array_literal(iter, comments, range, left_bracket)
        }
        Some((Token::LeftParen, left_paren)) => {
            let expr = parse_logical(iter, comments, range)?;
            expect_opposite(iter, comments, &Token::LeftParen, &left_paren)?;
            Ok(expr)
        }
        Some((Token::Match, match_range)) => parse_match_expr(iter, comments, range, match_range),
        Some((Token::Some, some_range)) => {
            let left_paren = expect_token(iter, comments, range, &Token::LeftParen)?;
            let value = parse_logical(iter, comments, range)?;
            let right_paren = expect_opposite(iter, comments, &Token::LeftParen, &left_paren)?;
            Ok(ParsedExpr::OptionLiteral {
                value: Some(Box::new(value)),
                range: some_range.to(right_paren),
            })
        }
        Some((Token::None, none_range)) => Ok(ParsedExpr::OptionLiteral {
            value: None,
            range: none_range,
        }),
        Some((token, token_range)) => Err(ParseError::UnexpectedToken {
            token,
            range: token_range,
        }),
        None => Err(ParseError::UnexpectedEof {
            range: range.clone(),
        }),
    }
}

fn parse_macro_invocation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    macro_name: CheapString,
    name_range: DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let name_str = macro_name.as_str();
    if name_str != "classes" {
        return Err(ParseError::UnknownMacro {
            name: macro_name,
            range: name_range,
        });
    }

    let left_paren = expect_token(iter, comments, range, &Token::LeftParen)?;
    let mut args = Vec::new();
    let right_paren = parse_delimited_list(
        iter,
        comments,
        range,
        &Token::LeftParen,
        &left_paren,
        |iter, comments, range| {
            args.push(parse_logical(iter, comments, range)?);
            Ok(())
        },
    )?;

    Ok(ParsedExpr::MacroInvocation {
        name: name_str.to_string(),
        args,
        range: name_range.to(right_paren),
    })
}

fn parse_record_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    name: CheapString,
    name_range: DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let left_paren = expect_token(iter, comments, range, &Token::LeftParen)?;
    let mut fields = Vec::new();
    let right_paren = parse_delimited_list(
        iter,
        comments,
        range,
        &Token::LeftParen,
        &left_paren,
        |iter, comments, range| {
            let (field_name, _) = expect_field_name(iter, comments, range)?;
            expect_token(iter, comments, range, &Token::Colon)?;
            fields.push((field_name, parse_logical(iter, comments, range)?));
            Ok(())
        },
    )?;
    Ok(ParsedExpr::RecordLiteral {
        record_name: name.as_str().to_string(),
        fields,
        range: name_range.to(right_paren),
    })
}

fn parse_enum_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    enum_name: CheapString,
    enum_name_range: DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    expect_token(iter, comments, range, &Token::ColonColon)?;
    let (variant_name, variant_range) = expect_type_name(iter, comments, range)?;
    let constructor_range = enum_name_range.clone().to(variant_range.clone());

    let (fields, end_range) = if let Some(left_paren) = advance_if(iter, comments, Token::LeftParen)
    {
        let mut fields = Vec::new();
        let right_paren = parse_delimited_list(
            iter,
            comments,
            range,
            &Token::LeftParen,
            &left_paren,
            |iter, comments, range| {
                let (field_name, field_name_range) = expect_field_name(iter, comments, range)?;
                expect_token(iter, comments, range, &Token::Colon)?;
                fields.push((
                    field_name,
                    field_name_range,
                    parse_logical(iter, comments, range)?,
                ));
                Ok(())
            },
        )?;
        (fields, right_paren)
    } else {
        (Vec::new(), variant_range)
    };

    Ok(ParsedExpr::EnumLiteral {
        enum_name: enum_name.as_str().to_string(),
        variant_name: variant_name.as_str().to_string(),
        fields,
        constructor_range,
        range: enum_name_range.to(end_range),
    })
}

pub fn parse_match_pattern(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedMatchPattern, ParseError> {
    if let Some(pattern_range) = advance_if(iter, comments, Token::Underscore) {
        return Ok(ParsedMatchPattern::Wildcard {
            range: pattern_range,
        });
    }

    if let Some(pattern_range) = advance_if(iter, comments, Token::True) {
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::BooleanTrue,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            range: pattern_range,
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, Token::False) {
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::BooleanFalse,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            range: pattern_range,
        });
    }

    if let Some(some_range) = advance_if(iter, comments, Token::Some) {
        expect_token(iter, comments, range, &Token::LeftParen)?;
        let inner_pattern = parse_match_pattern(iter, comments, range)?;
        let right_paren = expect_token(iter, comments, range, &Token::RightParen)?;
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::OptionSome,
            args: vec![inner_pattern],
            fields: Vec::new(),
            constructor_range: some_range.clone(),
            range: some_range.to(right_paren),
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, Token::None) {
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::OptionNone,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            range: pattern_range,
        });
    }

    if let Some(Ok((Token::TypeName(type_name_str), type_name_range))) =
        next_if(iter, comments, |res| {
            matches!(res, Ok((Token::TypeName(_), _)))
        })
    {
        let type_name =
            TypeName::new(&type_name_str).map_err(|error| ParseError::InvalidTypeName {
                error,
                range: type_name_range.clone(),
            })?;

        if advance_if(iter, comments, Token::ColonColon).is_some() {
            let (variant_name, variant_range) = expect_type_name(iter, comments, range)?;

            let (fields, end_range) = if let Some(left_paren) =
                advance_if(iter, comments, Token::LeftParen)
            {
                let mut fields = Vec::new();
                let right_paren = parse_delimited_list(
                    iter,
                    comments,
                    range,
                    &Token::LeftParen,
                    &left_paren,
                    |iter, comments, range| {
                        let (field_name, field_range) = expect_field_name(iter, comments, range)?;
                        expect_token(iter, comments, range, &Token::Colon)?;
                        let pattern = parse_match_pattern(iter, comments, range)?;
                        fields.push((field_name, field_range, pattern));
                        Ok(())
                    },
                )?;
                (fields, right_paren)
            } else {
                (Vec::new(), variant_range.clone())
            };

            let constructor_range = type_name_range.clone().to(variant_range);
            return Ok(ParsedMatchPattern::Constructor {
                constructor: Constructor::EnumVariant {
                    enum_name: type_name,
                    variant_name: CheapString::new(variant_name.to_string()),
                },
                args: Vec::new(),
                fields,
                constructor_range,
                range: type_name_range.to(end_range),
            });
        } else {
            let left_paren = expect_token(iter, comments, range, &Token::LeftParen)?;
            let mut fields = Vec::new();
            let right_paren = parse_delimited_list(
                iter,
                comments,
                range,
                &Token::LeftParen,
                &left_paren,
                |iter, comments, range| {
                    let (field_name, field_range) = expect_field_name(iter, comments, range)?;
                    expect_token(iter, comments, range, &Token::Colon)?;
                    let pattern = parse_match_pattern(iter, comments, range)?;
                    fields.push((field_name, field_range, pattern));
                    Ok(())
                },
            )?;
            return Ok(ParsedMatchPattern::Constructor {
                constructor: Constructor::Record { type_name },
                args: Vec::new(),
                fields,
                constructor_range: type_name_range.clone(),
                range: type_name_range.to(right_paren),
            });
        }
    }

    let (var_name, var_range) = expect_variable_name(iter, comments, range)?;
    Ok(ParsedMatchPattern::Binding {
        name: var_name.to_string(),
        range: var_range,
    })
}

fn parse_match_expr(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    match_range: DocumentRange,
) -> Result<ParsedExpr, ParseError> {
    let subject = parse_primary(iter, comments, range)?;
    let left_brace = expect_token(iter, comments, range, &Token::LeftBrace)?;

    let mut arms = Vec::new();
    let right_brace = parse_delimited_list(
        iter,
        comments,
        range,
        &Token::LeftBrace,
        &left_brace,
        |iter, comments, range| {
            let pattern = parse_match_pattern(iter, comments, range)?;
            expect_token(iter, comments, range, &Token::FatArrow)?;
            let body = parse_logical(iter, comments, range)?;
            arms.push(ParsedMatchArm { pattern, body });
            Ok(())
        },
    )?;

    Ok(ParsedExpr::Match {
        subject: Box::new(subject),
        arms,
        range: match_range.to(right_brace),
    })
}

fn parse_import_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedDeclaration, ParseError> {
    let import_range = expect_token(iter, comments, range, &Token::Import)?;

    let mut path_segments: Vec<DocumentRange> = Vec::new();

    let first_segment = match next(iter, comments).transpose()? {
        Some((Token::Identifier(_), seg_range)) | Some((Token::TypeName(_), seg_range)) => {
            seg_range
        }
        Some((_, seg_range)) => {
            return Err(ParseError::ExpectedModulePath { range: seg_range });
        }
        None => {
            return Err(ParseError::ExpectedModulePath {
                range: range.clone(),
            });
        }
    };
    path_segments.push(first_segment);

    while advance_if(iter, comments, Token::ColonColon).is_some() {
        let segment = match next(iter, comments).transpose()? {
            Some((Token::Identifier(_), seg_range)) | Some((Token::TypeName(_), seg_range)) => {
                seg_range
            }
            Some((_, seg_range)) => {
                return Err(ParseError::ExpectedIdentifierAfterColonColon { range: seg_range });
            }
            None => {
                return Err(ParseError::ExpectedIdentifierAfterColonColon {
                    range: range.clone(),
                });
            }
        };
        path_segments.push(segment);
    }

    if path_segments.len() < 2 {
        return Err(ParseError::ImportPathTooShort {
            range: path_segments[0].clone(),
        });
    }

    let name_range = path_segments.pop().unwrap();

    let name = TypeName::new(name_range.as_str()).map_err(|e| ParseError::InvalidTypeName {
        error: e,
        range: name_range.clone(),
    })?;

    let module_path_str = path_segments
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<_>>()
        .join("/");

    let module_name =
        ModuleName::new(&module_path_str).map_err(|e| ParseError::InvalidModuleName {
            error: e,
            range: path_segments
                .first()
                .unwrap()
                .clone()
                .to(path_segments.last().unwrap().clone()),
        })?;

    let path_range = path_segments
        .first()
        .unwrap()
        .clone()
        .to(name_range.clone());

    Ok(ParsedDeclaration::Import {
        name,
        name_range: name_range.clone(),
        path: path_range,
        module_name,
        range: import_range.to(name_range),
    })
}

fn parse_record_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedDeclaration, ParseError> {
    let start_range = expect_token(iter, comments, range, &Token::Record)?;
    let (name, name_range) = expect_type_name(iter, comments, range)?;
    let left_brace = expect_token(iter, comments, range, &Token::LeftBrace)?;

    let mut fields = Vec::new();
    let mut seen_names = HashSet::new();
    let right_brace = parse_delimited_list(
        iter,
        comments,
        range,
        &Token::LeftBrace,
        &left_brace,
        |iter, comments, range| {
            let (field_name, field_name_range) = expect_field_name(iter, comments, range)?;
            expect_token(iter, comments, range, &Token::Colon)?;
            let field_type = parse_type(iter, comments, range)?;
            if !seen_names.insert(field_name.as_str().to_string()) {
                return Err(ParseError::DuplicateField {
                    name: field_name_range.to_cheap_string(),
                    range: field_name_range.clone(),
                });
            }
            fields.push((field_name, field_name_range, field_type));
            Ok(())
        },
    )?;

    let full_range = start_range.to(right_brace);

    Ok(ParsedDeclaration::Record {
        name,
        name_range,
        fields,
        range: full_range,
    })
}

fn parse_enum_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<ParsedDeclaration, ParseError> {
    let start_range = expect_token(iter, comments, range, &Token::Enum)?;
    let (name, name_range) = expect_type_name(iter, comments, range)?;
    let left_brace = expect_token(iter, comments, range, &Token::LeftBrace)?;

    let mut variants = Vec::new();
    let mut seen_names = HashSet::new();
    let right_brace = parse_delimited_list(
        iter,
        comments,
        range,
        &Token::LeftBrace,
        &left_brace,
        |iter, comments, range| {
            let (variant_name, variant_range) = expect_type_name(iter, comments, range)?;
            if !seen_names.insert(variant_name.as_str().to_string()) {
                return Err(ParseError::DuplicateVariant {
                    name: variant_range.to_cheap_string(),
                    range: variant_range.clone(),
                });
            }

            let fields = if advance_if(iter, comments, Token::LeftParen).is_some() {
                parse_enum_variant_fields(iter, comments, range)?
            } else {
                Vec::new()
            };

            variants.push((variant_name, variant_range, fields));
            Ok(())
        },
    )?;

    let full_range = start_range.to(right_brace);

    Ok(ParsedDeclaration::Enum {
        name,
        name_range,
        variants,
        range: full_range,
    })
}

fn parse_enum_variant_fields(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
) -> Result<Vec<(FieldName, DocumentRange, ParsedType)>, ParseError> {
    let mut fields = Vec::new();
    let mut seen_names = HashSet::new();

    if advance_if(iter, comments, Token::RightParen).is_some() {
        return Ok(fields);
    }

    loop {
        let (field_name, field_name_range) = expect_field_name(iter, comments, range)?;
        expect_token(iter, comments, range, &Token::Colon)?;
        let field_type = parse_type(iter, comments, range)?;

        if !seen_names.insert(field_name.as_str().to_string()) {
            return Err(ParseError::DuplicateField {
                name: field_name_range.to_cheap_string(),
                range: field_name_range.clone(),
            });
        }

        fields.push((field_name, field_name_range, field_type));

        if advance_if(iter, comments, Token::Comma).is_some() {
            if advance_if(iter, comments, Token::RightParen).is_some() {
                break;
            }
        } else {
            expect_token(iter, comments, range, &Token::RightParen)?;
            break;
        }
    }

    Ok(fields)
}

pub fn parse_declarations(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    range: &DocumentRange,
    errors: &mut ErrorCollector<ParseError>,
) -> Vec<ParsedDeclaration> {
    let mut declarations = Vec::new();

    loop {
        match peek(iter) {
            Some(Ok((Token::Import, _))) => match parse_import_declaration(iter, comments, range) {
                Ok(decl) => declarations.push(decl),
                Err(err) => {
                    errors.push(err);
                    break;
                }
            },
            Some(Ok((Token::Record, _))) => match parse_record_declaration(iter, comments, range) {
                Ok(decl) => declarations.push(decl),
                Err(err) => {
                    errors.push(err);
                    break;
                }
            },
            Some(Ok((Token::Enum, _))) => match parse_enum_declaration(iter, comments, range) {
                Ok(decl) => declarations.push(decl),
                Err(err) => {
                    errors.push(err);
                    break;
                }
            },
            Some(Ok((_, token_range))) => {
                errors.push(ParseError::ExpectedDeclaration { range: token_range });
                break;
            }
            Some(Err(err)) => {
                errors.push(err);
                break;
            }
            None => break,
        }
    }

    // TODO: Expect eof?
    next(iter, comments);

    declarations
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::{DocumentAnnotator, SimpleAnnotation, document_cursor::Ranged as _};
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn annotate_error(error: ParseError) -> String {
        let annotator = DocumentAnnotator::new()
            .with_label("error")
            .without_location()
            .without_line_numbers();
        annotator.annotate(
            None,
            [SimpleAnnotation {
                message: error.to_string(),
                range: error.range().clone(),
            }],
        )
    }

    fn check_parse_expr(input: &str, expected: Expect) {
        let cursor = DocumentCursor::new(input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let actual = match parse_expr(&mut iter, &mut comments, &range) {
            Ok(result) => format!("{}\n", result),
            Err(err) => annotate_error(err),
        };
        expected.assert_eq(&actual);
    }

    fn check_parse_parameters(input: &str, expected: Expect) {
        let cursor = DocumentCursor::new(input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();

        let actual = match parse_parameters(&mut iter, &mut comments, &range) {
            Ok(result) => {
                let params: Vec<String> = result
                    .into_iter()
                    .map(|((var_name, _var_name_range), var_type, default_value)| {
                        match default_value {
                            Some(default) => format!("{}: {} = {}", var_name, var_type, default),
                            None => format!("{}: {}", var_name, var_type),
                        }
                    })
                    .collect();
                format!("[{}]\n", params.join(", "))
            }
            Err(err) => annotate_error(err),
        };

        expected.assert_eq(&actual);
    }

    fn check_parse_declarations(input: &str, expected: Expect) {
        use crate::error_collector::ErrorCollector;

        let mut errors = ErrorCollector::<ParseError>::new();
        let cursor = DocumentCursor::new(input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let declarations = parse_declarations(&mut iter, &mut comments, &range, &mut errors);

        let actual = if !errors.is_empty() {
            DocumentAnnotator::new()
                .with_label("error")
                .annotate(None, errors.to_vec())
        } else {
            declarations
                .iter()
                .map(|decl| format!("{:?}", decl))
                .collect::<Vec<_>>()
                .join("\n")
        };

        expected.assert_eq(&actual);
    }

    ///////////////////////////////////////////////////////////////////////////
    // RECORD LITERAL                                                        //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_record_literal_with_single_field() {
        check_parse_expr(
            r#"User(name: "John")"#,
            expect![[r#"
                User(name: "John")
            "#]],
        );
    }

    #[test]
    fn should_accept_record_literal_with_multiple_fields() {
        check_parse_expr(
            r#"User(name: "John", age: 30, active: true)"#,
            expect![[r#"
                User(name: "John", age: 30, active: true)
            "#]],
        );
    }

    #[test]
    fn should_accept_record_literal_with_no_fields() {
        check_parse_expr(
            "Empty()",
            expect![[r#"
                Empty()
            "#]],
        );
    }

    #[test]
    fn should_accept_record_literal_with_trailing_comma() {
        check_parse_expr(
            r#"User(name: "John", age: 30,)"#,
            expect![[r#"
                User(name: "John", age: 30)
            "#]],
        );
    }

    #[test]
    fn should_accept_record_literal_with_multiline_fields() {
        check_parse_expr(
            indoc! {r#"
                User(
                  name: "John",
                )
            "#},
            expect![[r#"
                User(name: "John")
            "#]],
        );
    }

    #[test]
    fn should_accept_record_literal_with_nested_records() {
        check_parse_expr(
            r#"Wrapper(inner: Inner(value: 42))"#,
            expect![[r#"
                Wrapper(inner: Inner(value: 42))
            "#]],
        );
    }

    #[test]
    fn should_accept_record_literal_with_expression_values() {
        check_parse_expr(
            "Point(x: a + b, y: c * 2)",
            expect![[r#"
                Point(x: a + b, y: c * 2)
            "#]],
        );
    }

    #[test]
    fn should_reject_record_literal_when_closing_paren_is_missing() {
        check_parse_expr(
            r#"User(name: "John""#,
            expect![[r#"
                error: Unmatched '('
                User(name: "John"
                    ^
            "#]],
        );
    }

    #[test]
    fn should_reject_record_literal_when_colon_is_missing() {
        check_parse_expr(
            r#"User(name "John")"#,
            expect![[r#"
                error: Expected token ':' but got '"John"'
                User(name "John")
                          ^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // PARAMETERS                                                            //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_parameters_with_builtin_type_keywords() {
        check_parse_parameters(
            "name: String, age: Int, score: Float, active: Bool, items: Array[String]",
            expect![[r#"
                [name: String, age: Int, score: Float, active: Bool, items: Array[String]]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_trusted_html_type() {
        check_parse_parameters(
            "content: TrustedHTML",
            expect![[r#"
                [content: TrustedHTML]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_named_type() {
        check_parse_parameters(
            "user: User, person: Person",
            expect![[r#"
                [user: User, person: Person]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_named_type_inside_array() {
        check_parse_parameters(
            "users: Array[User]",
            expect![[r#"
                [users: Array[User]]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_option_type() {
        check_parse_parameters(
            "name: Option[String]",
            expect![[r#"
                [name: Option[String]]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_named_type_inside_option() {
        check_parse_parameters(
            "user: Option[User]",
            expect![[r#"
                [user: Option[User]]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_option_inside_array() {
        check_parse_parameters(
            "names: Array[Option[String]]",
            expect![[r#"
                [names: Array[Option[String]]]
            "#]],
        );
    }

    #[test]
    fn should_accept_parameters_with_array_inside_option() {
        check_parse_parameters(
            "items: Option[Array[Int]]",
            expect![[r#"
                [items: Option[Array[Int]]]
            "#]],
        );
    }

    #[test]
    fn should_reject_option_with_multiple_type_parameters() {
        check_parse_parameters(
            "item: Option[String, String]",
            expect![[r#"
                error: Expected token ']' but got ','
                item: Option[String, String]
                                   ^
            "#]],
        );
    }

    #[test]
    fn should_reject_parameters_when_name_is_duplicated_with_different_type() {
        check_parse_parameters(
            "foo: String, foo: Float",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: String, foo: Float
                             ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_parameters_when_name_is_duplicated_with_same_type() {
        check_parse_parameters(
            "foo: String, foo: String",
            expect![[r#"
                error: Duplicate parameter 'foo'
                foo: String, foo: String
                             ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // COMMENTS                                                              //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_skip_comments_in_expression() {
        check_parse_expr(
            "x // this is a comment\n== y",
            expect![[r#"
                x == y
            "#]],
        );
    }

    #[test]
    fn should_skip_comments_between_tokens() {
        check_parse_expr(
            "// leading comment\nx + // middle\ny // trailing",
            expect![[r#"
                x + y
            "#]],
        );
    }

    #[test]
    fn should_skip_comments_in_array() {
        check_parse_expr(
            "[1, // comment\n2, 3]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // EXPRESSIONS                                                           //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_reject_expr_when_trailing_tokens_are_present() {
        check_parse_expr(
            "x y",
            expect![[r#"
                error: Unexpected token 'y'
                x y
                  ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_array_bracket_is_unmatched() {
        check_parse_expr(
            "[foo, bar == [foo, bar]",
            expect![[r#"
                error: Unmatched '['
                [foo, bar == [foo, bar]
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_array_has_invalid_token_after_comma() {
        check_parse_expr(
            "[foo, bar, == [foo, bar]",
            expect![[r#"
                error: Unexpected token '=='
                [foo, bar, == [foo, bar]
                           ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_array_closing_bracket_is_missing() {
        check_parse_expr(
            "[1,2",
            expect![[r#"
                error: Unmatched '['
                [1,2
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_array_has_unexpected_token_instead_of_bracket() {
        check_parse_expr(
            "[1,2 id",
            expect![[r#"
                error: Expected token ']' but got 'id'
                [1,2 id
                     ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_field_access_ends_with_dot() {
        check_parse_expr(
            "user == user.",
            expect![[r#"
                error: Unexpected end of field access
                user == user.
                        ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_field_name_is_number() {
        check_parse_expr(
            "user.123",
            expect![[r#"
                error: Expected identifier after '.'
                user.123
                     ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_starting_with_operator() {
        check_parse_expr(
            "== x",
            expect![[r#"
                error: Unexpected token '=='
                == x
                ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_closing_paren_is_missing() {
        check_parse_expr(
            "(x == y",
            expect![[r#"
                error: Unmatched '('
                (x == y
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_closing_paren_has_no_opening() {
        check_parse_expr(
            "x == y)",
            expect![[r#"
                error: Unexpected token ')'
                x == y)
                      ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_parens_are_empty() {
        check_parse_expr(
            "()",
            expect![[r#"
                error: Unexpected token ')'
                ()
                 ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_right_operand_is_invalid() {
        check_parse_expr(
            "x == )",
            expect![[r#"
                error: Unexpected token ')'
                x == )
                     ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_starting_with_dot() {
        check_parse_expr(
            ".field",
            expect![[r#"
                error: Unexpected token '.'
                .field
                ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_containing_double_dot() {
        check_parse_expr(
            "user..name",
            expect![[r#"
                error: Expected identifier after '.'
                user..name
                     ^
            "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_ending_with_operator() {
        check_parse_expr(
            "x ==",
            expect![[r#"
            error: Unexpected end of expression
            x ==
            ^^^^
        "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_not_operator_has_no_operand() {
        check_parse_expr(
            "!",
            expect![[r#"
            error: Unexpected end of expression
            !
            ^
        "#]],
        );
    }

    #[test]
    fn should_reject_expr_when_not_operator_is_trailing() {
        // Note: `x !` is now interpreted as a macro invocation `x!`
        check_parse_expr(
            "x !",
            expect![[r#"
                error: Unknown macro 'x'
                x !
                ^
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_equality_operators() {
        check_parse_expr(
            "a == b == c",
            expect![[r#"
                a == b == c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_field_accesses() {
        check_parse_expr(
            "user.name == admin.name",
            expect![[r#"
                user.name == admin.name
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_less_than_or_equal_operator() {
        check_parse_expr(
            "x <= y",
            expect![[r#"
                x <= y
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_less_than_or_equal_operators() {
        check_parse_expr(
            "a <= b <= c",
            expect![[r#"
                a <= b <= c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_greater_than_or_equal_operator() {
        check_parse_expr(
            "x >= y",
            expect![[r#"
                x >= y
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_greater_than_or_equal_operators() {
        check_parse_expr(
            "a >= b >= c",
            expect![[r#"
                a >= b >= c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_deeply_nested_field_access() {
        check_parse_expr(
            "app.user.profile.settings.theme",
            expect![[r#"
                app.user.profile.settings.theme
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_empty_string_literal() {
        check_parse_expr(
            r#""""#,
            expect![[r#"
                ""
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_integer_literal() {
        check_parse_expr(
            "99",
            expect![[r#"
                99
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_float_literal() {
        check_parse_expr(
            "3.14",
            expect![[r#"
                3.14
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_mixed_int_and_float_operands() {
        check_parse_expr(
            "42 + 3.14",
            expect![[r#"
                42 + 3.14
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_parenthesized_expression() {
        check_parse_expr(
            "(x == y)",
            expect![[r#"
                x == y
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_simple_field_access() {
        check_parse_expr(
            "user.name",
            expect![[r#"
                user.name
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_string_literal_to_field() {
        check_parse_expr(
            r#""guest" == user.role"#,
            expect![[r#"
                "guest" == user.role
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_two_variables() {
        check_parse_expr(
            "x == y",
            expect![[r#"
                x == y
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_literal() {
        check_parse_expr(
            r#""hello""#,
            expect![[r#"
                "hello"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_single_variable() {
        check_parse_expr(
            "x",
            expect![[r#"
                x
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_two_string_literals() {
        check_parse_expr(
            r#""apple" == "orange""#,
            expect![[r#"
                "apple" == "orange"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_comparing_field_to_string_literal() {
        check_parse_expr(
            r#"user.name == "admin""#,
            expect![[r#"
                user.name == "admin"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_containing_space() {
        check_parse_expr(
            r#""hello world""#,
            expect![[r#"
                "hello world"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_ignoring_surrounding_whitespace() {
        check_parse_expr(
            "  user . name   ==   admin . name  ",
            expect![[r#"
                user.name == admin.name
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_empty_array() {
        check_parse_expr(
            "[]",
            expect![[r#"
                []
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_of_integers() {
        check_parse_expr(
            "[1, 2, 3]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_of_mixed_types() {
        check_parse_expr(
            r#"[1, "hello", true]"#,
            expect![[r#"
                [1, "hello", true]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_nested_arrays() {
        check_parse_expr(
            "[[1, 2], [3, 4]]",
            expect![[r#"
                [[1, 2], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_nested_arrays_containing_expressions() {
        check_parse_expr(
            "[[1 == [1 == 2], [] == []], [3, 4]]",
            expect![[r#"
                [[1 == [1 == 2], [] == []], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_containing_variables() {
        check_parse_expr(
            "[x, user.name]",
            expect![[r#"
                [x, user.name]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_multiline_array_and_trailing_comma() {
        check_parse_expr(
            "[\n\t1,\n\t2,\n\t3,\n]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_single_element_array_and_trailing_comma() {
        check_parse_expr(
            "[\n\t1,\n]",
            expect![[r#"
                [1]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_array_containing_complex_expressions() {
        check_parse_expr(
            "[\n\tuser.name,\n\t!user.disabled,\n]",
            expect![[r#"
                [user.name, !user.disabled]
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_concatenation() {
        check_parse_expr(
            r#""hello" + "world""#,
            expect![[r#"
                "hello" + "world"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_multiple_string_concatenations() {
        check_parse_expr(
            r#""hello" + " " + "world""#,
            expect![[r#"
                "hello" + " " + "world"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_concatenation_using_variables() {
        check_parse_expr(
            r#"greeting + " " + name"#,
            expect![[r#"
                greeting + " " + name
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_concatenation_having_lower_precedence_than_equality() {
        check_parse_expr(
            r#""a" + "b" == "ab""#,
            expect![[r#"
                "a" + "b" == "ab"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_string_concatenation_using_field_access() {
        check_parse_expr(
            r#"user.first_name + " " + user.last_name"#,
            expect![[r#"
                user.first_name + " " + user.last_name
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_not_equals_operator() {
        check_parse_expr(
            "x != y",
            expect![[r#"
                x != y
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_not_equals_comparing_strings() {
        check_parse_expr(
            r#""hello" != "world""#,
            expect![[r#"
                "hello" != "world"
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_not_equals_operators() {
        check_parse_expr(
            "a != b != c",
            expect![[r#"
                a != b != c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_mixed_equals_and_not_equals() {
        check_parse_expr(
            "a == b != c",
            expect![[r#"
                a == b != c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_operator() {
        check_parse_expr(
            "a && b",
            expect![[r#"
                a && b
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_logical_and_operators() {
        check_parse_expr(
            "a && b && c",
            expect![[r#"
                a && b && c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_having_lower_precedence_than_equality() {
        check_parse_expr(
            "a && b == c",
            expect![[r#"
                a && b == c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_equality_having_higher_precedence_than_logical_and() {
        check_parse_expr(
            "a == b && c != d",
            expect![[r#"
                a == b && c != d
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_combining_comparisons() {
        check_parse_expr(
            "x > y && a <= b",
            expect![[r#"
                x > y && a <= b
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_combining_negations() {
        check_parse_expr(
            "!a && !b",
            expect![[r#"
                !a && !b
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_or_operator() {
        check_parse_expr(
            "a || b",
            expect![[r#"
                a || b
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_logical_or_operators() {
        check_parse_expr(
            "a || b || c",
            expect![[r#"
                a || b || c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_or_having_lower_precedence_than_equality() {
        check_parse_expr(
            "a || b == c",
            expect![[r#"
                a || b == c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_and_having_higher_precedence_than_or() {
        check_parse_expr(
            "a && b || c",
            expect![[r#"
                a && b || c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_mixed_logical_operators_respecting_precedence() {
        check_parse_expr(
            "a || b && c || d",
            expect![[r#"
                a || b && c || d
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_operators_and_comparisons_respecting_precedence() {
        check_parse_expr(
            "x > y && a || b < c",
            expect![[r#"
                x > y && a || b < c
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_logical_or_combining_negations() {
        check_parse_expr(
            "!a || !b",
            expect![[r#"
                !a || !b
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_addition_having_higher_precedence_than_equality() {
        check_parse_expr(
            "x + y == z",
            expect![[r#"
                x + y == z
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_addition_and_comparison_and_logical_and() {
        check_parse_expr(
            "x + y > z && enabled",
            expect![[r#"
                x + y > z && enabled
            "#]],
        );
    }

    #[test]
    fn should_accept_expr_with_chained_addition_operators() {
        check_parse_expr(
            "x + y + z",
            expect![[r#"
                x + y + z
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // DECLARATIONS                                                          //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_import_with_simple_path() {
        check_parse_declarations(
            indoc! {r#"
                import user_list::UserList
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  module_name: user_list,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_import_with_nested_path() {
        check_parse_declarations(
            indoc! {r#"
                import components::header::Header
            "#},
            expect![[r#"
                Import {
                  name: Header,
                  module_name: components::header,
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_import_when_path_has_only_one_segment() {
        check_parse_declarations(
            indoc! {r#"
                import Header
            "#},
            expect![[r#"
                error: Import path must have at least two segments: module::Component
                1 | import Header
                  |        ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_import_when_type_name_is_not_pascal_case() {
        check_parse_declarations(
            indoc! {r#"
                import foo::bar
            "#},
            expect![[r#"
                error: Type name must start with an uppercase letter
                1 | import foo::bar
                  |             ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_multiple_fields() {
        check_parse_declarations(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            expect![[r#"
                Record {
                  name: User,
                  fields: {
                    name: String,
                    age: Int,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_nested_array_field_type() {
        check_parse_declarations(
            indoc! {"
                record UserList {
                    users: Array[User],
                }
            "},
            expect![[r#"
                Record {
                  name: UserList,
                  fields: {
                    users: Array[User],
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_html_input_as_declaration() {
        check_parse_declarations(
            indoc! {"
                <div>hello</div>
            "},
            expect![[r#"
                error: Expected declaration (import, record, or enum)
                1 | <div>hello</div>
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_accept_import_with_leading_empty_lines() {
        check_parse_declarations(
            indoc! {r#"


                import user_list::UserList
            "#},
            expect![[r#"
                Import {
                  name: UserList,
                  module_name: user_list,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_mixed_declarations() {
        check_parse_declarations(
            indoc! {r#"
                import header::Header
                record User {
                    name: String,
                }
                import footer::Footer
            "#},
            expect![[r#"
                Import {
                  name: Header,
                  module_name: header,
                }

                Record {
                  name: User,
                  fields: {
                    name: String,
                  },
                }

                Import {
                  name: Footer,
                  module_name: footer,
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_text_starting_with_import_keyword() {
        check_parse_declarations(
            "important information",
            expect![[r#"
                error: Expected declaration (import, record, or enum)
                1 | important information
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_text_starting_with_record_keyword() {
        check_parse_declarations(
            "recording started",
            expect![[r#"
                error: Expected declaration (import, record, or enum)
                1 | recording started
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unexpected_content_before_declaration() {
        check_parse_declarations(
            "• bullet point\nrecord User {name: String}",
            expect![[r#"
                error: Unexpected character: '•'
                1 | • bullet point
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_field_without_type() {
        check_parse_declarations(
            indoc! {"
                record X {
                    foo
                }
            "},
            expect![[r#"
                error: Expected token ':' but got '}'
                3 | }
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_no_fields() {
        check_parse_declarations(
            "record Empty {}",
            expect![[r#"
                Record {
                  name: Empty,
                  fields: {},
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_declaration_record_with_trailing_comma() {
        check_parse_declarations(
            indoc! {"
                record User {
                    name: String,
                    age: Int,
                }
            "},
            expect![[r#"
                Record {
                  name: User,
                  fields: {
                    name: String,
                    age: Int,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_with_duplicate_field() {
        check_parse_declarations(
            "record Foo {bar: String, bar: Int}",
            expect![[r#"
                error: Duplicate field 'bar'
                1 | record Foo {bar: String, bar: Int}
                  |                          ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_with_missing_closing_brace() {
        check_parse_declarations(
            "record Foo {bar: String",
            expect![[r#"
                error: Unmatched '{'
                1 | record Foo {bar: String
                  |            ^
            "#]],
        );
    }

    #[test]
    fn should_reject_declaration_record_with_missing_type_name() {
        check_parse_declarations(
            "record {bar: String}",
            expect![[r#"
                error: Expected type name but got {
                1 | record {bar: String}
                  |        ^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_with_single_variant() {
        check_parse_declarations(
            "enum Color {Red}",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_with_multiple_variants() {
        check_parse_declarations(
            "enum Color {Red, Green, Blue}",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                    Green,
                    Blue,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_with_trailing_comma() {
        check_parse_declarations(
            "enum Color {Red, Green, Blue,}",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                    Green,
                    Blue,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_empty_enum() {
        check_parse_declarations(
            "enum Empty {}",
            expect![[r#"
                Enum {
                  name: Empty,
                  variants: {},
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_with_duplicate_variant() {
        check_parse_declarations(
            "enum Color {Red, Red}",
            expect![[r#"
                error: Duplicate variant 'Red'
                1 | enum Color {Red, Red}
                  |                  ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_with_missing_closing_brace() {
        check_parse_declarations(
            "enum Color {Red",
            expect![[r#"
                error: Unmatched '{'
                1 | enum Color {Red
                  |            ^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_with_lowercase_variant() {
        check_parse_declarations(
            "enum Color {red}",
            expect![[r#"
                error: Expected type name but got red
                1 | enum Color {red}
                  |             ^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_variant_with_single_field() {
        check_parse_declarations(
            "enum Result { Ok(value: Int), Err(message: String) }",
            expect![[r#"
                Enum {
                  name: Result,
                  variants: {
                    Ok(value: Int),
                    Err(message: String),
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_variant_with_multiple_fields() {
        check_parse_declarations(
            "enum Event { Click(x: Int, y: Int), KeyPress(key: String, shift: Bool) }",
            expect![[r#"
                Enum {
                  name: Event,
                  variants: {
                    Click(x: Int, y: Int),
                    KeyPress(key: String, shift: Bool),
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_mixed_enum_variants() {
        check_parse_declarations(
            "enum Maybe { Just(value: Int), Nothing }",
            expect![[r#"
                Enum {
                  name: Maybe,
                  variants: {
                    Just(value: Int),
                    Nothing,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_variant_with_trailing_comma_in_fields() {
        check_parse_declarations(
            "enum Result { Ok(value: Int,) }",
            expect![[r#"
                Enum {
                  name: Result,
                  variants: {
                    Ok(value: Int),
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_variant_with_duplicate_field() {
        check_parse_declarations(
            "enum Result { Ok(value: Int, value: String) }",
            expect![[r#"
                error: Duplicate field 'value'
                1 | enum Result { Ok(value: Int, value: String) }
                  |                              ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_variant_with_empty_parens() {
        check_parse_declarations(
            "enum Color { Red() }",
            expect![[r#"
                Enum {
                  name: Color,
                  variants: {
                    Red,
                  },
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_variant_with_complex_field_types() {
        check_parse_declarations(
            "enum Container { Box(items: Array[String], count: Option[Int]) }",
            expect![[r#"
                Enum {
                  name: Container,
                  variants: {
                    Box(items: Array[String], count: Option[Int]),
                  },
                }
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ENUM LITERAL                                                          //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_enum_literal() {
        check_parse_expr(
            "Color::Red",
            expect![[r#"
                Color::Red
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_in_equality() {
        check_parse_expr(
            "Color::Red == Color::Green",
            expect![[r#"
                Color::Red == Color::Green
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_in_record_field() {
        check_parse_expr(
            r#"User(name: "Alice", status: Status::Active)"#,
            expect![[r#"
                User(name: "Alice", status: Status::Active)
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_literal_with_lowercase_variant() {
        check_parse_expr(
            "Color::red",
            expect![[r#"
                error: Expected type name but got red
                Color::red
                       ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_enum_literal_missing_variant() {
        check_parse_expr(
            "Color::",
            expect![[r#"
                error: Expected type name but got end of file
                Color::
                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_with_single_field() {
        check_parse_expr(
            "Result::Ok(value: 42)",
            expect![[r#"
                Result::Ok(value: 42)
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_with_multiple_fields() {
        check_parse_expr(
            r#"Event::Click(x: 10, y: 20)"#,
            expect![[r#"
                Event::Click(x: 10, y: 20)
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_with_string_field() {
        check_parse_expr(
            r#"Result::Err(message: "something went wrong")"#,
            expect![[r#"
                Result::Err(message: "something went wrong")
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_with_nested_expression() {
        check_parse_expr(
            r#"Result::Ok(value: x + 1)"#,
            expect![[r#"
                Result::Ok(value: x + 1)
            "#]],
        );
    }

    #[test]
    fn should_accept_enum_literal_with_trailing_comma() {
        check_parse_expr(
            "Result::Ok(value: 42,)",
            expect![[r#"
                Result::Ok(value: 42)
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // OPTION LITERAL                                                        //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_some_with_string_literal() {
        check_parse_expr(
            r#"Some("hello")"#,
            expect![[r#"
                Some("hello")
            "#]],
        );
    }

    #[test]
    fn should_accept_some_with_integer_literal() {
        check_parse_expr(
            "Some(42)",
            expect![[r#"
                Some(42)
            "#]],
        );
    }

    #[test]
    fn should_accept_some_with_variable() {
        check_parse_expr(
            "Some(x)",
            expect![[r#"
                Some(x)
            "#]],
        );
    }

    #[test]
    fn should_accept_some_with_expression() {
        check_parse_expr(
            "Some(a + b)",
            expect![[r#"
                Some(a + b)
            "#]],
        );
    }

    #[test]
    fn should_accept_none() {
        check_parse_expr(
            "None",
            expect![[r#"
                None
            "#]],
        );
    }

    #[test]
    fn should_accept_some_with_nested_some() {
        check_parse_expr(
            "Some(Some(1))",
            expect![[r#"
                Some(Some(1))
            "#]],
        );
    }

    #[test]
    fn should_accept_some_with_none() {
        check_parse_expr(
            "Some(None)",
            expect![[r#"
                Some(None)
            "#]],
        );
    }

    #[test]
    fn should_accept_option_in_array() {
        check_parse_expr(
            "[Some(1), None, Some(2)]",
            expect![[r#"
                [Some(1), None, Some(2)]
            "#]],
        );
    }

    #[test]
    fn should_reject_some_without_parentheses() {
        check_parse_expr(
            "Some",
            expect![[r#"
                error: Expected token '(' but got end of file
                Some
                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_some_with_empty_parentheses() {
        check_parse_expr(
            "Some()",
            expect![[r#"
                error: Unexpected token ')'
                Some()
                     ^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MATCH EXPRESSION                                                      //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_match_expression_with_single_arm() {
        check_parse_expr(
            indoc! {r#"
                match color {Color::Red => "red"}
            "#},
            expect![[r#"
                match color {Color::Red => "red"}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_multiple_arms() {
        check_parse_expr(
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Blue => "blue",
                    Color::Green => "green",
                }
            "#},
            expect![[r#"
                match color {
                  Color::Red => "red",
                  Color::Blue => "blue",
                  Color::Green => "green",
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_trailing_comma() {
        check_parse_expr(
            indoc! {r#"
                match color {
                    Color::Red => "red",
                    Color::Blue => "blue",
                }
            "#},
            expect![[r#"
                match color {Color::Red => "red", Color::Blue => "blue"}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_complex_body() {
        check_parse_expr(
            indoc! {r#"
                match status {
                    Status::Active => user.name,
                    Status::Inactive => "unknown",
                }
            "#},
            expect![[r#"
                match status {
                  Status::Active => user.name,
                  Status::Inactive => "unknown",
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_nested_match_expression() {
        check_parse_expr(
            indoc! {r#"
                match outer {
                    Outer::A => match inner {Inner::X => 1, Inner::Y => 2},
                    Outer::B => 3,
                }
            "#},
            expect![[r#"
                match outer {
                  Outer::A => match inner {Inner::X => 1, Inner::Y => 2},
                  Outer::B => 3,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_empty_match_expression() {
        check_parse_expr(
            "match color {}",
            expect![[r#"
                match color {}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_enum_field_pattern() {
        check_parse_expr(
            "match result { Result::Ok(value: v) => v, Result::Err(message: m) => m }",
            expect![[r#"
                match result {
                  Result::Ok(value: v) => v,
                  Result::Err(message: m) => m,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_enum_multiple_field_pattern() {
        check_parse_expr(
            "match event { Event::Click(x: a, y: b) => a + b }",
            expect![[r#"
                match event {Event::Click(x: a, y: b) => a + b}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_mixed_enum_patterns() {
        check_parse_expr(
            "match maybe { Maybe::Just(value: v) => v, Maybe::Nothing => 0 }",
            expect![[r#"
                match maybe {
                  Maybe::Just(value: v) => v,
                  Maybe::Nothing => 0,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_nested_enum_field_pattern() {
        check_parse_expr(
            "match result { Result::Ok(data: Some(x)) => x, _ => 0 }",
            expect![[r#"
                match result {Result::Ok(data: Some(x)) => x, _ => 0}
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_wildcard_field_pattern() {
        check_parse_expr(
            "match result { Result::Ok(value: _) => 1, Result::Err(message: _) => 0 }",
            expect![[r#"
                match result {
                  Result::Ok(value: _) => 1,
                  Result::Err(message: _) => 0,
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_match_with_empty_parens_pattern() {
        // Empty parens are accepted but normalized away
        check_parse_expr(
            "match point { Point::XY() => 0 }",
            expect![[r#"
                match point {Point::XY => 0}
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MACRO INVOCATIONS                                                     //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_accept_classes_macro_with_no_args() {
        check_parse_expr(
            "classes!()",
            expect![[r#"
                classes!()
            "#]],
        );
    }

    #[test]
    fn should_accept_classes_macro_with_single_arg() {
        check_parse_expr(
            r#"classes!("hello")"#,
            expect![[r#"
                classes!("hello")
            "#]],
        );
    }

    #[test]
    fn should_accept_classes_macro_with_multiple_args() {
        check_parse_expr(
            "classes!(a, b, c)",
            expect![[r#"
                classes!(a, b, c)
            "#]],
        );
    }

    #[test]
    fn should_accept_classes_macro_with_expressions() {
        check_parse_expr(
            "classes!(user.first, user.last)",
            expect![[r#"
                classes!(user.first, user.last)
            "#]],
        );
    }

    #[test]
    fn should_accept_classes_macro_with_string_literals() {
        check_parse_expr(
            r#"classes!("hello", "world")"#,
            expect![[r#"
                classes!("hello", "world")
            "#]],
        );
    }

    #[test]
    fn should_reject_unknown_macro() {
        check_parse_expr(
            "unknown!(x)",
            expect![[r#"
                error: Unknown macro 'unknown'
                unknown!(x)
                ^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // LET BINDINGS                                                          //
    ///////////////////////////////////////////////////////////////////////////

    fn check_parse_let_bindings(input: &str, expected: Expect) {
        let cursor = DocumentCursor::new(input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let actual = match parse_let_bindings(&mut iter, &mut comments, &range) {
            Ok(result) => {
                let bindings: Vec<String> = result
                    .into_iter()
                    .map(|(var_name, _range, var_type, value_expr)| {
                        format!("{}: {} = {}", var_name, var_type, value_expr)
                    })
                    .collect();
                format!("[{}]\n", bindings.join(", "))
            }
            Err(err) => annotate_error(err),
        };
        expected.assert_eq(&actual);
    }

    #[test]
    fn should_accept_single_let_binding() {
        check_parse_let_bindings(
            r#"name: String = "World""#,
            expect![[r#"
                [name: String = "World"]
            "#]],
        );
    }

    #[test]
    fn should_accept_single_let_binding_with_trailing_comma() {
        check_parse_let_bindings(
            r#"name: String = "World","#,
            expect![[r#"
                [name: String = "World"]
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_let_bindings() {
        check_parse_let_bindings(
            r#"first: String = "Hello", second: String = "World""#,
            expect![[r#"
                [first: String = "Hello", second: String = "World"]
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_let_bindings_with_trailing_comma() {
        check_parse_let_bindings(
            r#"first: String = "Hello", second: String = "World","#,
            expect![[r#"
                [first: String = "Hello", second: String = "World"]
            "#]],
        );
    }

    #[test]
    fn should_accept_three_let_bindings() {
        check_parse_let_bindings(
            "a: Int = 1, b: Int = 2, c: Int = 3",
            expect![[r#"
                [a: Int = 1, b: Int = 2, c: Int = 3]
            "#]],
        );
    }

    #[test]
    fn should_accept_let_binding_with_expression_value() {
        check_parse_let_bindings(
            "sum: Int = a + b",
            expect![[r#"
                [sum: Int = a + b]
            "#]],
        );
    }

    #[test]
    fn should_accept_let_binding_with_field_access() {
        check_parse_let_bindings(
            "name: String = user.name",
            expect![[r#"
                [name: String = user.name]
            "#]],
        );
    }

    #[test]
    fn should_reject_let_binding_without_type() {
        check_parse_let_bindings(
            r#"name = "World""#,
            expect![[r#"
                error: Expected token ':' but got '='
                name = "World"
                     ^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_binding_without_value() {
        check_parse_let_bindings(
            "name: String",
            expect![[r#"
                error: Expected token '=' but got end of file
                name: String
                ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_binding_with_missing_comma() {
        check_parse_let_bindings(
            r#"first: String = "a" second: String = "b""#,
            expect![[r#"
                error: Unexpected token 'second'
                first: String = "a" second: String = "b"
                                    ^^^^^^
            "#]],
        );
    }
}
