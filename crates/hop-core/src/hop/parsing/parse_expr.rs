use std::collections::VecDeque;
use std::iter::Peekable;

use crate::document::{CheapString, DocumentCursor, DocumentRange};
use crate::hop::parsing::token::LangTokenPair;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

use super::parse_helpers::{
    advance_if, expect_field_name, expect_right_delimiter, expect_token, expect_type_name,
    expect_variable_name, next_if_map, parse_delimited, parse_delimited_list,
};
use super::parse_nodes;
use super::parse_type::parse_type;
use super::parsed_expr::{
    Constructor, ParsedArguments, ParsedBinaryOp, ParsedExpr, ParsedFieldInitializer,
    ParsedLoopSource, ParsedMatchArm, ParsedMatchPattern, ParsedNamedArgument,
};
use super::parsed_node::ParsedLetBinding;
use super::token::LangToken;
use super::tokenize_expr::{peek, peek2};
use crate::parse_error::{ErrorEmitted, ParseErrorKind, ParseErrors};

pub fn parse_expr(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = parse_logical_and(iter, comments, errors, eof_range)?;
    while advance_if(iter, comments, errors, LangToken::LogicalOr).is_some() {
        let right = parse_logical_and(iter, comments, errors, eof_range)?;
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
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = parse_equality(iter, comments, errors, eof_range)?;
    while advance_if(iter, comments, errors, LangToken::LogicalAnd).is_some() {
        let right = parse_equality(iter, comments, errors, eof_range)?;
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
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = parse_relational(iter, comments, errors, eof_range)?;
    loop {
        if advance_if(iter, comments, errors, LangToken::Eq).is_some() {
            let right = parse_relational(iter, comments, errors, eof_range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Eq,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, LangToken::NotEq).is_some() {
            let right = parse_relational(iter, comments, errors, eof_range)?;
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
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = parse_additive(iter, comments, errors, eof_range)?;
    loop {
        if advance_if(iter, comments, errors, LangToken::LessThan).is_some() {
            let right = parse_additive(iter, comments, errors, eof_range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::LessThan,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, LangToken::GreaterThan).is_some() {
            let right = parse_additive(iter, comments, errors, eof_range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::GreaterThan,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, LangToken::LessThanOrEqual).is_some() {
            let right = parse_additive(iter, comments, errors, eof_range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::LessThanOrEqual,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, LangToken::GreaterThanOrEqual).is_some() {
            let right = parse_additive(iter, comments, errors, eof_range)?;
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
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = parse_multiplicative(iter, comments, errors, eof_range)?;
    loop {
        if advance_if(iter, comments, errors, LangToken::Plus).is_some() {
            let right = parse_multiplicative(iter, comments, errors, eof_range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Plus,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, LangToken::Minus).is_some() {
            let right = parse_multiplicative(iter, comments, errors, eof_range)?;
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
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = parse_unary(iter, comments, errors, eof_range)?;
    while advance_if(iter, comments, errors, LangToken::Asterisk).is_some() {
        let right = parse_unary(iter, comments, errors, eof_range)?;
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
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    if let Some(operator_range) = advance_if(iter, comments, errors, LangToken::Not) {
        let expr = parse_unary(iter, comments, errors, eof_range)?; // Right associative for multiple !
        Ok(ParsedExpr::BooleanNegation {
            range: operator_range.to(expr.range().clone()),
            operand: Box::new(expr),
        })
    } else if let Some(operator_range) = advance_if(iter, comments, errors, LangToken::Minus) {
        let expr = parse_unary(iter, comments, errors, eof_range)?; // Right associative for multiple -
        Ok(ParsedExpr::NumericNegation {
            range: operator_range.to(expr.range().clone()),
            operand: Box::new(expr),
        })
    } else {
        parse_primary(iter, comments, errors, eof_range)
    }
}

fn parse_array_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    left_bracket: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let (elements, range) = parse_delimited_list(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Brackets,
        &left_bracket,
        &[],
        parse_expr,
    )?;
    Ok(ParsedExpr::ArrayLiteral { elements, range })
}

/// Parse the inside of a block: any number of `let` statements followed by a
/// tail expression. Without a leading `let` this is just `parse_expr`, so
/// sites that always have braces around an expression can call this instead.
pub fn parse_block_body(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let Some(let_range) = advance_if(iter, comments, errors, LangToken::Let) else {
        return parse_expr(iter, comments, errors, eof_range);
    };
    let (var_name, var_name_range) = expect_variable_name(iter, comments, errors, eof_range)?;
    let var_type = if advance_if(iter, comments, errors, LangToken::Colon).is_some() {
        Some(parse_type(iter, comments, errors, eof_range)?)
    } else {
        None
    };
    expect_token(iter, comments, errors, eof_range, &LangToken::Assign)?;
    let value_expr = parse_expr(iter, comments, errors, eof_range)?;
    let semicolon = expect_token(iter, comments, errors, eof_range, &LangToken::Semicolon)?;
    if let Some((LangToken::RightBrace, _)) = peek(iter) {
        return Err(errors.emit(ParseErrorKind::BlockMissingTailExpression {}, semicolon));
    }
    let body = parse_block_body(iter, comments, errors, eof_range)?;
    let range = let_range.to(body.range().clone());
    Ok(ParsedExpr::Let {
        binding: Box::new(ParsedLetBinding {
            var_name,
            var_name_range,
            var_type,
            value_expr,
        }),
        body: Box::new(body),
        range,
    })
}

pub fn parse_primary(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let mut expr = if let Some((name, name_range)) =
        next_if_map(iter, comments, errors, LangToken::not_uppercase_identifier)
    {
        // A macro invocation, a function call, or a variable reference.
        if let Some(bang_range) = advance_if(iter, comments, errors, LangToken::Not) {
            parse_macro_invocation(
                iter,
                comments,
                errors,
                eof_range,
                name,
                name_range.to(bang_range),
            )?
        } else {
            let var_name = VarName::from_cheap_string(name.clone()).map_err(|error| {
                errors.emit(
                    ParseErrorKind::InvalidVariableName { name, error },
                    name_range.clone(),
                )
            })?;
            if let Some(left_paren) = advance_if(iter, comments, errors, LangToken::LeftParen) {
                let (args, parens) = parse_delimited_list(
                    iter,
                    comments,
                    errors,
                    eof_range,
                    LangTokenPair::Parens,
                    &left_paren,
                    &[],
                    |iter, comments, errors, range| {
                        let name = if matches!(peek(iter), Some((LangToken::Identifier(_), _)))
                            && matches!(peek2(iter), Some((LangToken::Colon, _)))
                        {
                            let (name, name_range) =
                                expect_variable_name(iter, comments, errors, range)?;
                            expect_token(iter, comments, errors, range, &LangToken::Colon)?;
                            Some((name, name_range))
                        } else {
                            None
                        };
                        Ok((name, parse_expr(iter, comments, errors, range)?))
                    },
                )?;
                let is_named = args.first().is_some_and(|(name, _)| name.is_some());
                let mut positional = Vec::new();
                let mut named = Vec::new();
                for (name, value) in args {
                    match (is_named, name) {
                        (false, None) => positional.push(value),
                        (true, Some((name, name_range))) => named.push(ParsedNamedArgument {
                            name,
                            name_range,
                            value,
                        }),
                        (_, name) => {
                            let _ = errors.emit(
                                ParseErrorKind::MixedNamedAndPositionalArguments {},
                                match name {
                                    Some((_, name_range)) => name_range.to(value.range().clone()),
                                    None => value.range().clone(),
                                },
                            );
                        }
                    }
                }
                ParsedExpr::FunctionCall {
                    name: var_name,
                    name_range: name_range.clone(),
                    args: if is_named {
                        ParsedArguments::Named(named)
                    } else {
                        ParsedArguments::Positional(positional)
                    },
                    range: name_range.to(parens),
                }
            } else {
                ParsedExpr::VariableReference {
                    range: name_range,
                    value: var_name,
                }
            }
        }
    } else if let Some((name, name_range)) =
        next_if_map(iter, comments, errors, LangToken::uppercase_identifier)
    {
        let type_name = TypeName::from_cheap_string(name).map_err(|error| {
            errors.emit(
                ParseErrorKind::InvalidTypeName { error },
                name_range.clone(),
            )
        })?;
        if let Some(colon_colon) = advance_if(iter, comments, errors, LangToken::ColonColon) {
            parse_enum_literal(
                iter,
                comments,
                errors,
                eof_range,
                type_name,
                name_range,
                colon_colon,
            )?
        } else {
            parse_record_literal(iter, comments, errors, eof_range, type_name, name_range)?
        }
    } else if let Some((value, lit_range)) =
        next_if_map(iter, comments, errors, |token| match token {
            LangToken::StringLiteral(value) => Some(value),
            _ => None,
        })
    {
        ParsedExpr::StringLiteral {
            value,
            range: lit_range,
        }
    } else if let Some(lit_range) = advance_if(iter, comments, errors, LangToken::True) {
        ParsedExpr::BooleanLiteral {
            value: true,
            range: lit_range,
        }
    } else if let Some(lit_range) = advance_if(iter, comments, errors, LangToken::False) {
        ParsedExpr::BooleanLiteral {
            value: false,
            range: lit_range,
        }
    } else if let Some((value, lit_range)) =
        next_if_map(iter, comments, errors, |token| match token {
            LangToken::IntLiteral(value) => Some(value),
            _ => None,
        })
    {
        ParsedExpr::IntLiteral {
            value,
            range: lit_range,
        }
    } else if let Some((value, lit_range)) =
        next_if_map(iter, comments, errors, |token| match token {
            LangToken::FloatLiteral(value) => Some(value),
            _ => None,
        })
    {
        ParsedExpr::FloatLiteral {
            value,
            range: lit_range,
        }
    } else if let Some(left_bracket) = advance_if(iter, comments, errors, LangToken::LeftBracket) {
        parse_array_literal(iter, comments, errors, eof_range, left_bracket)?
    } else if let Some(left_paren) = advance_if(iter, comments, errors, LangToken::LeftParen) {
        let (inner, _) = parse_delimited(
            iter,
            comments,
            errors,
            eof_range,
            LangTokenPair::Parens,
            &left_paren,
            parse_expr,
        )?;
        inner
    } else if let Some(match_range) = advance_if(iter, comments, errors, LangToken::Match) {
        parse_match(iter, comments, errors, eof_range, match_range)?
    } else if let Some(for_range) = advance_if(iter, comments, errors, LangToken::For) {
        parse_for(iter, comments, errors, eof_range, for_range)?
    } else if let Some(some_range) = advance_if(iter, comments, errors, LangToken::Some) {
        let left_paren = expect_token(iter, comments, errors, eof_range, &LangToken::LeftParen)?;
        let (value, parens) = parse_delimited(
            iter,
            comments,
            errors,
            eof_range,
            LangTokenPair::Parens,
            &left_paren,
            parse_expr,
        )?;
        ParsedExpr::OptionLiteral {
            value: Some(Box::new(value)),
            range: some_range.to(parens),
        }
    } else if let Some(none_range) = advance_if(iter, comments, errors, LangToken::None) {
        ParsedExpr::OptionLiteral {
            value: None,
            range: none_range,
        }
    } else if let Some(left_brace) = advance_if(iter, comments, errors, LangToken::LeftBrace) {
        let (inner, _) = parse_delimited(
            iter,
            comments,
            errors,
            eof_range,
            LangTokenPair::Braces,
            &left_brace,
            parse_block_body,
        )?;
        inner
    } else if let Some(left_angle) = advance_if(iter, comments, errors, LangToken::LessThan) {
        parse_nodes::parse_markup(iter, comments, errors, left_angle)?
    } else {
        return Err(match peek(iter) {
            Some((LangToken::Let, token_range)) => {
                errors.emit(ParseErrorKind::LetOutsideBlock {}, token_range)
            }
            Some((token, token_range)) => {
                errors.emit(ParseErrorKind::UnexpectedToken { token }, token_range)
            }
            None => errors.emit(ParseErrorKind::UnexpectedEof {}, eof_range.clone()),
        });
    };
    while let Some(dot) = advance_if(iter, comments, errors, LangToken::Dot) {
        let dot_range = expr.range().clone().to(dot);
        let (field_name, field_range) = expect_field_name(iter, comments, errors, &dot_range)?;
        if let Some(left_paren) = advance_if(iter, comments, errors, LangToken::LeftParen) {
            let right_paren =
                expect_right_delimiter(iter, comments, errors, LangTokenPair::Parens, &left_paren)?;
            let new_range = expr.range().clone().to(right_paren);
            expr = ParsedExpr::MethodCall {
                receiver: Box::new(expr),
                method: field_name,
                method_range: field_range,
                range: new_range,
            };
        } else {
            let new_range = expr.range().clone().to(field_range);
            expr = ParsedExpr::FieldAccess {
                record: Box::new(expr),
                field: field_name,
                range: new_range,
            };
        }
    }
    Ok(expr)
}

fn parse_macro_invocation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    macro_name: CheapString,
    subject_range: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let left_paren = expect_token(iter, comments, errors, eof_range, &LangToken::LeftParen)?;
    let (args, parens) = parse_delimited_list(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Parens,
        &left_paren,
        &[],
        parse_expr,
    )?;
    Ok(ParsedExpr::MacroInvocation {
        name: macro_name,
        subject_range: subject_range.clone(),
        args,
        range: subject_range.to(parens),
    })
}

fn parse_record_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    name: TypeName,
    name_range: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    enum Entry {
        Field(ParsedFieldInitializer),
        Spread(ParsedExpr, DocumentRange),
    }
    let left_brace = expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
    let (entries, braces) = parse_delimited_list(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Braces,
        &left_brace,
        &[],
        |iter, comments, errors, range| {
            if let Some(spread_range) = advance_if(iter, comments, errors, LangToken::DotDotDot) {
                let subject = parse_expr(iter, comments, errors, range)?;
                let spread_range = spread_range.to(subject.range().clone());
                return Ok(Entry::Spread(subject, spread_range));
            }
            let (field_name, field_name_range) = expect_field_name(iter, comments, errors, range)?;
            expect_token(iter, comments, errors, range, &LangToken::Colon)?;
            Ok(Entry::Field(ParsedFieldInitializer {
                name: field_name,
                name_range: field_name_range,
                value: parse_expr(iter, comments, errors, range)?,
            }))
        },
    )?;
    let mut fields = Vec::new();
    let mut spread = None;
    for entry in entries {
        match entry {
            Entry::Field(field) => fields.push(field),
            Entry::Spread(subject, spread_range) => {
                if spread.is_some() {
                    return Err(
                        errors.emit(ParseErrorKind::DuplicateSpreadInRecordLiteral, spread_range)
                    );
                }
                spread = Some(Box::new(subject));
            }
        }
    }
    Ok(ParsedExpr::RecordLiteral {
        record_name: name,
        record_name_range: name_range.clone(),
        fields,
        spread,
        range: name_range.to(braces),
    })
}

fn parse_enum_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    enum_name: TypeName,
    enum_name_range: DocumentRange,
    colon_colon: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let path_range = enum_name_range.clone().to(colon_colon);
    let (variant_name, variant_range) = expect_type_name(iter, comments, errors, &path_range)?;
    let constructor_range = enum_name_range.clone().to(variant_range.clone());
    let (fields, end_range) =
        if let Some(left_delim) = advance_if(iter, comments, errors, LangToken::LeftBrace) {
            parse_delimited_list(
                iter,
                comments,
                errors,
                eof_range,
                LangTokenPair::Braces,
                &left_delim,
                &[],
                |iter, comments, errors, range| {
                    if let Some(spread_range) =
                        advance_if(iter, comments, errors, LangToken::DotDotDot)
                    {
                        return Err(errors
                            .emit(ParseErrorKind::SpreadNotAllowedInEnumLiteral, spread_range));
                    }
                    let (field_name, field_name_range) =
                        expect_field_name(iter, comments, errors, range)?;
                    expect_token(iter, comments, errors, range, &LangToken::Colon)?;
                    Ok(ParsedFieldInitializer {
                        name: field_name,
                        name_range: field_name_range,
                        value: parse_expr(iter, comments, errors, range)?,
                    })
                },
            )?
        } else {
            (Vec::new(), variant_range)
        };
    Ok(ParsedExpr::EnumLiteral {
        enum_name,
        variant_name,
        fields,
        constructor_range,
        enum_name_range: enum_name_range.clone(),
        range: enum_name_range.to(end_range),
    })
}

pub struct LoopHeader {
    pub var_name: Option<VarName>,
    pub var_name_range: Option<DocumentRange>,
    pub loop_source: Box<ParsedLoopSource>,
}

pub fn parse_loop_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<LoopHeader, ErrorEmitted> {
    let (var_name, var_name_range) =
        if let Some(underscore_range) = advance_if(iter, comments, errors, LangToken::Underscore) {
            (None, Some(underscore_range))
        } else {
            let (name, name_range) = expect_variable_name(iter, comments, errors, eof_range)?;
            (Some(name), Some(name_range))
        };
    expect_token(iter, comments, errors, eof_range, &LangToken::In)?;
    let start_expr = parse_expr(iter, comments, errors, eof_range)?;
    let source = if advance_if(iter, comments, errors, LangToken::DotDotEq).is_some() {
        let end_expr = parse_expr(iter, comments, errors, eof_range)?;
        ParsedLoopSource::RangeInclusive {
            start: start_expr,
            end: end_expr,
        }
    } else {
        ParsedLoopSource::Array(start_expr)
    };
    Ok(LoopHeader {
        var_name,
        var_name_range,
        loop_source: Box::new(source),
    })
}

fn parse_for(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    for_range: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let header = parse_loop_header(iter, comments, errors, eof_range)?;
    let left_brace = expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
    let (body, braces) = parse_delimited(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Braces,
        &left_brace,
        parse_block_body,
    )?;
    Ok(ParsedExpr::For {
        var_name: header.var_name,
        var_name_range: header.var_name_range,
        source: header.loop_source,
        body: Box::new(body),
        range: for_range.to(braces),
    })
}

fn parse_match(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    match_range: DocumentRange,
) -> Result<ParsedExpr, ErrorEmitted> {
    let subject = parse_expr(iter, comments, errors, eof_range)?;
    let left_brace = expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
    let (arms, braces) = parse_delimited_list(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Braces,
        &left_brace,
        &[],
        |iter, comments, errors, range| {
            let pattern = parse_match_pattern(iter, comments, errors, range)?;
            expect_token(iter, comments, errors, range, &LangToken::FatArrow)?;
            let body = parse_expr(iter, comments, errors, range)?;
            Ok(ParsedMatchArm { pattern, body })
        },
    )?;
    Ok(ParsedExpr::Match {
        subject: Box::new(subject),
        arms,
        range: match_range.to(braces),
    })
}

fn parse_match_pattern(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
) -> Result<ParsedMatchPattern, ErrorEmitted> {
    if let Some(pattern_range) = advance_if(iter, comments, errors, LangToken::Underscore) {
        return Ok(ParsedMatchPattern::Wildcard {
            range: pattern_range,
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, errors, LangToken::True) {
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::BooleanTrue,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            enum_name_range: None,
            range: pattern_range,
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, errors, LangToken::False) {
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::BooleanFalse,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            enum_name_range: None,
            range: pattern_range,
        });
    }
    if let Some(some_range) = advance_if(iter, comments, errors, LangToken::Some) {
        let left_paren = expect_token(iter, comments, errors, eof_range, &LangToken::LeftParen)?;
        let (inner_pattern, parens) = parse_delimited(
            iter,
            comments,
            errors,
            eof_range,
            LangTokenPair::Parens,
            &left_paren,
            parse_match_pattern,
        )?;
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::OptionSome,
            args: vec![inner_pattern],
            fields: Vec::new(),
            constructor_range: some_range.clone(),
            enum_name_range: None,
            range: some_range.to(parens),
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, errors, LangToken::None) {
        return Ok(ParsedMatchPattern::Constructor {
            constructor: Constructor::OptionNone,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            enum_name_range: None,
            range: pattern_range,
        });
    }
    if let Some((type_name_str, type_name_range)) =
        next_if_map(iter, comments, errors, LangToken::uppercase_identifier)
    {
        let type_name = match TypeName::from_cheap_string(type_name_str) {
            Ok(name) => name,
            Err(error) => {
                return Err(errors.emit(ParseErrorKind::InvalidTypeName { error }, type_name_range));
            }
        };
        if advance_if(iter, comments, errors, LangToken::ColonColon).is_some() {
            let (variant_name, variant_range) =
                expect_type_name(iter, comments, errors, eof_range)?;

            let (fields, end_range) = if let Some(left_brace) =
                advance_if(iter, comments, errors, LangToken::LeftBrace)
            {
                parse_delimited_list(
                    iter,
                    comments,
                    errors,
                    eof_range,
                    LangTokenPair::Braces,
                    &left_brace,
                    &[],
                    |iter, comments, errors, range| {
                        let (field_name, field_range) =
                            expect_field_name(iter, comments, errors, range)?;
                        let pattern =
                            if advance_if(iter, comments, errors, LangToken::Colon).is_some() {
                                parse_match_pattern(iter, comments, errors, range)?
                            } else {
                                match VarName::from_cheap_string(field_name.to_cheap_string()) {
                                    Ok(name) => ParsedMatchPattern::Binding {
                                        name,
                                        range: field_range.clone(),
                                    },
                                    Err(error) => {
                                        return Err(errors.emit(
                                            ParseErrorKind::InvalidVariableName {
                                                name: field_name.to_cheap_string(),
                                                error,
                                            },
                                            field_range,
                                        ));
                                    }
                                }
                            };
                        Ok((field_name, field_range, pattern))
                    },
                )?
            } else {
                (Vec::new(), variant_range.clone())
            };

            let constructor_range = type_name_range.clone().to(variant_range);
            return Ok(ParsedMatchPattern::Constructor {
                constructor: Constructor::EnumVariant {
                    enum_name: type_name,
                    variant_name,
                },
                args: Vec::new(),
                fields,
                constructor_range,
                enum_name_range: Some(type_name_range.clone()),
                range: type_name_range.to(end_range),
            });
        } else {
            let left_brace =
                expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
            let (fields, braces) = parse_delimited_list(
                iter,
                comments,
                errors,
                eof_range,
                LangTokenPair::Braces,
                &left_brace,
                &[],
                |iter, comments, errors, range| {
                    let (field_name, field_range) =
                        expect_field_name(iter, comments, errors, range)?;
                    let pattern = if advance_if(iter, comments, errors, LangToken::Colon).is_some()
                    {
                        parse_match_pattern(iter, comments, errors, range)?
                    } else {
                        match VarName::new(field_name.as_str()) {
                            Ok(name) => ParsedMatchPattern::Binding {
                                name,
                                range: field_range.clone(),
                            },
                            Err(error) => {
                                return Err(errors.emit(
                                    ParseErrorKind::InvalidVariableName {
                                        name: field_name.to_cheap_string(),
                                        error,
                                    },
                                    field_range,
                                ));
                            }
                        }
                    };
                    Ok((field_name, field_range, pattern))
                },
            )?;
            return Ok(ParsedMatchPattern::Constructor {
                constructor: Constructor::Record { type_name },
                args: Vec::new(),
                fields,
                constructor_range: type_name_range.clone(),
                enum_name_range: None,
                range: type_name_range.to(braces),
            });
        }
    }
    let (var_name, var_range) = expect_variable_name(iter, comments, errors, eof_range)?;
    Ok(ParsedMatchPattern::Binding {
        name: var_name,
        range: var_range,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use crate::document_id::DocumentId;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn accept(input: &str, expected: Expect) {
        let document_id = DocumentId::new("test.hop").unwrap();
        let cursor = DocumentCursor::new(document_id, input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = ParseErrors::new();
        let Ok(result) = parse_expr(&mut iter, &mut comments, &mut errors, &range) else {
            panic!("expected expression to parse, got errors: {errors:?}");
        };
        expected.assert_eq(&format!("{result}\n"));
    }

    fn reject(input: &str, expected: Expect) {
        let document_id = DocumentId::new("test.hop").unwrap();
        let cursor = DocumentCursor::new(document_id.clone(), input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = ParseErrors::new();
        let result = parse_expr(&mut iter, &mut comments, &mut errors, &range);
        if errors.is_empty() {
            panic!("expected parse errors but got none");
        }
        let rendered = DocumentAnnotator::new()
            .with_label("error")
            .without_location()
            .without_line_numbers()
            .annotate(&document_id, errors.clone())
            .render();
        let actual = match result {
            Ok(expr) => format!("-- errors --\n{rendered}-- ast --\n{expr}\n"),
            Err(_) => format!("-- errors --\n{rendered}"),
        };
        expected.assert_eq(&actual);
    }

    ///////////////////////////////////////////////////////////////////////////
    // RECORD LITERAL                                                        //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_record_literal_with_single_field() {
        accept(
            r#"User {name: "John"}"#,
            expect![[r#"
                User {name: "John"}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_multiple_fields() {
        accept(
            r#"User {name: "John", age: 30, active: true}"#,
            expect![[r#"
                User {name: "John", age: 30, active: true}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_no_fields() {
        accept(
            "Empty {}",
            expect![[r#"
                Empty {}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_trailing_comma() {
        accept(
            r#"User {name: "John", age: 30,}"#,
            expect![[r#"
                User {name: "John", age: 30}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_multiline_fields() {
        accept(
            indoc! {r#"
                User {
                  name: "John",
                }
            "#},
            expect![[r#"
                User {name: "John"}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_nested_records() {
        accept(
            r#"Wrapper {inner: Inner {value: 42}}"#,
            expect![[r#"
                Wrapper {inner: Inner {value: 42}}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_expression_values() {
        accept(
            "Point {x: a + b, y: c * 2}",
            expect![[r#"
                Point {x: a + b, y: c * 2}
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_when_closing_brace_is_missing() {
        reject(
            r#"User {name: "John""#,
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                User {name: "John"
                     ^
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_when_colon_is_missing() {
        reject(
            r#"User {name "John"}"#,
            expect![[r#"
                -- errors --
                error: Expected token ':' but got '"John"'
                User {name "John"}
                           ^^^^^^
                -- ast --
                User {}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_braces_single_field() {
        accept(
            r#"User {name: "John"}"#,
            expect![[r#"
                User {name: "John"}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_braces_multiple_fields() {
        accept(
            r#"User {name: "John", age: 30}"#,
            expect![[r#"
                User {name: "John", age: 30}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_braces_no_fields() {
        accept(
            "Empty {}",
            expect![[r#"
                Empty {}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_braces_trailing_comma() {
        accept(
            r#"User {name: "John",}"#,
            expect![[r#"
                User {name: "John"}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_spread_first() {
        accept(
            r#"User {...base, name: "John"}"#,
            expect![[r#"
                User {...base, name: "John"}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_spread_last_canonicalized_to_first() {
        accept(
            r#"User {name: "John", ...base}"#,
            expect![[r#"
                User {...base, name: "John"}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_only_spread() {
        accept(
            "User {...base}",
            expect![[r#"
                User {...base}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_spread_of_field_access() {
        accept(
            "State {...app.state, num: 1}",
            expect![[r#"
                State {...app.state, num: 1}
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_with_spread_and_trailing_comma() {
        accept(
            r#"User {...base, name: "John",}"#,
            expect![[r#"
                User {...base, name: "John"}
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_with_two_spreads() {
        reject(
            "User {...a, ...b}",
            expect![[r#"
                -- errors --
                error: At most one spread is allowed in a record literal
                User {...a, ...b}
                            ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_with_spread_missing_subject() {
        reject(
            "User {...}",
            expect![[r#"
                -- errors --
                error: Unexpected token '}'
                User {...}
                         ^
                -- ast --
                User {}
            "#]],
        );
    }

    #[test]
    fn rejects_record_pattern_with_spread() {
        reject(
            r#"match x {User {...y} => "a"}"#,
            expect![[r#"
                -- errors --
                error: Expected field name but got '...'
                match x {User {...y} => "a"}
                               ^^^
                -- ast --
                match x {User => "a"}
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_with_mismatched_brace_paren() {
        reject(
            r#"Foo {bar: "baz")"#,
            expect![[r#"
                -- errors --
                error: Expected token '}' but got ')'
                Foo {bar: "baz")
                               ^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // FRAGMENT                                                              //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_empty_fragment() {
        accept(
            "<></>",
            expect![[r#"
                fragment()
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // COMMENTS                                                              //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_comments_in_expression() {
        accept(
            "x // this is a comment\n== y",
            expect![[r#"
                x == y
            "#]],
        );
    }

    #[test]
    fn accepts_comments_between_tokens() {
        accept(
            "// leading comment\nx + // middle\ny // trailing",
            expect![[r#"
                x + y
            "#]],
        );
    }

    #[test]
    fn accepts_comments_in_array() {
        accept(
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
    fn rejects_expr_when_array_bracket_is_unmatched() {
        reject(
            "[foo, bar == [foo, bar]",
            expect![[r#"
                -- errors --
                error: Unmatched '['
                [foo, bar == [foo, bar]
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_array_has_invalid_token_after_comma() {
        reject(
            "[foo, bar, == [foo, bar]",
            expect![[r#"
                -- errors --
                error: Unexpected token '=='
                [foo, bar, == [foo, bar]
                           ^^
                -- ast --
                [foo, bar, bar]
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_array_closing_bracket_is_missing() {
        reject(
            "[1,2",
            expect![[r#"
                -- errors --
                error: Unmatched '['
                [1,2
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_array_has_unexpected_token_instead_of_bracket() {
        reject(
            "[1,2 id",
            expect![[r#"
                -- errors --
                error: Unmatched '['
                [1,2 id
                ^

                error: Expected token ',' but got 'id'
                [1,2 id
                     ^^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_field_access_ends_with_dot() {
        reject(
            "user == user.",
            expect![[r#"
                -- errors --
                error: Unexpected end of expression
                user == user.
                        ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_field_name_is_number() {
        reject(
            "user.123",
            expect![[r#"
                -- errors --
                error: Expected field name but got '123'
                user.123
                     ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_starting_with_operator() {
        reject(
            "== x",
            expect![[r#"
                -- errors --
                error: Unexpected token '=='
                == x
                ^^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_closing_paren_is_missing() {
        reject(
            "(x == y",
            expect![[r#"
                -- errors --
                error: Unmatched '('
                (x == y
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_parens_are_empty() {
        reject(
            "()",
            expect![[r#"
                -- errors --
                error: Unexpected token ')'
                ()
                 ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_right_operand_is_invalid() {
        reject(
            "x == )",
            expect![[r#"
                -- errors --
                error: Unexpected token ')'
                x == )
                     ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_starting_with_dot() {
        reject(
            ".field",
            expect![[r#"
                -- errors --
                error: Unexpected token '.'
                .field
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_containing_double_dot() {
        reject(
            "user..name",
            expect![[r#"
                -- errors --
                error: Expected field name but got '.'
                user..name
                     ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_ending_with_operator() {
        reject(
            "x ==",
            expect![[r#"
                -- errors --
                error: Unexpected end of expression
                x ==
                ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_not_operator_has_no_operand() {
        reject(
            "!",
            expect![[r#"
                -- errors --
                error: Unexpected end of expression
                !
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_not_operator_is_trailing() {
        reject(
            "x !",
            expect![[r#"
                -- errors --
                error: Expected token '(' but got end of file
                x !
                ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_equality_operators() {
        accept(
            "a == b == c",
            expect![[r#"
                a == b == c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_comparing_field_accesses() {
        accept(
            "user.name == admin.name",
            expect![[r#"
                user.name == admin.name
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_less_than_or_equal_operator() {
        accept(
            "x <= y",
            expect![[r#"
                x <= y
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_less_than_or_equal_operators() {
        accept(
            "a <= b <= c",
            expect![[r#"
                a <= b <= c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_greater_than_or_equal_operator() {
        accept(
            "x >= y",
            expect![[r#"
                x >= y
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_greater_than_or_equal_operators() {
        accept(
            "a >= b >= c",
            expect![[r#"
                a >= b >= c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_deeply_nested_field_access() {
        accept(
            "app.user.profile.settings.theme",
            expect![[r#"
                app.user.profile.settings.theme
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // METHOD CALLS                                                          //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_simple_method_call() {
        accept(
            "x.foo()",
            expect![[r#"
                x.foo()
            "#]],
        );
    }

    #[test]
    fn accepts_chained_method_calls() {
        accept(
            "x.foo().bar()",
            expect![[r#"
                x.foo().bar()
            "#]],
        );
    }

    #[test]
    fn accepts_triple_chained_method_calls() {
        accept(
            "x.foo().bar().baz()",
            expect![[r#"
                x.foo().bar().baz()
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_then_method_call() {
        accept(
            "x.field.method()",
            expect![[r#"
                x.field.method()
            "#]],
        );
    }

    #[test]
    fn accepts_positional_call_arguments() {
        accept(
            "foo(1, x)",
            expect![[r#"
                foo(1, x)
            "#]],
        );
    }

    #[test]
    fn accepts_named_call_arguments() {
        accept(
            "foo(x: 1, y: bar)",
            expect![[r#"
                foo(x: 1, y: bar)
            "#]],
        );
    }

    #[test]
    fn accepts_variable_as_positional_argument() {
        accept(
            "foo(x)",
            expect![[r#"
                foo(x)
            "#]],
        );
    }

    #[test]
    fn rejects_call_mixing_positional_and_named_arguments() {
        reject(
            "foo(1, y: 2)",
            expect![[r#"
                -- errors --
                error: Arguments must either all be named or all be positional
                foo(1, y: 2)
                       ^^^^
                -- ast --
                foo(1)
            "#]],
        );
    }

    #[test]
    fn rejects_call_mixing_named_and_positional_arguments() {
        reject(
            "foo(x: 1, 2)",
            expect![[r#"
                -- errors --
                error: Arguments must either all be named or all be positional
                foo(x: 1, 2)
                          ^
                -- ast --
                foo(x: 1)
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_then_field_access() {
        accept(
            "x.method().field",
            expect![[r#"
                x.method().field
            "#]],
        );
    }

    #[test]
    fn accepts_mixed_field_and_method_chain() {
        accept(
            "x.a.b().c.d()",
            expect![[r#"
                x.a.b().c.d()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_in_equality() {
        accept(
            "x.foo() == y.bar()",
            expect![[r#"
                x.foo() == y.bar()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_in_logical_expression() {
        accept(
            "x.valid() && y.ready()",
            expect![[r#"
                x.valid() && y.ready()
            "#]],
        );
    }

    #[test]
    fn rejects_method_call_with_missing_closing_paren() {
        reject(
            "x.foo(",
            expect![[r#"
                -- errors --
                error: Unmatched '('
                x.foo(
                     ^
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_empty_string_literal() {
        accept(
            r#""""#,
            expect![[r#"
                ""
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_integer_literal() {
        accept(
            "99",
            expect![[r#"
                99
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_float_literal() {
        accept(
            "3.14",
            expect![[r#"
                3.14
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_mixed_int_and_float_operands() {
        accept(
            "42 + 3.14",
            expect![[r#"
                42 + 3.14
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_parenthesized_expression() {
        accept(
            "(x == y)",
            expect![[r#"
                x == y
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_simple_field_access() {
        accept(
            "user.name",
            expect![[r#"
                user.name
            "#]],
        );
    }

    #[test]
    fn accepts_expr_comparing_string_literal_to_field() {
        accept(
            r#""guest" == user.role"#,
            expect![[r#"
                "guest" == user.role
            "#]],
        );
    }

    #[test]
    fn accepts_expr_comparing_two_variables() {
        accept(
            "x == y",
            expect![[r#"
                x == y
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_string_literal() {
        accept(
            r#""hello""#,
            expect![[r#"
                "hello"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_single_variable() {
        accept(
            "x",
            expect![[r#"
                x
            "#]],
        );
    }

    #[test]
    fn accepts_expr_comparing_two_string_literals() {
        accept(
            r#""apple" == "orange""#,
            expect![[r#"
                "apple" == "orange"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_comparing_field_to_string_literal() {
        accept(
            r#"user.name == "admin""#,
            expect![[r#"
                user.name == "admin"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_string_containing_space() {
        accept(
            r#""hello world""#,
            expect![[r#"
                "hello world"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_ignoring_surrounding_whitespace() {
        accept(
            "  user . name   ==   admin . name  ",
            expect![[r#"
                user.name == admin.name
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_empty_array() {
        accept(
            "[]",
            expect![[r#"
                []
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_array_of_integers() {
        accept(
            "[1, 2, 3]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_array_of_mixed_types() {
        accept(
            r#"[1, "hello", true]"#,
            expect![[r#"
                [1, "hello", true]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_nested_arrays() {
        accept(
            "[[1, 2], [3, 4]]",
            expect![[r#"
                [[1, 2], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_nested_arrays_containing_expressions() {
        accept(
            "[[1 == [1 == 2], [] == []], [3, 4]]",
            expect![[r#"
                [[1 == [1 == 2], [] == []], [3, 4]]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_array_containing_variables() {
        accept(
            "[x, user.name]",
            expect![[r#"
                [x, user.name]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_multiline_array_and_trailing_comma() {
        accept(
            "[\n\t1,\n\t2,\n\t3,\n]",
            expect![[r#"
                [1, 2, 3]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_single_element_array_and_trailing_comma() {
        accept(
            "[\n\t1,\n]",
            expect![[r#"
                [1]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_array_containing_complex_expressions() {
        accept(
            "[\n\tuser.name,\n\t!user.disabled,\n]",
            expect![[r#"
                [user.name, !user.disabled]
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_string_concatenation() {
        accept(
            r#""hello" + "world""#,
            expect![[r#"
                "hello" + "world"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_multiple_string_concatenations() {
        accept(
            r#""hello" + " " + "world""#,
            expect![[r#"
                "hello" + " " + "world"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_string_concatenation_using_variables() {
        accept(
            r#"greeting + " " + name"#,
            expect![[r#"
                greeting + " " + name
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_concatenation_having_lower_precedence_than_equality() {
        accept(
            r#""a" + "b" == "ab""#,
            expect![[r#"
                "a" + "b" == "ab"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_string_concatenation_using_field_access() {
        accept(
            r#"user.first_name + " " + user.last_name"#,
            expect![[r#"
                user.first_name + " " + user.last_name
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_not_equals_operator() {
        accept(
            "x != y",
            expect![[r#"
                x != y
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_not_equals_comparing_strings() {
        accept(
            r#""hello" != "world""#,
            expect![[r#"
                "hello" != "world"
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_not_equals_operators() {
        accept(
            "a != b != c",
            expect![[r#"
                a != b != c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_mixed_equals_and_not_equals() {
        accept(
            "a == b != c",
            expect![[r#"
                a == b != c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_and_operator() {
        accept(
            "a && b",
            expect![[r#"
                a && b
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_logical_and_operators() {
        accept(
            "a && b && c",
            expect![[r#"
                a && b && c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_and_having_lower_precedence_than_equality() {
        accept(
            "a && b == c",
            expect![[r#"
                a && b == c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_equality_having_higher_precedence_than_logical_and() {
        accept(
            "a == b && c != d",
            expect![[r#"
                a == b && c != d
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_and_combining_comparisons() {
        accept(
            "x > y && a <= b",
            expect![[r#"
                x > y && a <= b
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_and_combining_negations() {
        accept(
            "!a && !b",
            expect![[r#"
                !a && !b
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_or_operator() {
        accept(
            "a || b",
            expect![[r#"
                a || b
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_logical_or_operators() {
        accept(
            "a || b || c",
            expect![[r#"
                a || b || c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_or_having_lower_precedence_than_equality() {
        accept(
            "a || b == c",
            expect![[r#"
                a || b == c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_and_having_higher_precedence_than_or() {
        accept(
            "a && b || c",
            expect![[r#"
                a && b || c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_mixed_logical_operators_respecting_precedence() {
        accept(
            "a || b && c || d",
            expect![[r#"
                a || b && c || d
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_operators_and_comparisons_respecting_precedence() {
        accept(
            "x > y && a || b < c",
            expect![[r#"
                x > y && a || b < c
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_logical_or_combining_negations() {
        accept(
            "!a || !b",
            expect![[r#"
                !a || !b
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_addition_having_higher_precedence_than_equality() {
        accept(
            "x + y == z",
            expect![[r#"
                x + y == z
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_addition_and_comparison_and_logical_and() {
        accept(
            "x + y > z && enabled",
            expect![[r#"
                x + y > z && enabled
            "#]],
        );
    }

    #[test]
    fn accepts_expr_with_chained_addition_operators() {
        accept(
            "x + y + z",
            expect![[r#"
                x + y + z
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // NUMERIC NEGATION                                                      //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_negative_integer_literal() {
        accept(
            "-5",
            expect![[r#"
                -5
            "#]],
        );
    }

    #[test]
    fn accepts_negative_float_literal() {
        accept(
            "-3.14",
            expect![[r#"
                -3.14
            "#]],
        );
    }

    #[test]
    fn accepts_negative_variable() {
        accept(
            "-x",
            expect![[r#"
                -x
            "#]],
        );
    }

    #[test]
    fn accepts_double_negative() {
        accept(
            "--5",
            expect![[r#"
                --5
            "#]],
        );
    }

    #[test]
    fn accepts_negative_parenthesized_expression() {
        accept(
            "-(a + b)",
            expect![[r#"
                -(a + b)
            "#]],
        );
    }

    #[test]
    fn accepts_negative_in_addition() {
        accept(
            "x + -y",
            expect![[r#"
                x + -y
            "#]],
        );
    }

    #[test]
    fn accepts_subtraction_of_negative() {
        accept(
            "x - -y",
            expect![[r#"
                x - -y
            "#]],
        );
    }

    #[test]
    fn accepts_negative_in_comparison() {
        accept(
            "-5 < x",
            expect![[r#"
                -5 < x
            "#]],
        );
    }

    #[test]
    fn accepts_negative_in_array() {
        accept(
            "[-1, -2, -3]",
            expect![[r#"
                [-1, -2, -3]
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ENUM LITERAL                                                          //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_enum_literal() {
        accept(
            "Color::Red",
            expect![[r#"
                Color::Red
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_in_equality() {
        accept(
            "Color::Red == Color::Green",
            expect![[r#"
                Color::Red == Color::Green
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_in_record_field() {
        accept(
            r#"User {name: "Alice", status: Status::Active}"#,
            expect![[r#"
                User {name: "Alice", status: Status::Active}
            "#]],
        );
    }

    #[test]
    fn rejects_enum_literal_with_lowercase_variant() {
        reject(
            "Color::red",
            expect![[r#"
                -- errors --
                error: Type name must start with an uppercase letter
                Color::red
                       ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_enum_literal_missing_variant() {
        reject(
            "Color::",
            expect![[r#"
                -- errors --
                error: Expected type name but got end of file
                Color::
                ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_single_field() {
        accept(
            "Outcome::Success {value: 42}",
            expect![[r#"
                Outcome::Success {value: 42}
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_multiple_fields() {
        accept(
            r#"Event::Click {x: 10, y: 20}"#,
            expect![[r#"
                Event::Click {x: 10, y: 20}
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_string_field() {
        accept(
            r#"Outcome::Failure {message: "something went wrong"}"#,
            expect![[r#"
                Outcome::Failure {message: "something went wrong"}
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_nested_expression() {
        accept(
            r#"Outcome::Success {value: x + 1}"#,
            expect![[r#"
                Outcome::Success {value: x + 1}
            "#]],
        );
    }

    #[test]
    fn rejects_enum_literal_with_spread() {
        reject(
            "Outcome::Success {...other, value: 42}",
            expect![[r#"
                -- errors --
                error: Spread is not allowed in an enum variant literal
                Outcome::Success {...other, value: 42}
                                  ^^^
                -- ast --
                Outcome::Success {value: 42}
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_trailing_comma() {
        accept(
            "Outcome::Success {value: 42,}",
            expect![[r#"
                Outcome::Success {value: 42}
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ENUM LITERAL (BRACE SYNTAX)                                           //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_enum_literal_with_braces_single_field() {
        accept(
            "Outcome::Success {value: 42}",
            expect![[r#"
                Outcome::Success {value: 42}
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_braces_multiple_fields() {
        accept(
            "Event::Click {x: 10, y: 20}",
            expect![[r#"
                Event::Click {x: 10, y: 20}
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_with_braces_containing_record_with_braces() {
        accept(
            r#"Outcome::Success {value: Inner {x: 1}}"#,
            expect![[r#"
                Outcome::Success {value: Inner {x: 1}}
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // OPTION LITERAL                                                        //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_some_with_string_literal() {
        accept(
            r#"Some("hello")"#,
            expect![[r#"
                Some("hello")
            "#]],
        );
    }

    #[test]
    fn accepts_some_with_integer_literal() {
        accept(
            "Some(42)",
            expect![[r#"
                Some(42)
            "#]],
        );
    }

    #[test]
    fn accepts_some_with_variable() {
        accept(
            "Some(x)",
            expect![[r#"
                Some(x)
            "#]],
        );
    }

    #[test]
    fn accepts_some_with_expression() {
        accept(
            "Some(a + b)",
            expect![[r#"
                Some(a + b)
            "#]],
        );
    }

    #[test]
    fn accepts_none() {
        accept(
            "None",
            expect![[r#"
                None
            "#]],
        );
    }

    #[test]
    fn accepts_some_with_nested_some() {
        accept(
            "Some(Some(1))",
            expect![[r#"
                Some(Some(1))
            "#]],
        );
    }

    #[test]
    fn accepts_some_with_none() {
        accept(
            "Some(None)",
            expect![[r#"
                Some(None)
            "#]],
        );
    }

    #[test]
    fn accepts_option_in_array() {
        accept(
            "[Some(1), None, Some(2)]",
            expect![[r#"
                [Some(1), None, Some(2)]
            "#]],
        );
    }

    #[test]
    fn rejects_some_without_parentheses() {
        reject(
            "Some",
            expect![[r#"
                -- errors --
                error: Expected token '(' but got end of file
                Some
                ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_some_with_empty_parentheses() {
        reject(
            "Some()",
            expect![[r#"
                -- errors --
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
    fn accepts_match_expression_with_single_arm() {
        accept(
            indoc! {r#"
                match color {Color::Red => "red"}
            "#},
            expect![[r#"
                match color {Color::Red => "red"}
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_with_multiple_arms() {
        accept(
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
    fn accepts_match_expression_with_trailing_comma() {
        accept(
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
    fn accepts_match_expression_with_complex_body() {
        accept(
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
    fn accepts_nested_match_expression() {
        accept(
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
    fn accepts_empty_match_expression() {
        accept(
            "match color {}",
            expect![[r#"
                match color {}
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_enum_field_pattern() {
        accept(
            "match result { Outcome::Success{value: v} => v, Outcome::Failure{message: m} => m }",
            expect![[r#"
                match result {
                  Outcome::Success{value: v} => v,
                  Outcome::Failure{message: m} => m,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_enum_multiple_field_pattern() {
        accept(
            "match event { Event::Click{x: a, y: b} => a + b }",
            expect![[r#"
                match event {Event::Click{x: a, y: b} => a + b}
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_mixed_enum_patterns() {
        accept(
            "match maybe { Maybe::Just{value: v} => v, Maybe::Nothing => 0 }",
            expect![[r#"
                match maybe {
                  Maybe::Just{value: v} => v,
                  Maybe::Nothing => 0,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_nested_enum_field_pattern() {
        accept(
            "match result { Outcome::Success{data: Some(x)} => x, _ => 0 }",
            expect![[r#"
                match result {Outcome::Success{data: Some(x)} => x, _ => 0}
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_wildcard_field_pattern() {
        accept(
            "match result { Outcome::Success{value: _} => 1, Outcome::Failure{message: _} => 0 }",
            expect![[r#"
                match result {
                  Outcome::Success{value: _} => 1,
                  Outcome::Failure{message: _} => 0,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_empty_braces_pattern() {
        // Empty braces are accepted but normalized away
        accept(
            "match point { Point::XY{} => 0 }",
            expect![[r#"
                match point {Point::XY => 0}
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_binary_expr_subject() {
        accept(
            r#"match path == "" { true => git_ref, _ => git_ref + " - " + path }"#,
            expect![[r#"
                match path == "" {
                  true => git_ref,
                  _ => git_ref + " - " + path,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_shorthand_enum_field_pattern() {
        accept(
            "match result { Outcome::Success{value} => value, Outcome::Failure{message} => message }",
            expect![[r#"
                match result {
                  Outcome::Success{value} => value,
                  Outcome::Failure{message} => message,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_shorthand_record_field_pattern() {
        accept(
            "match user { User{name, age} => name }",
            expect![[r#"
                match user {User{name, age} => name}
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_mixed_shorthand_and_explicit_field_pattern() {
        accept(
            "match event { Event::Click{x, y: b} => x + b }",
            expect![[r#"
                match event {Event::Click{x, y: b} => x + b}
            "#]],
        );
    }

    #[test]
    fn rejects_match_when_some_pattern_parenthesis_is_unmatched() {
        reject(
            "match maybe { Some(x",
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                match maybe { Some(x
                            ^

                error: Unmatched '('
                match maybe { Some(x
                                  ^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // FOR EXPRESSION                                                        //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_for_expression_over_array() {
        accept(
            "for item in items { item }",
            expect![[r#"
                for item in items { item }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_over_array_literal() {
        accept(
            "for item in [1, 2, 3] { item }",
            expect![[r#"
                for item in [1, 2, 3] { item }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_over_inclusive_range() {
        accept(
            "for i in 1..=n { i }",
            expect![[r#"
                for i in 1..=n { i }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_with_discarded_variable() {
        accept(
            "for _ in items { x }",
            expect![[r#"
                for _ in items { x }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_with_let_in_body() {
        accept(
            "for item in items { let name = item.name; name }",
            expect![[r#"
                for item in items {
                  let name = item.name;
                  name
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_for_expressions() {
        accept(
            "for row in rows { for cell in row { cell } }",
            expect![[r#"
                for row in rows { for cell in row { cell } }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_in_match_arm() {
        accept(
            "match x { Some(items) => for item in items { item }, None => <></> }",
            expect![[r#"
                match x {
                  Some(items) => for item in items { item },
                  None => fragment(),
                }
            "#]],
        );
    }

    #[test]
    fn rejects_for_expression_without_in() {
        reject(
            "for item items { item }",
            expect![[r#"
                -- errors --
                error: Expected token 'in' but got 'items'
                for item items { item }
                         ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_for_expression_without_opening_brace() {
        reject(
            "for item in items item",
            expect![[r#"
                -- errors --
                error: Expected token '{' but got 'item'
                for item in items item
                                  ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_for_expression_with_empty_body() {
        reject(
            "for item in items {}",
            expect![[r#"
                -- errors --
                error: Unexpected token '}'
                for item in items {}
                                   ^
            "#]],
        );
    }

    #[test]
    fn rejects_for_expression_without_closing_brace() {
        reject(
            "for item in items { item",
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                for item in items { item
                                  ^
            "#]],
        );
    }

    #[test]
    fn rejects_for_expression_with_let_and_no_tail_expression() {
        reject(
            "for item in items { let name = item.name; }",
            expect![[r#"
                -- errors --
                error: A block must end with an expression
                for item in items { let name = item.name; }
                                                        ^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // MACRO INVOCATIONS                                                     //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_join_macro_with_no_args() {
        accept(
            "join!()",
            expect![[r#"
                join!()
            "#]],
        );
    }

    #[test]
    fn accepts_join_macro_with_single_arg() {
        accept(
            r#"join!("hello")"#,
            expect![[r#"
                join!("hello")
            "#]],
        );
    }

    #[test]
    fn accepts_join_macro_with_multiple_args() {
        accept(
            r#"join!("foo", "bar", "baz")"#,
            expect![[r#"
                join!("foo", "bar", "baz")
            "#]],
        );
    }

    #[test]
    fn accepts_unknown_macro() {
        accept(
            "unknown!(x)",
            expect![[r#"
                unknown!(x)
            "#]],
        );
    }

    #[test]
    fn accepts_asset_macro_with_string_literal() {
        accept(
            r#"asset!("/logo.svg")"#,
            expect![[r#"
                asset!("/logo.svg")
            "#]],
        );
    }

    #[test]
    fn rejects_array_with_trailing_comma_and_missing_closing_bracket() {
        reject(
            "[1, 2,",
            expect![[r#"
                -- errors --
                error: Unmatched '['
                [1, 2,
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_trailing_comma_and_missing_closing_brace() {
        reject(
            r#"User {name: "John","#,
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                User {name: "John",
                     ^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // METHOD CALLS ON LITERALS AND PARENTHESIZED EXPRESSIONS                //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_method_call_on_integer_literal() {
        accept(
            "42.to_string()",
            expect![[r#"
                42.to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_float_literal() {
        accept(
            "3.14.to_string()",
            expect![[r#"
                3.14.to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_string_literal() {
        accept(
            r#""hello".length()"#,
            expect![[r#"
                "hello".length()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_boolean_literal() {
        accept(
            "true.to_string()",
            expect![[r#"
                true.to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_parenthesized_expression() {
        accept(
            "(x + y).to_string()",
            expect![[r#"
                (x + y).to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_chained_method_calls_on_parenthesized_expression() {
        accept(
            "(x.to_float() + 0.5).to_string().length()",
            expect![[r#"
                (x.to_float() + 0.5).to_string().length()
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_on_parenthesized_expression() {
        accept(
            "(rec).field",
            expect![[r#"
                rec.field
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_array_literal() {
        accept(
            "[1, 2, 3].length()",
            expect![[r#"
                [1, 2, 3].length()
            "#]],
        );
    }

    // Field access (not method calls) on literals - parser should accept,
    // type checker will reject invalid combinations

    #[test]
    fn accepts_field_access_on_int_literal() {
        accept(
            "42.field",
            expect![[r#"
                42.field
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_on_string_literal() {
        accept(
            r#""hello".length"#,
            expect![[r#"
                "hello".length
            "#]],
        );
    }

    #[test]
    fn accepts_chained_field_access_on_literal() {
        accept(
            "42.foo.bar",
            expect![[r#"
                42.foo.bar
            "#]],
        );
    }

    // Record and enum literals with postfix access

    #[test]
    fn accepts_field_access_on_record_literal() {
        accept(
            r#"User {name: "John"}.name"#,
            expect![[r#"
                User {name: "John"}.name
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_record_literal() {
        accept(
            r#"User {name: "John"}.to_string()"#,
            expect![[r#"
                User {name: "John"}.to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_on_enum_literal() {
        accept(
            "Status::Active.value",
            expect![[r#"
                Status::Active.value
            "#]],
        );
    }

    // Option literals with postfix access

    #[test]
    fn accepts_method_call_on_some_literal() {
        accept(
            "Some(42).unwrap()",
            expect![[r#"
                Some(42).unwrap()
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_none_literal() {
        accept(
            "None.is_none()",
            expect![[r#"
                None.is_none()
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_on_some_literal() {
        accept(
            "Some(42).value",
            expect![[r#"
                Some(42).value
            "#]],
        );
    }

    // Precedence verification - method calls bind tighter than binary ops

    #[test]
    fn accepts_method_call_with_higher_precedence_than_addition() {
        accept(
            "1 + 2.to_string()",
            expect![[r#"
                1 + 2.to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_with_higher_precedence_than_multiplication() {
        accept(
            "x * y.field",
            expect![[r#"
                x * y.field
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // FUNCTION CALL                                                         //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn accepts_function_call_with_no_arguments() {
        accept(
            "foo()",
            expect![[r#"
                foo()
            "#]],
        );
    }

    #[test]
    fn accepts_function_call_with_single_argument() {
        accept(
            "foo(10)",
            expect![[r#"
                foo(10)
            "#]],
        );
    }

    #[test]
    fn accepts_function_call_with_multiple_arguments() {
        accept(
            "foo(1, 2, 3)",
            expect![[r#"
                foo(1, 2, 3)
            "#]],
        );
    }

    #[test]
    fn accepts_let_statements_in_block() {
        accept(
            "{ let a = 1; let b = a + 1; a + b }",
            expect![[r#"
            {
              let a = 1;
              let b = a + 1;
              a + b
            }
        "#]],
        );
    }

    #[test]
    fn accepts_let_with_type_annotation() {
        accept(
            "{ let a: Int = 1; a }",
            expect![[r#"
            {
              let a: Int = 1;
              a
            }
        "#]],
        );
    }

    #[test]
    fn accepts_block_in_match_arm() {
        accept(
            "match x { Some(t) => { let s = t; s }, None => \"\" }",
            expect![[r#"
                match x {
                  Some(t) => {
                    let s = t;
                    s
                  },
                  None => "",
                }
            "#]],
        );
    }

    #[test]
    fn accepts_block_as_operand_and_argument() {
        accept(
            "1 + { let a = 2; a }",
            expect![[r#"
            1 + {
              let a = 2;
              a
            }
        "#]],
        );
        accept(
            "f({ let a = 2; a })",
            expect![[r#"
            f(
              {
                let a = 2;
                a
              },
            )
        "#]],
        );
    }

    #[test]
    fn accepts_block_as_let_value() {
        accept(
            "{ let x = { let a = 1; a }; x }",
            expect![[r#"
            {
              let x = {
                let a = 1;
                a
              };
              x
            }
        "#]],
        );
    }

    #[test]
    fn accepts_redundant_braces_around_expression() {
        accept(
            "{ a }",
            expect![[r#"
            a
        "#]],
        );
    }

    #[test]
    fn rejects_block_ending_in_let() {
        reject(
            "{ let a = 1; }",
            expect![[r#"
            -- errors --
            error: A block must end with an expression
            { let a = 1; }
                       ^
        "#]],
        );
    }

    #[test]
    fn rejects_block_with_semicolon_after_tail() {
        reject(
            "{ let a = 1; a; }",
            expect![[r#"
            -- errors --
            error: Expected token '}' but got ';'
            { let a = 1; a; }
                          ^
        "#]],
        );
    }

    #[test]
    fn rejects_let_outside_block() {
        reject(
            "match x { Some(t) => let s = t; s, None => \"\" }",
            expect![[r#"
                -- errors --
                error: let is only allowed inside a block: wrap the expression in braces
                match x { Some(t) => let s = t; s, None => "" }
                                     ^^^
                -- ast --
                match x {None => ""}
            "#]],
        );
    }

    #[test]
    fn rejects_let_without_semicolon() {
        reject(
            "{ let a = 1 a }",
            expect![[r#"
            -- errors --
            error: Expected token ';' but got 'a'
            { let a = 1 a }
                        ^
        "#]],
        );
    }

    #[test]
    fn accepts_function_call_with_expression_arguments() {
        accept(
            "foo(x + 1, bar(y))",
            expect![[r#"
                foo(x + 1, bar(y))
            "#]],
        );
    }

    #[test]
    fn accepts_method_call_on_function_call_result() {
        accept(
            "foo(1).to_string()",
            expect![[r#"
                foo(1).to_string()
            "#]],
        );
    }

    #[test]
    fn accepts_function_call_in_binary_expression() {
        accept(
            "foo(1) + foo(2)",
            expect![[r#"
                foo(1) + foo(2)
            "#]],
        );
    }
}
