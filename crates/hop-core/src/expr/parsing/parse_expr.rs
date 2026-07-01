use std::collections::VecDeque;
use std::iter::Peekable;

use crate::document::{CheapString, DocumentCursor, DocumentRange};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

use super::parsed_expr::{
    Constructor, ParsedBinaryOp, ParsedExpr, ParsedMatchArm, ParsedMatchPattern,
};
use super::token::Token;
use super::tokenizer::{
    advance_if, expect_eof, expect_field_name, expect_opposite, expect_token, expect_type_name,
    expect_variable_name, next_collecting_comments as next, next_if, parse_delimited_list,
    peek_past_comments as peek,
};
use crate::parse_error::{ParseError, ParseErrorKind};

pub fn parse_expr(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let result = parse_logical(iter, comments, errors, range)?;
    expect_eof(iter, comments, errors)?;
    Some(result)
}

pub fn parse_logical(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = parse_logical_and(iter, comments, errors, range)?;
    while advance_if(iter, comments, errors, Token::LogicalOr).is_some() {
        let right = parse_logical_and(iter, comments, errors, range)?;
        expr = ParsedExpr::BinaryOp {
            range: expr.range().clone().to(right.range().clone()),
            left: Box::new(expr),
            operator: ParsedBinaryOp::LogicalOr,
            right: Box::new(right),
        };
    }
    Some(expr)
}

fn parse_logical_and(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = parse_equality(iter, comments, errors, range)?;
    while advance_if(iter, comments, errors, Token::LogicalAnd).is_some() {
        let right = parse_equality(iter, comments, errors, range)?;
        expr = ParsedExpr::BinaryOp {
            range: expr.range().clone().to(right.range().clone()),
            left: Box::new(expr),
            operator: ParsedBinaryOp::LogicalAnd,
            right: Box::new(right),
        };
    }
    Some(expr)
}

fn parse_equality(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = parse_relational(iter, comments, errors, range)?;
    loop {
        if advance_if(iter, comments, errors, Token::Eq).is_some() {
            let right = parse_relational(iter, comments, errors, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Eq,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, Token::NotEq).is_some() {
            let right = parse_relational(iter, comments, errors, range)?;
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
    Some(expr)
}

fn parse_relational(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = parse_additive(iter, comments, errors, range)?;
    loop {
        if advance_if(iter, comments, errors, Token::LessThan).is_some() {
            let right = parse_additive(iter, comments, errors, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::LessThan,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, Token::GreaterThan).is_some() {
            let right = parse_additive(iter, comments, errors, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::GreaterThan,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, Token::LessThanOrEqual).is_some() {
            let right = parse_additive(iter, comments, errors, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::LessThanOrEqual,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, Token::GreaterThanOrEqual).is_some() {
            let right = parse_additive(iter, comments, errors, range)?;
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
    Some(expr)
}

fn parse_additive(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = parse_multiplicative(iter, comments, errors, range)?;
    loop {
        if advance_if(iter, comments, errors, Token::Plus).is_some() {
            let right = parse_multiplicative(iter, comments, errors, range)?;
            expr = ParsedExpr::BinaryOp {
                range: expr.range().clone().to(right.range().clone()),
                left: Box::new(expr),
                operator: ParsedBinaryOp::Plus,
                right: Box::new(right),
            };
        } else if advance_if(iter, comments, errors, Token::Minus).is_some() {
            let right = parse_multiplicative(iter, comments, errors, range)?;
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
    Some(expr)
}

fn parse_multiplicative(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = parse_unary(iter, comments, errors, range)?;
    while advance_if(iter, comments, errors, Token::Asterisk).is_some() {
        let right = parse_unary(iter, comments, errors, range)?;
        expr = ParsedExpr::BinaryOp {
            range: expr.range().clone().to(right.range().clone()),
            left: Box::new(expr),
            operator: ParsedBinaryOp::Multiply,
            right: Box::new(right),
        };
    }
    Some(expr)
}

fn parse_unary(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    if let Some(operator_range) = advance_if(iter, comments, errors, Token::Not) {
        let expr = parse_unary(iter, comments, errors, range)?; // Right associative for multiple !
        Some(ParsedExpr::BooleanNegation {
            range: operator_range.to(expr.range().clone()),
            operand: Box::new(expr),
        })
    } else if let Some(operator_range) = advance_if(iter, comments, errors, Token::Minus) {
        let expr = parse_unary(iter, comments, errors, range)?; // Right associative for multiple -
        Some(ParsedExpr::NumericNegation {
            range: operator_range.to(expr.range().clone()),
            operand: Box::new(expr),
        })
    } else {
        parse_primary(iter, comments, errors, range)
    }
}

fn parse_array_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    left_bracket: DocumentRange,
) -> Option<ParsedExpr> {
    let (elements, right_bracket) = parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &Token::LeftBracket,
        &left_bracket,
        parse_logical,
    )?;
    Some(ParsedExpr::ArrayLiteral {
        elements,
        range: left_bracket.to(right_bracket),
    })
}

pub fn parse_primary(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedExpr> {
    let mut expr = match next(iter, comments, errors) {
        Some((Token::Identifier(name), name_range)) => {
            if let Some(bang_range) = advance_if(iter, comments, errors, Token::Not) {
                return parse_macro_invocation(
                    iter,
                    comments,
                    errors,
                    range,
                    name,
                    name_range.to(bang_range),
                );
            }
            match VarName::from_cheap_string(name.clone()) {
                Ok(var_name) => ParsedExpr::Var {
                    range: name_range,
                    value: var_name,
                },
                Err(error) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::InvalidVariableName { name, error },
                        name_range,
                    ));
                    return None;
                }
            }
        }
        Some((Token::TypeName(name), name_range)) => {
            let type_name = match TypeName::from_cheap_string(name) {
                Ok(type_name) => type_name,
                Err(error) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::InvalidTypeName { error },
                        name_range,
                    ));
                    return None;
                }
            };
            let is_enum_variant = peek(iter).map(|(t, _)| t) == Some(Token::ColonColon);
            if is_enum_variant {
                parse_enum_literal(iter, comments, errors, range, type_name, name_range)?
            } else {
                parse_record_literal(iter, comments, errors, range, type_name, name_range)?
            }
        }
        Some((Token::StringLiteral(value), lit_range)) => ParsedExpr::StringLiteral {
            value,
            range: lit_range,
        },
        Some((Token::True, lit_range)) => ParsedExpr::BooleanLiteral {
            value: true,
            range: lit_range,
        },
        Some((Token::False, lit_range)) => ParsedExpr::BooleanLiteral {
            value: false,
            range: lit_range,
        },
        Some((Token::IntLiteral(value), lit_range)) => ParsedExpr::IntLiteral {
            value,
            range: lit_range,
        },
        Some((Token::FloatLiteral(value), lit_range)) => ParsedExpr::FloatLiteral {
            value,
            range: lit_range,
        },
        Some((Token::LeftBracket, left_bracket)) => {
            parse_array_literal(iter, comments, errors, range, left_bracket)?
        }
        Some((Token::LeftParen, left_paren)) => {
            let inner = parse_logical(iter, comments, errors, range)?;
            expect_opposite(iter, comments, errors, &Token::LeftParen, &left_paren)?;
            inner
        }
        Some((Token::Match, match_range)) => {
            parse_match(iter, comments, errors, range, match_range)?
        }
        Some((Token::Some, some_range)) => {
            let left_paren = expect_token(iter, comments, errors, range, &Token::LeftParen)?;
            let value = parse_logical(iter, comments, errors, range)?;
            let right_paren =
                expect_opposite(iter, comments, errors, &Token::LeftParen, &left_paren)?;
            ParsedExpr::OptionLiteral {
                value: Some(Box::new(value)),
                range: some_range.to(right_paren),
            }
        }
        Some((Token::None, none_range)) => ParsedExpr::OptionLiteral {
            value: None,
            range: none_range,
        },
        Some((Token::TypeFragment, start_range)) => {
            expect_token(iter, comments, errors, range, &Token::ColonColon)?;
            let (method_name, method_range) = match next(iter, comments, errors) {
                Some((Token::Identifier(name), name_range)) => (name, name_range),
                Some((token, token_range)) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedToken { token },
                        token_range,
                    ));
                    return None;
                }
                None => {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedEof {},
                        range.clone(),
                    ));
                    return None;
                }
            };
            if method_name.as_str() != "empty" {
                errors.push(ParseError::new(
                    ParseErrorKind::UnexpectedToken {
                        token: Token::Identifier(method_name),
                    },
                    method_range,
                ));
                return None;
            }
            let left_paren = expect_token(iter, comments, errors, range, &Token::LeftParen)?;
            let right_paren =
                expect_opposite(iter, comments, errors, &Token::LeftParen, &left_paren)?;
            ParsedExpr::FragmentEmpty {
                range: start_range.to(right_paren),
            }
        }
        Some((token, token_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::UnexpectedToken { token },
                token_range,
            ));
            return None;
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::UnexpectedEof {},
                range.clone(),
            ));
            return None;
        }
    };
    while let Some(dot) = advance_if(iter, comments, errors, Token::Dot) {
        match next(iter, comments, errors) {
            Some((Token::Identifier(field_ident), field_range)) => {
                let field_name = match FieldName::from_cheap_string(field_ident.clone()) {
                    Ok(name) => name,
                    Err(error) => {
                        errors.push(ParseError::new(
                            ParseErrorKind::InvalidFieldName {
                                name: field_ident,
                                error,
                            },
                            field_range,
                        ));
                        return None;
                    }
                };
                if let Some(left_paren) = advance_if(iter, comments, errors, Token::LeftParen) {
                    let right_paren =
                        expect_opposite(iter, comments, errors, &Token::LeftParen, &left_paren)?;
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
            Some((_, token_range)) => {
                errors.push(ParseError::new(
                    ParseErrorKind::ExpectedIdentifierAfterDot {},
                    token_range,
                ));
                return None;
            }
            None => {
                errors.push(ParseError::new(
                    ParseErrorKind::UnexpectedEndOfFieldAccess {},
                    expr.range().clone().to(dot),
                ));
                return None;
            }
        }
    }
    Some(expr)
}

fn parse_macro_invocation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    macro_name: CheapString,
    subject_range: DocumentRange,
) -> Option<ParsedExpr> {
    let name_str = macro_name.as_str();
    if name_str != "join" && name_str != "asset" {
        errors.push(ParseError::new(
            ParseErrorKind::UnknownMacro { name: macro_name },
            subject_range,
        ));
        return None;
    }
    let left_paren = expect_token(iter, comments, errors, range, &Token::LeftParen)?;
    let (args, right_paren) = parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &Token::LeftParen,
        &left_paren,
        parse_logical,
    )?;
    Some(ParsedExpr::MacroInvocation {
        name: macro_name,
        subject_range: subject_range.clone(),
        args,
        range: subject_range.to(right_paren),
    })
}

fn parse_record_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    name: TypeName,
    name_range: DocumentRange,
) -> Option<ParsedExpr> {
    let left_delim = expect_token(iter, comments, errors, range, &Token::LeftBrace)?;
    let (fields, right_delim) = parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &Token::LeftBrace,
        &left_delim,
        |iter, comments, errors, range| {
            let (field_name, _) = expect_field_name(iter, comments, errors, range)?;
            expect_token(iter, comments, errors, range, &Token::Colon)?;
            Some((field_name, parse_logical(iter, comments, errors, range)?))
        },
    )?;
    Some(ParsedExpr::RecordLiteral {
        record_name: name,
        record_name_range: name_range.clone(),
        fields,
        range: name_range.to(right_delim),
    })
}

fn parse_enum_literal(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    enum_name: TypeName,
    enum_name_range: DocumentRange,
) -> Option<ParsedExpr> {
    expect_token(iter, comments, errors, range, &Token::ColonColon)?;
    let (variant_name, variant_range) = expect_type_name(iter, comments, errors, range)?;
    let constructor_range = enum_name_range.clone().to(variant_range.clone());
    let (fields, end_range) =
        if let Some(left_delim) = advance_if(iter, comments, errors, Token::LeftBrace) {
            parse_delimited_list(
                iter,
                comments,
                errors,
                range,
                &Token::LeftBrace,
                &left_delim,
                |iter, comments, errors, range| {
                    let (field_name, field_name_range) =
                        expect_field_name(iter, comments, errors, range)?;
                    expect_token(iter, comments, errors, range, &Token::Colon)?;
                    Some((
                        field_name,
                        field_name_range,
                        parse_logical(iter, comments, errors, range)?,
                    ))
                },
            )?
        } else {
            (Vec::new(), variant_range)
        };
    Some(ParsedExpr::EnumLiteral {
        enum_name,
        variant_name,
        fields,
        constructor_range,
        enum_name_range: enum_name_range.clone(),
        range: enum_name_range.to(end_range),
    })
}

fn parse_match(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    match_range: DocumentRange,
) -> Option<ParsedExpr> {
    let subject = parse_logical(iter, comments, errors, range)?;
    let left_brace = expect_token(iter, comments, errors, range, &Token::LeftBrace)?;
    let (arms, right_brace) = parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &Token::LeftBrace,
        &left_brace,
        |iter, comments, errors, range| {
            let pattern = parse_match_pattern(iter, comments, errors, range)?;
            expect_token(iter, comments, errors, range, &Token::FatArrow)?;
            let body = parse_logical(iter, comments, errors, range)?;
            Some(ParsedMatchArm { pattern, body })
        },
    )?;
    Some(ParsedExpr::Match {
        subject: Box::new(subject),
        arms,
        range: match_range.to(right_brace),
    })
}

pub fn parse_match_pattern(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedMatchPattern> {
    if let Some(pattern_range) = advance_if(iter, comments, errors, Token::Underscore) {
        return Some(ParsedMatchPattern::Wildcard {
            range: pattern_range,
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, errors, Token::True) {
        return Some(ParsedMatchPattern::Constructor {
            constructor: Constructor::BooleanTrue,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            enum_name_range: None,
            range: pattern_range,
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, errors, Token::False) {
        return Some(ParsedMatchPattern::Constructor {
            constructor: Constructor::BooleanFalse,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            enum_name_range: None,
            range: pattern_range,
        });
    }
    if let Some(some_range) = advance_if(iter, comments, errors, Token::Some) {
        expect_token(iter, comments, errors, range, &Token::LeftParen)?;
        let inner_pattern = parse_match_pattern(iter, comments, errors, range)?;
        let right_paren = expect_token(iter, comments, errors, range, &Token::RightParen)?;
        return Some(ParsedMatchPattern::Constructor {
            constructor: Constructor::OptionSome,
            args: vec![inner_pattern],
            fields: Vec::new(),
            constructor_range: some_range.clone(),
            enum_name_range: None,
            range: some_range.to(right_paren),
        });
    }
    if let Some(pattern_range) = advance_if(iter, comments, errors, Token::None) {
        return Some(ParsedMatchPattern::Constructor {
            constructor: Constructor::OptionNone,
            args: Vec::new(),
            fields: Vec::new(),
            constructor_range: pattern_range.clone(),
            enum_name_range: None,
            range: pattern_range,
        });
    }
    if let Some((Token::TypeName(type_name_str), type_name_range)) =
        next_if(iter, comments, errors, |res| {
            matches!(res, (Token::TypeName(_), _))
        })
    {
        let type_name = match TypeName::from_cheap_string(type_name_str) {
            Ok(name) => name,
            Err(error) => {
                errors.push(ParseError::new(
                    ParseErrorKind::InvalidTypeName { error },
                    type_name_range,
                ));
                return None;
            }
        };
        if advance_if(iter, comments, errors, Token::ColonColon).is_some() {
            let (variant_name, variant_range) = expect_type_name(iter, comments, errors, range)?;

            let (fields, end_range) =
                if let Some(left_brace) = advance_if(iter, comments, errors, Token::LeftBrace) {
                    parse_delimited_list(
                        iter,
                        comments,
                        errors,
                        range,
                        &Token::LeftBrace,
                        &left_brace,
                        |iter, comments, errors, range| {
                            let (field_name, field_range) =
                                expect_field_name(iter, comments, errors, range)?;
                            let pattern =
                                if advance_if(iter, comments, errors, Token::Colon).is_some() {
                                    parse_match_pattern(iter, comments, errors, range)?
                                } else {
                                    ParsedMatchPattern::Binding {
                                        name: VarName::new(field_name.as_str()).unwrap(),
                                        range: field_range.clone(),
                                    }
                                };
                            Some((field_name, field_range, pattern))
                        },
                    )?
                } else {
                    (Vec::new(), variant_range.clone())
                };

            let constructor_range = type_name_range.clone().to(variant_range);
            return Some(ParsedMatchPattern::Constructor {
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
            let left_brace = expect_token(iter, comments, errors, range, &Token::LeftBrace)?;
            let (fields, right_brace) = parse_delimited_list(
                iter,
                comments,
                errors,
                range,
                &Token::LeftBrace,
                &left_brace,
                |iter, comments, errors, range| {
                    let (field_name, field_range) =
                        expect_field_name(iter, comments, errors, range)?;
                    let pattern = if advance_if(iter, comments, errors, Token::Colon).is_some() {
                        parse_match_pattern(iter, comments, errors, range)?
                    } else {
                        ParsedMatchPattern::Binding {
                            name: VarName::new(field_name.as_str()).unwrap(),
                            range: field_range.clone(),
                        }
                    };
                    Some((field_name, field_range, pattern))
                },
            )?;
            return Some(ParsedMatchPattern::Constructor {
                constructor: Constructor::Record { type_name },
                args: Vec::new(),
                fields,
                constructor_range: type_name_range.clone(),
                enum_name_range: None,
                range: type_name_range.to(right_brace),
            });
        }
    }
    let (var_name, var_range) = expect_variable_name(iter, comments, errors, range)?;
    Some(ParsedMatchPattern::Binding {
        name: var_name,
        range: var_range,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_id::DocumentId;
    use crate::simple_annotation::SimpleAnnotation;
    use crate::{annotation::Annotation, document_annotator::DocumentAnnotator};
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn accept(input: &str, expected: Expect) {
        let document_id = DocumentId::new("test.hop").unwrap();
        let cursor = DocumentCursor::new(document_id, input.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = Vec::new();
        let Some(result) = parse_expr(&mut iter, &mut comments, &mut errors, &range) else {
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
        let mut errors = Vec::new();
        if parse_expr(&mut iter, &mut comments, &mut errors, &range).is_some() {
            panic!("expected parse error but expression parsed successfully");
        }
        let err = errors.to_vec().pop().expect("expected at least one error");
        let actual = DocumentAnnotator::new()
            .with_label("error")
            .without_location()
            .without_line_numbers()
            .annotate(
                &document_id,
                [SimpleAnnotation {
                    message: err.message(),
                    range: err.range().clone(),
                }],
            )
            .render();
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
                error: Expected token ':' but got '"John"'
                User {name "John"}
                           ^^^^^^
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
    fn rejects_record_literal_with_mismatched_brace_paren() {
        reject(
            r#"Foo {bar: "baz")"#,
            expect![[r#"
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
    fn accepts_empty_fragment_literal() {
        accept(
            "Fragment::empty()",
            expect![[r#"
                    Fragment::empty()
                "#]],
        );
    }

    #[test]
    fn rejects_fragment_literal_with_unknown_method() {
        reject(
            "Fragment::nonempty()",
            expect![[r#"
                    error: Unexpected token 'nonempty'
                    Fragment::nonempty()
                              ^^^^^^^^
                "#]],
        );
    }

    #[test]
    fn rejects_empty_fragment_literal_without_parentheses() {
        reject(
            "Fragment::empty",
            expect![[r#"
                    error: Expected token '(' but got end of file
                    Fragment::empty
                    ^^^^^^^^^^^^^^^
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
    fn rejects_expr_when_trailing_tokens_are_present() {
        reject(
            "x y",
            expect![[r#"
                error: Unexpected token 'y'
                x y
                  ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_array_bracket_is_unmatched() {
        reject(
            "[foo, bar == [foo, bar]",
            expect![[r#"
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
                error: Unexpected token '=='
                [foo, bar, == [foo, bar]
                           ^^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_array_closing_bracket_is_missing() {
        reject(
            "[1,2",
            expect![[r#"
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
                error: Expected token ']' but got 'id'
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
                error: Unexpected end of field access
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
                error: Expected identifier after '.'
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
                error: Unmatched '('
                (x == y
                ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_closing_paren_has_no_opening() {
        reject(
            "x == y)",
            expect![[r#"
                error: Unexpected token ')'
                x == y)
                      ^
            "#]],
        );
    }

    #[test]
    fn rejects_expr_when_parens_are_empty() {
        reject(
            "()",
            expect![[r#"
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
                error: Expected identifier after '.'
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
                error: Unknown macro 'x'
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
                error: Expected type name but got red
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
    fn rejects_unknown_macro() {
        reject(
            "unknown!(x)",
            expect![[r#"
                error: Unknown macro 'unknown'
                unknown!(x)
                ^^^^^^^^
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
}
