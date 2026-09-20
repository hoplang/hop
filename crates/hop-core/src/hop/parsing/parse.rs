use super::parse_expr;
use super::parse_helpers;
use super::parsed_ast::{
    ParsedAst, ParsedDeclaration, ParsedEnumDeclaration, ParsedEnumDeclarationVariant,
    ParsedFieldDeclaration, ParsedFunctionDeclaration, ParsedImportDeclaration,
    ParsedPageDeclaration, ParsedRecordDeclaration,
};
use super::tokenize_expr;
use crate::document::{CheapString, Document, DocumentCursor, DocumentRange};
use crate::document_id::DocumentId;
use crate::examples_annotation::ExamplesAnnotation;

use crate::hop::parsing::ParsedType;
use crate::hop::parsing::parse_type::parse_type;
use crate::hop::parsing::parsed_ast::ParsedParameter;
use crate::hop::parsing::token::LangToken;
use crate::hop::parsing::token::LangTokenPair;
use crate::parse_error::{Emit, ErrorEmitted, OrEmit, ParseError, ParseErrorKind};
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::HashSet;

pub fn parse(
    document_id: DocumentId,
    document: Document,
    errors: &mut Vec<ParseError>,
) -> ParsedAst {
    let mut iter = document.cursor();
    let mut declarations = Vec::new();
    let mut comments = Vec::new();

    loop {
        let pub_range = parse_helpers::next_if_eq(&mut iter, &mut comments, errors, LangToken::Pub);
        let declaration = match tokenize_expr::next(&mut iter, &mut comments, errors) {
            Some((LangToken::Import, keyword)) => {
                parse_import_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                    .map(ParsedDeclaration::Import)
            }
            Some((LangToken::Record, keyword)) => {
                parse_record_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                    .map(ParsedDeclaration::Record)
            }
            Some((LangToken::Enum, keyword)) => {
                parse_enum_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                    .map(ParsedDeclaration::Enum)
            }
            Some((LangToken::Page, keyword)) => {
                parse_page_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                    .map(|page| ParsedDeclaration::Page(Box::new(page)))
            }
            Some((LangToken::Fn, keyword)) => {
                parse_function_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                    .map(|function| ParsedDeclaration::Function(Box::new(function)))
            }
            token => {
                if let Some(pub_range) = pub_range {
                    let _ = errors.emit(ParseErrorKind::UnexpectedPubKeyword {}, pub_range);
                }
                let Some((token, token_range)) = token else {
                    break;
                };
                Err(errors.emit(ParseErrorKind::UnexpectedToken { token }, token_range))
            }
        };
        match declaration {
            Ok(declaration) => declarations.push(declaration),
            Err(reported) => {
                parse_helpers::skip_to(&mut iter, reported, |token| {
                    parse_helpers::DECLARATION_KEYWORDS.contains(token)
                });
            }
        }
    }

    debug_assert!(iter.peek().is_none(), "parser stopped before end of input");

    ParsedAst::new(document_id, declarations, comments)
}

fn parse_import_declaration(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedImportDeclaration, ErrorEmitted> {
    if let Some(pub_range) = pub_range {
        let _ = errors.emit(ParseErrorKind::UnexpectedPubKeyword {}, pub_range);
    }
    let mut last_segment = match tokenize_expr::next(iter, comments, errors) {
        Some((LangToken::Identifier(name), segment)) => (name, segment),
        Some((_, range)) => {
            return Err(errors.emit(ParseErrorKind::ExpectedModulePath {}, range));
        }
        None => {
            return Err(errors.emit(ParseErrorKind::ExpectedModulePath {}, iter.eof_range()));
        }
    };
    let mut module_segments: Vec<CheapString> = Vec::new();
    let mut module_path: Option<DocumentRange> = None;
    while parse_helpers::next_if_eq(iter, comments, errors, LangToken::ColonColon).is_some() {
        let segment = match tokenize_expr::next(iter, comments, errors) {
            Some((LangToken::Identifier(name), segment)) => (name, segment),
            Some((_, range)) => {
                return Err(
                    errors.emit(ParseErrorKind::ExpectedIdentifierAfterColonColon {}, range)
                );
            }
            None => {
                return Err(errors.emit(
                    ParseErrorKind::ExpectedIdentifierAfterColonColon {},
                    iter.eof_range(),
                ));
            }
        };
        let (name, range) = last_segment;
        module_segments.push(name);
        module_path = Some(match module_path {
            Some(module_path) => module_path.to(range),
            None => range,
        });
        last_segment = segment;
    }
    let (name, name_range) = last_segment;
    let Some(module_path_range) = module_path else {
        return Err(errors.emit(ParseErrorKind::ImportPathTooShort {}, name_range));
    };
    Ok(ParsedImportDeclaration {
        name: FunctionName::new(name).or_emit(errors, &name_range)?,
        import_range: keyword_range.to(name_range.clone()),
        module_name: ModuleName::new(module_segments).or_emit(errors, &module_path_range)?,
        path_range: module_path_range.to(name_range.clone()),
        name_range,
    })
}

fn parse_record_declaration(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedRecordDeclaration, ErrorEmitted> {
    let (name, name_range) = parse_helpers::expect_identifier(iter, comments, errors)?;
    let left_brace = parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftBrace)?;
    let (fields, braces) = parse_field_declarations(iter, comments, errors, &left_brace)?;
    Ok(ParsedRecordDeclaration {
        name: TypeName::new(name).or_emit(errors, &name_range)?,
        name_range,
        range: pub_range.clone().unwrap_or(keyword_range).to(braces),
        fields,
        pub_range,
    })
}

fn parse_enum_declaration(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedEnumDeclaration, ErrorEmitted> {
    let (name, name_range) = parse_helpers::expect_identifier(iter, comments, errors)?;
    let left_brace = parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftBrace)?;
    let mut seen_names = HashSet::new();
    let (variants, braces) = parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        LangTokenPair::Braces,
        &left_brace,
        &[],
        |iter, comments, errors| {
            let (variant_name, variant_range) =
                parse_helpers::expect_identifier(iter, comments, errors)?;
            if !seen_names.insert(variant_name.clone()) {
                return Err(errors.emit(
                    ParseErrorKind::DuplicateVariant { name: variant_name },
                    variant_range,
                ));
            }
            let fields = parse_helpers::next_if_eq(iter, comments, errors, LangToken::LeftBrace)
                .map(|left_brace| parse_field_declarations(iter, comments, errors, &left_brace))
                .transpose()?
                .map(|(fields, _)| fields);
            Ok(ParsedEnumDeclarationVariant {
                name: TypeName::new(variant_name).or_emit(errors, &variant_range)?,
                name_range: variant_range,
                fields: fields.unwrap_or_default(),
            })
        },
    )?;
    Ok(ParsedEnumDeclaration {
        name: TypeName::new(name).or_emit(errors, &name_range)?,
        name_range,
        range: pub_range.clone().unwrap_or(keyword_range).to(braces),
        variants,
        pub_range,
    })
}

fn parse_field_declarations(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    left_brace: &DocumentRange,
) -> Result<(Vec<ParsedFieldDeclaration>, DocumentRange), ErrorEmitted> {
    let mut seen_names = HashSet::new();
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        LangTokenPair::Braces,
        left_brace,
        &[],
        |iter, comments, errors| {
            let examples =
                parse_helpers::next_if_eq(iter, comments, errors, LangToken::HashBracket)
                    .map(|hash_bracket| {
                        parse_examples_annotation(iter, comments, errors, hash_bracket)
                    })
                    .transpose()?
                    .map(|(examples, _)| examples);
            let (name, name_range) = parse_helpers::expect_identifier(iter, comments, errors)?;
            parse_helpers::expect_token(iter, comments, errors, &LangToken::Colon)?;
            let field_type = parse_type(iter, comments, errors)?;
            if !seen_names.insert(name_range.to_cheap_string()) {
                return Err(errors.emit(
                    ParseErrorKind::DuplicateField {
                        name: name_range.to_cheap_string(),
                    },
                    name_range,
                ));
            }
            Ok(ParsedFieldDeclaration {
                name: FieldName::new(name).or_emit(errors, &name_range)?,
                name_range,
                field_type,
                examples,
            })
        },
    )
}

fn parse_page_declaration(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedPageDeclaration, ErrorEmitted> {
    let header: Result<_, ErrorEmitted> = (|| {
        let name = parse_helpers::expect_identifier(iter, comments, errors)?;
        if let Some((LangToken::LeftBrace, _)) = tokenize_expr::peek(iter) {
            return Ok((name, Vec::new()));
        }
        let left_paren =
            parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftParen)?;
        let items = parse_parameters(iter, comments, errors, &left_paren)?;
        let mut params = Vec::new();
        for item in items {
            match item {
                ParameterItem::Parameter(parameter) => {
                    if let Some(value) = &parameter.default_value {
                        let _ = errors.emit(
                            ParseErrorKind::DefaultValueNotAllowedOnPage {},
                            value.range().clone(),
                        );
                    }
                    params.push(*parameter);
                }
                ParameterItem::Rest { range, .. } => {
                    let _ = errors.emit(ParseErrorKind::RestParamNotAllowedOnPage {}, range);
                }
            }
        }
        Ok((name, params))
    })();
    let left_brace = match &header {
        Ok(_) => parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftBrace),
        Err(reported) => Err(*reported),
    };
    // Synchronize at left brace
    let left_brace = match left_brace {
        Ok(left_brace) => left_brace,
        Err(reported) => {
            parse_helpers::skip_to(iter, reported, |token| {
                matches!(token, LangToken::LeftBrace | LangToken::RightBrace)
                    || parse_helpers::DECLARATION_KEYWORDS.contains(token)
            });
            match parse_helpers::next_if_eq(iter, comments, errors, LangToken::LeftBrace) {
                // Recovery succeeded
                Some(left_brace) => left_brace,
                // No member block to consume, leave the rest to the caller
                None => return Err(reported),
            }
        }
    };

    let mut head: Option<ParsedFunctionDeclaration> = None;
    let mut body: Option<ParsedFunctionDeclaration> = None;
    let mut failed_member: Option<ErrorEmitted> = None;
    let right_brace = loop {
        if let Some(right_brace) =
            parse_helpers::next_if_eq(iter, comments, errors, LangToken::RightBrace)
        {
            break right_brace;
        }
        if let Some(member_pub_range) =
            parse_helpers::next_if_eq(iter, comments, errors, LangToken::Pub)
        {
            let _ = errors.emit(ParseErrorKind::UnexpectedPubKeyword {}, member_pub_range);
        }
        let Some(fn_keyword) = parse_helpers::next_if_eq(iter, comments, errors, LangToken::Fn)
        else {
            let Some((_, range)) = tokenize_expr::peek(iter) else {
                return Err(errors.emit(
                    ParseErrorKind::UnmatchedToken {
                        token: LangToken::LeftBrace,
                    },
                    left_brace,
                ));
            };
            return Err(errors.emit(ParseErrorKind::ExpectedPageMember {}, range));
        };
        // A member that fails to parse has already skipped past its body,
        // so the next member can still be parsed and reported.
        let member = match parse_function_declaration(iter, comments, errors, fn_keyword, None) {
            Ok(member) => member,
            Err(reported) => {
                failed_member = Some(reported);
                continue;
            }
        };
        let member_name = member.name.to_cheap_string();
        let slot = match member.name.as_str() {
            "head" => &mut head,
            "body" => &mut body,
            _ => {
                let _ = errors.emit(
                    ParseErrorKind::UnknownPageMember { name: member_name },
                    member.name_range.clone(),
                );
                continue;
            }
        };
        let parameters_range = match (member.params.first(), &member.rest_param) {
            (Some(param), _) => Some(param.var_name_range.clone()),
            (None, Some((_, range))) => Some(range.clone()),
            (None, None) => None,
        };
        if let Some(parameters_range) = parameters_range {
            let _ = errors.emit(
                ParseErrorKind::PageMemberHasParameters {
                    name: member_name.clone(),
                },
                parameters_range,
            );
        }
        if !matches!(member.return_type, ParsedType::Html { .. }) {
            let _ = errors.emit(
                ParseErrorKind::PageMemberMustReturnHtml {
                    name: member_name.clone(),
                },
                member.return_type.range().clone(),
            );
        }
        if slot.is_some() {
            let _ = errors.emit(
                ParseErrorKind::DuplicatePageMember { name: member_name },
                member.name_range.clone(),
            );
            continue;
        }
        *slot = Some(member);
    };
    let Some(body) = body else {
        return Err(match failed_member {
            Some(reported) => reported,
            None => errors.emit(ParseErrorKind::ExpectedPageBodyBlock {}, right_brace),
        });
    };
    let ((name, name_range), params) = header?;
    Ok(ParsedPageDeclaration {
        name: TypeName::new(name).or_emit(errors, &name_range)?,
        name_range,
        params,
        head,
        body,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(right_brace),
        pub_range,
    })
}

fn parse_function_declaration(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedFunctionDeclaration, ErrorEmitted> {
    let name = parse_helpers::expect_identifier(iter, comments, errors);
    let signature: Result<_, ErrorEmitted> = (|| {
        let (name, name_range) = name.as_ref().map_err(|reported| *reported)?;
        let left_paren =
            parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftParen)?;
        let items = parse_parameters(iter, comments, errors, &left_paren)?;
        let (params, rest_param) = build_function_parameters(items, errors);
        if let Some((LangToken::LeftBrace, _)) = tokenize_expr::peek(iter) {
            return Err(errors.emit(
                ParseErrorKind::FunctionMissingReturnTypeAnnotation { name: name.clone() },
                name_range.clone(),
            ));
        }
        parse_helpers::expect_token(iter, comments, errors, &LangToken::Arrow)?;
        let return_type = parse_type(iter, comments, errors)?;
        Ok((params, rest_param, return_type))
    })();
    let left_brace = match &signature {
        Ok(_) => parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftBrace),
        Err(reported) => Err(*reported),
    };
    // Synchronize at left brace
    let left_brace = match left_brace {
        Ok(left_brace) => left_brace,
        Err(reported) => {
            parse_helpers::skip_to(iter, reported, |token| {
                matches!(token, LangToken::LeftBrace | LangToken::RightBrace)
                    || parse_helpers::DECLARATION_KEYWORDS.contains(token)
            });
            match parse_helpers::next_if_eq(iter, comments, errors, LangToken::LeftBrace) {
                // Recovery succeeded
                Some(left_brace) => left_brace,
                // No body to consume, leave the rest to the caller
                None => return Err(reported),
            }
        }
    };
    // Provide a dedicated error message for empty bodies.
    if parse_helpers::next_if_eq(iter, comments, errors, LangToken::RightBrace).is_some() {
        return Err(match &name {
            Ok((name, name_range)) => errors.emit(
                ParseErrorKind::EmptyFunctionBody { name: name.clone() },
                name_range.clone(),
            ),
            Err(reported) => *reported,
        });
    }
    let (body, braces) = parse_expr::parse_block(iter, comments, errors, &left_brace)?;
    let (name, name_range) = name?;
    let (params, rest_param, return_type) = signature?;
    Ok(ParsedFunctionDeclaration {
        name: FunctionName::new(name).or_emit(errors, &name_range)?,
        name_range,
        params,
        rest_param,
        return_type,
        body,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(braces),
        pub_range,
    })
}

fn build_function_parameters(
    items: Vec<ParameterItem>,
    errors: &mut Vec<ParseError>,
) -> (Vec<ParsedParameter>, Option<(VarName, DocumentRange)>) {
    let mut params = Vec::new();
    let mut rest_param: Option<(VarName, DocumentRange)> = None;
    let count = items.len();
    for (index, item) in items.into_iter().enumerate() {
        match item {
            ParameterItem::Parameter(parameter) => {
                if let Some(examples_range) = &parameter.examples_range {
                    let _ = errors.emit(
                        ParseErrorKind::ExamplesNotAllowedOnFunction {},
                        examples_range.clone(),
                    );
                }
                params.push(*parameter);
            }
            ParameterItem::Rest { var_name, range } => {
                if index + 1 != count {
                    let _ = errors.emit(ParseErrorKind::RestParamMustBeLast {}, range.clone());
                }
                match rest_param {
                    Some(_) => {
                        let _ = errors.emit(ParseErrorKind::DuplicateRestParam {}, range);
                    }
                    None => rest_param = Some((var_name, range)),
                }
            }
        }
    }
    (params, rest_param)
}

/// An item in a parameter list as written. Every declaration's parameter
/// list is parsed the same way; the declaration then rejects the items it
/// does not take.
enum ParameterItem {
    Parameter(Box<ParsedParameter>),
    /// A `...name` rest parameter.
    Rest {
        var_name: VarName,
        range: DocumentRange,
    },
}

/// Parse a parameter list from a `(` the caller has already consumed,
/// through the `)` that closes it.
fn parse_parameters(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    left_paren: &DocumentRange,
) -> Result<Vec<ParameterItem>, ErrorEmitted> {
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        LangTokenPair::Parens,
        left_paren,
        &[LangToken::LeftBrace, LangToken::Arrow],
        |iter, comments, errors| {
            if let Some(dots_range) =
                parse_helpers::next_if_eq(iter, comments, errors, LangToken::DotDotDot)
            {
                let (var_name, var_name_range) =
                    parse_helpers::expect_identifier(iter, comments, errors)?;
                return Ok(ParameterItem::Rest {
                    var_name: VarName::new(var_name).or_emit(errors, &var_name_range)?,
                    range: dots_range.to(var_name_range),
                });
            }
            let (examples, examples_range) =
                parse_helpers::next_if_eq(iter, comments, errors, LangToken::HashBracket)
                    .map(|hash_bracket| {
                        parse_examples_annotation(iter, comments, errors, hash_bracket)
                    })
                    .transpose()?
                    .unzip();
            let (var_name, var_name_range) =
                parse_helpers::expect_identifier(iter, comments, errors)?;
            parse_helpers::expect_token(iter, comments, errors, &LangToken::Colon)?;
            let var_type = parse_type(iter, comments, errors)?;
            let default_value =
                if parse_helpers::next_if_eq(iter, comments, errors, LangToken::Assign).is_some() {
                    Some(parse_expr::parse_expr(iter, comments, errors)?)
                } else {
                    None
                };
            Ok(ParameterItem::Parameter(Box::new(ParsedParameter {
                var_name: VarName::new(var_name).or_emit(errors, &var_name_range)?,
                var_name_range,
                var_type,
                default_value,
                examples,
                examples_range,
            })))
        },
    )
    .map(|(items, _)| items)
}

/// Parse an `#[examples(...)]` annotation from the `#[` the caller has
/// already consumed. Returns the annotation with the range from `#[`
/// through `]`.
fn parse_examples_annotation(
    iter: &mut DocumentCursor,
    comments: &mut Vec<DocumentRange>,
    errors: &mut Vec<ParseError>,
    hash_bracket: DocumentRange,
) -> Result<(ExamplesAnnotation, DocumentRange), ErrorEmitted> {
    parse_helpers::parse_delimited(
        iter,
        comments,
        errors,
        LangTokenPair::HashBrackets,
        &hash_bracket,
        |iter, comments, errors| {
            let Some((name, name_range)) =
                parse_helpers::next_if_map(iter, comments, errors, LangToken::identifier)
            else {
                return Err(match tokenize_expr::peek(iter) {
                    Some((token, range)) => {
                        errors.emit(ParseErrorKind::UnexpectedToken { token }, range)
                    }
                    None => errors.emit(ParseErrorKind::UnexpectedEof {}, iter.eof_range()),
                });
            };
            if name.as_str() != "examples" {
                let reported = errors.emit(ParseErrorKind::UnknownAnnotation { name }, name_range);
                // Skip the whole annotation, whatever it holds, so that the
                // caller's skip stops at the `]` rather than at a `)` inside.
                parse_helpers::skip_to(iter, reported, |token| {
                    *token == LangToken::RightBracket
                        || parse_helpers::DECLARATION_KEYWORDS.contains(token)
                });
                return Err(reported);
            }
            let left_paren =
                parse_helpers::expect_token(iter, comments, errors, &LangToken::LeftParen)?;
            let mut annotation = ExamplesAnnotation::default();
            parse_helpers::parse_delimited_list(
                iter,
                comments,
                errors,
                LangTokenPair::Parens,
                &left_paren,
                &[],
                |iter, comments, errors| {
                    let Some((key, key_range)) =
                        parse_helpers::next_if_map(iter, comments, errors, LangToken::identifier)
                    else {
                        return Err(match tokenize_expr::peek(iter) {
                            Some((token, range)) => {
                                errors.emit(ParseErrorKind::UnexpectedToken { token }, range)
                            }
                            None => errors.emit(ParseErrorKind::UnexpectedEof {}, iter.eof_range()),
                        });
                    };
                    parse_helpers::expect_token(iter, comments, errors, &LangToken::Assign)?;
                    if key.as_str() == "pattern" {
                        let Some((value, _)) = parse_helpers::next_if_map(
                            iter,
                            comments,
                            errors,
                            |token| match token {
                                LangToken::StringLiteral(value) => Some(value),
                                _ => None,
                            },
                        ) else {
                            return Err(match tokenize_expr::peek(iter) {
                                Some((actual, range)) => errors.emit(
                                    ParseErrorKind::ExpectedStringLiteralButGot { actual },
                                    range,
                                ),
                                None => {
                                    errors.emit(ParseErrorKind::UnexpectedEof {}, iter.eof_range())
                                }
                            });
                        };
                        annotation.pattern = Some(value);
                        return Ok(());
                    }
                    let slot = match key.as_str() {
                        "min" => &mut annotation.min,
                        "max" => &mut annotation.max,
                        "min_len" => &mut annotation.min_len,
                        "max_len" => &mut annotation.max_len,
                        _ => {
                            return Err(errors.emit(
                                ParseErrorKind::UnknownExamplesKey { name: key },
                                key_range,
                            ));
                        }
                    };
                    let negative =
                        parse_helpers::next_if_eq(iter, comments, errors, LangToken::Minus)
                            .is_some();
                    let Some((value, _)) =
                        parse_helpers::next_if_map(iter, comments, errors, |token| match token {
                            LangToken::IntLiteral(value) => Some(value),
                            _ => None,
                        })
                    else {
                        return Err(match tokenize_expr::peek(iter) {
                            Some((actual, range)) => errors
                                .emit(ParseErrorKind::ExpectedIntLiteralButGot { actual }, range),
                            None => errors.emit(ParseErrorKind::UnexpectedEof {}, iter.eof_range()),
                        });
                    };
                    *slot = Some(if negative { -value } else { value });
                    Ok(())
                },
            )?;
            Ok(annotation)
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use crate::hop::parsing::source_generator;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn accept(input: &str, expected: Expect) {
        let mut errors = Vec::new();
        let document_id = DocumentId::new("test.hop").unwrap();
        let module = parse(
            document_id.clone(),
            Document::new(document_id, input.to_string()),
            &mut errors,
        );
        if !errors.is_empty() {
            let rendered = DocumentAnnotator::new()
                .with_severity_label()
                .with_lines_before(1)
                .annotate(errors.iter().map(|e| e.to_diagnostic()))
                .render();
            panic!("expected no parse errors, got:\n{rendered}");
        }
        expected.assert_eq(&module.to_string());
    }

    fn reject(input: &str, expected: Expect) {
        let mut errors = Vec::new();
        let document_id = DocumentId::new("test.hop").unwrap();
        let module = parse(
            document_id.clone(),
            Document::new(document_id, input.to_string()),
            &mut errors,
        );
        if errors.is_empty() {
            panic!("expected parse errors but got none");
        }
        let rendered = DocumentAnnotator::new()
            .with_severity_label()
            .with_lines_before(1)
            .annotate(errors.iter().map(|e| e.to_diagnostic()))
            .render();
        let actual = format!("-- errors --\n{rendered}-- ast --\n{module}");
        expected.assert_eq(&actual);
    }

    #[test]
    fn accepts_empty_file() {
        accept("", expect![[""]]);
    }

    #[test]
    fn rejects_function_without_return_type_and_body() {
        reject(
            indoc! {r#"
              fn f() {
              }
            "#},
            expect![[r#"
                -- errors --
                error: Function 'f' is missing a return type annotation
                1 | fn f() {
                  |    ^

                error: Function 'f' has an empty body: a function body must be a single expression
                1 | fn f() {
                  |    ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_as_match_subject() {
        accept(
            indoc! {r#"
              fn f() -> Int {
                match Color::Red {
                  Color::Red => 1,
                }
              }
            "#},
            expect![[r#"
                fn f() -> Int {
                  match Color::Red {Color::Red => 1}
                }
            "#]],
        );
    }

    #[test]
    fn accepts_enum_literal_as_for_source() {
        accept(
            indoc! {r#"
              fn f() -> Int {
                for x in Color::Red {
                  x
                }
              }
            "#},
            expect![[r#"
                fn f() -> Int {
                  for x in Color::Red { x }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_braced_enum_literal_as_match_subject() {
        reject(
            indoc! {r#"
              fn f() -> Int {
                match Point::XY { x: 1, y: 2 } {
                  _ => 1,
                }
              }
            "#},
            expect![[r#"
                -- errors --
                error: A record or enum literal is not allowed here: surround it with parentheses
                1 | fn f() -> Int {
                2 |   match Point::XY { x: 1, y: 2 } {
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^
                -- ast --
                fn f() -> Int {
                  match Point::XY {x: 1, y: 2} {_ => 1}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_with_spread_as_for_source() {
        reject(
            indoc! {r#"
              fn f(p: Point) -> Int {
                for x in Point { ...p } {
                  x
                }
              }
            "#},
            expect![[r#"
                -- errors --
                error: A record or enum literal is not allowed here: surround it with parentheses
                1 | fn f(p: Point) -> Int {
                2 |   for x in Point { ...p } {
                  |            ^^^^^^^^^^^^^^
                -- ast --
                fn f(p: Point) -> Int {
                  for x in Point {...p} { x }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_record_literal_before_postfix_in_match_subject() {
        reject(
            indoc! {r#"
              fn f() -> Int {
                match Point { x: 1 }.x {
                  _ => 1,
                }
              }
            "#},
            expect![[r#"
                -- errors --
                error: A record or enum literal is not allowed here: surround it with parentheses
                1 | fn f() -> Int {
                2 |   match Point { x: 1 }.x {
                  |         ^^^^^^^^^^^^^^
                -- ast --
                fn f() -> Int {
                  match Point {x: 1}.x {_ => 1}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_each_record_literal_in_for_range_bounds() {
        reject(
            indoc! {"
                fn f() -> Int {
                  for x in Point { x: 1 }.x..=Point { x: 2 }.x { 1 }
                }
            "},
            expect![[r#"
                -- errors --
                error: A record or enum literal is not allowed here: surround it with parentheses
                1 | fn f() -> Int {
                2 |   for x in Point { x: 1 }.x..=Point { x: 2 }.x { 1 }
                  |            ^^^^^^^^^^^^^^

                error: A record or enum literal is not allowed here: surround it with parentheses
                1 | fn f() -> Int {
                2 |   for x in Point { x: 1 }.x..=Point { x: 2 }.x { 1 }
                  |                               ^^^^^^^^^^^^^^
                -- ast --
                fn f() -> Int {
                  for x in Point {x: 1}.x..=Point {
                    x: 2,
                  }.x { 1 }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parenthesized_record_literal_as_match_subject() {
        accept(
            indoc! {r#"
              fn f() -> Int {
                match (Point { x: 1 }) {
                  _ => 1,
                }
              }
            "#},
            expect![[r#"
                fn f() -> Int {
                  match Point {x: 1} {_ => 1}
                }
            "#]],
        );
    }

    #[test]
    fn accepts_record_literal_inside_call_in_match_subject() {
        accept(
            indoc! {r#"
              fn f() -> Int {
                match g(Point { x: 1 }, [Color::Red { a: 1 }]) {
                  _ => 1,
                }
              }
            "#},
            expect![[r#"
                fn f() -> Int {
                  match g(
                    Point {x: 1},
                    [Color::Red {a: 1}],
                  ) {_ => 1}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_bare_type_name_as_match_subject() {
        reject(
            indoc! {r#"
                fn f() -> Int {
                  match Point {
                    Point => 1,
                  }
                }
            "#},
            expect![[r#"
                -- errors --
                error: A record or enum literal is not allowed here: surround it with parentheses
                1 | fn f() -> Int {
                2 |   match Point {
                  |         ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unexpected_token_inside_nested_record_literals() {
        reject(
            "fn f() -> Int { A { a: B { a: C { a: D { a: ] } } } } }",
            expect![[r#"
                -- errors --
                error: Unexpected token ']'
                1 | fn f() -> Int { A { a: B { a: C { a: D { a: ] } } } } }
                  |                                             ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_pub_on_record() {
        accept(
            indoc! {"
                pub record User {
                  name: String,
                }
            "},
            expect![[r#"
                pub record User {
                  name: String,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_pub_on_enum() {
        accept(
            indoc! {"
                pub enum Status {
                  Active,
                  Inactive,
                }
            "},
            expect![[r#"
                pub enum Status {
                  Active,
                  Inactive,
                }
            "#]],
        );
    }

    #[test]
    fn accepts_children_round_trip() {
        accept(
            indoc! {"
                fn Card(children: Html) -> Html {
                  <div>{children}</div>
                }
            "},
            expect![[r#"
                fn Card(children: Html) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(children)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_in_function_body() {
        accept(
            indoc! {"
                fn Greeting(first: String, last: String) -> Html {
                  let name = first + \" \" + last;
                  <h1>{name}</h1>
                }
            "},
            expect![[r#"
                fn Greeting(first: String, last: String) -> Html {
                  {
                    let name = first + " " + last;
                    html(
                      tag: "h1",
                      attrs: [],
                      children: [interpolate(name)],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_in_interpolation_and_attribute() {
        accept(
            indoc! {"
                fn Main(a: Int) -> Html {
                  <div class={ let base = \"btn\"; base }>{ let b = a + 1; b }</div>
                }
            "},
            expect![[r#"
                fn Main(a: Int) -> Html {
                  html(
                    tag: "div",
                    attrs: [
                      class: {
                        let base = "btn";
                        base
                      },
                    ],
                    children: [
                      interpolate(
                        {
                          let b = a + 1;
                          b
                        },
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_as_match_subject() {
        accept(
            indoc! {"
                fn Main(a: Int) -> Html {
                  match { let n = a; n == 1 } {
                    true => {
                      match { let m = a; m == 1 } {
                        true => <>one</>,
                        false => <></>,
                      }
                    },
                    false => <></>,
                  }
                }
            "},
            expect![[r#"
                fn Main(a: Int) -> Html {
                  match {
                    let n = a;
                    n == 1
                  } {
                    true => match {
                      let m = a;
                      m == 1
                    } {
                      true => fragment(text("one")),
                      false => fragment(),
                    },
                    false => fragment(),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_function_body_ending_in_let() {
        reject(
            indoc! {"
                fn Main() -> Int {
                  let a = 1;
                }
            "},
            expect![[r#"
                -- errors --
                error: A block must end with an expression
                1 | fn Main() -> Int {
                2 |   let a = 1;
                  |            ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_pub_on_function() {
        accept(
            indoc! {"
                pub fn Button(label: String) -> Html {
                  <button>{label}</button>
                }
            "},
            expect![[r#"
                pub fn Button(label: String) -> Html {
                  html(
                    tag: "button",
                    attrs: [],
                    children: [interpolate(label)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_pub_on_page() {
        accept(
            indoc! {"
                pub page Home() {
                  fn body() -> Html {
                    <div>hi</div>
                  }
                }
            "},
            expect![[r#"
                pub page Home() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("hi")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_pub_on_import() {
        reject(
            indoc! {"
                pub import other::Foo

                fn Main() -> Html {
                  <Foo/>
                }
            "},
            expect![[r#"
                -- errors --
                error: 'pub' is not allowed here
                1 | pub import other::Foo
                  | ^^^
                -- ast --
                import other::Foo

                fn Main() -> Html {
                  Foo(attrs: [])
                }
            "#]],
        );
    }

    #[test]
    fn rejects_pub_at_end_of_file() {
        reject(
            "pub",
            expect![[r#"
                -- errors --
                error: 'pub' is not allowed here
                1 | pub
                  | ^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_comment_between_functions() {
        accept(
            indoc! {"
                fn First() -> Html {<></>}
                // This is a comment
                fn Second() -> Html {<></>}
            "},
            expect![[r#"
                fn First() -> Html {
                  fragment()
                }

                fn Second() -> Html {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_lexer_error_at_end_of_file() {
        reject(
            indoc! {"
                record A {}
                // trailing
                #
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '#'
                2 | // trailing
                3 | #
                  | ^
                -- ast --
                record A {}
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_string_at_end_of_file() {
        reject(
            indoc! {"
                record A {}
                \"abc
            "},
            expect![[r#"
                -- errors --
                error: Unterminated string literal
                1 | record A {}
                2 | "abc
                  | ^^^^
                -- ast --
                record A {}
            "#]],
        );
    }

    #[test]
    fn accepts_keyword_function_syntax() {
        accept(
            indoc! {"
                fn Foo() -> Html {
                  <div>hello</div>
                }
            "},
            expect![[r#"
                fn Foo() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("hello")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_keyword_function_with_params() {
        accept(
            indoc! {"
                fn Foo(name: String, count: Int) -> Html {
                  <div>{name}</div>
                }
            "},
            expect![[r#"
                fn Foo(name: String, count: Int) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(name)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_keyword_function_with_trailing_comment() {
        accept(
            indoc! {r#"
                fn Button(
                  // The button label
                  label: String,
                  // Whether the button is disabled
                  disabled: Bool = false,
                  // More params to come
                ) -> Html {
                  <>{label}</>
                }
            "#},
            expect![[r#"
                fn Button(label: String, disabled: Bool = false) -> Html {
                  fragment(interpolate(label))
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_for_loops() {
        accept(
            indoc! {"
                record T {
                  t: Array[String],
                }
                record S {
                  s: T,
                }
                fn Main(i: Array[S]) -> Html {
                    <>
                        {for j in i {
                          for k in j.s.t {
                            match k {
                              true => <></>,
                              false => <></>,
                            }
                          }
                        }}
                        {for p in i {
                          for k in p.s.t {
                            for item in k { <></> }
                          }
                        }}
                    </>
                }
            "},
            expect![[r#"
                record T {
                  t: Array[String],
                }

                record S {
                  s: T,
                }

                fn Main(i: Array[S]) -> Html {
                  fragment(
                    interpolate(
                      for j in i {
                        for k in j.s.t {
                          match k {
                            true => fragment(),
                            false => fragment(),
                          },
                        },
                      },
                    ),
                    interpolate(
                      for p in i {
                        for k in p.s.t {
                          for item in k { fragment() },
                        },
                      },
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_form_with_inputs() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <form id="form">
                        <input type="text" required>
                        <button type="submit">Send</button>
                    </form>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "form",
                    attrs: [id: "form"],
                    children: [
                      html(
                        tag: "input",
                        attrs: [type: "text", required],
                      ),
                      html(
                        tag: "button",
                        attrs: [type: "submit"],
                        children: [text("Send")],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_fragment_with_several_children() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    <><p>one</p><p>two</p></>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  fragment(
                    html(
                      tag: "p",
                      attrs: [],
                      children: [text("one")],
                    ),
                    html(
                      tag: "p",
                      attrs: [],
                      children: [text("two")],
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_empty_fragment() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    <></>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_fragments() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    <><>one</>two</>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  fragment(
                    fragment(text("one")),
                    text("two"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_fragment_inside_an_element() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    <div><>one</></div>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [fragment(text("one"))],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_whitespace_in_a_closing_fragment_tag() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    <>one</ >
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  fragment(text("one"))
                }
            "#]],
        );
    }

    #[test]
    fn reads_a_fragment_in_raw_text_as_text() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <script><></script>
                }
            "},
            expect![[r#"
                -- errors --
                error: Inline <script> content is not allowed: move the code to a file and reference it with <script src="...">
                1 | fn Main() -> Html {
                2 |     <script><></script>
                  |             ^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "script",
                    attrs: [],
                    children: [text("<>")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_script_referencing_an_external_file() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <script src="/app.js"></script>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "script",
                    attrs: [src: "/app.js"],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_whitespace_between_script_tags() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <script src="/app.js">
                    </script>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "script",
                    attrs: [src: "/app.js"],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_inline_script_content() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <script>alert(1)</script>
                }
            "},
            expect![[r#"
                -- errors --
                error: Inline <script> content is not allowed: move the code to a file and reference it with <script src="...">
                1 | fn Main() -> Html {
                2 |     <script>alert(1)</script>
                  |             ^^^^^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "script",
                    attrs: [],
                    children: [text("alert(1)")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_style_element() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <style>.a { color: red; }</style>
                }
            "},
            expect![[r#"
                -- errors --
                error: <style> elements are not allowed: put the CSS in the project stylesheet, or reference it with <link rel="stylesheet">
                1 | fn Main() -> Html {
                2 |     <style>.a { color: red; }</style>
                  |      ^^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "style",
                    attrs: [],
                    children: [
                      text(".a { color: red; }"),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_empty_style_element() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <style></style>
                }
            "},
            expect![[r#"
                -- errors --
                error: <style> elements are not allowed: put the CSS in the project stylesheet, or reference it with <link rel="stylesheet">
                1 | fn Main() -> Html {
                2 |     <style></style>
                  |      ^^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "style",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_self_closing_style_element() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <style />
                }
            "},
            expect![[r#"
                -- errors --
                error: <style> elements are not allowed: put the CSS in the project stylesheet, or reference it with <link rel="stylesheet">
                1 | fn Main() -> Html {
                2 |     <style />
                  |      ^^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "style",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_tags_are_not_closed() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <div>
                    <p>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | fn Main() -> Html {
                2 |     <div>
                  |      ^^^

                error: Unclosed <p>
                2 |     <div>
                3 |     <p>
                  |      ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      html(
                        tag: "p",
                        attrs: [],
                        children: [],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn recovers_at_the_next_declaration_after_a_broken_one() {
        reject(
            indoc! {r#"
                record User {
                  name: String
                  age: Int,
                }
                enum Color { Red, Green }
                fn double(x: Int) -> Int { x }
                fn Card(title: String) -> Html {
                  <div>{title}</div>
                }
                page Home() {
                  fn body() -> Html {
                    <Card title="hi"/>
                  }
                }
            "#},
            expect![[r#"
                -- errors --
                error: Expected token ',' but got 'age'
                 2 |   name: String
                 3 |   age: Int,
                   |   ^^^
                -- ast --
                record User {
                  name: String,
                }

                enum Color {
                  Red,
                  Green,
                }

                fn double(x: Int) -> Int {
                  x
                }

                fn Card(title: String) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }

                page Home() {
                  fn body() -> Html {
                    Card(attrs: [title: "hi"])
                  }
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_broken_expression_inside_a_body() {
        reject(
            indoc! {"
                fn First() -> Html {
                  <div>{1 +}</div>
                }
                fn Second() -> Html {
                  <></>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected token '}'
                1 | fn First() -> Html {
                2 |   <div>{1 +}</div>
                  |            ^
                -- ast --
                fn First() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }

                fn Second() -> Html {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_a_declaration_cut_short() {
        reject(
            indoc! {"
                fn broken(x: Int
                fn Whole() -> Html {
                  <></>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ')' but got 'fn'
                1 | fn broken(x: Int
                2 | fn Whole() -> Html {
                  | ^^
                -- ast --
                fn Whole() -> Html {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_closing_tag_closes_an_outer_tag() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <div><span></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <span>
                1 | fn Main() -> Html {
                2 |     <div><span></div>
                  |           ^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      html(
                        tag: "span",
                        attrs: [],
                        children: [],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_closing_tag_closes_past_several_open_tags() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <div><span><><b></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <span>
                1 | fn Main() -> Html {
                2 |     <div><span><><b></div>
                  |           ^^^^

                error: Unclosed <>
                1 | fn Main() -> Html {
                2 |     <div><span><><b></div>
                  |                ^^

                error: Unclosed <b>
                1 | fn Main() -> Html {
                2 |     <div><span><><b></div>
                  |                   ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      html(
                        tag: "span",
                        attrs: [],
                        children: [
                          fragment(
                            html(
                              tag: "b",
                              attrs: [],
                              children: [],
                            ),
                          ),
                        ],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_closing_tag_for_a_tag_that_was_never_opened() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <div></p></></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unmatched </p>
                1 | fn Main() -> Html {
                2 |     <div></p></></div>
                  |          ^^^^

                error: Unmatched </>
                1 | fn Main() -> Html {
                2 |     <div></p></></div>
                  |              ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_closing_tag_once_the_tag_it_names_is_already_closed() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <div></div></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                1 | fn Main() -> Html {
                2 |     <div></div></div>
                  |                 ^

                error: Unexpected token '}'
                2 |     <div></div></div>
                3 | }
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_an_empty_body() {
        reject(
            indoc! {"
                fn Main() -> Html {
                }
            "},
            expect![[r#"
                -- errors --
                error: Function 'Main' has an empty body: a function body must be a single expression
                1 | fn Main() -> Html {
                  |    ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_several_roots_in_a_function_body() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <p>one</p>
                    <p>two</p>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                2 |     <p>one</p>
                3 |     <p>two</p>
                  |            ^

                error: Unexpected token '}'
                3 |     <p>two</p>
                4 | }
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_several_roots_in_a_page_body() {
        reject(
            indoc! {"
                page Main() {
                  fn body() -> Html {
                      <p>one</p>
                      <p>two</p>
                  }
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                3 |       <p>one</p>
                4 |       <p>two</p>
                  |              ^

                error: Unexpected token '}'
                4 |       <p>two</p>
                5 |   }
                  |   ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_several_roots_in_a_page_head_and_body() {
        reject(
            indoc! {"
                page Main {
                    fn head() -> Html {
                        <title>one</title>
                        <meta charset=\"utf-8\"/>
                    }
                    fn body() -> Html {
                        <p>one</p>
                        <p>two</p>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got 'charset'
                 3 |         <title>one</title>
                 4 |         <meta charset="utf-8"/>
                   |               ^^^^^^^

                error: Unexpected character: '/'
                 7 |         <p>one</p>
                 8 |         <p>two</p>
                   |                ^

                error: Unexpected token '}'
                 8 |         <p>two</p>
                 9 |     }
                   |     ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_text_beside_an_expression_inside_a_fragment() {
        accept(
            indoc! {"
                fn Greeting(name: String) -> Html {
                    <>Hello, {name}!</>
                }
            "},
            expect![[r#"
                fn Greeting(name: String) -> Html {
                  fragment(
                    text("Hello, "),
                    interpolate(name),
                    text("!"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_fragment_is_not_closed() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <>one
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <>
                1 | fn Main() -> Html {
                2 |     <>one
                  |     ^^
                -- ast --
                fn Main() -> Html {
                  fragment(text("one"))
                }
            "#]],
        );
    }

    #[test]
    fn rejects_closing_fragment_for_a_fragment_that_was_never_opened() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <div></></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unmatched </>
                1 | fn Main() -> Html {
                2 |     <div></></div>
                  |          ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_closing_fragment_closes_an_outer_tag() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <><div></>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | fn Main() -> Html {
                2 |     <><div></>
                  |        ^^^
                -- ast --
                fn Main() -> Html {
                  fragment(
                    html(
                      tag: "div",
                      attrs: [],
                      children: [],
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_left_angle_that_does_not_open_a_fragment() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    < >
                }
            "},
            expect![[r#"
                -- errors --
                error: Unterminated tag start
                1 | fn Main() -> Html {
                2 |     < >
                  |     ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_void_tag_is_closed_with_closing_tag() {
        reject(
            indoc! {"
                fn Main() -> Html {
                    <>
                        <hr></hr>
                        <br></br>
                        <input></input>
                    </>
                }
            "},
            expect![[r#"
                -- errors --
                error: <hr> should not be closed using a closing tag
                2 |     <>
                3 |         <hr></hr>
                  |             ^^^^^

                error: <br> should not be closed using a closing tag
                3 |         <hr></hr>
                4 |         <br></br>
                  |             ^^^^^

                error: <input> should not be closed using a closing tag
                4 |         <br></br>
                5 |         <input></input>
                  |                ^^^^^^^^
                -- ast --
                fn Main() -> Html {
                  fragment(
                    html(tag: "hr", attrs: []),
                    html(tag: "br", attrs: []),
                    html(tag: "input", attrs: []),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_void_tags_to_be_self_closing() {
        accept(
            indoc! {r#"
                import bar::Bar
                fn Main() -> Html {
                    <>
                        <hr/>
                        <br/>
                        <input/>
                    </>
                }
                fn Foo() -> Html {
                    <></>
                }
            "#},
            expect![[r#"
                import bar::Bar

                fn Main() -> Html {
                  fragment(
                    html(tag: "hr", attrs: []),
                    html(tag: "br", attrs: []),
                    html(tag: "input", attrs: []),
                  )
                }

                fn Foo() -> Html {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unquoted_attribute_value() {
        reject(
            "fn Main() -> Html {<div class=foo></div>}",
            expect![[r#"
                -- errors --
                error: Expected quoted attribute value or expression
                1 | fn Main() -> Html {<div class=foo></div>}
                  |                         ^^^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [foo],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_single_quoted_attribute_value() {
        reject(
            "fn Main() -> Html {<input type='number'/>}",
            expect![[r#"
                -- errors --
                error: Single-quoted attribute values are not supported: use double quotes
                1 | fn Main() -> Html {<input type='number'/>}
                  |                                ^^^^^^^^
                -- ast --
                fn Main() -> Html {
                  html(tag: "input", attrs: [])
                }
            "#]],
        );
    }

    #[test]
    fn rejects_invalid_markup_declaration() {
        reject(
            "fn Main() -> Html {<!foo>}",
            expect![[r#"
                -- errors --
                error: Invalid markup declaration
                1 | fn Main() -> Html {<!foo>}
                  |                    ^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_comment() {
        reject(
            "fn Main() -> Html {<!--",
            expect![[r#"
                -- errors --
                error: Unterminated comment
                1 | fn Main() -> Html {<!--
                  |                    ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_opening_tag() {
        reject(
            "fn Main() -> Html {<div <div>}",
            expect![[r#"
                -- errors --
                error: Unterminated opening tag
                1 | fn Main() -> Html {<div <div>}
                  |                     ^^^

                error: Unclosed <div>
                1 | fn Main() -> Html {<div <div>}
                  |                     ^^^

                error: Unclosed <div>
                1 | fn Main() -> Html {<div <div>}
                  |                          ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      html(
                        tag: "div",
                        attrs: [],
                        children: [],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_closing_tag() {
        reject(
            "fn Main() -> Html {<div></div }",
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | fn Main() -> Html {<div></div }
                  |                     ^^^

                error: Unterminated closing tag
                1 | fn Main() -> Html {<div></div }
                  |                           ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_duplicate_attribute() {
        reject(
            r#"fn Main() -> Html {<div class="foo" class="bar"></div>}"#,
            expect![[r#"
                -- errors --
                error: Duplicate attribute 'class'
                1 | fn Main() -> Html {<div class="foo" class="bar"></div>}
                  |                                     ^^^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [class: "foo"],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_spread_without_a_name() {
        reject(
            "fn Main() -> Html {<div ...>text</div>}",
            expect![[r#"
                -- errors --
                error: Missing variable name for spread
                1 | fn Main() -> Html {<div ...>text</div>}
                  |                         ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("text")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_tag_start() {
        reject(
            "fn Main() -> Html {< div>}",
            expect![[r#"
                -- errors --
                error: Unterminated tag start
                1 | fn Main() -> Html {< div>}
                  |                    ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_doctype_tags_inside_functions() {
        reject(
            indoc! {"
                fn Main(foo: String) -> Html {
                    <!DOCTYPE html>
                    <html>
                        <body>
                            <div>hello world</div>
                        </body>
                    </html>
                }
            "},
            expect![[r#"
                -- errors --
                error: <!doctype> declarations are not allowed: one is inserted automatically
                1 | fn Main(foo: String) -> Html {
                2 |     <!DOCTYPE html>
                  |     ^^^^^^^^^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_function_parameter_has_parse_error_in_type_name() {
        reject(
            indoc! {"
                fn Main(data: Array[) -> Html {
                    <div>{data}</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected type name but got ')'
                1 | fn Main(data: Array[) -> Html {
                  |                     ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(data)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_field_access_on_record() {
        accept(
            indoc! {r#"
                record User {
                  url: String,
                  theme: String,
                }
                fn Main(user: User) -> Html {
                    <a href={user.url} class={user.theme}>Link</a>
                }
            "#},
            expect![[r#"
                record User {
                  url: String,
                  theme: String,
                }

                fn Main(user: User) -> Html {
                  html(
                    tag: "a",
                    attrs: [
                      href: user.url,
                      class: user.theme,
                    ],
                    children: [text("Link")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_record_with_invalid_type_in_declaration() {
        reject(
            indoc! {r#"
                record User {
                  url: x,
                  theme: String,
                }
            "#},
            expect![[r#"
                -- errors --
                error: Type name must start with an uppercase letter
                1 | record User {
                2 |   url: x,
                  |        ^
                -- ast --
                record User {
                  theme: String,
                }
            "#]],
        );
    }

    #[test]
    fn rejects_multiple_expressions_in_attribute() {
        reject(
            indoc! {r#"
                fn Main(style1: String, style2: String, style3: String) -> Html {
                    <div class={style1, style2, style3}>Content</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got ','
                1 | fn Main(style1: String, style2: String, style3: String) -> Html {
                2 |     <div class={style1, style2, style3}>Content</div>
                  |                       ^
                -- ast --
                fn Main(style1: String, style2: String, style3: String) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("Content")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_import_has_only_one_segment() {
        reject(
            indoc! {r#"
                import Foo

                fn Main() -> Html {
                	<Foo/>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Import path must have at least two segments: module::Name
                1 | import Foo
                  |        ^^^
                -- ast --
                fn Main() -> Html {
                  Foo(attrs: [])
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_invocations() {
        accept(
            indoc! {"
                fn Main(p: String) -> Html {
                    <>
                        <Foo/>
                        <Foo/>
                    </>
                }
            "},
            expect![[r#"
                fn Main(p: String) -> Html {
                  fragment(
                    Foo(attrs: []),
                    Foo(attrs: []),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_invocations_with_params() {
        accept(
            indoc! {r#"
                import foo::Foo
                import bar::Bar
                record Data {
                  user: String,
                }
                fn Main(data: Data) -> Html {
                    <>
                        <Foo a={data}/>
                        <Bar b={data.user}/>
                    </>
                }
            "#},
            expect![[r#"
                import foo::Foo
                import bar::Bar

                record Data {
                  user: String,
                }

                fn Main(data: Data) -> Html {
                  fragment(
                    Foo(attrs: [a: data]),
                    Bar(attrs: [b: data.user]),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop() {
        accept(
            indoc! {"
                fn Main(item: Array[String]) -> Html {
                    for item in items { <div>Item content</div> }
                }
            "},
            expect![[r#"
                fn Main(item: Array[String]) -> Html {
                  for item in items {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Item content")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_text_expression() {
        accept(
            indoc! {"
                fn Main(foo: Array[String]) -> Html {
                    for v in foo { <div>{v}</div> }
                }
            "},
            expect![[r#"
                fn Main(foo: Array[String]) -> Html {
                  for v in foo {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(v)],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_inclusive_range() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    for i in 0..=5 { <>{i}</> }
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  for i in 0..=5 {
                    fragment(interpolate(i)),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_variable_range_bounds() {
        accept(
            indoc! {"
                fn Main(start: Int, end: Int) -> Html {
                    for x in start..=end { <>{x}</> }
                }
            "},
            expect![[r#"
                fn Main(start: Int, end: Int) -> Html {
                  for x in start..=end {
                    fragment(interpolate(x)),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_expression_range_bounds() {
        accept(
            indoc! {"
                fn Main(count: Int) -> Html {
                    for i in 1..=count + 1 { <>{i}</> }
                }
            "},
            expect![[r#"
                fn Main(count: Int) -> Html {
                  for i in 1..=count + 1 {
                    fragment(interpolate(i)),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_underscore_binding() {
        accept(
            indoc! {"
                fn Main(items: Array[String]) -> Html {
                    for _ in items { <>item</> }
                }
            "},
            expect![[r#"
                fn Main(items: Array[String]) -> Html {
                  for _ in items {
                    fragment(text("item")),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_underscore_and_range() {
        accept(
            indoc! {"
                fn Main() -> Html {
                    for _ in 0..=5 { <>item</> }
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  for _ in 0..=5 {
                    fragment(text("item")),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_underscore_and_variable_range() {
        accept(
            indoc! {"
                fn Main(start: Int, end: Int) -> Html {
                    for _ in start..=end { <>item</> }
                }
            "},
            expect![[r#"
                fn Main(start: Int, end: Int) -> Html {
                  for _ in start..=end {
                    fragment(text("item")),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_as_function_body() {
        accept(
            indoc! {"
                fn Dots(n: Int) -> Html {
                    for _ in 1..=n {
                        <span>.</span>
                    }
                }
            "},
            expect![[r#"
                fn Dots(n: Int) -> Html {
                  for _ in 1..=n {
                    html(
                      tag: "span",
                      attrs: [],
                      children: [text(".")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_in_interpolation() {
        accept(
            indoc! {"
                fn ItemList(items: Array[Item]) -> Html {
                    <ul>
                        {for item in items {
                            let name = item.name;
                            <li>{name}</li>
                        }}
                    </ul>
                }
            "},
            expect![[r#"
                fn ItemList(items: Array[Item]) -> Html {
                  html(
                    tag: "ul",
                    attrs: [],
                    children: [
                      interpolate(
                        for item in items {
                          let name = item.name;
                          html(
                            tag: "li",
                            attrs: [],
                            children: [
                              interpolate(name),
                            ],
                          )
                        },
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_expression_in_attribute() {
        accept(
            indoc! {"
                fn Main(items: Array[Item]) -> Html {
                    <Table rows={for item in items { <tr>{item.name}</tr> }}/>
                }
            "},
            expect![[r#"
                fn Main(items: Array[Item]) -> Html {
                  Table(
                    attrs: [
                      rows: for item in items {
                        html(
                          tag: "tr",
                          attrs: [],
                          children: [
                            interpolate(item.name),
                          ],
                        ),
                      },
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_on_bool_with_nested_for_loop() {
        accept(
            indoc! {"
                fn Main(x: Bool, data: Array[String]) -> Html {
	                match x {
	                  true => for d in data { <>{d}</> },
	                  false => <></>,
	                }
                }
            "},
            expect![[r#"
                fn Main(x: Bool, data: Array[String]) -> Html {
                  match x {
                    true => for d in data {
                      fragment(interpolate(d)),
                    },
                    false => fragment(),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_html_element() {
        reject(
            indoc! {r#"
                fn Main() -> Html {
                    <dvi>oops</dvi>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unknown HTML element <dvi>
                1 | fn Main() -> Html {
                2 |     <dvi>oops</dvi>
                  |      ^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_mathml_element() {
        reject(
            indoc! {r#"
                fn Main() -> Html {
                    <math></math>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unknown HTML element <math>
                1 | fn Main() -> Html {
                2 |     <math></math>
                  |      ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_custom_hyphenated_element() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <my-widget>hi</my-widget>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "my-widget",
                    attrs: [],
                    children: [text("hi")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_complex_svg_structure() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <div class="navbar">
                        <svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" version="1.1" viewBox="0 0 128 128" class="size-12">
                            <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                                <path d="M20.04 38 64 22l43.96 16L64 54Z"></path>
                                <path d="M17.54 47.09v48l35.099 12.775"></path>
                                <path d="M64 112V64l46.46-16.91v48L77.988 106.91"></path>
                            </g>
                        </svg>
                        <ul>
                            <li><a href="/">Home</a></li>
                        </ul>
                    </div>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [class: "navbar"],
                    children: [
                      html(
                        tag: "svg",
                        attrs: [
                          xmlns: "http://www.w3.org/2000/svg",
                          width: "128",
                          height: "128",
                          version: "1.1",
                          viewBox: "0 0 128 128",
                          class: "size-12",
                        ],
                        children: [
                          html(
                            tag: "g",
                            attrs: [
                              style: "fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;",
                            ],
                            children: [
                              html(
                                tag: "path",
                                attrs: [
                                  d: "M20.04 38 64 22l43.96 16L64 54Z",
                                ],
                                children: [],
                              ),
                              html(
                                tag: "path",
                                attrs: [
                                  d: "M17.54 47.09v48l35.099 12.775",
                                ],
                                children: [],
                              ),
                              html(
                                tag: "path",
                                attrs: [
                                  d: "M64 112V64l46.46-16.91v48L77.988 106.91",
                                ],
                                children: [],
                              ),
                            ],
                          ),
                        ],
                      ),
                      html(
                        tag: "ul",
                        attrs: [],
                        children: [
                          html(
                            tag: "li",
                            attrs: [],
                            children: [
                              html(
                                tag: "a",
                                attrs: [href: "/"],
                                children: [
                                  text("Home"),
                                ],
                              ),
                            ],
                          ),
                        ],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_parameter_with_string_type() {
        accept(
            indoc! {"
                fn Main(data: String) -> Html {
                    <div>{data}</div>
                }
            "},
            expect![[r#"
                fn Main(data: String) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(data)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_parameter_with_record_type() {
        accept(
            indoc! {"
                record Data {
                  message: String,
                }

                fn Main(data: Data) -> Html {
                    <>
                        <h1>Hello World</h1>
                        <p>{data.message}</p>
                    </>
                }
            "},
            expect![[r#"
                record Data {
                  message: String,
                }

                fn Main(data: Data) -> Html {
                  fragment(
                    html(
                      tag: "h1",
                      attrs: [],
                      children: [text("Hello World")],
                    ),
                    html(
                      tag: "p",
                      attrs: [],
                      children: [
                        interpolate(data.message),
                      ],
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_parameter_with_array_type() {
        accept(
            indoc! {"
                fn Main(items: Array[String]) -> Html {
                    for item in items { <div>{item}</div> }
                }
            "},
            expect![[r#"
                fn Main(items: Array[String]) -> Html {
                  for item in items {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(item)],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_parameter_with_tuple_type() {
        accept(
            indoc! {"
                fn Main(pair: (Int, String)) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(pair: (Int, String)) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_tuple_type() {
        accept(
            indoc! {"
                fn Main(rows: Array[(Int, (String, Bool))]) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(rows: Array[(Int, (String, Bool))]) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_one_tuple_type() {
        accept(
            indoc! {"
                fn Main(only: (Int,), rows: Array[(String,)]) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(only: (Int,), rows: Array[(String,)]) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parenthesized_type_as_the_type_itself() {
        accept(
            indoc! {"
                fn Main(plain: (Int), nested: ((Int, String))) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(plain: Int, nested: (Int, String)) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_empty_tuple_type() {
        accept(
            indoc! {"
                fn Main(nothing: (), rows: Array[()]) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(nothing: (), rows: Array[()]) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_parameter_with_array_of_record_type() {
        accept(
            indoc! {"
                record Section {
                  title: String,
                  items: Array[String],
                }

                fn Main(data: Array[Section]) -> Html {
                    for section in data {
                        <>
                            <h1>{section.title}</h1>
                            {for item in section.items {
                                <div>{item}</div>
                            }}
                        </>
                    }
                }
            "},
            expect![[r#"
                record Section {
                  title: String,
                  items: Array[String],
                }

                fn Main(data: Array[Section]) -> Html {
                  for section in data {
                    fragment(
                      html(
                        tag: "h1",
                        attrs: [],
                        children: [
                          interpolate(section.title),
                        ],
                      ),
                      interpolate(
                        for item in section.items {
                          html(
                            tag: "div",
                            attrs: [],
                            children: [
                              interpolate(item),
                            ],
                          ),
                        },
                      ),
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_with_single_expression() {
        accept(
            "fn Main() -> Html {<h1>Hello {name}!</h1>}",
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [
                      text("Hello "),
                      interpolate(name),
                      text("!"),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_with_multiple_expressions() {
        accept(
            "fn Main() -> Html {<p>User {user.name} has {user.count} items</p>}",
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "p",
                    attrs: [],
                    children: [
                      text("User "),
                      interpolate(user.name),
                      text(" has "),
                      interpolate(user.count),
                      text(" items"),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_with_expression_at_start() {
        accept(
            "fn Main() -> Html {<span>{greeting} world!</span>}",
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "span",
                    attrs: [],
                    children: [
                      interpolate(greeting),
                      text(" world!"),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_with_expression_at_end() {
        accept(
            "fn Main() -> Html {<div>Price: {price}</div>}",
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      text("Price: "),
                      interpolate(price),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_with_only_expression() {
        accept(
            "fn Main() -> Html {<h2>{title}</h2>}",
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "h2",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_empty_expression_in_text() {
        reject(
            "fn Main() -> Html {<div>Empty: {}</div>}",
            expect![[r#"
                -- errors --
                error: Unexpected token '}'
                1 | fn Main() -> Html {<div>Empty: {}</div>}
                  |                                 ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("Empty:")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_complex_expression_in_text() {
        accept(
            r#"fn Main() -> Html {<p>Status: {user.profile.status == "active"}</p>}"#,
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "p",
                    attrs: [],
                    children: [
                      text("Status: "),
                      interpolate(
                        user.profile.status == "active",
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_adjacent_expressions_in_text() {
        accept(
            "fn Main() -> Html {<span>{first}{second}</span>}",
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "span",
                    attrs: [],
                    children: [
                      interpolate(first),
                      interpolate(second),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_expression_with_string_containing_html() {
        accept(
            r#"fn Main() -> Html {<div>{"<div></div>"}</div>}"#,
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      interpolate("<div></div>"),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_incomplete_record_declaration() {
        reject(
            indoc! {"
                record
                fn Main() -> Html {
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected identifier but got 'fn'
                1 | record
                2 | fn Main() -> Html {
                  | ^^

                error: Function 'Main' has an empty body: a function body must be a single expression
                1 | record
                2 | fn Main() -> Html {
                  |    ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_field_preceded_by_extra_identifier() {
        reject(
            "enum E { V { i a: Array[Int] } }",
            expect![[r#"
                -- errors --
                error: Expected token ':' but got 'a'
                1 | enum E { V { i a: Array[Int] } }
                  |                ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_enum_variant_body_closed_by_a_right_paren() {
        reject(
            indoc! {"
                enum E { A0, B1 {) }
                fn f() -> Int { 1 }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got ')'
                1 | enum E { A0, B1 {) }
                  |                  ^
                -- ast --
                fn f() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_text_after_page_name() {
        reject(
            indoc! {"
                page P x { fn body() -> Html { 1 } }
                fn f() -> Int { 1 }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '(' but got 'x'
                1 | page P x { fn body() -> Html { 1 } }
                  |        ^
                -- ast --
                fn f() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn rejects_a_page_with_a_lowercase_name() {
        reject(
            indoc! {"
                page foo(x: ) { fn body() -> Html { 1 } }
                fn f() -> Int { 1 }
            "},
            expect![[r#"
                -- errors --
                error: Type name must start with an uppercase letter
                1 | page foo(x: ) { fn body() -> Html { 1 } }
                  |      ^^^

                error: Expected type name but got ')'
                1 | page foo(x: ) { fn body() -> Html { 1 } }
                  |             ^
                -- ast --
                fn f() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_a_page_with_an_invalid_name() {
        reject(
            indoc! {"
                page 123 { fn body() -> Html { 1 } }
                fn f() -> Int { 1 }
            "},
            expect![[r#"
                -- errors --
                error: Expected identifier but got '123'
                1 | page 123 { fn body() -> Html { 1 } }
                  |      ^^^
                -- ast --
                fn f() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_a_page_with_an_unclosed_parameter_list() {
        reject(
            indoc! {"
                page P(x: Int { fn body() -> Html { 1 } }
                fn f() -> Int { 1 }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ')' but got '{'
                1 | page P(x: Int { fn body() -> Html { 1 } }
                  |               ^
                -- ast --
                fn f() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn rejects_page_member_missing_its_parameter_list() {
        reject(
            indoc! {"
                page P { fn body) -> Html { 1 } }
                fn f() -> Int { 1 }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '(' but got ')'
                1 | page P { fn body) -> Html { 1 } }
                  |                 ^
                -- ast --
                fn f() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_a_page_member_with_a_broken_signature() {
        reject(
            indoc! {"
                page P {
                  fn head) -> Html { 1 }
                  fn body() -> Html { 2 }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '(' but got ')'
                1 | page P {
                2 |   fn head) -> Html { 1 }
                  |          ^
                -- ast --
                page P() {
                  fn body() -> Html {
                    2
                  }
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_a_page_member_missing_its_body_braces() {
        reject(
            indoc! {"
                page P {
                  fn head() -> Html 1
                  fn body() -> Html { 2 }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '{' but got '1'
                1 | page P {
                2 |   fn head() -> Html 1
                  |                     ^
                -- ast --
                page P() {
                  fn body() -> Html {
                    2
                  }
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_a_page_member_missing_its_name() {
        reject(
            indoc! {"
                page P {
                  fn ) -> Html { 1 }
                  fn body() -> Html { 2 }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected identifier but got ')'
                1 | page P {
                2 |   fn ) -> Html { 1 }
                  |      ^
                -- ast --
                page P() {
                  fn body() -> Html {
                    2
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_text_before_function() {
        reject(
            indoc! {"
                foo
                fn Main() -> Html {
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected token 'foo'
                1 | foo
                  | ^^^

                error: Function 'Main' has an empty body: a function body must be a single expression
                1 | foo
                2 | fn Main() -> Html {
                  |    ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_in_template() {
        accept(
            indoc! {r#"
                enum Color {Red, Green, Blue}

                fn Main(color: Color) -> Html {
                    <>{match color {Color::Red => "red", Color::Blue => "blue"}}</>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                fn Main(color: Color) -> Html {
                  fragment(
                    interpolate(
                      match color {
                        Color::Red => "red",
                        Color::Blue => "blue",
                      },
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_in_attribute() {
        accept(
            indoc! {r#"
                enum Color {Red, Green, Blue}

                fn Main(color: Color) -> Html {
                    <div class={match color {Color::Red => "text-red", Color::Blue => "text-blue"}}></div>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                fn Main(color: Color) -> Html {
                  html(
                    tag: "div",
                    attrs: [
                      class: match color {
                        Color::Red => "text-red",
                        Color::Blue => "text-blue",
                      },
                    ],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_with_multiline_arms() {
        accept(
            indoc! {r#"
                enum Status {Active, Inactive, Pending}

                fn Main(status: Status) -> Html {
                    <>
                        {match status {
                            Status::Active => "active",
                            Status::Inactive => "inactive",
                            Status::Pending => "pending",
                        }}
                    </>
                }
            "#},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                fn Main(status: Status) -> Html {
                  fragment(
                    interpolate(
                      match status {
                        Status::Active => "active",
                        Status::Inactive => "inactive",
                        Status::Pending => "pending",
                      },
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_string_value() {
        accept(
            indoc! {r#"
                fn Main(name: String = "World") -> Html {
                    <div>{name}</div>
                }
            "#},
            expect![[r#"
                fn Main(name: String = "World") -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(name)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_int_value() {
        accept(
            indoc! {"
                fn Main(count: Int = 42) -> Html {
                    <span>{count}</span>
                }
            "},
            expect![[r#"
                fn Main(count: Int = 42) -> Html {
                  html(
                    tag: "span",
                    attrs: [],
                    children: [interpolate(count)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_bool_value() {
        accept(
            indoc! {"
                fn Main(enabled: Bool = true) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(enabled: Bool = true) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_mixed_required_and_default_parameters() {
        accept(
            indoc! {r#"
                fn Main(name: String, role: String = "user", active: Bool = true) -> Html {
                    <div>{name}</div>
                }
            "#},
            expect![[r#"
                fn Main(name: String, role: String = "user", active: Bool = true) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(name)],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_array_value() {
        accept(
            indoc! {r#"
                fn Main(items: Array[String] = ["a", "b"]) -> Html {
                    for item in items { <>{item}</> }
                }
            "#},
            expect![[r#"
                fn Main(items: Array[String] = [
                  "a",
                  "b",
                ]) -> Html {
                  for item in items {
                    fragment(interpolate(item)),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_record_value() {
        accept(
            indoc! {r#"
                record Config { debug: Bool, timeout: Int }
                fn Main(config: Config = Config {debug: false, timeout: 30}) -> Html {
                    <div></div>
                }
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                fn Main(config: Config = Config {
                  debug: false,
                  timeout: 30,
                }) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_enum_value() {
        accept(
            indoc! {"
                enum Status { Active, Inactive, Pending }
                fn Main(status: Status = Status::Active) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                fn Main(status: Status = Status::Active) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_option_type() {
        accept(
            indoc! {"
                fn Main(name: Option[String]) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(name: Option[String]) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_none_value() {
        accept(
            indoc! {"
                fn Main(name: Option[String] = None) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(name: Option[String] = None) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_some_value() {
        accept(
            indoc! {r#"
                fn Main(name: Option[String] = Some("default")) -> Html {
                    <div></div>
                }
            "#},
            expect![[r#"
                fn Main(name: Option[String] = Some("default")) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_int_array() {
        accept(
            indoc! {"
                fn Main(offsets: Array[Int] = [1, 2]) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(offsets: Array[Int] = [
                  1,
                  2,
                ]) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_empty_fragment() {
        accept(
            indoc! {"
                fn Main(children: Html = <></>) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                fn Main(children: Html = fragment()) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_parameter_with_malformed_default_value() {
        reject(
            indoc! {"
                fn Main(x: Int = = 1, y: Int) -> Html {
                    <div></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected token '='
                1 | fn Main(x: Int = = 1, y: Int) -> Html {
                  |                  ^
                -- ast --
                fn Main(y: Int) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_with_empty_arm() {
        accept(
            indoc! {r#"
                fn Main(x: Option[String]) -> Html {
                    match x {
                        Some(y) => <>found {y}</>,
                        None => <></>,
                    }
                }
            "#},
            expect![[r#"
                fn Main(x: Option[String]) -> Html {
                  match x {
                    Some(y) => fragment(
                      text("found "),
                      interpolate(y),
                    ),
                    None => fragment(),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_with_enum_variant_fields_in_arms() {
        accept(
            indoc! {r#"
                enum Outcome { Success {value: Int}, Failure {message: String} }
                fn Main(r: Outcome) -> Html {
                    match r {
                        Outcome::Success{value: v} => <>Success: {v}</>,
                        Outcome::Failure{message: m} => <>Error: {m}</>,
                    }
                }
            "#},
            expect![[r#"
                enum Outcome {
                  Success { value: Int },
                  Failure { message: String },
                }

                fn Main(r: Outcome) -> Html {
                  match r {
                    Outcome::Success{value: v} => fragment(
                      text("Success: "),
                      interpolate(v),
                    ),
                    Outcome::Failure{message: m} => fragment(
                      text("Error: "),
                      interpolate(m),
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_with_enum_literal_subject() {
        accept(
            indoc! {r#"
                enum Status { Active {name: String}, Inactive }
                fn Main() -> Html {
                    match (Status::Active {name: "test"}) {
                        Status::Active{name: n} => <>{n}</>,
                        Status::Inactive => <>none</>,
                    }
                }
            "#},
            expect![[r#"
                enum Status {
                  Active { name: String },
                  Inactive,
                }

                fn Main() -> Html {
                  match Status::Active {name: "test"} {
                    Status::Active{name: n} => fragment(
                      interpolate(n),
                    ),
                    Status::Inactive => fragment(
                      text("none"),
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_let_blocks() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    let a: Int = 1;
                    let b: Int = 2;
                    <div>{a} + {b}</div>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  {
                    let a: Int = 1;
                    let b: Int = 2;
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(a),
                        text(" + "),
                        interpolate(b),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_multiple_bindings() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    let first: String = "Hello";
                    let second: String = "World";
                    <div>{first} {second}</div>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  {
                    let first: String = "Hello";
                    let second: String = "World";
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(first),
                        text(" "),
                        interpolate(second),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_sibling_let_blocks() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <>
                        {
                            let a: String = "Hello";
                            <>{a}</>
                        }
                        {
                            let b: String = "World";
                            <>{b}</>
                        }
                    </>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  fragment(
                    interpolate(
                      {
                        let a: String = "Hello";
                        fragment(interpolate(a))
                      },
                    ),
                    interpolate(
                      {
                        let b: String = "World";
                        fragment(interpolate(b))
                      },
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_after_html_element() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <>
                        <div>First</div>
                        {
                            let name: String = "World";
                            <div>Hello {name}</div>
                        }
                    </>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  fragment(
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("First")],
                    ),
                    interpolate(
                      {
                        let name: String = "World";
                        html(
                          tag: "div",
                          attrs: [],
                          children: [
                            text("Hello "),
                            interpolate(name),
                          ],
                        )
                      },
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_before_html_element() {
        accept(
            indoc! {r#"
                fn Main() -> Html {
                    <>
                        {
                            let name: String = "World";
                            <div>Hello {name}</div>
                        }
                        <div>Last</div>
                    </>
                }
            "#},
            expect![[r#"
                fn Main() -> Html {
                  fragment(
                    interpolate(
                      {
                        let name: String = "World";
                        html(
                          tag: "div",
                          attrs: [],
                          children: [
                            text("Hello "),
                            interpolate(name),
                          ],
                        )
                      },
                    ),
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Last")],
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_top_level_html_element() {
        reject(
            "<div></div>",
            expect![[r#"
                -- errors --
                error: Unexpected token '<'
                1 | <div></div>
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_page_without_parentheses() {
        accept(
            indoc! {"
                page Index {
                  fn body() -> Html {
                      <div>Hello</div>
                  }
                }
            "},
            expect![[r#"
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_parameters() {
        accept(
            indoc! {"
                page Index(name: String, count: Int) {
                  fn body() -> Html {
                      <div>{name}: {count}</div>
                  }
                }
            "},
            expect![[r#"
                page Index(name: String, count: Int) {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(name),
                        text(": "),
                        interpolate(count),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_function_invocation() {
        accept(
            indoc! {"
                fn Header(title: String) -> Html {
                    <h1>{title}</h1>
                }

                page Index(title: String) {
                  fn body() -> Html {
                      <Header title={title} />
                  }
                }
            "},
            expect![[r#"
                fn Header(title: String) -> Html {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }

                page Index(title: String) {
                  fn body() -> Html {
                    Header(attrs: [title: title])
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_pages() {
        accept(
            indoc! {"
                page Index() {
                  fn body() -> Html {
                      <div>Index</div>
                  }
                }

                page About() {
                  fn body() -> Html {
                      <div>About</div>
                  }
                }
            "},
            expect![[r#"
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Index")],
                    )
                  }
                }

                page About() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("About")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_binding_with_reserved_name() {
        reject(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    let default: String = "x";
                    <div></div>
                  }
                }
            "#},
            expect![[r#"
                -- errors --
                error: Variable name is a reserved word
                2 |   fn body() -> Html {
                3 |     let default: String = "x";
                  |         ^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_page_with_reserved_name() {
        reject(
            indoc! {"
                page Error() {
                  fn body() -> Html {
                      <div>Hello</div>
                  }
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name 'Error' is a reserved word
                1 | page Error() {
                  |      ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_function_with_reserved_name() {
        reject(
            indoc! {"
                fn Error() -> Html {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name 'Error' is a reserved word
                1 | fn Error() -> Html {
                  |    ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_function_invocation_with_invalid_character() {
        reject(
            indoc! {"
                fn Card() -> Html {
                    <Foo-Bar />
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name contains invalid character: '-'
                1 | fn Card() -> Html {
                2 |     <Foo-Bar />
                  |      ^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_page_with_lowercase_name() {
        reject(
            indoc! {"
                page index() {
                  fn body() -> Html {
                      <div>Hello</div>
                  }
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name must start with an uppercase letter
                1 | page index() {
                  |      ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_page_with_default_parameter() {
        reject(
            indoc! {r#"
                page Index(name: String = "World") {
                  fn body() -> Html {
                      <div>Hello {name}</div>
                  }
                }
            "#},
            expect![[r#"
                -- errors --
                error: Default values are not allowed on page parameters
                1 | page Index(name: String = "World") {
                  |                           ^^^^^^^
                -- ast --
                page Index(name: String = "World") {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        text("Hello "),
                        interpolate(name),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_declaration() {
        accept(
            indoc! {r#"
                page Index(name: String) {
                    fn head() -> Html {
                        <title>My page</title>
                    }
                    fn body() -> Html {
                        <div>Hello {name}</div>
                    }
                }
            "#},
            expect![[r#"
                page Index(name: String) {
                  fn head() -> Html {
                    html(
                      tag: "title",
                      attrs: [],
                      children: [text("My page")],
                    )
                  }
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        text("Hello "),
                        interpolate(name),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_declaration_without_head() {
        accept(
            indoc! {"
                page Index() {
                    fn body() -> Html {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_page_declaration_without_body() {
        reject(
            indoc! {"
                page Index() {
                    fn head() -> Html {
                        <title>My page</title>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected a 'fn body() -> Html' member
                4 |     }
                5 | }
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_page_member_with_an_unknown_name() {
        reject(
            indoc! {"
                page Index() {
                    fn footer() -> Html {
                        <div>Bye</div>
                    }
                    fn body() -> Html {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Unknown page member 'footer': expected 'head' or 'body'
                1 | page Index() {
                2 |     fn footer() -> Html {
                  |        ^^^^^^
                -- ast --
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_duplicate_page_member() {
        reject(
            indoc! {"
                page Index() {
                    fn body() -> Html {
                        <div>Hello</div>
                    }
                    fn body() -> Html {
                        <div>Again</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Duplicate page member 'body'
                4 |     }
                5 |     fn body() -> Html {
                  |        ^^^^
                -- ast --
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_page_member_with_parameters() {
        reject(
            indoc! {"
                page Index() {
                    fn body(name: String) -> Html {
                        <div>{name}</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Page member 'body' cannot have parameters
                1 | page Index() {
                2 |     fn body(name: String) -> Html {
                  |             ^^^^
                -- ast --
                page Index() {
                  fn body(name: String) -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(name)],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_page_member_with_a_rest_parameter() {
        reject(
            indoc! {"
                page Index() {
                    fn body(...rest) -> Html {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Page member 'body' cannot have parameters
                1 | page Index() {
                2 |     fn body(...rest) -> Html {
                  |             ^^^^^^^
                -- ast --
                page Index() {
                  fn body(...rest) -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_page_member_that_does_not_return_a_fragment() {
        reject(
            indoc! {"
                page Index() {
                    fn body() -> String {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Page member 'body' must return Html
                1 | page Index() {
                2 |     fn body() -> String {
                  |                  ^^^^^^
                -- ast --
                page Index() {
                  fn body() -> String {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_pub_on_a_page_member() {
        reject(
            indoc! {"
                page Index() {
                    pub fn body() -> Html {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: 'pub' is not allowed here
                1 | page Index() {
                2 |     pub fn body() -> Html {
                  |     ^^^
                -- ast --
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_a_non_member_token_inside_a_page() {
        reject(
            indoc! {"
                page Index() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected 'fn head' or 'fn body'
                1 | page Index() {
                2 |     <div>Hello</div>
                  |     ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_end_of_input_inside_a_page() {
        reject(
            indoc! {"
                page Index() {
                    fn body() -> Html {
                        <div>Hello</div>
                    }
            "},
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                1 | page Index() {
                  |              ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn keeps_parsing_page_members_after_one_fails() {
        reject(
            indoc! {"
                page Index() {
                    fn head() -> Html {
                        hello world
                    }
                    fn body() -> Html {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got 'world'
                2 |     fn head() -> Html {
                3 |         hello world
                  |               ^^^^^
                -- ast --
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn reports_a_page_body_member_that_fails_to_parse_once() {
        reject(
            indoc! {"
                page Index() {
                    fn body() -> Html {
                        hello world
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got 'world'
                2 |     fn body() -> Html {
                3 |         hello world
                  |               ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_an_empty_body_on_a_function_without_the_fragment_hint() {
        reject(
            indoc! {"
                fn f() -> Int {
                }
            "},
            expect![[r#"
                -- errors --
                error: Function 'f' has an empty body: a function body must be a single expression
                1 | fn f() -> Int {
                  |    ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_page_declaration_with_body_before_head() {
        accept(
            indoc! {"
                page Index() {
                    fn body() -> Html {
                        <div>Hello</div>
                    }
                    fn head() -> Html {
                        <title>My page</title>
                    }
                }
            "},
            expect![[r#"
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Hello")],
                    )
                  }
                  fn head() -> Html {
                    html(
                      tag: "title",
                      attrs: [],
                      children: [text("My page")],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_a_page_with_an_empty_body() {
        reject(
            indoc! {"
                page Index() {
                  fn body() -> Html {
                  }
                }
            "},
            expect![[r#"
                -- errors --
                error: Function 'body' has an empty body: a function body must be a single expression
                1 | page Index() {
                2 |   fn body() -> Html {
                  |      ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_match_on_bool() {
        accept(
            indoc! {"
                page Index(show: Bool) {
                  fn body() -> Html {
                      match show {
                        true => <div>Visible</div>,
                        false => <></>,
                      }
                  }
                }
            "},
            expect![[r#"
                page Index(show: Bool) {
                  fn body() -> Html {
                    match show {
                      true => html(
                        tag: "div",
                        attrs: [],
                        children: [text("Visible")],
                      ),
                      false => fragment(),
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_for_loop() {
        accept(
            indoc! {"
                page Index(items: Array[String]) {
                  fn body() -> Html {
                      for item in items { <div>{item}</div> }
                  }
                }
            "},
            expect![[r#"
                page Index(items: Array[String]) {
                  fn body() -> Html {
                    for item in items {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [interpolate(item)],
                      ),
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_let_binding() {
        accept(
            indoc! {r#"
                page Index() {
                  fn body() -> Html {
                      let name: String = "World";
                      <div>Hello {name}</div>
                  }
                }
            "#},
            expect![[r#"
                page Index() {
                  fn body() -> Html {
                    {
                      let name: String = "World";
                      html(
                        tag: "div",
                        attrs: [],
                        children: [
                          text("Hello "),
                          interpolate(name),
                        ],
                      )
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_void_elements() {
        accept(
            indoc! {"
                page Index() {
                  fn body() -> Html {
                      <div>
                          <br />
                          <input type=\"text\" />
                          <hr />
                      </div>
                  }
                }
            "},
            expect![[r#"
                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        html(tag: "br", attrs: []),
                        html(
                          tag: "input",
                          attrs: [type: "text"],
                        ),
                        html(tag: "hr", attrs: []),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_trailing_comma_in_params() {
        accept(
            indoc! {"
                page Index(name: String,) {
                  fn body() -> Html {
                      <div>{name}</div>
                  }
                }
            "},
            expect![[r#"
                page Index(name: String) {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(name)],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_match_expression() {
        accept(
            indoc! {"
                page Index(value: Option[String]) {
                  fn body() -> Html {
                      match value {
                          Some(s) => <div>{s}</div>,
                          None => <div>No value</div>,
                      }
                  }
                }
            "},
            expect![[r#"
                page Index(value: Option[String]) {
                  fn body() -> Html {
                    match value {
                      Some(s) => html(
                        tag: "div",
                        attrs: [],
                        children: [interpolate(s)],
                      ),
                      None => html(
                        tag: "div",
                        attrs: [],
                        children: [text("No value")],
                      ),
                    }
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_with_nested_functions() {
        accept(
            indoc! {"
                fn Header(title: String) -> Html {
                    <h1>{title}</h1>
                }

                fn Footer() -> Html {
                    <p>Copyright 2024</p>
                }

                page Index(title: String) {
                  fn body() -> Html {
                      <div>
                          <Header title={title} />
                          <main>Content</main>
                          <Footer />
                      </div>
                  }
                }
            "},
            expect![[r#"
                fn Header(title: String) -> Html {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }

                fn Footer() -> Html {
                  html(
                    tag: "p",
                    attrs: [],
                    children: [text("Copyright 2024")],
                  )
                }

                page Index(title: String) {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        Header(attrs: [title: title]),
                        html(
                          tag: "main",
                          attrs: [],
                          children: [text("Content")],
                        ),
                        Footer(attrs: []),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_between_functions() {
        accept(
            indoc! {"
                fn Header() -> Html {
                    <h1>Header</h1>
                }

                page Index() {
                  fn body() -> Html {
                      <div>Index</div>
                  }
                }

                fn Footer() -> Html {
                    <p>Footer</p>
                }
            "},
            expect![[r#"
                fn Header() -> Html {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [text("Header")],
                  )
                }

                page Index() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Index")],
                    )
                  }
                }

                fn Footer() -> Html {
                  html(
                    tag: "p",
                    attrs: [],
                    children: [text("Footer")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_page_with_multiple_params_mixed_defaults() {
        reject(
            indoc! {r#"
                page Index(required: String, optional: Int = 42) {
                  fn body() -> Html {
                      <div>{required}: {optional}</div>
                  }
                }
            "#},
            expect![[r#"
                -- errors --
                error: Default values are not allowed on page parameters
                1 | page Index(required: String, optional: Int = 42) {
                  |                                              ^^
                -- ast --
                page Index(required: String, optional: Int = 42) {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(required),
                        text(": "),
                        interpolate(optional),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_bare_text_as_a_page_body() {
        reject(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    hello world
                  }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got 'world'
                2 |   fn body() -> Html {
                3 |     hello world
                  |           ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_escape_sequences_in_strings() {
        accept(
            indoc! {r#"
                fn Test() -> Html {
                    <>
                        {"hello\nworld"}
                        {"tab\there"}
                        {"back\\slash"}
                        {"quote\"here"}
                    </>
                }
            "#},
            expect![[r#"
                fn Test() -> Html {
                  fragment(
                    interpolate("hello\nworld"),
                    interpolate("tab\there"),
                    interpolate("back\\slash"),
                    interpolate("quote\"here"),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_unknown_escape_sequences() {
        // Reported when the literal is cooked, not while parsing.
        accept(
            r#"fn test() -> String {"invalid\q"}"#,
            expect![[r#"
                fn test() -> String {
                  "invalid\q"
                }
            "#]],
        );
    }

    #[test]
    fn accepts_rest_param() {
        accept(
            indoc! {r#"
                fn Foo(class: String, ...rest) -> Html {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                fn Foo(class: String, ...rest) -> Html {
                  html(
                    tag: "div",
                    attrs: [...rest],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_rest_param_not_last() {
        reject(
            indoc! {r#"
                fn Foo(...rest, a: String, b: String) -> Html {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Rest parameter must be the last parameter
                1 | fn Foo(...rest, a: String, b: String) -> Html {
                  |        ^^^^^^^
                -- ast --
                fn Foo(a: String, b: String, ...rest) -> Html {
                  html(
                    tag: "div",
                    attrs: [...rest],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_duplicate_rest_param() {
        reject(
            indoc! {r#"
                fn Foo(...a, ...b) -> Html {
                  <div ...a></div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Rest parameter must be the last parameter
                1 | fn Foo(...a, ...b) -> Html {
                  |        ^^^^

                error: At most one rest parameter is allowed
                1 | fn Foo(...a, ...b) -> Html {
                  |              ^^^^
                -- ast --
                fn Foo(...a) -> Html {
                  html(
                    tag: "div",
                    attrs: [...a],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_spread_attribute_on_element() {
        accept(
            indoc! {r#"
                fn Foo(...rest) -> Html {
                  <button ...rest></button>
                }
            "#},
            expect![[r#"
                fn Foo(...rest) -> Html {
                  html(
                    tag: "button",
                    attrs: [...rest],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_spread_attribute_on_function() {
        accept(
            indoc! {r#"
                fn Bar(...rest) -> Html {
                  <Foo ...rest></Foo>
                }
            "#},
            expect![[r#"
                fn Bar(...rest) -> Html {
                  Foo(attrs: [...rest], children: [])
                }
            "#]],
        );
    }

    #[test]
    fn rejects_spread_attribute_with_uppercase_name() {
        reject(
            indoc! {r#"
                fn Foo() -> Html {
                  <button ...Bar></button>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Variable name must be lowercase (found uppercase: 'B')
                1 | fn Foo() -> Html {
                2 |   <button ...Bar></button>
                  |              ^^^
                -- ast --
                fn Foo() -> Html {
                  html(
                    tag: "button",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_spread_attribute_with_leading_underscore() {
        reject(
            indoc! {r#"
                fn Foo() -> Html {
                  <button ..._x></button>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Variable name cannot start with underscore
                1 | fn Foo() -> Html {
                2 |   <button ..._x></button>
                  |              ^^
                -- ast --
                fn Foo() -> Html {
                  html(
                    tag: "button",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_declaration() {
        accept(
            indoc! {"
                fn foo(x: Int) -> Int {
                  x + 10
                }

                fn Foo() -> Html {
                  <div>
                    {for x in 0..=foo(10) {
                      <>{x.to_string()}</>
                    }}
                    {foo(10)}
                  </div>
                }
            "},
            expect![[r#"
                fn foo(x: Int) -> Int {
                  x + 10
                }

                fn Foo() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      interpolate(
                        for x in 0..=foo(10) {
                          fragment(
                            interpolate(x.to_string()),
                          ),
                        },
                      ),
                      interpolate(foo(10)),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_with_empty_params() {
        accept(
            indoc! {"
                fn answer() -> Int {
                  42
                }
            "},
            expect![[r#"
                fn answer() -> Int {
                  42
                }
            "#]],
        );
    }

    #[test]
    fn accepts_function_with_multiple_params() {
        accept(
            indoc! {"
                fn clamp(value: Int, low: Int, high: Int) -> Int {
                  value
                }
            "},
            expect![[r#"
                fn clamp(value: Int, low: Int, high: Int) -> Int {
                  value
                }
            "#]],
        );
    }

    #[test]
    fn accepts_import_of_function() {
        accept(
            indoc! {"
                import other::foo

                fn bar() -> Int {
                  foo()
                }
            "},
            expect![[r#"
                import other::foo

                fn bar() -> Int {
                  foo()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_import_with_trivia_around_path_separators() {
        accept(
            indoc! {"
                import other :: nested
                  ::
                  foo
                import lib // module
                  ::Button

                fn bar() -> Int {
                  foo()
                }
            "},
            expect![[r#"
                import other::nested::foo
                import lib::Button

                fn bar() -> Int {
                  foo()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_import_of_invalid_function_name() {
        reject(
            indoc! {"
                import other::foo_

                fn bar() -> Int {
                  foo()
                }
            "},
            expect![[r#"
                -- errors --
                error: Variable name cannot end with underscore
                1 | import other::foo_
                  |               ^^^^
                -- ast --
                fn bar() -> Int {
                  foo()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_function_without_return_type() {
        reject(
            indoc! {"
                fn foo(x: Int) {
                  x
                }
            "},
            expect![[r#"
                -- errors --
                error: Function 'foo' is missing a return type annotation
                1 | fn foo(x: Int) {
                  |    ^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_function_with_colon_before_return_type() {
        reject(
            indoc! {"
                fn foo(): Int {
                  1
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '->' but got ':'
                1 | fn foo(): Int {
                  |         ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_function_without_parameter_parens() {
        reject(
            indoc! {"
                fn foo -> Int {
                  1
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '(' but got '->'
                1 | fn foo -> Int {
                  |        ^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_default_value_on_function_parameter() {
        accept(
            indoc! {"
                fn foo(x: Int = 1) -> Int {
                  x
                }
            "},
            expect![[r#"
                fn foo(x: Int = 1) -> Int {
                  x
                }
            "#]],
        );
    }

    #[test]
    fn accepts_rest_param_on_function() {
        accept(
            indoc! {"
                fn Foo(class: String, ...rest) -> Html {
                  <div class={class} ...rest></div>
                }
            "},
            expect![[r#"
                fn Foo(class: String, ...rest) -> Html {
                  html(
                    tag: "div",
                    attrs: [class: class, ...rest],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_rest_param_on_page() {
        reject(
            indoc! {"
                page Foo(...rest) {
                  fn body() -> Html {
                    <div></div>
                  }
                }
            "},
            expect![[r#"
                -- errors --
                error: Rest parameters are not allowed on pages
                1 | page Foo(...rest) {
                  |          ^^^^^^^
                -- ast --
                page Foo() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_examples_annotation_on_function() {
        reject(
            indoc! {"
                fn foo(#[examples(min = 1)] x: Int) -> Int {
                  x
                }
            "},
            expect![[r#"
                -- errors --
                error: Examples annotations are not allowed on function parameters
                1 | fn foo(#[examples(min = 1)] x: Int) -> Int {
                  |        ^^^^^^^^^^^^^^^^^^^^
                -- ast --
                fn foo(#[examples(min = 1)] x: Int) -> Int {
                  x
                }
            "#]],
        );
    }

    #[test]
    fn rejects_param_after_rest_param() {
        reject(
            indoc! {r#"
                fn Foo(...rest, class: String) -> Html {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Rest parameter must be the last parameter
                1 | fn Foo(...rest, class: String) -> Html {
                  |        ^^^^^^^
                -- ast --
                fn Foo(class: String, ...rest) -> Html {
                  html(
                    tag: "div",
                    attrs: [...rest],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_as_a_function_body() {
        accept(
            indoc! {"
                fn card() -> Html {
                  <div>hello</div>
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("hello")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_a_fragment_as_a_function_body() {
        accept(
            indoc! {"
                fn card() -> Html {
                  <></>
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_in_an_interpolation() {
        accept(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>{<span>hello</span>}</div>
                  }
                }
            "},
            expect![[r#"
                page Test() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(
                          html(
                            tag: "span",
                            attrs: [],
                            children: [text("hello")],
                          ),
                        ),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_nested_through_two_interpolations() {
        accept(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>{<span>{<b>hello</b>}</span>}</div>
                  }
                }
            "},
            expect![[r#"
                page Test() {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(
                          html(
                            tag: "span",
                            attrs: [],
                            children: [
                              interpolate(
                                html(
                                  tag: "b",
                                  attrs: [],
                                  children: [
                                    text("hello"),
                                  ],
                                ),
                              ),
                            ],
                          ),
                        ),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_as_a_call_argument() {
        accept(
            indoc! {"
                fn wrap(children: Html) -> Html {
                  <div>{children}</div>
                }

                fn card() -> Html {
                  wrap(<span>hello</span>)
                }
            "},
            expect![[r#"
                fn wrap(children: Html) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(children)],
                  )
                }

                fn card() -> Html {
                  wrap(
                    html(
                      tag: "span",
                      attrs: [],
                      children: [text("hello")],
                    ),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_in_an_array_literal() {
        accept(
            indoc! {"
                fn cards() -> Array[Html] {
                  [<div>a</div>, <div>b</div>]
                }
            "},
            expect![[r#"
                fn cards() -> Array[Html] {
                  [
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("a")],
                    ),
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("b")],
                    ),
                  ]
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_in_a_match_arm() {
        accept(
            indoc! {"
                fn badge(on: Bool) -> Html {
                  match on {
                    true => <b>yes</b>,
                    false => <i>no</i>,
                  }
                }
            "},
            expect![[r#"
                fn badge(on: Bool) -> Html {
                  match on {
                    true => html(
                      tag: "b",
                      attrs: [],
                      children: [text("yes")],
                    ),
                    false => html(
                      tag: "i",
                      attrs: [],
                      children: [text("no")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_as_a_function_attribute_value() {
        accept(
            indoc! {"
                fn Card(slot: Html) -> Html {
                  <div>{slot}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card slot={<span>hello</span>}/>
                  }
                }
            "},
            expect![[r#"
                fn Card(slot: Html) -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(slot)],
                  )
                }

                page Test() {
                  fn body() -> Html {
                    Card(
                      attrs: [
                        slot: html(
                          tag: "span",
                          attrs: [],
                          children: [text("hello")],
                        ),
                      ],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_a_comment_before_markup_in_expression_position() {
        accept(
            indoc! {"
                fn card() -> Html {
                  // a note
                  <div>hello</div>
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("hello")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_a_markup_comment_in_expression_position() {
        accept(
            indoc! {"
                fn card() -> Html {
                  <!-- a note -->
                }
            "},
            expect![[r#"
                fn card() -> Html {
                  comment("<!-- a note -->")
                }
            "#]],
        );
    }

    #[test]
    fn accepts_comparisons_in_expression_position() {
        accept(
            indoc! {"
                fn check(a: Int, b: Int, c: Int, d: Int) -> Bool {
                  a < b && c > d
                }

                fn at_most(a: Int, b: Int) -> Bool {
                  a <= b
                }
            "},
            expect![[r#"
                fn check(a: Int, b: Int, c: Int, d: Int) -> Bool {
                  a < b && c > d
                }

                fn at_most(a: Int, b: Int) -> Bool {
                  a <= b
                }
            "#]],
        );
    }

    #[test]
    fn rejects_a_second_root_in_expression_position() {
        reject(
            indoc! {"
                fn card() -> Html {
                  <div/><span/>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                1 | fn card() -> Html {
                2 |   <div/><span/>
                  |              ^

                error: Unexpected token '}'
                2 |   <div/><span/>
                3 | }
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_a_space_between_the_angle_and_the_tag_name() {
        reject(
            indoc! {"
                fn card() -> Html {
                  < div
                }
            "},
            expect![[r#"
                -- errors --
                error: Unterminated tag start
                1 | fn card() -> Html {
                2 |   < div
                  |   ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_an_unclosed_tag_in_expression_position() {
        reject(
            indoc! {"
                fn card() -> Html {
                  <div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | fn card() -> Html {
                2 |   <div>
                  |    ^^^
                -- ast --
                fn card() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_newline_between_text_lines() {
        accept(
            indoc! {"
                fn Main() -> Html {
                  <p>
                    first line
                    second line
                  </p>
                }
            "},
            expect![[r#"
                fn Main() -> Html {
                  html(
                    tag: "p",
                    attrs: [],
                    children: [
                      text("first line"),
                      newline(),
                      text("second line"),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_examples_annotations_on_fields_and_params() {
        accept(
            indoc! {r#"
                record User {
                  #[examples(pattern = "[a-z]+", min_len = 1)]
                  name: String,
                  #[examples(min = 0, max = 120)]
                  age: Int,
                }

                page Main(#[examples(min = 1)] count: Int) {
                  fn body() -> Html {
                    <div>{count}</div>
                  }
                }
            "#},
            expect![[r#"
                record User {
                  #[examples(pattern = "[a-z]+", min_len = 1)] name: String,
                  #[examples(min = 0, max = 120)] age: Int,
                }

                page Main(#[examples(min = 1)] count: Int) {
                  fn body() -> Html {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(count)],
                    )
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_negative_examples_bounds() {
        accept(
            indoc! {"
                record Reading {
                  #[examples(min = -40, max = 60)]
                  celsius: Int,
                }
            "},
            expect![[r#"
                record Reading {
                  #[examples(min = -40, max = 60)] celsius: Int,
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_annotation() {
        reject(
            indoc! {"
                record User {
                  #[docs(min = 1)]
                  age: Int,
                }
            "},
            expect![[r#"
                -- errors --
                error: Unknown annotation 'docs'
                1 | record User {
                2 |   #[docs(min = 1)]
                  |     ^^^^
                -- ast --
                record User {}
            "#]],
        );
    }

    #[test]
    fn rejects_examples_key_without_value() {
        reject(
            indoc! {"
                record User {
                  #[examples(bogus)]
                  age: Int,
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '=' but got ')'
                1 | record User {
                2 |   #[examples(bogus)]
                  |                   ^
                -- ast --
                record User {
                  #[examples()] age: Int,
                }
            "#]],
        );
    }

    #[test]
    fn rejects_annotation_missing_right_paren() {
        reject(
            indoc! {"
                record User {
                  #[examples(min = 1]
                  age: Int,
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ')' but got ']'
                1 | record User {
                2 |   #[examples(min = 1]
                  |                     ^
                -- ast --
                record User {}
            "#]],
        );
    }

    #[test]
    fn rejects_annotation_missing_right_bracket() {
        reject(
            indoc! {"
                record User {
                  #[examples(min = 1)
                  age: Int,
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ']' but got 'age'
                2 |   #[examples(min = 1)
                3 |   age: Int,
                  |   ^^^
                -- ast --
                record User {}
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_examples_key() {
        reject(
            indoc! {"
                record User {
                  #[examples(minimum = 1, max = 5)]
                  age: Int,
                }
            "},
            expect![[r#"
                -- errors --
                error: Unknown examples key 'minimum'
                1 | record User {
                2 |   #[examples(minimum = 1, max = 5)]
                  |              ^^^^^^^
                -- ast --
                record User {
                  #[examples(max = 5)] age: Int,
                }
            "#]],
        );
    }

    #[test]
    fn preserves_the_spelling_of_an_examples_pattern() {
        accept(
            indoc! {r#"
                record User {
                  #[examples(pattern = "\\d+\\s\\w")]
                  name: String,
                }
            "#},
            expect![[r#"
                record User {
                  #[examples(pattern = "\\d+\\s\\w")] name: String,
                }
            "#]],
        );
    }

    #[test]
    fn rejects_examples_value_of_wrong_kind() {
        reject(
            indoc! {r#"
                record User {
                  #[examples(pattern = 5)]
                  name: String,
                  #[examples(min = "a", max = 5)]
                  age: Int,
                }
            "#},
            expect![[r#"
                -- errors --
                error: Expected string literal but got '5'
                1 | record User {
                2 |   #[examples(pattern = 5)]
                  |                        ^

                error: Expected integer literal but got '"a"'
                3 |   name: String,
                4 |   #[examples(min = "a", max = 5)]
                  |                    ^^^
                -- ast --
                record User {
                  #[examples()] name: String,
                  #[examples(max = 5)] age: Int,
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_examples_annotation() {
        reject(
            indoc! {"
                fn Main(#[examples(min = 1 count: Int) -> Html {
                  <div>{count}</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ',' but got 'count'
                1 | fn Main(#[examples(min = 1 count: Int) -> Html {
                  |                            ^^^^^

                error: Expected token ']' but got '->'
                1 | fn Main(#[examples(min = 1 count: Int) -> Html {
                  |                                        ^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unexpected_token_inside_opening_tag() {
        reject(
            indoc! {r#"
                fn Main() -> Html {
                  <div class="a" @ id="b">hi</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unexpected character: '@'
                1 | fn Main() -> Html {
                2 |   <div class="a" @ id="b">hi</div>
                  |                  ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [class: "a", id: "b"],
                    children: [text("hi")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_slash_inside_opening_tag() {
        reject(
            indoc! {r#"
                fn Main() -> Html {
                  <div / class="a">hi</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                1 | fn Main() -> Html {
                2 |   <div / class="a">hi</div>
                  |        ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [class: "a"],
                    children: [text("hi")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_opening_tag_ended_by_next_tag() {
        reject(
            indoc! {r#"
                fn Main() -> Html {
                  <div class="a" <span>hi</span>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unterminated opening tag
                1 | fn Main() -> Html {
                2 |   <div class="a" <span>hi</span>
                  |    ^^^

                error: Unclosed <div>
                1 | fn Main() -> Html {
                2 |   <div class="a" <span>hi</span>
                  |    ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [class: "a"],
                    children: [
                      html(
                        tag: "span",
                        attrs: [],
                        children: [text("hi")],
                      ),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unclosed_raw_text_element() {
        reject(
            indoc! {r#"
                fn Main() -> Html {
                  <script>alert(1)
            "#},
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                1 | fn Main() -> Html {
                  |                   ^

                error: Unclosed <script>
                1 | fn Main() -> Html {
                2 |   <script>alert(1)
                  |    ^^^^^^

                error: Inline <script> content is not allowed: move the code to a file and reference it with <script src="...">
                1 | fn Main() -> Html {
                2 |   <script>alert(1)
                  |           ^^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_raw_text_opening_tag() {
        reject(
            "fn Main() -> Html {<style",
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                1 | fn Main() -> Html {<style
                  |                   ^

                error: Unterminated opening tag
                1 | fn Main() -> Html {<style
                  |                     ^^^^^

                error: <style> elements are not allowed: put the CSS in the project stylesheet, or reference it with <link rel="stylesheet">
                1 | fn Main() -> Html {<style
                  |                     ^^^^^

                error: Unclosed <style>
                1 | fn Main() -> Html {<style
                  |                     ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_expression_on_html_element() {
        reject(
            indoc! {"
                fn Main() -> Html {
                  <div {x}>hi</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | fn Main() -> Html {
                2 |   <div {x}>hi</div>
                  |        ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("hi")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_expression_on_function() {
        reject(
            indoc! {"
                fn Main() -> Html {
                  <Card {x} title=\"a\"/>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <Card>: use attribute syntax instead (e.g. attr={value})
                1 | fn Main() -> Html {
                2 |   <Card {x} title="a"/>
                  |         ^^^
                -- ast --
                fn Main() -> Html {
                  Card(attrs: [title: "a"])
                }
            "#]],
        );
    }

    #[test]
    fn rejects_duplicate_expression_on_html_element() {
        reject(
            indoc! {"
                fn Main() -> Html {
                  <div {x} {y}>hi</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | fn Main() -> Html {
                2 |   <div {x} {y}>hi</div>
                  |        ^^^

                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | fn Main() -> Html {
                2 |   <div {x} {y}>hi</div>
                  |            ^^^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("hi")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unparseable_expression_on_html_element() {
        reject(
            indoc! {"
                fn Main() -> Html {
                  <div {x +}>hi</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | fn Main() -> Html {
                2 |   <div {x +}>hi</div>
                  |        ^

                error: Unexpected token '}'
                1 | fn Main() -> Html {
                2 |   <div {x +}>hi</div>
                  |            ^
                -- ast --
                fn Main() -> Html {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("hi")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn fuzz_generated_sources_parse() {
        arbtest::arbtest(|u| {
            let source = source_generator::random_source(u)?;
            let mut errors = Vec::new();
            let document_id = DocumentId::new("test.hop").unwrap();
            parse(
                document_id.clone(),
                Document::new(document_id, source.clone()),
                &mut errors,
            );
            if !errors.is_empty() {
                let rendered = DocumentAnnotator::new()
                    .with_severity_label()
                    .with_lines_before(1)
                    .annotate(errors.iter().map(|e| e.to_diagnostic()))
                    .render();
                panic!("expected no parse errors, got:\n{rendered}\nsource:\n{source}");
            }
            Ok(())
        });
    }

    /// Check that the tree-sitter grammar in `tree-sitter-hop/` accepts every
    /// source the hand-written parser accepts.
    ///
    /// Ignored because it shells out to the `tree-sitter` CLI, which the dev
    /// shell supplies.
    #[test]
    #[ignore]
    fn fuzz_tree_sitter_grammar_accepts_generated_sources() {
        let workspace = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let grammar_dir = workspace.join("tree-sitter-hop");

        let generate = std::process::Command::new("tree-sitter")
            .arg("generate")
            .current_dir(&grammar_dir)
            .output()
            .expect("`tree-sitter` must be on PATH");
        assert!(
            generate.status.success(),
            "tree-sitter generate failed:\n{}{}",
            String::from_utf8_lossy(&generate.stdout),
            String::from_utf8_lossy(&generate.stderr),
        );
        let sample_dir = tempfile::TempDir::new().unwrap();
        let sample = sample_dir.path().join("sample.hop");

        arbtest::arbtest(|u| {
            let source = source_generator::random_source(u)?;
            std::fs::write(&sample, &source).unwrap();
            let output = std::process::Command::new("tree-sitter")
                .arg("parse")
                .arg("--quiet")
                .arg(&sample)
                .current_dir(&grammar_dir)
                .output()
                .expect("`tree-sitter` must be on PATH");
            if !output.status.success() {
                panic!(
                    "tree-sitter failed:\n{}{}\nsource:\n{source}",
                    String::from_utf8_lossy(&output.stdout),
                    String::from_utf8_lossy(&output.stderr),
                );
            }
            Ok(())
        });
    }
}
