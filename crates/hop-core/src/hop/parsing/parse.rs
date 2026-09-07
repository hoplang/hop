use super::parse_expr;
use super::parse_helpers;
use super::parsed_ast::{
    ParsedAst, ParsedComponentDeclaration, ParsedDeclaration, ParsedEnumDeclaration,
    ParsedEnumDeclarationVariant, ParsedFieldDeclaration, ParsedFunctionDeclaration,
    ParsedImportDeclaration, ParsedPageDeclaration, ParsedRecordDeclaration,
};
use super::tokenize_expr;
use crate::document::{CheapString, Document, DocumentCursor, DocumentRange};
use crate::document_id::DocumentId;
use crate::examples_annotation::ExamplesAnnotation;

use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::parse_type::parse_type;
use crate::hop::parsing::parsed_ast::ParsedParameter;
use crate::hop::parsing::parsed_node::ParsedNode;
use crate::hop::parsing::token::LangToken;
use crate::hop::parsing::token::LangTokenPair;
use crate::parse_error::{ErrorEmitted, ParseErrorKind, ParseErrors};
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::{HashSet, VecDeque};
use std::iter::Peekable;

pub fn parse(document_id: DocumentId, document: Document, errors: &mut ParseErrors) -> ParsedAst {
    let cursor = document.cursor();
    let document_range = cursor.range();
    let mut iter = cursor.peekable();
    let mut declarations = Vec::new();
    let mut comments = VecDeque::new();

    loop {
        let pub_range = parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Pub);
        let declaration = if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Import)
        {
            parse_import_declaration(
                &mut iter,
                &mut comments,
                errors,
                &document_range,
                keyword,
                pub_range,
            )
            .map(ParsedDeclaration::Import)
        } else if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Record)
        {
            parse_record_declaration(
                &mut iter,
                &mut comments,
                errors,
                &document_range,
                keyword,
                pub_range,
            )
            .map(ParsedDeclaration::Record)
        } else if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Enum)
        {
            parse_enum_declaration(
                &mut iter,
                &mut comments,
                errors,
                &document_range,
                keyword,
                pub_range,
            )
            .map(ParsedDeclaration::Enum)
        } else if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Component)
        {
            parse_component_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                .map(ParsedDeclaration::Component)
        } else if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::View)
        {
            parse_view_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                .map(ParsedDeclaration::Page)
        } else if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Page)
        {
            parse_page_declaration(&mut iter, &mut comments, errors, keyword, pub_range)
                .map(ParsedDeclaration::Page)
        } else if let Some(keyword) =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, LangToken::Fn)
        {
            parse_function_declaration(
                &mut iter,
                &mut comments,
                errors,
                &document_range,
                keyword,
                pub_range,
            )
            .map(ParsedDeclaration::Function)
        } else {
            if let Some(pub_range) = pub_range {
                let _ = errors.emit(ParseErrorKind::UnexpectedPubKeyword {}, pub_range);
            }
            // Consume rather than peek: at end of input this is the only
            // chance to collect trailing comments and lexer errors.
            let Some((_, token_range)) = tokenize_expr::next(&mut iter, &mut comments, errors)
            else {
                break;
            };
            let reported = errors.emit(ParseErrorKind::UnexpectedTopLevelText {}, token_range);
            parse_helpers::skip_to(&mut iter, reported, |token| {
                parse_helpers::DECLARATION_KEYWORDS.contains(token)
            });
            continue;
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
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedImportDeclaration, ErrorEmitted> {
    if let Some(pub_range) = pub_range {
        let _ = errors.emit(ParseErrorKind::UnexpectedPubKeyword {}, pub_range);
    }
    let mut last_segment = match tokenize_expr::next(iter, comments, errors) {
        Some((LangToken::Identifier(_) | LangToken::TypeName(_), segment)) => segment,
        Some((_, range)) => {
            return Err(errors.emit(ParseErrorKind::ExpectedModulePath {}, range));
        }
        None => {
            return Err(errors.emit(ParseErrorKind::ExpectedModulePath {}, eof_range.clone()));
        }
    };
    let mut module_path: Option<DocumentRange> = None;
    while parse_helpers::advance_if(iter, comments, errors, LangToken::ColonColon).is_some() {
        let segment = match tokenize_expr::next(iter, comments, errors) {
            Some((LangToken::Identifier(_) | LangToken::TypeName(_), segment)) => segment,
            Some((_, range)) => {
                return Err(
                    errors.emit(ParseErrorKind::ExpectedIdentifierAfterColonColon {}, range)
                );
            }
            None => {
                return Err(errors.emit(
                    ParseErrorKind::ExpectedIdentifierAfterColonColon {},
                    eof_range.clone(),
                ));
            }
        };
        module_path = Some(match module_path {
            Some(module_path) => module_path.to(last_segment),
            None => last_segment,
        });
        last_segment = segment;
    }
    let Some(module_path_range) = module_path else {
        return Err(errors.emit(ParseErrorKind::ImportPathTooShort {}, last_segment));
    };
    let type_name_range = last_segment;
    let type_name = match TypeName::from_cheap_string(type_name_range.to_cheap_string()) {
        Ok(name) => name,
        Err(e) => {
            return Err(errors.emit(
                ParseErrorKind::InvalidTypeName { error: e },
                type_name_range,
            ));
        }
    };
    let module_name = match ModuleName::new(module_path_range.as_str()) {
        Ok(name) => name,
        Err(e) => {
            return Err(errors.emit(
                ParseErrorKind::InvalidModuleName { error: e },
                module_path_range.clone(),
            ));
        }
    };
    Ok(ParsedImportDeclaration {
        type_name,
        path_range: module_path_range.to(type_name_range.clone()),
        import_range: keyword_range.to(type_name_range.clone()),
        type_name_range,
        module_name,
    })
}

fn parse_record_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedRecordDeclaration, ErrorEmitted> {
    let (name, name_range) = parse_helpers::expect_type_name(iter, comments, errors, eof_range)?;
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
    let (fields, braces) = parse_field_declarations(iter, comments, errors, &left_brace)?;
    Ok(ParsedRecordDeclaration {
        name,
        name_range,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(braces),
        fields,
        pub_range,
    })
}

fn parse_enum_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedEnumDeclaration, ErrorEmitted> {
    let (name, name_range) = parse_helpers::expect_type_name(iter, comments, errors, eof_range)?;
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
    let mut seen_names = HashSet::new();
    let (variants, braces) = parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Braces,
        &left_brace,
        &[],
        |iter, comments, errors, range| {
            let (variant_name, variant_range) =
                parse_helpers::expect_type_name(iter, comments, errors, range)?;
            if !seen_names.insert(variant_range.to_cheap_string()) {
                return Err(errors.emit(
                    ParseErrorKind::DuplicateVariant {
                        name: variant_range.to_cheap_string(),
                    },
                    variant_range,
                ));
            }
            let fields =
                match parse_helpers::advance_if(iter, comments, errors, LangToken::LeftBrace) {
                    Some(left_brace) => {
                        parse_field_declarations(iter, comments, errors, &left_brace).ok()
                    }
                    None => None,
                };
            Ok(ParsedEnumDeclarationVariant {
                name: variant_name,
                name_range: variant_range,
                fields: fields.map(|(f, _)| f).unwrap_or_else(Vec::new),
            })
        },
    )?;
    Ok(ParsedEnumDeclaration {
        name,
        name_range,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(braces),
        variants,
        pub_range,
    })
}

fn parse_field_declarations(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    left_brace: &DocumentRange,
) -> Result<(Vec<ParsedFieldDeclaration>, DocumentRange), ErrorEmitted> {
    let mut seen_names = HashSet::new();
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        left_brace,
        LangTokenPair::Braces,
        left_brace,
        &[],
        |iter, comments, errors, range| {
            let examples =
                match parse_helpers::advance_if(iter, comments, errors, LangToken::HashBracket) {
                    Some(hash_bracket) => {
                        let (examples, _) =
                            parse_examples_annotation(iter, comments, errors, range, hash_bracket)?;
                        Some(examples)
                    }
                    None => None,
                };
            let (name, name_range) =
                parse_helpers::expect_field_name(iter, comments, errors, range)?;
            parse_helpers::expect_token(iter, comments, errors, range, &LangToken::Colon)?;
            let field_type = parse_type(iter, comments, errors, range)?;
            if !seen_names.insert(name_range.to_cheap_string()) {
                return Err(errors.emit(
                    ParseErrorKind::DuplicateField {
                        name: name_range.to_cheap_string(),
                    },
                    name_range,
                ));
            }
            Ok(ParsedFieldDeclaration {
                name,
                name_range,
                field_type,
                examples,
            })
        },
    )
}

fn parse_component_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedComponentDeclaration, ErrorEmitted> {
    let (name_str, name_range) = match tokenize_expr::next(iter, comments, errors) {
        Some((LangToken::TypeName(name_str), range)) => (name_str, range),
        Some((actual, range)) => {
            return Err(errors.emit(ParseErrorKind::ExpectedTypeNameButGot { actual }, range));
        }
        None => {
            return Err(errors.emit(ParseErrorKind::ExpectedTypeNameButGotEof {}, keyword_range));
        }
    };
    let parsed_params =
        match parse_helpers::advance_if(iter, comments, errors, LangToken::LeftParen) {
            Some(left_paren) => Some(parse_parameters(iter, comments, errors, &left_paren)?),
            None => None,
        };
    let mut params = Vec::new();
    let mut params_range = None;
    let mut rest_param: Option<(VarName, DocumentRange)> = None;
    if let Some((items, range)) = parsed_params {
        params_range = Some(range);
        let count = items.len();
        for (index, item) in items.into_iter().enumerate() {
            match item {
                ParameterItem::Parameter(parameter) => params.push(*parameter),
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
    }
    let (body, body_end) = parse_declaration_body(iter, comments, errors, &name_range)?;
    let component_name = match TypeName::new(&name_str) {
        Ok(n) => n,
        Err(error) => {
            return Err(errors.emit(ParseErrorKind::InvalidTypeName { error }, name_range));
        }
    };
    Ok(ParsedComponentDeclaration {
        component_name,
        name_range,
        params,
        params_range,
        rest_param,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(body_end),
        body,
        pub_range,
    })
}

fn parse_page_or_view_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    keyword_range: &DocumentRange,
) -> Result<(TypeName, DocumentRange, Vec<ParsedParameter>, DocumentRange), ErrorEmitted> {
    let (name_str, name_range) = match tokenize_expr::next(iter, comments, errors) {
        Some((LangToken::TypeName(name_str), range)) => (name_str, range),
        Some((actual, range)) => {
            return Err(errors.emit(ParseErrorKind::ExpectedTypeNameButGot { actual }, range));
        }
        None => {
            return Err(errors.emit(
                ParseErrorKind::ExpectedTypeNameButGotEof {},
                keyword_range.clone(),
            ));
        }
    };

    let (params, params_range) =
        match parse_helpers::advance_if(iter, comments, errors, LangToken::LeftParen) {
            Some(left_paren) => {
                let (items, parens) = parse_parameters(iter, comments, errors, &left_paren)?;
                let mut params = Vec::new();
                for item in items {
                    match item {
                        ParameterItem::Parameter(parameter) => {
                            if let Some(value) = &parameter.default_value {
                                let _ = errors.emit(
                                    ParseErrorKind::DefaultValueNotAllowedOnView {},
                                    value.range().clone(),
                                );
                            }
                            params.push(*parameter);
                        }
                        ParameterItem::Rest { range, .. } => {
                            let _ =
                                errors.emit(ParseErrorKind::RestParamNotAllowedOnView {}, range);
                        }
                    }
                }
                (params, parens)
            }
            None => (Vec::new(), name_range.clone()),
        };

    let name = match TypeName::new(&name_str) {
        Ok(name) => name,
        Err(error) => {
            return Err(errors.emit(ParseErrorKind::InvalidTypeName { error }, name_range));
        }
    };

    Ok((name, name_range, params, params_range))
}

fn parse_view_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedPageDeclaration, ErrorEmitted> {
    let (name, name_range, params, params_range) =
        parse_page_or_view_header(iter, comments, errors, &keyword_range)?;
    let (body, body_end) = parse_declaration_body(iter, comments, errors, &params_range)?;
    Ok(ParsedPageDeclaration {
        name,
        name_range,
        params,
        head: None,
        body,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(body_end),
        pub_range,
        is_view: true,
    })
}

fn parse_page_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedPageDeclaration, ErrorEmitted> {
    let (name, name_range, params, params_range) =
        parse_page_or_view_header(iter, comments, errors, &keyword_range)?;
    let outer_body_start =
        parse_helpers::expect_token(iter, comments, errors, &params_range, &LangToken::LeftBrace)?;
    let head = if let Some((_, head_keyword_range)) =
        parse_helpers::next_if_map(iter, comments, errors, |token| {
            token.identifier().filter(|word| word.as_str() == "head")
        }) {
        let (head, _) = parse_declaration_body(iter, comments, errors, &head_keyword_range)?;
        Some(head)
    } else {
        None
    };
    let Some((_, body_keyword_range)) =
        parse_helpers::next_if_map(iter, comments, errors, |token| {
            token.identifier().filter(|word| word.as_str() == "body")
        })
    else {
        let range = match tokenize_expr::peek(iter) {
            Some((_, range)) => range,
            None => name_range,
        };
        return Err(errors.emit(ParseErrorKind::ExpectedPageBodyBlock {}, range));
    };
    let (body, _) = parse_declaration_body(iter, comments, errors, &body_keyword_range)?;
    let right_brace = parse_helpers::expect_right_delimiter(
        iter,
        comments,
        errors,
        LangTokenPair::Braces,
        &outer_body_start,
    )?;
    Ok(ParsedPageDeclaration {
        name,
        name_range,
        params,
        head,
        body,
        range: pub_range
            .clone()
            .unwrap_or_else(|| keyword_range.clone())
            .to(right_brace),
        pub_range,
        is_view: false,
    })
}

fn parse_function_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    keyword_range: DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Result<ParsedFunctionDeclaration, ErrorEmitted> {
    if let Some(pub_range) = pub_range {
        let _ = errors.emit(ParseErrorKind::UnexpectedPubKeyword {}, pub_range);
    }
    let (name, name_range) =
        parse_helpers::expect_variable_name(iter, comments, errors, eof_range)?;
    let left_paren =
        parse_helpers::expect_token(iter, comments, errors, eof_range, &LangToken::LeftParen)?;
    let (items, _) = parse_parameters(iter, comments, errors, &left_paren)?;
    let mut params = Vec::new();
    for item in items {
        match item {
            ParameterItem::Parameter(parameter) => {
                if let Some(examples_range) = &parameter.examples_range {
                    let _ = errors.emit(
                        ParseErrorKind::ExamplesNotAllowedOnFunction {},
                        examples_range.clone(),
                    );
                }
                if let Some(value) = &parameter.default_value {
                    let _ = errors.emit(
                        ParseErrorKind::DefaultValueNotAllowedOnFunction {},
                        value.range().clone(),
                    );
                }
                params.push(*parameter);
            }
            ParameterItem::Rest { range, .. } => {
                let _ = errors.emit(ParseErrorKind::RestParamNotAllowedOnFunction {}, range);
            }
        }
    }
    let return_type = match parse_helpers::advance_if(iter, comments, errors, LangToken::Arrow) {
        Some(_) => parse_type(iter, comments, errors, eof_range),
        None => Err(errors.emit(
            ParseErrorKind::FunctionMissingReturnType {
                name: CheapString::new(name.as_str().to_string()),
            },
            name_range.clone(),
        )),
    };
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, eof_range, &LangToken::LeftBrace)?;
    let (body, braces) = parse_helpers::parse_delimited(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Braces,
        &left_brace,
        parse_expr::parse_expr,
    )?;
    Ok(ParsedFunctionDeclaration {
        name,
        name_range,
        params,
        return_type: return_type?,
        body,
        range: keyword_range.to(braces),
    })
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
/// through the `)` that closes it. Returns the items with the range of the
/// parentheses.
fn parse_parameters(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    left_paren: &DocumentRange,
) -> Result<(Vec<ParameterItem>, DocumentRange), ErrorEmitted> {
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        left_paren,
        LangTokenPair::Parens,
        left_paren,
        &[LangToken::LeftBrace, LangToken::Arrow],
        |iter, comments, errors, range| {
            if let Some(dots_range) =
                parse_helpers::advance_if(iter, comments, errors, LangToken::DotDotDot)
            {
                let (var_name, var_name_range) =
                    parse_helpers::expect_variable_name(iter, comments, errors, range)?;
                return Ok(ParameterItem::Rest {
                    range: dots_range.to(var_name_range),
                    var_name,
                });
            }
            let (examples, examples_range) =
                match parse_helpers::advance_if(iter, comments, errors, LangToken::HashBracket) {
                    Some(hash_bracket) => Some(parse_examples_annotation(
                        iter,
                        comments,
                        errors,
                        range,
                        hash_bracket,
                    )?),
                    None => None,
                }
                .unzip();
            let (var_name, var_name_range) =
                parse_helpers::expect_variable_name(iter, comments, errors, range)?;
            parse_helpers::expect_token(iter, comments, errors, range, &LangToken::Colon)?;
            let var_type = parse_type(iter, comments, errors, range)?;
            let default_value =
                if parse_helpers::advance_if(iter, comments, errors, LangToken::Assign).is_some() {
                    Some(parse_expr::parse_primary(iter, comments, errors, range)?)
                } else {
                    None
                };
            Ok(ParameterItem::Parameter(Box::new(ParsedParameter {
                var_name,
                var_name_range,
                var_type,
                default_value,
                examples,
                examples_range,
            })))
        },
    )
}

fn parse_declaration_body(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    range: &DocumentRange,
) -> Result<(ParsedExpr, DocumentRange), ErrorEmitted> {
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, range, &LangToken::LeftBrace)?;
    parse_helpers::parse_delimited(
        iter,
        comments,
        errors,
        &left_brace,
        LangTokenPair::Braces,
        &left_brace,
        |iter, comments, errors, left_brace| {
            if let Some((LangToken::RightBrace, _)) = tokenize_expr::peek(iter) {
                let _ = errors.emit(ParseErrorKind::EmptyBody {}, left_brace.clone());
                return Ok(ParsedExpr::Markup {
                    node: Box::new(ParsedNode::Fragment {
                        children: Vec::new(),
                        range: left_brace.clone(),
                    }),
                });
            }
            parse_expr::parse_expr(iter, comments, errors, left_brace)
        },
    )
}

/// Parse an `#[examples(...)]` annotation from the `#[` the caller has
/// already consumed. Returns the annotation with the range from `#[`
/// through `]`.
fn parse_examples_annotation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut ParseErrors,
    eof_range: &DocumentRange,
    hash_bracket: DocumentRange,
) -> Result<(ExamplesAnnotation, DocumentRange), ErrorEmitted> {
    let Some((name, name_range)) =
        parse_helpers::next_if_map(iter, comments, errors, LangToken::identifier)
    else {
        return Err(match tokenize_expr::peek(iter) {
            Some((token, range)) => errors.emit(ParseErrorKind::UnexpectedToken { token }, range),
            None => errors.emit(ParseErrorKind::UnexpectedEof {}, eof_range.clone()),
        });
    };
    if name.as_str() != "examples" {
        let reported = errors.emit(ParseErrorKind::UnknownAnnotation { name }, name_range);
        // Skip the whole annotation, whatever it holds, so that the parse
        // resumes after its `]` rather than inside it.
        parse_helpers::skip_to(iter, reported, |token| {
            *token == LangToken::RightBracket || parse_helpers::DECLARATION_KEYWORDS.contains(token)
        });
        parse_helpers::advance_if(iter, comments, errors, LangToken::RightBracket);
        return Err(reported);
    }
    let left_paren =
        parse_helpers::expect_token(iter, comments, errors, eof_range, &LangToken::LeftParen)?;
    let mut annotation = ExamplesAnnotation::default();
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        eof_range,
        LangTokenPair::Parens,
        &left_paren,
        &[],
        |iter, comments, errors, range| {
            let Some((key, key_range)) =
                parse_helpers::next_if_map(iter, comments, errors, LangToken::identifier)
            else {
                return Err(match tokenize_expr::peek(iter) {
                    Some((token, range)) => {
                        errors.emit(ParseErrorKind::UnexpectedToken { token }, range)
                    }
                    None => errors.emit(ParseErrorKind::UnexpectedEof {}, range.clone()),
                });
            };
            parse_helpers::expect_token(iter, comments, errors, range, &LangToken::Assign)?;
            if key.as_str() == "pattern" {
                let Some((value, _)) =
                    parse_helpers::next_if_map(iter, comments, errors, |token| match token {
                        LangToken::StringLiteral(value) => Some(value),
                        _ => None,
                    })
                else {
                    return Err(match tokenize_expr::peek(iter) {
                        Some((actual, range)) => errors.emit(
                            ParseErrorKind::ExpectedStringLiteralButGot { actual },
                            range,
                        ),
                        None => errors.emit(ParseErrorKind::UnexpectedEof {}, range.clone()),
                    });
                };
                annotation.pattern = Some(value.to_string());
                return Ok(());
            }
            let slot = match key.as_str() {
                "min" => &mut annotation.min,
                "max" => &mut annotation.max,
                "min_len" => &mut annotation.min_len,
                "max_len" => &mut annotation.max_len,
                _ => {
                    return Err(
                        errors.emit(ParseErrorKind::UnknownExamplesKey { name: key }, key_range)
                    );
                }
            };
            let negative =
                parse_helpers::advance_if(iter, comments, errors, LangToken::Minus).is_some();
            let Some((value, _)) =
                parse_helpers::next_if_map(iter, comments, errors, |token| match token {
                    LangToken::IntLiteral(value) => Some(value),
                    _ => None,
                })
            else {
                return Err(match tokenize_expr::peek(iter) {
                    Some((actual, range)) => {
                        errors.emit(ParseErrorKind::ExpectedIntLiteralButGot { actual }, range)
                    }
                    None => errors.emit(ParseErrorKind::UnexpectedEof {}, range.clone()),
                });
            };
            *slot = Some(if negative { -value } else { value });
            Ok(())
        },
    )?;
    let right_bracket = parse_helpers::expect_right_delimiter(
        iter,
        comments,
        errors,
        LangTokenPair::Brackets,
        &hash_bracket,
    )?;
    Ok((annotation, hash_bracket.to(right_bracket)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn accept(input: &str, expected: Expect) {
        let mut errors = ParseErrors::new();
        let document_id = DocumentId::new("test.hop").unwrap();
        let module = parse(
            document_id.clone(),
            Document::new(document_id, input.to_string()),
            &mut errors,
        );
        if !errors.is_empty() {
            let rendered = DocumentAnnotator::new()
                .with_label("error")
                .with_lines_before(1)
                .annotate(&DocumentId::new("test.hop").unwrap(), errors.clone())
                .render();
            panic!("expected no parse errors, got:\n{rendered}");
        }
        expected.assert_eq(&module.to_string());
    }

    fn reject(input: &str, expected: Expect) {
        let mut errors = ParseErrors::new();
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
            .with_label("error")
            .with_lines_before(1)
            .annotate(&DocumentId::new("test.hop").unwrap(), errors.clone())
            .render();
        let actual = format!("-- errors --\n{rendered}-- ast --\n{module}");
        expected.assert_eq(&actual);
    }

    #[test]
    fn accepts_empty_file() {
        accept("", expect![[""]]);
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
                component Card(children: Fragment) {
                  <div>{children}</div>
                }
            "},
            expect![[r#"
                component Card(children: Fragment) {
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
    fn accepts_pub_on_component() {
        accept(
            indoc! {"
                pub component Button(label: String) {
                  <button>{label}</button>
                }
            "},
            expect![[r#"
                pub component Button(label: String) {
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
    fn accepts_pub_on_view() {
        accept(
            indoc! {"
                pub view Home {
                  <div>hi</div>
                }
            "},
            expect![[r#"
                pub view Home() {
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
    fn rejects_pub_on_import() {
        reject(
            indoc! {"
                pub import other::Foo

                component Main {
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

                component Main {
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
    fn accepts_comment_between_components() {
        accept(
            indoc! {"
                component First {<></>}
                // This is a comment
                component Second {<></>}
            "},
            expect![[r#"
                component First {
                  fragment()
                }

                component Second {
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
    fn accepts_keyword_component_syntax() {
        accept(
            indoc! {"
                component Foo {
                  <div>hello</div>
                }
            "},
            expect![[r#"
                component Foo {
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
    fn accepts_keyword_component_with_params() {
        accept(
            indoc! {"
                component Foo(name: String, count: Int) {
                  <div>{name}</div>
                }
            "},
            expect![[r#"
                component Foo(name: String, count: Int) {
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
    fn accepts_keyword_component_with_trailing_comment() {
        accept(
            indoc! {r#"
                component Button(
                  // The button label
                  label: String,
                  // Whether the button is disabled
                  disabled: Bool = false,
                  // More params to come
                ) {
                  <>{label}</>
                }
            "#},
            expect![[r#"
                component Button(label: String, disabled: Bool = false) {
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
                component Main(i: Array[S]) {
                    <>
                        <for {j in i}>
                            <for {k in j.s.t}>
                                <if {k}>
                                </if>
                            </for>
                        </for>
                        <for {p in i}>
                            <for {k in p.s.t}>
                                <for {item in k}>
                                </for>
                            </for>
                        </for>
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

                component Main(i: Array[S]) {
                  fragment(
                    for j in i {
                      for k in j.s.t { if k {} },
                    },
                    for p in i {
                      for k in p.s.t {
                        for item in k {},
                      },
                    },
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_form_with_inputs() {
        accept(
            indoc! {r#"
                component Main {
                    <form id="form">
                        <input type="text" required>
                        <button type="submit">Send</button>
                    </form>
                }
            "#},
            expect![[r#"
                component Main {
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
                component Main {
                    <><p>one</p><p>two</p></>
                }
            "},
            expect![[r#"
                component Main {
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
                component Main {
                    <></>
                }
            "},
            expect![[r#"
                component Main {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_fragments() {
        accept(
            indoc! {"
                component Main {
                    <><>one</>two</>
                }
            "},
            expect![[r#"
                component Main {
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
                component Main {
                    <div><>one</></div>
                }
            "},
            expect![[r#"
                component Main {
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
                component Main {
                    <>one</ >
                }
            "},
            expect![[r#"
                component Main {
                  fragment(text("one"))
                }
            "#]],
        );
    }

    #[test]
    fn accepts_fragment_in_raw_text_as_text() {
        accept(
            indoc! {"
                component Main {
                    <script><></script>
                }
            "},
            expect![[r#"
                component Main {
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
    fn rejects_when_tags_are_not_closed() {
        reject(
            indoc! {"
                component Main {
                    <div>
                    <p>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | component Main {
                2 |     <div>
                  |      ^^^

                error: Unclosed <p>
                2 |     <div>
                3 |     <p>
                  |      ^
                -- ast --
                component Main {
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
                component Card(title: String) {
                  <div>{title}</div>
                }
                view Home {
                  <Card title="hi"/>
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

                component Card(title: String) {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }

                view Home() {
                  Card(attrs: [title: "hi"])
                }
            "#]],
        );
    }

    #[test]
    fn recovers_after_broken_expression_inside_a_body() {
        reject(
            indoc! {"
                component First {
                  <div>{1 +}</div>
                }
                component Second {
                  <></>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected token '}'
                1 | component First {
                2 |   <div>{1 +}</div>
                  |            ^
                -- ast --
                component First {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [],
                  )
                }

                component Second {
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
                component Whole {
                  <></>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ')' but got 'component'
                1 | fn broken(x: Int
                2 | component Whole {
                  | ^^^^^^^^^
                -- ast --
                component Whole {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_closing_tag_closes_an_outer_tag() {
        reject(
            indoc! {"
                component Main {
                    <div><span></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <span>
                1 | component Main {
                2 |     <div><span></div>
                  |           ^^^^
                -- ast --
                component Main {
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
                component Main {
                    <div><span><><b></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <span>
                1 | component Main {
                2 |     <div><span><><b></div>
                  |           ^^^^

                error: Unclosed <>
                1 | component Main {
                2 |     <div><span><><b></div>
                  |                ^^

                error: Unclosed <b>
                1 | component Main {
                2 |     <div><span><><b></div>
                  |                   ^
                -- ast --
                component Main {
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
                component Main {
                    <div></p></></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unmatched </p>
                1 | component Main {
                2 |     <div></p></></div>
                  |          ^^^^

                error: Unmatched </>
                1 | component Main {
                2 |     <div></p></></div>
                  |              ^^^
                -- ast --
                component Main {
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
                component Main {
                    <div></div></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                1 | component Main {
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
                component Main {
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected an expression: use <></> for an empty body
                1 | component Main {
                  |                ^
                -- ast --
                component Main {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_several_roots_in_a_component_body() {
        reject(
            indoc! {"
                component Main {
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
    fn rejects_several_roots_in_a_view_body() {
        reject(
            indoc! {"
                view Main {
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
    fn rejects_several_roots_in_a_page_head_and_body() {
        reject(
            indoc! {"
                page Main {
                    head {
                        <title>one</title>
                        <meta charset=\"utf-8\"/>
                    }
                    body {
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
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_text_beside_an_expression_inside_a_fragment() {
        accept(
            indoc! {"
                component Greeting(name: String) {
                    <>Hello, {name}!</>
                }
            "},
            expect![[r#"
                component Greeting(name: String) {
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
                component Main {
                    <>one
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <>
                1 | component Main {
                2 |     <>one
                  |     ^^
                -- ast --
                component Main {
                  fragment(text("one"))
                }
            "#]],
        );
    }

    #[test]
    fn rejects_closing_fragment_for_a_fragment_that_was_never_opened() {
        reject(
            indoc! {"
                component Main {
                    <div></></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unmatched </>
                1 | component Main {
                2 |     <div></></div>
                  |          ^^^
                -- ast --
                component Main {
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
                component Main {
                    <><div></>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | component Main {
                2 |     <><div></>
                  |        ^^^
                -- ast --
                component Main {
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
                component Main {
                    < >
                }
            "},
            expect![[r#"
                -- errors --
                error: Unterminated tag start
                1 | component Main {
                2 |     < >
                  |     ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_fragment_as_a_child_of_match() {
        reject(
            indoc! {"
                component Main {
                    <match {x}><>one</></match>
                }
            "},
            expect![[r#"
                -- errors --
                error: Only <case> tags are allowed inside <match>
                1 | component Main {
                2 |     <match {x}><>one</></match>
                  |                ^^^^^^^^
                -- ast --
                component Main {
                  match x {}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_case_wrapped_in_a_fragment() {
        reject(
            indoc! {"
                component Main {
                    <match {x}><><case {None}>one</case></></match>
                }
            "},
            expect![[r#"
                -- errors --
                error: Only <case> tags are allowed inside <match>
                1 | component Main {
                2 |     <match {x}><><case {None}>one</case></></match>
                  |                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                error: <case> is only allowed inside <match>
                1 | component Main {
                2 |     <match {x}><><case {None}>one</case></></match>
                  |                   ^^^^
                -- ast --
                component Main {
                  match x {}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_when_void_tag_is_closed_with_closing_tag() {
        reject(
            indoc! {"
                component Main {
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
                component Main {
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
                component Main {
                    <>
                        <hr/>
                        <br/>
                        <input/>
                    </>
                }
                component Foo {
                    <></>
                }
            "#},
            expect![[r#"
                import bar::Bar

                component Main {
                  fragment(
                    html(tag: "hr", attrs: []),
                    html(tag: "br", attrs: []),
                    html(tag: "input", attrs: []),
                  )
                }

                component Foo {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unquoted_attribute_value() {
        reject(
            "component Main {<div class=foo></div>}",
            expect![[r#"
                -- errors --
                error: Expected quoted attribute value or expression
                1 | component Main {<div class=foo></div>}
                  |                      ^^^^^^
                -- ast --
                component Main {
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
            "component Main {<input type='number'/>}",
            expect![[r#"
                -- errors --
                error: Single-quoted attribute values are not supported: use double quotes
                1 | component Main {<input type='number'/>}
                  |                             ^^^^^^^^
                -- ast --
                component Main {
                  html(tag: "input", attrs: [])
                }
            "#]],
        );
    }

    #[test]
    fn rejects_invalid_markup_declaration() {
        reject(
            "component Main {<!foo>}",
            expect![[r#"
                -- errors --
                error: Invalid markup declaration
                1 | component Main {<!foo>}
                  |                 ^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_comment() {
        reject(
            "component Main {<!--",
            expect![[r#"
                -- errors --
                error: Unterminated comment
                1 | component Main {<!--
                  |                 ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_opening_tag() {
        reject(
            "component Main {<div <div>}",
            expect![[r#"
                -- errors --
                error: Unterminated opening tag
                1 | component Main {<div <div>}
                  |                  ^^^

                error: Unclosed <div>
                1 | component Main {<div <div>}
                  |                  ^^^

                error: Unclosed <div>
                1 | component Main {<div <div>}
                  |                       ^^^
                -- ast --
                component Main {
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
            "component Main {<div></div }",
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | component Main {<div></div }
                  |                  ^^^

                error: Unterminated closing tag
                1 | component Main {<div></div }
                  |                        ^^^
                -- ast --
                component Main {
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
            r#"component Main {<div class="foo" class="bar"></div>}"#,
            expect![[r#"
                -- errors --
                error: Duplicate attribute 'class'
                1 | component Main {<div class="foo" class="bar"></div>}
                  |                                  ^^^^^
                -- ast --
                component Main {
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
            "component Main {<div ...>text</div>}",
            expect![[r#"
                -- errors --
                error: Missing variable name for spread
                1 | component Main {<div ...>text</div>}
                  |                      ^^^
                -- ast --
                component Main {
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
            "component Main {< div>}",
            expect![[r#"
                -- errors --
                error: Unterminated tag start
                1 | component Main {< div>}
                  |                 ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_doctype_tags_inside_components() {
        reject(
            indoc! {"
                component Main(foo: String) {
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
                1 | component Main(foo: String) {
                2 |     <!DOCTYPE html>
                  |     ^^^^^^^^^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_expression_is_missing_in_if_tag() {
        reject(
            indoc! {"
                component Main {
                    <if>
                        <div>Content</div>
                    </if>
                }
            "},
            expect![[r#"
                -- errors --
                error: Missing expression in <if> tag
                1 | component Main {
                2 |     <if>
                  |     ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_expression_is_missing_in_for_tag() {
        reject(
            indoc! {"
                component Main {
                    <for>
                        <div>Content</div>
                    </for>
                }
            "},
            expect![[r#"
                -- errors --
                error: Missing loop generator expression in <for> tag
                1 | component Main {
                2 |     <for>
                  |     ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_for_tag_has_invalid_expression() {
        reject(
            indoc! {"
                component Main {
                    <for {foo}>
                        <div>Content</div>
                    </for>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token 'in' but got '}'
                1 | component Main {
                2 |     <for {foo}>
                  |              ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_if_tag_has_invalid_expression() {
        reject(
            indoc! {"
                component Main {
                    <if {~}>
                        <div>Content</div>
                    </if>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '~'
                1 | component Main {
                2 |     <if {~}>
                  |          ^

                error: Unexpected token '}'
                1 | component Main {
                2 |     <if {~}>
                  |           ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_when_component_parameter_has_parse_error_in_type_name() {
        reject(
            indoc! {"
                component Main(data: Array[) {
                    <div>{data}</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected type name but got ')'
                1 | component Main(data: Array[) {
                  |                            ^
                -- ast --
                component Main {
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
                component Main(user: User) {
                    <a href={user.url} class={user.theme}>Link</a>
                }
            "#},
            expect![[r#"
                record User {
                  url: String,
                  theme: String,
                }

                component Main(user: User) {
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
                error: Expected type name but got 'x'
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
                component Main(style1: String, style2: String, style3: String) {
                    <div class={style1, style2, style3}>Content</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got ','
                1 | component Main(style1: String, style2: String, style3: String) {
                2 |     <div class={style1, style2, style3}>Content</div>
                  |                       ^
                -- ast --
                component Main(style1: String, style2: String, style3: String) {
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

                component Main {
                	<Foo/>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Import path must have at least two segments: module::Component
                1 | import Foo
                  |        ^^^
                -- ast --
                component Main {
                  Foo(attrs: [])
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_invocations() {
        accept(
            indoc! {"
                component Main(p: String) {
                    <>
                        <Foo/>
                        <Foo/>
                    </>
                }
            "},
            expect![[r#"
                component Main(p: String) {
                  fragment(
                    Foo(attrs: []),
                    Foo(attrs: []),
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_component_invocations_with_params() {
        accept(
            indoc! {r#"
                import foo::Foo
                import bar::Bar
                record Data {
                  user: String,
                }
                component Main(data: Data) {
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

                component Main(data: Data) {
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
                component Main(item: Array[String]) {
                    <for {item in items}>
                        <div>Item content</div>
                    </for>
                }
            "},
            expect![[r#"
                component Main(item: Array[String]) {
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
                component Main(foo: Array[String]) {
                    <for {v in foo}>
                        <div>{v}</div>
                    </for>
                }
            "},
            expect![[r#"
                component Main(foo: Array[String]) {
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
                component Main {
                    <for {i in 0..=5}>
                        {i}
                    </for>
                }
            "},
            expect![[r#"
                component Main {
                  for i in 0..=5 { interpolate(i) }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_variable_range_bounds() {
        accept(
            indoc! {"
                component Main(start: Int, end: Int) {
                    <for {x in start..=end}>
                        {x}
                    </for>
                }
            "},
            expect![[r#"
                component Main(start: Int, end: Int) {
                  for x in start..=end {
                    interpolate(x),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_expression_range_bounds() {
        accept(
            indoc! {"
                component Main(count: Int) {
                    <for {i in 1..=count + 1}>
                        {i}
                    </for>
                }
            "},
            expect![[r#"
                component Main(count: Int) {
                  for i in 1..=count + 1 {
                    interpolate(i),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_underscore_binding() {
        accept(
            indoc! {"
                component Main(items: Array[String]) {
                    <for {_ in items}>
                        item
                    </for>
                }
            "},
            expect![[r#"
                component Main(items: Array[String]) {
                  for _ in items { text("item") }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_underscore_and_range() {
        accept(
            indoc! {"
                component Main {
                    <for {_ in 0..=5}>
                        item
                    </for>
                }
            "},
            expect![[r#"
                component Main {
                  for _ in 0..=5 { text("item") }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_for_loop_with_underscore_and_variable_range() {
        accept(
            indoc! {"
                component Main(start: Int, end: Int) {
                    <for {_ in start..=end}>
                        item
                    </for>
                }
            "},
            expect![[r#"
                component Main(start: Int, end: Int) {
                  for _ in start..=end { text("item") }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_if_statement() {
        accept(
            indoc! {"
                component Main(x: Int, y: Int) {
                    <if {x == y}>
                        <div>Equal</div>
                    </if>
                }
            "},
            expect![[r#"
                component Main(x: Int, y: Int) {
                  if x == y {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Equal")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_if_with_two_expressions() {
        reject(
            indoc! {"
                component Main(x: Int, y: Int) {
                    <if {x == 1} {y == 2}>
                        <div>Which</div>
                    </if>
                }
            "},
            expect![[r#"
                -- errors --
                error: <if> already has an expression
                1 | component Main(x: Int, y: Int) {
                2 |     <if {x == 1} {y == 2}>
                  |                  ^^^^^^^^
                -- ast --
                component Main(x: Int, y: Int) {
                  if x == 1 {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Which")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_for_with_two_expressions() {
        reject(
            indoc! {"
                component Main(xs: Array[Int], ys: Array[Int]) {
                    <for {x in xs} {y in ys}>
                        <div>{x}</div>
                    </for>
                }
            "},
            expect![[r#"
                -- errors --
                error: <for> already has an expression
                1 | component Main(xs: Array[Int], ys: Array[Int]) {
                2 |     <for {x in xs} {y in ys}>
                  |                    ^^^^^^^^^
                -- ast --
                component Main(xs: Array[Int], ys: Array[Int]) {
                  for x in xs {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(x)],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_if_statement_with_nested_for_loop() {
        accept(
            indoc! {"
                component Main(x: Bool, data: Array[String]) {
	                <if {x}>
		                <for {d in data}>
                          {d}
		                </for>
	                </if>
                }
            "},
            expect![[r#"
                component Main(x: Bool, data: Array[String]) {
                  if x {
                    for d in data { interpolate(d) },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_html_element() {
        reject(
            indoc! {r#"
                component Main {
                    <dvi>oops</dvi>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unknown HTML element <dvi>
                1 | component Main {
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
                component Main {
                    <math></math>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unknown HTML element <math>
                1 | component Main {
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
                component Main {
                    <my-widget>hi</my-widget>
                }
            "#},
            expect![[r#"
                component Main {
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
                component Main {
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
                component Main {
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
    fn accepts_component_parameter_with_string_type() {
        accept(
            indoc! {"
                component Main(data: String) {
                    <div>{data}</div>
                }
            "},
            expect![[r#"
                component Main(data: String) {
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
    fn accepts_component_parameter_with_record_type() {
        accept(
            indoc! {"
                record Data {
                  message: String,
                }

                component Main(data: Data) {
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

                component Main(data: Data) {
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
    fn accepts_component_parameter_with_array_type() {
        accept(
            indoc! {"
                component Main(items: Array[String]) {
                    <for {item in items}>
                        <div>{item}</div>
                    </for>
                }
            "},
            expect![[r#"
                component Main(items: Array[String]) {
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
    fn accepts_component_parameter_with_array_of_record_type() {
        accept(
            indoc! {"
                record Section {
                  title: String,
                  items: Array[String],
                }

                component Main(data: Array[Section]) {
                    <for {section in data}>
                        <h1>{section.title}</h1>
                        <for {item in section.items}>
                            <div>{item}</div>
                        </for>
                    </for>
                }
            "},
            expect![[r#"
                record Section {
                  title: String,
                  items: Array[String],
                }

                component Main(data: Array[Section]) {
                  for section in data {
                    html(
                      tag: "h1",
                      attrs: [],
                      children: [
                        interpolate(section.title),
                      ],
                    ),
                    for item in section.items {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [interpolate(item)],
                      ),
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_text_with_single_expression() {
        accept(
            "component Main {<h1>Hello {name}!</h1>}",
            expect![[r#"
                component Main {
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
            "component Main {<p>User {user.name} has {user.count} items</p>}",
            expect![[r#"
                component Main {
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
            "component Main {<span>{greeting} world!</span>}",
            expect![[r#"
                component Main {
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
            "component Main {<div>Price: {price}</div>}",
            expect![[r#"
                component Main {
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
            "component Main {<h2>{title}</h2>}",
            expect![[r#"
                component Main {
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
            "component Main {<div>Empty: {}</div>}",
            expect![[r#"
                -- errors --
                error: Unexpected token '}'
                1 | component Main {<div>Empty: {}</div>}
                  |                              ^
                -- ast --
                component Main {
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
            r#"component Main {<p>Status: {user.profile.status == "active"}</p>}"#,
            expect![[r#"
                component Main {
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
            "component Main {<span>{first}{second}</span>}",
            expect![[r#"
                component Main {
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
            r#"component Main {<div>{"<div></div>"}</div>}"#,
            expect![[r#"
                component Main {
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
                component Main {
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected type name but got 'component'
                1 | record
                2 | component Main {
                  | ^^^^^^^^^

                error: Expected an expression: use <></> for an empty body
                1 | record
                2 | component Main {
                  |                ^
                -- ast --
                component Main {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn rejects_unknown_text_before_component() {
        reject(
            indoc! {"
                foo
                component Main {
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected text at top level
                1 | foo
                  | ^^^

                error: Expected an expression: use <></> for an empty body
                1 | foo
                2 | component Main {
                  |                ^
                -- ast --
                component Main {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_in_template() {
        accept(
            indoc! {r#"
                enum Color {Red, Green, Blue}

                component Main(color: Color) {
                    <>{match color {Color::Red => "red", Color::Blue => "blue"}}</>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(color: Color) {
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

                component Main(color: Color) {
                    <div class={match color {Color::Red => "text-red", Color::Blue => "text-blue"}}></div>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(color: Color) {
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

                component Main(status: Status) {
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

                component Main(status: Status) {
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
                component Main(name: String = "World") {
                    <div>{name}</div>
                }
            "#},
            expect![[r#"
                component Main(name: String = "World") {
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
                component Main(count: Int = 42) {
                    <span>{count}</span>
                }
            "},
            expect![[r#"
                component Main(count: Int = 42) {
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
                component Main(enabled: Bool = true) {
                    <div></div>
                }
            "},
            expect![[r#"
                component Main(enabled: Bool = true) {
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
                component Main(name: String, role: String = "user", active: Bool = true) {
                    <div>{name}</div>
                }
            "#},
            expect![[r#"
                component Main(name: String, role: String = "user", active: Bool = true) {
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
                component Main(items: Array[String] = ["a", "b"]) {
                    <for {item in items}>
                        {item}
                    </for>
                }
            "#},
            expect![[r#"
                component Main(items: Array[String] = [
                  "a",
                  "b",
                ]) {
                  for item in items {
                    interpolate(item),
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
                component Main(config: Config = Config {debug: false, timeout: 30}) {
                    <div></div>
                }
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                component Main(config: Config = Config {
                  debug: false,
                  timeout: 30,
                }) {
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
                component Main(status: Status = Status::Active) {
                    <div></div>
                }
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                component Main(status: Status = Status::Active) {
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
                component Main(name: Option[String]) {
                    <div></div>
                }
            "},
            expect![[r#"
                component Main(name: Option[String]) {
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
                component Main(name: Option[String] = None) {
                    <div></div>
                }
            "},
            expect![[r#"
                component Main(name: Option[String] = None) {
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
                component Main(name: Option[String] = Some("default")) {
                    <div></div>
                }
            "#},
            expect![[r#"
                component Main(name: Option[String] = Some("default")) {
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
                component Main(offsets: Array[Int] = [1, 2]) {
                    <div></div>
                }
            "},
            expect![[r#"
                component Main(offsets: Array[Int] = [
                  1,
                  2,
                ]) {
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
                component Main(children: Fragment = <></>) {
                    <div></div>
                }
            "},
            expect![[r#"
                component Main(children: Fragment = fragment()) {
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
                component Main(x: Int = = 1, y: Int) {
                    <div></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected token '='
                1 | component Main(x: Int = = 1, y: Int) {
                  |                         ^
                -- ast --
                component Main(y: Int) {
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
    fn accepts_self_closing_match_with_no_cases() {
        accept(
            "component Main(x: Option[String]) {<match {x}/>}\n",
            expect![[r#"
                component Main(x: Option[String]) {
                  match x {}
                }
            "#]],
        );
    }

    #[test]
    fn accepts_self_closing_case_with_no_children() {
        accept(
            indoc! {r#"
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(y)}>found {y}</case>
                        <case {None}/>
                    </match>
                }
            "#},
            expect![[r#"
                component Main(x: Option[String]) {
                  match x {
                    Some(y) => {
                      text("found "),
                      interpolate(y),
                    },
                    None => {},
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_option_cases() {
        accept(
            indoc! {r#"
                component Main(x: Option[String]) {
                    <match {x}>
                        <case {Some(y)}>
                            found {y}
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                component Main(x: Option[String]) {
                  match x {
                    Some(y) => {
                      text("found "),
                      interpolate(y),
                    },
                    None => { text("nothing") },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_enum_cases() {
        accept(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                component Main(c: Color) {
                    <match {c}>
                        <case {Color::Red}>red</case>
                        <case {Color::Green}>green</case>
                        <case {Color::Blue}>blue</case>
                    </match>
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(c: Color) {
                  match c {
                    Color::Red => { text("red") },
                    Color::Green => { text("green") },
                    Color::Blue => { text("blue") },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_enum_variant_fields() {
        accept(
            indoc! {r#"
                enum Outcome { Success {value: Int}, Failure {message: String} }
                component Main(r: Outcome) {
                    <match {r}>
                        <case {Outcome::Success{value: v}}>
                            Success: {v}
                        </case>
                        <case {Outcome::Failure{message: m}}>
                            Error: {m}
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                enum Outcome {
                  Success { value: Int },
                  Failure { message: String },
                }

                component Main(r: Outcome) {
                  match r {
                    Outcome::Success{value: v} => {
                      text("Success: "),
                      interpolate(v),
                    },
                    Outcome::Failure{message: m} => {
                      text("Error: "),
                      interpolate(m),
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_on_enum_literal_expression() {
        accept(
            indoc! {r#"
                enum Status { Active {name: String}, Inactive }
                component Main {
                    <match {Status::Active {name: "test"}}>
                        <case {Status::Active{name: n}}>
                            {n}
                        </case>
                        <case {Status::Inactive}>
                            none
                        </case>
                    </match>
                }
            "#},
            expect![[r#"
                enum Status {
                  Active { name: String },
                  Inactive,
                }

                component Main {
                  match Status::Active {name: "test"} {
                    Status::Active{name: n} => {
                      interpolate(n),
                    },
                    Status::Inactive => {
                      text("none"),
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_match_with_boolean_cases() {
        accept(
            indoc! {r#"
                component Main(flag: Bool) {
                    <match {flag}>
                        <case {true}>yes</case>
                        <case {false}>no</case>
                    </match>
                }
            "#},
            expect![[r#"
                component Main(flag: Bool) {
                  match flag {
                    true => { text("yes") },
                    false => { text("no") },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_on_match_without_expression() {
        reject(
            indoc! {r#"
                component Main {
                    <match>
                        <case {true}>yes</case>
                    </match>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Missing expression in <match> tag
                1 | component Main {
                2 |     <match>
                  |     ^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_on_case_without_pattern() {
        reject(
            indoc! {r#"
                component Main(flag: Bool) {
                    <match {flag}>
                        <case>yes</case>
                    </match>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Missing pattern in <case> tag
                2 |     <match {flag}>
                3 |         <case>yes</case>
                  |         ^^^^^^
                -- ast --
                component Main(flag: Bool) {
                  match flag {}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_on_non_case_children_in_match() {
        reject(
            indoc! {r#"
                component Main(flag: Bool) {
                    <match {flag}>
                        <div>not allowed</div>
                    </match>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Only <case> tags are allowed inside <match>
                2 |     <match {flag}>
                3 |         <div>not allowed</div>
                  |         ^^^^^^^^^^^^^^^^^^^^^^
                -- ast --
                component Main(flag: Bool) {
                  match flag {}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_case_outside_match() {
        reject(
            indoc! {r#"
                component Main {
                    <case {true}>standalone case</case>
                }
            "#},
            expect![[r#"
                -- errors --
                error: <case> is only allowed inside <match>
                1 | component Main {
                2 |     <case {true}>standalone case</case>
                  |      ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_string_value() {
        accept(
            indoc! {r#"
                component Main {
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  let name: String = "World" in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        text("Hello "),
                        interpolate(name),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_int_value() {
        accept(
            indoc! {"
                component Main {
                    <let {count: Int = 42}>
                        <span>{count}</span>
                    </let>
                }
            "},
            expect![[r#"
                component Main {
                  let count: Int = 42 in {
                    html(
                      tag: "span",
                      attrs: [],
                      children: [interpolate(count)],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_expression_value() {
        accept(
            indoc! {r#"
                record User { name: String }
                component Main(user: User) {
                    <let {greeting: String = user.name}>
                        <div>{greeting}</div>
                    </let>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                }

                component Main(user: User) {
                  let greeting: String = user.name in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(greeting)],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_nested_let_tags() {
        accept(
            indoc! {r#"
                component Main {
                    <let {a: Int = 1}>
                        <let {b: Int = 2}>
                            <div>{a} + {b}</div>
                        </let>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  let a: Int = 1 in {
                    let b: Int = 2 in {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [
                          interpolate(a),
                          text(" + "),
                          interpolate(b),
                        ],
                      ),
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_without_binding() {
        reject(
            indoc! {"
                component Main {
                    <let>
                        <div>Content</div>
                    </let>
                }
            "},
            expect![[r#"
                -- errors --
                error: Missing binding in <let> tag
                1 | component Main {
                2 |     <let>
                  |     ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_omitted_type() {
        accept(
            indoc! {"
                component Main {
                    <let {x = 1}>
                        <div>Content</div>
                    </let>
                }
            "},
            expect![[r#"
                component Main {
                  let x = 1 in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Content")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_with_no_bindings() {
        reject(
            indoc! {"
                component Main {
                    <let {}>
                        <div>Content</div>
                    </let>
                }
            "},
            expect![[r#"
                -- errors --
                error: Missing binding in <let> tag
                1 | component Main {
                2 |     <let {}>
                  |          ^^
                -- ast --
                component Main {
                  let  in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Content")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_with_missing_value() {
        reject(
            indoc! {"
                component Main {
                    <let {x: String}>
                        <div>Content</div>
                    </let>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '=' but got '}'
                1 | component Main {
                2 |     <let {x: String}>
                  |                    ^
                -- ast --
                component Main {
                  let  in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Content")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_multiple_bindings() {
        accept(
            indoc! {r#"
                component Main {
                    <let {first: String = "Hello", second: String = "World"}>
                        <div>{first} {second}</div>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  let first: String = "Hello", second: String = "World" in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(first),
                        text(" "),
                        interpolate(second),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_three_bindings() {
        accept(
            indoc! {r#"
                component Main {
                    <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                        <div>{a} + {b} + {c}</div>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  let a: Int = 1, b: Int = 2, c: Int = 3 in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(a),
                        text(" + "),
                        interpolate(b),
                        text(" + "),
                        interpolate(c),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_trailing_comma() {
        accept(
            indoc! {r#"
                component Main {
                    <let {name: String = "World",}>
                        <div>Hello {name}</div>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  let name: String = "World" in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        text("Hello "),
                        interpolate(name),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_multiple_bindings_and_trailing_comma() {
        accept(
            indoc! {r#"
                component Main {
                    <let {first: String = "Hello", second: String = "World",}>
                        <div>{first} {second}</div>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  let first: String = "Hello", second: String = "World" in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(first),
                        text(" "),
                        interpolate(second),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_with_field_access_value() {
        accept(
            indoc! {r#"
                record User { name: String }
                component Main(user: User) {
                    <let {name: String = user.name}>
                        <div>{name}</div>
                    </let>
                }
            "#},
            expect![[r#"
                record User {
                  name: String,
                }

                component Main(user: User) {
                  let name: String = user.name in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [interpolate(name)],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_with_missing_comma_between_bindings() {
        reject(
            indoc! {r#"
                component Main {
                    <let {first: String = "a" second: String = "b"}>
                        <div>{first} {second}</div>
                    </let>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Expected token ',' but got 'second'
                1 | component Main {
                2 |     <let {first: String = "a" second: String = "b"}>
                  |                               ^^^^^^
                -- ast --
                component Main {
                  let first: String = "a" in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        interpolate(first),
                        text(" "),
                        interpolate(second),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_sibling_let_tags() {
        accept(
            indoc! {r#"
                component Main {
                    <>
                        <let {a: String = "Hello"}>
                            {a}
                        </let>
                        <let {b: String = "World"}>
                            {b}
                        </let>
                    </>
                }
            "#},
            expect![[r#"
                component Main {
                  fragment(
                    let a: String = "Hello" in {
                      interpolate(a),
                    },
                    let b: String = "World" in {
                      interpolate(b),
                    },
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_after_html_element() {
        accept(
            indoc! {r#"
                component Main {
                    <>
                        <div>First</div>
                        <let {name: String = "World"}>
                            <div>Hello {name}</div>
                        </let>
                    </>
                }
            "#},
            expect![[r#"
                component Main {
                  fragment(
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("First")],
                    ),
                    let name: String = "World" in {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [
                          text("Hello "),
                          interpolate(name),
                        ],
                      ),
                    },
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_before_html_element() {
        accept(
            indoc! {r#"
                component Main {
                    <>
                        <let {name: String = "World"}>
                            <div>Hello {name}</div>
                        </let>
                        <div>Last</div>
                    </>
                }
            "#},
            expect![[r#"
                component Main {
                  fragment(
                    let name: String = "World" in {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [
                          text("Hello "),
                          interpolate(name),
                        ],
                      ),
                    },
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
                error: Unexpected text at top level
                1 | <div></div>
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_view_declaration() {
        accept(
            indoc! {"
                view Index() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                view Index() {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("Hello")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_without_parentheses() {
        accept(
            indoc! {"
                view Index {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                view Index() {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("Hello")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_parameters() {
        accept(
            indoc! {"
                view Index(name: String, count: Int) {
                    <div>{name}: {count}</div>
                }
            "},
            expect![[r#"
                view Index(name: String, count: Int) {
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
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_component_invocation() {
        accept(
            indoc! {"
                component Header(title: String) {
                    <h1>{title}</h1>
                }

                view Index(title: String) {
                    <Header title={title} />
                }
            "},
            expect![[r#"
                component Header(title: String) {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }

                view Index(title: String) {
                  Header(attrs: [title: title])
                }
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_views() {
        accept(
            indoc! {"
                view Index() {
                    <div>Index</div>
                }

                view About() {
                    <div>About</div>
                }
            "},
            expect![[r#"
                view Index() {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("Index")],
                  )
                }

                view About() {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("About")],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn rejects_let_binding_with_reserved_name() {
        reject(
            indoc! {r#"
                view Test {
                  <let {default: String = "x"}>
                    <div></div>
                  </let>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Invalid variable name 'default': Variable name is a reserved word
                1 | view Test {
                2 |   <let {default: String = "x"}>
                  |         ^^^^^^^
                -- ast --
                view Test() {
                  let  in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_reserved_name() {
        reject(
            indoc! {"
                view Error() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name 'Error' is a reserved word
                1 | view Error() {
                  |      ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_component_with_reserved_name() {
        reject(
            indoc! {"
                component Error() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name 'Error' is a reserved word
                1 | component Error() {
                  |           ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_component_with_lowercase_name() {
        reject(
            indoc! {"
                component card() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected type name but got 'card'
                1 | component card() {
                  |           ^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_component_invocation_with_invalid_character() {
        reject(
            indoc! {"
                component Card() {
                    <Foo-Bar />
                }
            "},
            expect![[r#"
                -- errors --
                error: Type name contains invalid character: '-'
                1 | component Card() {
                2 |     <Foo-Bar />
                  |      ^^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_lowercase_name() {
        reject(
            indoc! {"
                view index() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected type name but got 'index'
                1 | view index() {
                  |      ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_default_parameter() {
        reject(
            indoc! {r#"
                view Index(name: String = "World") {
                    <div>Hello {name}</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Default values are not allowed on view parameters
                1 | view Index(name: String = "World") {
                  |                           ^^^^^^^
                -- ast --
                view Index(name: String = "World") {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      text("Hello "),
                      interpolate(name),
                    ],
                  )
                }
            "#]],
        );
    }

    #[test]
    fn accepts_page_declaration() {
        accept(
            indoc! {r#"
                page Index(name: String) {
                    head {
                        <title>My page</title>
                    }
                    body {
                        <div>Hello {name}</div>
                    }
                }
            "#},
            expect![[r#"
                page Index(name: String) {
                  head {
                    html(
                      tag: "title",
                      attrs: [],
                      children: [text("My page")],
                    )
                  }
                  body {
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
                    body {
                        <div>Hello</div>
                    }
                }
            "},
            expect![[r#"
                page Index() {
                  body {
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
                    head {
                        <title>My page</title>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected a 'body' block
                4 |     }
                5 | }
                  | ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_page_declaration_with_body_before_head() {
        reject(
            indoc! {"
                page Index() {
                    body {
                        <div>Hello</div>
                    }
                    head {
                        <title>My page</title>
                    }
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got 'head'
                4 |     }
                5 |     head {
                  |     ^^^^

                error: Expected type name but got '<'
                5 |     head {
                6 |         <title>My page</title>
                  |                       ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_a_view_with_an_empty_body() {
        reject(
            indoc! {"
                view Index() {
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected an expression: use <></> for an empty body
                1 | view Index() {
                  |              ^
                -- ast --
                view Index() {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_if_statement() {
        accept(
            indoc! {"
                view Index(show: Bool) {
                    <if {show}>
                        <div>Visible</div>
                    </if>
                }
            "},
            expect![[r#"
                view Index(show: Bool) {
                  if show {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("Visible")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_for_loop() {
        accept(
            indoc! {"
                view Index(items: Array[String]) {
                    <for {item in items}>
                        <div>{item}</div>
                    </for>
                }
            "},
            expect![[r#"
                view Index(items: Array[String]) {
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
    fn accepts_view_with_let_binding() {
        accept(
            indoc! {r#"
                view Index() {
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                }
            "#},
            expect![[r#"
                view Index() {
                  let name: String = "World" in {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [
                        text("Hello "),
                        interpolate(name),
                      ],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_void_elements() {
        accept(
            indoc! {"
                view Index() {
                    <div>
                        <br />
                        <input type=\"text\" />
                        <hr />
                    </div>
                }
            "},
            expect![[r#"
                view Index() {
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
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_trailing_comma_in_params() {
        accept(
            indoc! {"
                view Index(name: String,) {
                    <div>{name}</div>
                }
            "},
            expect![[r#"
                view Index(name: String) {
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
    fn accepts_view_with_match_expression() {
        accept(
            indoc! {"
                view Index(value: Option[String]) {
                    <match {value}>
                        <case {Some(s)}>
                            <div>{s}</div>
                        </case>
                        <case {None}>
                            <div>No value</div>
                        </case>
                    </match>
                }
            "},
            expect![[r#"
                view Index(value: Option[String]) {
                  match value {
                    Some(s) => {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [interpolate(s)],
                      ),
                    },
                    None => {
                      html(
                        tag: "div",
                        attrs: [],
                        children: [text("No value")],
                      ),
                    },
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_nested_components() {
        accept(
            indoc! {"
                component Header(title: String) {
                    <h1>{title}</h1>
                }

                component Footer {
                    <p>Copyright 2024</p>
                }

                view Index(title: String) {
                    <div>
                        <Header title={title} />
                        <main>Content</main>
                        <Footer />
                    </div>
                }
            "},
            expect![[r#"
                component Header(title: String) {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [interpolate(title)],
                  )
                }

                component Footer {
                  html(
                    tag: "p",
                    attrs: [],
                    children: [text("Copyright 2024")],
                  )
                }

                view Index(title: String) {
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
            "#]],
        );
    }

    #[test]
    fn accepts_view_between_components() {
        accept(
            indoc! {"
                component Header {
                    <h1>Header</h1>
                }

                view Index() {
                    <div>Index</div>
                }

                component Footer {
                    <p>Footer</p>
                }
            "},
            expect![[r#"
                component Header {
                  html(
                    tag: "h1",
                    attrs: [],
                    children: [text("Header")],
                  )
                }

                view Index() {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [text("Index")],
                  )
                }

                component Footer {
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
    fn rejects_view_with_multiple_params_mixed_defaults() {
        reject(
            indoc! {r#"
                view Index(required: String, optional: Int = 42) {
                    <div>{required}: {optional}</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Default values are not allowed on view parameters
                1 | view Index(required: String, optional: Int = 42) {
                  |                                              ^^
                -- ast --
                view Index(required: String, optional: Int = 42) {
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
            "#]],
        );
    }

    #[test]
    fn rejects_bare_text_as_a_view_body() {
        reject(
            indoc! {"
                view Test {
                  hello world
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token '}' but got 'world'
                1 | view Test {
                2 |   hello world
                  |         ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn accepts_escape_sequences_in_strings() {
        accept(
            indoc! {r#"
                component Test {
                    <>
                        {"hello\nworld"}
                        {"tab\there"}
                        {"back\\slash"}
                        {"quote\"here"}
                    </>
                }
            "#},
            expect![[r#"
                component Test {
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
    fn rejects_invalid_escape_sequences_in_strings() {
        reject(
            r#"fn test() -> String {"invalid\q"}"#,
            expect![[r#"
                -- errors --
                error: Invalid escape sequence '\q'
                1 | fn test() -> String {"invalid\q"}
                  |                              ^^
                -- ast --
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
                component Foo(class: String, ...rest) {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                component Foo(class: String, ...rest) {
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
                component Foo(...rest, a: String, b: String) {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Rest parameter must be the last parameter
                1 | component Foo(...rest, a: String, b: String) {
                  |               ^^^^^^^
                -- ast --
                component Foo(a: String, b: String, ...rest) {
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
                component Foo(...a, ...b) {
                  <div ...a></div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Rest parameter must be the last parameter
                1 | component Foo(...a, ...b) {
                  |               ^^^^

                error: At most one rest parameter is allowed
                1 | component Foo(...a, ...b) {
                  |                     ^^^^
                -- ast --
                component Foo(...a) {
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
                component Foo(...rest) {
                  <button ...rest></button>
                }
            "#},
            expect![[r#"
                component Foo(...rest) {
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
    fn accepts_spread_attribute_on_component() {
        accept(
            indoc! {r#"
                component Bar(...rest) {
                  <Foo ...rest></Foo>
                }
            "#},
            expect![[r#"
                component Bar(...rest) {
                  Foo(attrs: [...rest], children: [])
                }
            "#]],
        );
    }

    #[test]
    fn rejects_spread_attribute_with_uppercase_name() {
        reject(
            indoc! {r#"
                component Foo() {
                  <button ...Bar></button>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Invalid variable name 'Bar': Variable name must be lowercase (found uppercase: 'B')
                1 | component Foo() {
                2 |   <button ...Bar></button>
                  |              ^^^
                -- ast --
                component Foo {
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
                component Foo() {
                  <button ..._x></button>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Invalid variable name '_x': Variable name cannot start with underscore
                1 | component Foo() {
                2 |   <button ..._x></button>
                  |              ^^
                -- ast --
                component Foo {
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

                component Foo {
                  <div>
                    <for {x in 0..=foo(10)}>
                      {x.to_string()}
                    </for>
                    {foo(10)}
                  </div>
                }
            "},
            expect![[r#"
                fn foo(x: Int) -> Int {
                  x + 10
                }

                component Foo {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [
                      for x in 0..=foo(10) {
                        interpolate(x.to_string()),
                      },
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
    fn rejects_pub_on_function() {
        reject(
            indoc! {"
                pub fn foo(x: Int) -> Int {
                  x
                }
            "},
            expect![[r#"
                -- errors --
                error: 'pub' is not allowed here
                1 | pub fn foo(x: Int) -> Int {
                  | ^^^
                -- ast --
                fn foo(x: Int) -> Int {
                  x
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
                error: Function 'foo' is missing a return type
                1 | fn foo(x: Int) {
                  |    ^^^
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
    fn rejects_default_value_on_function_parameter() {
        reject(
            indoc! {"
                fn foo(x: Int = 1) -> Int {
                  x
                }
            "},
            expect![[r#"
                -- errors --
                error: Default values are not allowed on function parameters
                1 | fn foo(x: Int = 1) -> Int {
                  |                 ^
                -- ast --
                fn foo(x: Int = 1) -> Int {
                  x
                }
            "#]],
        );
    }

    #[test]
    fn rejects_rest_param_on_function() {
        reject(
            indoc! {"
                fn foo(...rest) -> Int {
                  1
                }
            "},
            expect![[r#"
                -- errors --
                error: Rest parameters are not allowed on functions
                1 | fn foo(...rest) -> Int {
                  |        ^^^^^^^
                -- ast --
                fn foo() -> Int {
                  1
                }
            "#]],
        );
    }

    #[test]
    fn rejects_rest_param_on_view() {
        reject(
            indoc! {"
                view Foo(...rest) {
                  <div></div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Rest parameters are not allowed on views
                1 | view Foo(...rest) {
                  |          ^^^^^^^
                -- ast --
                view Foo() {
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
                component Foo(...rest, class: String) {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Rest parameter must be the last parameter
                1 | component Foo(...rest, class: String) {
                  |               ^^^^^^^
                -- ast --
                component Foo(class: String, ...rest) {
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
                fn card() -> Fragment {
                  <div>hello</div>
                }
            "},
            expect![[r#"
                fn card() -> Fragment {
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
                fn card() -> Fragment {
                  <></>
                }
            "},
            expect![[r#"
                fn card() -> Fragment {
                  fragment()
                }
            "#]],
        );
    }

    #[test]
    fn accepts_markup_in_an_interpolation() {
        accept(
            indoc! {"
                view Test {
                  <div>{<span>hello</span>}</div>
                }
            "},
            expect![[r#"
                view Test() {
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
            "#]],
        );
    }

    #[test]
    fn accepts_markup_nested_through_two_interpolations() {
        accept(
            indoc! {"
                view Test {
                  <div>{<span>{<b>hello</b>}</span>}</div>
                }
            "},
            expect![[r#"
                view Test() {
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
            "#]],
        );
    }

    #[test]
    fn accepts_markup_as_a_call_argument() {
        accept(
            indoc! {"
                fn wrap(children: Fragment) -> Fragment {
                  <div>{children}</div>
                }

                fn card() -> Fragment {
                  wrap(<span>hello</span>)
                }
            "},
            expect![[r#"
                fn wrap(children: Fragment) -> Fragment {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(children)],
                  )
                }

                fn card() -> Fragment {
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
                fn cards() -> Array[Fragment] {
                  [<div>a</div>, <div>b</div>]
                }
            "},
            expect![[r#"
                fn cards() -> Array[Fragment] {
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
                fn badge(on: Bool) -> Fragment {
                  match on {
                    true => <b>yes</b>,
                    false => <i>no</i>,
                  }
                }
            "},
            expect![[r#"
                fn badge(on: Bool) -> Fragment {
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
    fn accepts_markup_as_a_component_attribute_value() {
        accept(
            indoc! {"
                component Card(slot: Fragment) {
                  <div>{slot}</div>
                }

                view Test {
                  <Card slot={<span>hello</span>}/>
                }
            "},
            expect![[r#"
                component Card(slot: Fragment) {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(slot)],
                  )
                }

                view Test() {
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
            "#]],
        );
    }

    #[test]
    fn accepts_a_control_flow_tag_in_expression_position() {
        accept(
            indoc! {"
                fn card(on: Bool) -> Fragment {
                  <if {on}>
                    <div>hello</div>
                  </if>
                }
            "},
            expect![[r#"
                fn card(on: Bool) -> Fragment {
                  if on {
                    html(
                      tag: "div",
                      attrs: [],
                      children: [text("hello")],
                    ),
                  }
                }
            "#]],
        );
    }

    #[test]
    fn accepts_a_comment_before_markup_in_expression_position() {
        accept(
            indoc! {"
                fn card() -> Fragment {
                  // a note
                  <div>hello</div>
                }
            "},
            expect![[r#"
                fn card() -> Fragment {
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
                fn card() -> Fragment {
                  <!-- a note -->
                }
            "},
            expect![[r#"
                fn card() -> Fragment {
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
                fn card() -> Fragment {
                  <div/><span/>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                1 | fn card() -> Fragment {
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
                fn card() -> Fragment {
                  < div
                }
            "},
            expect![[r#"
                -- errors --
                error: Unterminated tag start
                1 | fn card() -> Fragment {
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
                fn card() -> Fragment {
                  <div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unclosed <div>
                1 | fn card() -> Fragment {
                2 |   <div>
                  |    ^^^
                -- ast --
                fn card() -> Fragment {
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
                component Main {
                  <p>
                    first line
                    second line
                  </p>
                }
            "},
            expect![[r#"
                component Main {
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

                component Main(#[examples(min = 1)] count: Int) {
                  <div>{count}</div>
                }
            "#},
            expect![[r#"
                record User {
                  #[examples(pattern = "[a-z]+", min_len = 1)] name: String,
                  #[examples(min = 0, max = 120)] age: Int,
                }

                component Main(#[examples(min = 1)] count: Int) {
                  html(
                    tag: "div",
                    attrs: [],
                    children: [interpolate(count)],
                  )
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
                component Main(#[examples(min = 1 count: Int) {
                  <div>{count}</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Expected token ',' but got 'count'
                1 | component Main(#[examples(min = 1 count: Int) {
                  |                                   ^^^^^

                error: Expected token ']' but got '{'
                1 | component Main(#[examples(min = 1 count: Int) {
                  |                                               ^

                error: Expected token ')' but got '{'
                1 | component Main(#[examples(min = 1 count: Int) {
                  |                                               ^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unexpected_token_inside_opening_tag() {
        reject(
            indoc! {r#"
                component Main {
                  <div class="a" @ id="b">hi</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unexpected character: '@'
                1 | component Main {
                2 |   <div class="a" @ id="b">hi</div>
                  |                  ^
                -- ast --
                component Main {
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
                component Main {
                  <div / class="a">hi</div>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unexpected character: '/'
                1 | component Main {
                2 |   <div / class="a">hi</div>
                  |        ^
                -- ast --
                component Main {
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
                component Main {
                  <div class="a" <span>hi</span>
                }
            "#},
            expect![[r#"
                -- errors --
                error: Unterminated opening tag
                1 | component Main {
                2 |   <div class="a" <span>hi</span>
                  |    ^^^

                error: Unclosed <div>
                1 | component Main {
                2 |   <div class="a" <span>hi</span>
                  |    ^^^
                -- ast --
                component Main {
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
                component Main {
                  <script>alert(1)
            "#},
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                1 | component Main {
                  |                ^

                error: Unclosed <script>
                1 | component Main {
                2 |   <script>alert(1)
                  |    ^^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unterminated_raw_text_opening_tag() {
        reject(
            "component Main {<style",
            expect![[r#"
                -- errors --
                error: Unmatched '{'
                1 | component Main {<style
                  |                ^

                error: Unterminated opening tag
                1 | component Main {<style
                  |                  ^^^^^

                error: Unclosed <style>
                1 | component Main {<style
                  |                  ^^^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_expression_on_html_element() {
        reject(
            indoc! {"
                component Main {
                  <div {x}>hi</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | component Main {
                2 |   <div {x}>hi</div>
                  |        ^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_expression_on_component() {
        reject(
            indoc! {"
                component Main {
                  <Card {x} title=\"a\"/>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <Card>: use attribute syntax instead (e.g. attr={value})
                1 | component Main {
                2 |   <Card {x} title="a"/>
                  |         ^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_duplicate_expression_on_html_element() {
        reject(
            indoc! {"
                component Main {
                  <div {x} {y}>hi</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | component Main {
                2 |   <div {x} {y}>hi</div>
                  |        ^^^

                error: <div> already has an expression
                1 | component Main {
                2 |   <div {x} {y}>hi</div>
                  |            ^^^
                -- ast --
            "#]],
        );
    }

    #[test]
    fn rejects_unparseable_expression_on_html_element() {
        reject(
            indoc! {"
                component Main {
                  <div {x +}>hi</div>
                }
            "},
            expect![[r#"
                -- errors --
                error: Unexpected expression on <div>: use attribute syntax instead (e.g. attr={value})
                1 | component Main {
                2 |   <div {x +}>hi</div>
                  |        ^

                error: Unexpected token '}'
                1 | component Main {
                2 |   <div {x +}>hi</div>
                  |            ^
                -- ast --
            "#]],
        );
    }
}
