use super::parse_expr;
use super::parse_helpers;
use super::parsed_ast::{
    ParsedAst, ParsedComponentDeclaration, ParsedDeclaration, ParsedEnumDeclaration,
    ParsedEnumDeclarationField, ParsedEnumDeclarationVariant, ParsedFunctionDeclaration,
    ParsedImportDeclaration, ParsedPageDeclaration, ParsedRecordDeclaration,
    ParsedRecordDeclarationField,
};
use super::token;
use super::tokenize_expr;
use crate::document::{Document, DocumentCursor, DocumentRange};
use crate::document_id::DocumentId;
use crate::examples_annotation::ExamplesAnnotation;
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::parse_type::parse_type;
use crate::hop::parsing::parsed_ast::ParsedParameter;
use crate::hop::parsing::token::LangTokenPair;
use crate::parse_error::{ParseError, ParseErrorKind};
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::{HashSet, VecDeque};
use std::iter::Peekable;

pub fn parse(
    document_id: DocumentId,
    document: Document,
    errors: &mut Vec<ParseError>,
) -> ParsedAst {
    let cursor = document.cursor();
    let document_range = cursor.range();
    let mut iter = cursor.peekable();
    let mut declarations = Vec::new();
    let mut comments = VecDeque::new();

    loop {
        let pub_range =
            parse_helpers::advance_if(&mut iter, &mut comments, errors, token::LangToken::Pub);

        match tokenize_expr::peek(&iter) {
            Some((token::LangToken::Import, _)) => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedPubKeyword {},
                        pub_r,
                    ));
                }
                if let Some(import) =
                    parse_import_declaration(&mut iter, &mut comments, errors, &document_range)
                {
                    declarations.push(ParsedDeclaration::Import(import));
                }
            }
            Some((token::LangToken::Record, _)) => {
                if let Some(record) = parse_record_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &document_range,
                    pub_range,
                ) {
                    declarations.push(ParsedDeclaration::Record(record));
                }
            }
            Some((token::LangToken::Enum, _)) => {
                if let Some(enum_decl) = parse_enum_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &document_range,
                    pub_range,
                ) {
                    declarations.push(ParsedDeclaration::Enum(enum_decl));
                }
            }
            Some((token::LangToken::Component, _)) => {
                if let Some(component) =
                    parse_component_declaration(&mut iter, &mut comments, errors, pub_range)
                {
                    declarations.push(ParsedDeclaration::Component(component));
                }
            }
            Some((token::LangToken::View, _)) => {
                if let Some(view) =
                    parse_view_declaration(&mut iter, &mut comments, errors, pub_range)
                {
                    declarations.push(ParsedDeclaration::Page(view));
                }
            }
            Some((token::LangToken::Page, _)) => {
                if let Some(page) =
                    parse_page_declaration(&mut iter, &mut comments, errors, pub_range)
                {
                    declarations.push(ParsedDeclaration::Page(page));
                }
            }
            Some((token::LangToken::Fn, _)) => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedPubKeyword {},
                        pub_r,
                    ));
                }
                if let Some(function) =
                    parse_function_declaration(&mut iter, &mut comments, errors, &document_range)
                {
                    declarations.push(ParsedDeclaration::Function(function));
                }
            }
            Some((_, token_range)) => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedPubKeyword {},
                        pub_r,
                    ));
                }
                // Unexpected token at top level
                errors.push(ParseError::new(
                    ParseErrorKind::UnexpectedTopLevelText {},
                    token_range,
                ));
                break;
            }
            None => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedPubKeyword {},
                        pub_r,
                    ));
                }
                break; // EOF
            }
        }
    }

    ParsedAst::new(document_id, declarations, comments)
}

fn parse_import_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedImportDeclaration> {
    let import_range =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Import)?;
    let mut path_segments: Vec<DocumentRange> = Vec::new();
    let first_segment = match tokenize_expr::next(iter, comments, errors) {
        Some((token::LangToken::Identifier(_), seg_range))
        | Some((token::LangToken::TypeName(_), seg_range)) => seg_range,
        Some((_, seg_range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedModulePath {},
                seg_range,
            ));
            return None;
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedModulePath {},
                range.clone(),
            ));
            return None;
        }
    };
    path_segments.push(first_segment);
    while parse_helpers::advance_if(iter, comments, errors, token::LangToken::ColonColon).is_some()
    {
        let segment = match tokenize_expr::next(iter, comments, errors) {
            Some((token::LangToken::Identifier(_), seg_range))
            | Some((token::LangToken::TypeName(_), seg_range)) => seg_range,
            Some((_, seg_range)) => {
                errors.push(ParseError::new(
                    ParseErrorKind::ExpectedIdentifierAfterColonColon {},
                    seg_range,
                ));
                return None;
            }
            None => {
                errors.push(ParseError::new(
                    ParseErrorKind::ExpectedIdentifierAfterColonColon {},
                    range.clone(),
                ));
                return None;
            }
        };
        path_segments.push(segment);
    }
    if path_segments.len() < 2 {
        errors.push(ParseError::new(
            ParseErrorKind::ImportPathTooShort {},
            path_segments[0].clone(),
        ));
        return None;
    }
    let type_name_range = path_segments.pop().unwrap();
    let type_name = match TypeName::from_cheap_string(type_name_range.to_cheap_string()) {
        Ok(name) => name,
        Err(e) => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidTypeName { error: e },
                type_name_range,
            ));
            return None;
        }
    };
    let module_path_range = path_segments
        .first()
        .unwrap()
        .clone()
        .to(path_segments.last().unwrap().clone());
    let module_name = match ModuleName::new(module_path_range.as_str()) {
        Ok(name) => name,
        Err(e) => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidModuleName { error: e },
                module_path_range.clone(),
            ));
            return None;
        }
    };
    let path_range = module_path_range.to(type_name_range.clone());
    let full_import_range = import_range.to(type_name_range.clone());
    Some(ParsedImportDeclaration {
        type_name,
        type_name_range,
        path_range,
        import_range: full_import_range,
        module_name,
    })
}

fn parse_record_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedRecordDeclaration> {
    let keyword_range =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Record)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let (name, name_range) = parse_helpers::expect_type_name(iter, comments, errors, range)?;
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::LeftBrace)?;
    let mut seen_names = HashSet::new();
    let (fields, braces) = parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        LangTokenPair::Braces,
        &left_brace,
        |iter, comments, errors, range| {
            let examples =
                parse_examples_annotation(iter, comments, errors).map(|(examples, _)| examples);
            let (field_name, field_name_range) =
                parse_helpers::expect_field_name(iter, comments, errors, range)?;
            parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Colon)?;
            let field_type = parse_type(iter, comments, errors, range)?;
            if !seen_names.insert(field_name_range.to_cheap_string()) {
                errors.push(ParseError::new(
                    ParseErrorKind::DuplicateField {
                        name: field_name_range.to_cheap_string(),
                    },
                    field_name_range,
                ));
                return None;
            }
            Some(ParsedRecordDeclarationField {
                name: field_name,
                name_range: field_name_range,
                field_type,
                examples,
            })
        },
    )?;
    let full_range = start_range.to(braces);
    Some(ParsedRecordDeclaration {
        name,
        name_range,
        range: full_range,
        fields,
        pub_range,
    })
}

fn parse_enum_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedEnumDeclaration> {
    let keyword_range =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Enum)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let (name, name_range) = parse_helpers::expect_type_name(iter, comments, errors, range)?;
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::LeftBrace)?;
    let mut seen_names = HashSet::new();
    let (variants, braces) = parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        LangTokenPair::Braces,
        &left_brace,
        |iter, comments, errors, range| {
            let (variant_name, variant_range) =
                parse_helpers::expect_type_name(iter, comments, errors, range)?;
            if !seen_names.insert(variant_range.to_cheap_string()) {
                errors.push(ParseError::new(
                    ParseErrorKind::DuplicateVariant {
                        name: variant_range.to_cheap_string(),
                    },
                    variant_range,
                ));
                return None;
            }
            let fields = if let Some(left_brace) =
                parse_helpers::advance_if(iter, comments, errors, token::LangToken::LeftBrace)
            {
                let mut seen_field_names = HashSet::new();
                let (fields, _) = parse_helpers::parse_delimited_list(
                    iter,
                    comments,
                    errors,
                    range,
                    LangTokenPair::Braces,
                    &left_brace,
                    |iter, comments, errors, range| {
                        let examples = parse_examples_annotation(iter, comments, errors)
                            .map(|(examples, _)| examples);
                        let (field_name, field_name_range) =
                            parse_helpers::expect_field_name(iter, comments, errors, range)?;
                        parse_helpers::expect_token(
                            iter,
                            comments,
                            errors,
                            range,
                            &token::LangToken::Colon,
                        )?;
                        let field_type = parse_type(iter, comments, errors, range)?;
                        if !seen_field_names.insert(field_name_range.to_cheap_string()) {
                            errors.push(ParseError::new(
                                ParseErrorKind::DuplicateField {
                                    name: field_name_range.to_cheap_string(),
                                },
                                field_name_range,
                            ));
                            return None;
                        }
                        Some(ParsedEnumDeclarationField {
                            name: field_name,
                            name_range: field_name_range,
                            field_type,
                            examples,
                        })
                    },
                )?;
                fields
            } else {
                Vec::new()
            };
            Some(ParsedEnumDeclarationVariant {
                name: variant_name,
                name_range: variant_range,
                fields,
            })
        },
    )?;
    let full_range = start_range.to(braces);
    Some(ParsedEnumDeclaration {
        name,
        name_range,
        range: full_range,
        variants,
        pub_range,
    })
}

fn parse_component_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedComponentDeclaration> {
    // Consume the 'component' keyword
    let Some((token::LangToken::Component, keyword_range)) =
        tokenize_expr::next(iter, comments, errors)
    else {
        return None;
    };

    // Parse the component name (must be PascalCase)
    let (name_str, name_range) = match tokenize_expr::next(iter, comments, errors) {
        Some((token::LangToken::TypeName(name_str), range)) => (name_str, range),
        Some((actual, range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTypeNameButGot { actual },
                range,
            ));
            return None;
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTypeNameButGotEof {},
                keyword_range,
            ));
            return None;
        }
    };

    // Parse parameters (parentheses are optional if no parameters)
    let parsed_params =
        match parse_helpers::advance_if(iter, comments, errors, token::LangToken::LeftParen) {
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
                        errors.push(ParseError::new(
                            ParseErrorKind::RestParamMustBeLast {},
                            range.clone(),
                        ));
                    }
                    match rest_param {
                        Some(_) => errors.push(ParseError::new(
                            ParseErrorKind::DuplicateRestParam {},
                            range,
                        )),
                        None => rest_param = Some((var_name, range)),
                    }
                }
            }
        }
    }

    let (body, body_end) = parse_declaration_body(iter, comments, errors, &name_range)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(body_end);

    let component_name = match TypeName::new(&name_str) {
        Ok(n) => n,
        Err(error) => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidTypeName { error },
                name_range,
            ));
            return None;
        }
    };

    Some(ParsedComponentDeclaration {
        component_name,
        name_range,
        params,
        params_range,
        rest_param,
        range,
        body,
        pub_range,
    })
}

fn parse_page_or_view_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    keyword_range: &DocumentRange,
) -> Option<(TypeName, DocumentRange, Vec<ParsedParameter>, DocumentRange)> {
    let (name_str, name_range) = match tokenize_expr::next(iter, comments, errors) {
        Some((token::LangToken::TypeName(name_str), range)) => (name_str, range),
        Some((actual, range)) => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTypeNameButGot { actual },
                range,
            ));
            return None;
        }
        None => {
            errors.push(ParseError::new(
                ParseErrorKind::ExpectedTypeNameButGotEof {},
                keyword_range.clone(),
            ));
            return None;
        }
    };

    let (params, params_range) =
        match parse_helpers::advance_if(iter, comments, errors, token::LangToken::LeftParen) {
            Some(left_paren) => {
                let (items, parens) = parse_parameters(iter, comments, errors, &left_paren)?;
                let mut params = Vec::new();
                for item in items {
                    match item {
                        ParameterItem::Parameter(parameter) => {
                            if let Some(value) = &parameter.default_value {
                                errors.push(ParseError::new(
                                    ParseErrorKind::DefaultValueNotAllowedOnView {},
                                    value.range().clone(),
                                ));
                            }
                            params.push(*parameter);
                        }
                        ParameterItem::Rest { range, .. } => errors.push(ParseError::new(
                            ParseErrorKind::RestParamNotAllowedOnView {},
                            range,
                        )),
                    }
                }
                (params, parens)
            }
            None => (Vec::new(), name_range.clone()),
        };

    let name = match TypeName::new(&name_str) {
        Ok(name) => name,
        Err(error) => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidTypeName { error },
                name_range,
            ));
            return None;
        }
    };

    Some((name, name_range, params, params_range))
}

fn parse_view_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedPageDeclaration> {
    let Some((token::LangToken::View, keyword_range)) = tokenize_expr::next(iter, comments, errors)
    else {
        return None;
    };

    let (name, name_range, params, params_range) =
        parse_page_or_view_header(iter, comments, errors, &keyword_range)?;

    let (body, body_end) = parse_declaration_body(iter, comments, errors, &params_range)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(body_end);
    Some(ParsedPageDeclaration {
        name,
        name_range,
        params,
        head: None,
        body,
        range,
        pub_range,
        is_view: true,
    })
}

fn parse_page_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedPageDeclaration> {
    let Some((token::LangToken::Page, keyword_range)) = tokenize_expr::next(iter, comments, errors)
    else {
        return None;
    };

    let (name, name_range, params, params_range) =
        parse_page_or_view_header(iter, comments, errors, &keyword_range)?;

    let outer_body_start = parse_helpers::expect_token(
        iter,
        comments,
        errors,
        &params_range,
        &token::LangToken::LeftBrace,
    )?;

    let head = if let Some((token::LangToken::Identifier(_), head_keyword_range)) =
        parse_helpers::next_if(
            iter,
            comments,
            errors,
            |(token, _)| matches!(token, token::LangToken::Identifier(word) if word.as_str() == "head"),
        ) {
        let (head, _) = parse_declaration_body(iter, comments, errors, &head_keyword_range)?;
        Some(head)
    } else {
        None
    };

    let Some((token::LangToken::Identifier(_), body_keyword_range)) = parse_helpers::next_if(
        iter,
        comments,
        errors,
        |(token, _)| matches!(token, token::LangToken::Identifier(word) if word.as_str() == "body"),
    ) else {
        let range = match tokenize_expr::peek(iter) {
            Some((_, range)) => range,
            None => name_range,
        };
        errors.push(ParseError::new(
            ParseErrorKind::ExpectedPageBodyBlock {},
            range,
        ));
        return None;
    };
    let (body, _) = parse_declaration_body(iter, comments, errors, &body_keyword_range)?;

    let outer_body_end = parse_helpers::expect_right_delimiter(
        iter,
        comments,
        errors,
        LangTokenPair::Braces,
        &outer_body_start,
    )?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(outer_body_end);
    Some(ParsedPageDeclaration {
        name,
        name_range,
        params,
        head,
        body,
        range,
        pub_range,
        is_view: false,
    })
}

fn parse_function_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedFunctionDeclaration> {
    let keyword_range =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Fn)?;
    let (name, name_range) = parse_helpers::expect_variable_name(iter, comments, errors, range)?;
    let left_paren =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::LeftParen)?;
    let (items, _) = parse_parameters(iter, comments, errors, &left_paren)?;
    let mut params = Vec::new();
    for item in items {
        match item {
            ParameterItem::Parameter(parameter) => {
                if let Some(examples_range) = &parameter.examples_range {
                    errors.push(ParseError::new(
                        ParseErrorKind::ExamplesNotAllowedOnFunction {},
                        examples_range.clone(),
                    ));
                }
                if let Some(value) = &parameter.default_value {
                    errors.push(ParseError::new(
                        ParseErrorKind::DefaultValueNotAllowedOnFunction {},
                        value.range().clone(),
                    ));
                }
                params.push(*parameter);
            }
            ParameterItem::Rest { range, .. } => errors.push(ParseError::new(
                ParseErrorKind::RestParamNotAllowedOnFunction {},
                range,
            )),
        }
    }
    parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Arrow)?;
    let return_type = parse_type(iter, comments, errors, range)?;
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::LeftBrace)?;
    let (body, braces) = parse_helpers::parse_delimited(
        iter,
        comments,
        errors,
        range,
        LangTokenPair::Braces,
        &left_brace,
        parse_expr::parse_expr,
    )?;
    let full_range = keyword_range.to(braces);
    Some(ParsedFunctionDeclaration {
        name,
        name_range,
        params,
        return_type,
        body,
        range: full_range,
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
    errors: &mut Vec<ParseError>,
    left_paren: &DocumentRange,
) -> Option<(Vec<ParameterItem>, DocumentRange)> {
    parse_helpers::parse_delimited_list(
        iter,
        comments,
        errors,
        left_paren,
        LangTokenPair::Parens,
        left_paren,
        |iter, comments, errors, range| {
            if let Some(dots_range) =
                parse_helpers::advance_if(iter, comments, errors, token::LangToken::DotDotDot)
            {
                let (var_name, var_name_range) =
                    parse_helpers::expect_variable_name(iter, comments, errors, range)?;
                return Some(ParameterItem::Rest {
                    range: dots_range.to(var_name_range),
                    var_name,
                });
            }
            let (examples, examples_range) =
                parse_examples_annotation(iter, comments, errors).unzip();
            let (var_name, var_name_range) =
                parse_helpers::expect_variable_name(iter, comments, errors, range)?;
            parse_helpers::expect_token(iter, comments, errors, range, &token::LangToken::Colon)?;
            let var_type = parse_type(iter, comments, errors, range)?;
            let default_value =
                if parse_helpers::advance_if(iter, comments, errors, token::LangToken::Assign)
                    .is_some()
                {
                    parse_expr::parse_primary(iter, comments, errors, range)
                } else {
                    None
                };
            Some(ParameterItem::Parameter(Box::new(ParsedParameter {
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
    errors: &mut Vec<ParseError>,
    before: &DocumentRange,
) -> Option<(ParsedExpr, DocumentRange)> {
    let left_brace =
        parse_helpers::expect_token(iter, comments, errors, before, &token::LangToken::LeftBrace)?;
    parse_helpers::parse_delimited(
        iter,
        comments,
        errors,
        &left_brace,
        LangTokenPair::Braces,
        &left_brace,
        |iter, comments, errors, left_brace| {
            if let Some((token::LangToken::RightBrace, _)) = tokenize_expr::peek(iter) {
                errors.push(ParseError::new(
                    ParseErrorKind::EmptyBody {},
                    left_brace.clone(),
                ));
                return Some(ParsedExpr::FragmentEmpty {
                    range: left_brace.clone(),
                });
            }
            parse_expr::parse_expr(iter, comments, errors, left_brace)
        },
    )
}

/// Parse a `#[examples(...)]` annotation using the expr tokenizer. Returns
/// the annotation with its range.
fn parse_examples_annotation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<(ExamplesAnnotation, DocumentRange)> {
    if let Some((token::LangToken::HashBracket, hash_bracket)) = tokenize_expr::peek(iter) {
        tokenize_expr::next(iter, comments, errors);
        match tokenize_expr::next(iter, comments, errors) {
            Some((token::LangToken::Identifier(name), _)) if &*name == "examples" => {}
            _ => return None,
        }
        if tokenize_expr::next(iter, comments, errors).map(|(t, _)| t)
            != Some(token::LangToken::LeftParen)
        {
            return None;
        }
        let mut annotation = ExamplesAnnotation::default();
        loop {
            let key = match tokenize_expr::next(iter, comments, errors) {
                Some((token::LangToken::Identifier(name), _)) => name.to_string(),
                _ => return None,
            };
            if tokenize_expr::next(iter, comments, errors).map(|(t, _)| t)
                != Some(token::LangToken::Assign)
            {
                return None;
            }
            match key.as_str() {
                "pattern" => {
                    let pattern = match tokenize_expr::next(iter, comments, errors) {
                        Some((token::LangToken::StringLiteral(s), _)) => s.to_string(),
                        _ => return None,
                    };
                    annotation.pattern = Some(pattern);
                }
                "min" | "max" | "min_len" | "max_len" => {
                    let (negative, token) = match tokenize_expr::peek(iter) {
                        Some((token::LangToken::Minus, _)) => {
                            tokenize_expr::next(iter, comments, errors);
                            (true, tokenize_expr::next(iter, comments, errors))
                        }
                        _ => (false, tokenize_expr::next(iter, comments, errors)),
                    };
                    let n = match token {
                        Some((token::LangToken::IntLiteral(n), _)) => {
                            if negative {
                                -n
                            } else {
                                n
                            }
                        }
                        _ => return None,
                    };
                    match key.as_str() {
                        "min" => annotation.min = Some(n),
                        "max" => annotation.max = Some(n),
                        "min_len" => annotation.min_len = Some(n),
                        "max_len" => annotation.max_len = Some(n),
                        _ => unreachable!(),
                    }
                }
                _ => return None,
            }
            match tokenize_expr::peek(iter) {
                Some((token::LangToken::Comma, _)) => {
                    tokenize_expr::next(iter, comments, errors);
                }
                _ => break,
            }
        }
        if tokenize_expr::next(iter, comments, errors).map(|(t, _)| t)
            != Some(token::LangToken::RightParen)
        {
            return None;
        }
        let Some((token::LangToken::RightBracket, right_bracket)) =
            tokenize_expr::next(iter, comments, errors)
        else {
            return None;
        };
        Some((annotation, hash_bracket.to(right_bracket)))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use crate::hop::format;
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
                .with_label("error")
                .with_lines_before(1)
                .annotate(&DocumentId::new("test.hop").unwrap(), errors.clone())
                .render();
            panic!("expected no parse errors, got:\n{rendered}");
        }
        expected.assert_eq(&format(&module));
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
            .with_label("error")
            .with_lines_before(1)
            .annotate(&DocumentId::new("test.hop").unwrap(), errors.clone())
            .render();
        let actual = format!("-- errors --\n{rendered}-- ast --\n{}", format(&module));
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
                  <div>
                    {children}
                  </div>
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
                  <button>
                    {label}
                  </button>
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
                pub view Home {
                  <div>
                    hi
                  </div>
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
                  <Foo/>
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
                  <>
                  </>
                }

                // This is a comment
                component Second {
                  <>
                  </>
                }
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
                  <div>
                    hello
                  </div>
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
                component Foo(
                  name: String,
                  count: Int,
                ) {
                  <div>
                    {name}
                  </div>
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
                component Button(
                  // The button label
                  label: String,
                  // Whether the button is disabled
                  disabled: Bool = false,
                  // More params to come
                ) {
                  <>
                    {label}
                  </>
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
                  <form id="form">
                    <input type="text" required>
                    <button type="submit">
                      Send
                    </button>
                  </form>
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
                  <>
                    <p>
                      one
                    </p>
                    <p>
                      two
                    </p>
                  </>
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
                  <>
                  </>
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
                  <>
                    <>
                      one
                    </>
                    two
                  </>
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
                  <div>
                    <>
                      one
                    </>
                  </div>
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
                  <>
                    one
                  </>
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
                  <script><></script>
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
                  <div>
                    <p>
                    </p>
                  </div>
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
                  <div>
                    <span>
                    </span>
                  </div>
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
                  <div>
                    <span>
                      <>
                        <b>
                        </b>
                      </>
                    </span>
                  </div>
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
                  <div>
                  </div>
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
                  Fragment::empty()
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

                error: Unexpected text at top level
                 3 |         <title>one</title>
                 4 |         <meta charset="utf-8"/>
                   |                      ^
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
                  <>
                    Hello,
                    {" "}
                    {name}
                    !
                  </>
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
                  <>
                    one
                  </>
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
                  <div>
                  </div>
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
                  <>
                    <div>
                    </div>
                  </>
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

                error: Unexpected text at top level
                1 | component Main {
                2 |     < >
                  |       ^
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
                  <match {x}></match>
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
                  <match {x}></match>
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
                  <>
                    <hr>
                    <br>
                    <input>
                  </>
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
                  <>
                    <hr>
                    <br>
                    <input>
                  </>
                }

                component Foo {
                  <>
                  </>
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
                  <div foo>
                  </div>
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
                  <input>
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

                error: Unexpected text at top level
                1 | component Main {<!foo>}
                  |                   ^^^
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
                -- ast --
                component Main {
                  <div>
                  </div>
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
                  <div>
                  </div>
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
                  <div class="foo">
                  </div>
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
                  <div>
                    text
                  </div>
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

                error: Unexpected text at top level
                1 | component Main {< div>}
                  |                   ^^^
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

                error: Unexpected text at top level
                2 |     <!DOCTYPE html>
                3 |     <html>
                  |     ^
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

                error: Unexpected text at top level
                4 |     </if>
                5 | }
                  | ^
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

                error: Unexpected text at top level
                4 |     </for>
                5 | }
                  | ^
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

                error: Unexpected text at top level
                4 |     </for>
                5 | }
                  | ^
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

                error: Unexpected text at top level
                4 |     </if>
                5 | }
                  | ^
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

                error: Unexpected text at top level
                1 | component Main(data: Array[) {
                  |                              ^
                -- ast --
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
                  <a href={user.url} class={user.theme}>
                    Link
                  </a>
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

                error: Unexpected text at top level
                1 | record User {
                2 |   url: x,
                  |         ^
                -- ast --
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
                error: Unterminated opening tag
                1 | component Main(style1: String, style2: String, style3: String) {
                2 |     <div class={style1, style2, style3}>Content</div>
                  |      ^^^

                error: Expected token '}' but got ','
                1 | component Main(style1: String, style2: String, style3: String) {
                2 |     <div class={style1, style2, style3}>Content</div>
                  |                       ^
                -- ast --
                component Main(
                  style1: String,
                  style2: String,
                  style3: String,
                ) {
                  <div style2>
                    Content
                  </div>
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
                  <Foo/>
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
                  <>
                    <Foo/>
                    <Foo/>
                  </>
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
                  <>
                    <Foo a={data}/>
                    <Bar b={data.user}/>
                  </>
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
                  <for {item in items}>
                    <div>
                      Item content
                    </div>
                  </for>
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
                  <for {v in foo}>
                    <div>
                      {v}
                    </div>
                  </for>
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
                  <for {i in 0..=5}>
                    {i}
                  </for>
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
                component Main(
                  start: Int,
                  end: Int,
                ) {
                  <for {x in start..=end}>
                    {x}
                  </for>
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
                  <for {i in 1..=count + 1}>
                    {i}
                  </for>
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
                  <for {_ in items}>
                    item
                  </for>
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
                  <for {_ in 0..=5}>
                    item
                  </for>
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
                component Main(
                  start: Int,
                  end: Int,
                ) {
                  <for {_ in start..=end}>
                    item
                  </for>
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
                component Main(
                  x: Int,
                  y: Int,
                ) {
                  <if {x == y}>
                    <div>
                      Equal
                    </div>
                  </if>
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
                component Main(
                  x: Bool,
                  data: Array[String],
                ) {
                  <if {x}>
                    <for {d in data}>
                      {d}
                    </for>
                  </if>
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

                error: Unexpected text at top level
                2 |     <dvi>oops</dvi>
                3 | }
                  | ^
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

                error: Unexpected text at top level
                2 |     <math></math>
                3 | }
                  | ^
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
                  <my-widget>
                    hi
                  </my-widget>
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
                  <div class="navbar">
                    <svg
                      xmlns="http://www.w3.org/2000/svg"
                      width="128"
                      height="128"
                      version="1.1"
                      viewBox="0 0 128 128"
                      class="size-12"
                    >
                      <g style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;">
                        <path d="M20.04 38 64 22l43.96 16L64 54Z">
                        </path>
                        <path d="M17.54 47.09v48l35.099 12.775">
                        </path>
                        <path d="M64 112V64l46.46-16.91v48L77.988 106.91">
                        </path>
                      </g>
                    </svg>
                    <ul>
                      <li>
                        <a href="/">
                          Home
                        </a>
                      </li>
                    </ul>
                  </div>
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
                  <div>
                    {data}
                  </div>
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
                  <>
                    <h1>
                      Hello World
                    </h1>
                    <p>
                      {data.message}
                    </p>
                  </>
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
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
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
                  <for {section in data}>
                    <h1>
                      {section.title}
                    </h1>
                    <for {item in section.items}>
                      <div>
                        {item}
                      </div>
                    </for>
                  </for>
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
                  <h1>
                    Hello
                    {" "}
                    {name}
                    !
                  </h1>
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
                  <p>
                    User
                    {" "}
                    {user.name}
                    {" "}
                    has
                    {" "}
                    {user.count}
                    {" "}
                    items
                  </p>
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
                  <span>
                    {greeting}
                    {" "}
                    world!
                  </span>
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
                  <div>
                    Price:
                    {" "}
                    {price}
                  </div>
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
                  <h2>
                    {title}
                  </h2>
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
                  <div>
                    Empty:
                  </div>
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
                  <p>
                    Status:
                    {" "}
                    {user.profile.status == "active"}
                  </p>
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
                  <span>
                    {first}
                    {second}
                  </span>
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
                  <div>
                    {"<div></div>"}
                  </div>
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

                error: Unexpected text at top level
                1 | record
                2 | component Main {
                  |           ^^^^
                -- ast --
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
                -- ast --
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
                  <>
                    {match color {
                      Color::Red => "red",
                      Color::Blue => "blue",
                    }}
                  </>
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
                  <div class={
                    match color {
                      Color::Red => "text-red",
                      Color::Blue => "text-blue",
                    }
                  }>
                  </div>
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
                  <>
                    {match status {
                      Status::Active => "active",
                      Status::Inactive => "inactive",
                      Status::Pending => "pending",
                    }}
                  </>
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
                  <div>
                    {name}
                  </div>
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
                  <span>
                    {count}
                  </span>
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
                  <div>
                  </div>
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
                component Main(
                  name: String,
                  role: String = "user",
                  active: Bool = true,
                ) {
                  <div>
                    {name}
                  </div>
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
                component Main(items: Array[String] = ["a", "b"]) {
                  <for {item in items}>
                    {item}
                  </for>
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

                component Main(
                  config: Config = Config {debug: false, timeout: 30},
                ) {
                  <div>
                  </div>
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
                  <div>
                  </div>
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
                  <div>
                  </div>
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
                  <div>
                  </div>
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
                  <div>
                  </div>
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
                component Main(offsets: Array[Int] = [1, 2]) {
                  <div>
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_parameter_with_default_fragment_empty() {
        accept(
            indoc! {"
                component Main(children: Fragment = Fragment::empty()) {
                    <div></div>
                }
            "},
            expect![[r#"
                component Main(children: Fragment = Fragment::empty()) {
                  <div>
                  </div>
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
                  <match {x}></match>
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
                  <match {x}>
                    <case {Some(y)}>
                      found
                      {" "}
                      {y}
                    </case>
                    <case {None}>
                    </case>
                  </match>
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
                  <match {x}>
                    <case {Some(y)}>
                      found
                      {" "}
                      {y}
                    </case>
                    <case {None}>
                      nothing
                    </case>
                  </match>
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
                  <match {c}>
                    <case {Color::Red}>
                      red
                    </case>
                    <case {Color::Green}>
                      green
                    </case>
                    <case {Color::Blue}>
                      blue
                    </case>
                  </match>
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
                  Success {
                    value: Int,
                  },
                  Failure {
                    message: String,
                  },
                }

                component Main(r: Outcome) {
                  <match {r}>
                    <case {Outcome::Success {value: v}}>
                      Success:
                      {" "}
                      {v}
                    </case>
                    <case {Outcome::Failure {message: m}}>
                      Error:
                      {" "}
                      {m}
                    </case>
                  </match>
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
                  Active {
                    name: String,
                  },
                  Inactive,
                }

                component Main {
                  <match {Status::Active {name: "test"}}>
                    <case {Status::Active {name: n}}>
                      {n}
                    </case>
                    <case {Status::Inactive}>
                      none
                    </case>
                  </match>
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
                  <match {flag}>
                    <case {true}>
                      yes
                    </case>
                    <case {false}>
                      no
                    </case>
                  </match>
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

                error: Unexpected text at top level
                4 |     </match>
                5 | }
                  | ^
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
                  <match {flag}></match>
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
                  <match {flag}></match>
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

                error: Unexpected text at top level
                2 |     <case {true}>standalone case</case>
                3 | }
                  | ^
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
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {" "}
                      {name}
                    </div>
                  </let>
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
                  <let {count: Int = 42}>
                    <span>
                      {count}
                    </span>
                  </let>
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
                  <let {greeting: String = user.name}>
                    <div>
                      {greeting}
                    </div>
                  </let>
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
                  <let {a: Int = 1}>
                    <let {b: Int = 2}>
                      <div>
                        {a}
                        {" "}
                        +
                        {" "}
                        {b}
                      </div>
                    </let>
                  </let>
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

                error: Unexpected text at top level
                4 |     </let>
                5 | }
                  | ^
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
                  <let {x = 1}>
                    <div>
                      Content
                    </div>
                  </let>
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
                  <let {}>
                    <div>
                      Content
                    </div>
                  </let>
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

                error: Unexpected text at top level
                4 |     </let>
                5 | }
                  | ^
                -- ast --
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
                  <let {first: String = "Hello", second: String = "World"}>
                    <div>
                      {first}
                      {" "}
                      {second}
                    </div>
                  </let>
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
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>
                      {a}
                      {" "}
                      +
                      {" "}
                      {b}
                      {" "}
                      +
                      {" "}
                      {c}
                    </div>
                  </let>
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
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {" "}
                      {name}
                    </div>
                  </let>
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
                  <let {first: String = "Hello", second: String = "World"}>
                    <div>
                      {first}
                      {" "}
                      {second}
                    </div>
                  </let>
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
                  <let {name: String = user.name}>
                    <div>
                      {name}
                    </div>
                  </let>
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
                error: Unterminated opening tag
                1 | component Main {
                2 |     <let {first: String = "a" second: String = "b"}>
                  |      ^^^

                error: Expected token '}' but got 'second'
                1 | component Main {
                2 |     <let {first: String = "a" second: String = "b"}>
                  |                               ^^^^^^

                error: Unexpected text at top level
                4 |     </let>
                5 | }
                  | ^
                -- ast --
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
                  <>
                    <let {a: String = "Hello"}>
                      {a}
                    </let>
                    <let {b: String = "World"}>
                      {b}
                    </let>
                  </>
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
                  <>
                    <div>
                      First
                    </div>
                    <let {name: String = "World"}>
                      <div>
                        Hello
                        {" "}
                        {name}
                      </div>
                    </let>
                  </>
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
                  <>
                    <let {name: String = "World"}>
                      <div>
                        Hello
                        {" "}
                        {name}
                      </div>
                    </let>
                    <div>
                      Last
                    </div>
                  </>
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
                view Index {
                  <div>
                    Hello
                  </div>
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
                view Index {
                  <div>
                    Hello
                  </div>
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
                view Index(
                  name: String,
                  count: Int,
                ) {
                  <div>
                    {name}
                    :
                    {" "}
                    {count}
                  </div>
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
                  <h1>
                    {title}
                  </h1>
                }

                view Index(title: String) {
                  <Header title={title}/>
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
                view Index {
                  <div>
                    Index
                  </div>
                }

                view About {
                  <div>
                    About
                  </div>
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
                error: Unterminated opening tag
                1 | view Test {
                2 |   <let {default: String = "x"}>
                  |    ^^^

                error: Invalid variable name 'default': Variable name is a reserved word
                1 | view Test {
                2 |   <let {default: String = "x"}>
                  |         ^^^^^^^

                error: Unexpected text at top level
                4 |   </let>
                5 | }
                  | ^
                -- ast --
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

                error: Unexpected text at top level
                1 | view Error() {
                  |              ^
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

                error: Unexpected text at top level
                1 | component card() {
                  |               ^
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

                error: Unexpected text at top level
                2 |     <Foo-Bar />
                3 | }
                  | ^
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

                error: Unexpected text at top level
                1 | view index() {
                  |           ^
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
                  <div>
                    Hello
                    {" "}
                    {name}
                  </div>
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
                    <title>
                      My page
                    </title>
                  }
                  body {
                    <div>
                      Hello
                      {" "}
                      {name}
                    </div>
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
                page Index {
                  body {
                    <div>
                      Hello
                    </div>
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

                error: Unexpected text at top level
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

                error: Unexpected text at top level
                4 |     }
                5 |     head {
                  |          ^
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
                view Index {
                  Fragment::empty()
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
                  <if {show}>
                    <div>
                      Visible
                    </div>
                  </if>
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
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
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
                view Index {
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {" "}
                      {name}
                    </div>
                  </let>
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
                view Index {
                  <div>
                    <br>
                    <input type="text">
                    <hr>
                  </div>
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
                  <div>
                    {name}
                  </div>
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
                  <match {value}>
                    <case {Some(s)}>
                      <div>
                        {s}
                      </div>
                    </case>
                    <case {None}>
                      <div>
                        No value
                      </div>
                    </case>
                  </match>
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
                  <h1>
                    {title}
                  </h1>
                }

                component Footer {
                  <p>
                    Copyright 2024
                  </p>
                }

                view Index(title: String) {
                  <div>
                    <Header title={title}/>
                    <main>
                      Content
                    </main>
                    <Footer/>
                  </div>
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
                  <h1>
                    Header
                  </h1>
                }

                view Index {
                  <div>
                    Index
                  </div>
                }

                component Footer {
                  <p>
                    Footer
                  </p>
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
                view Index(
                  required: String,
                  optional: Int = 42,
                ) {
                  <div>
                    {required}
                    :
                    {" "}
                    {optional}
                  </div>
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

                error: Unexpected text at top level
                2 |   hello world
                3 | }
                  | ^
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
                  <>
                    {"hello\nworld"}
                    {"tab\there"}
                    {"back\\slash"}
                    {"quote\"here"}
                  </>
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
                component Foo(
                  class: String,
                  ...rest,
                ) {
                  <div ...rest>
                  </div>
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
                component Foo(
                  a: String,
                  b: String,
                  ...rest,
                ) {
                  <div ...rest>
                  </div>
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
                  <div ...a>
                  </div>
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
                  <button ...rest>
                  </button>
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
                  <Foo ...rest>
                  </Foo>
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
                  <button>
                  </button>
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
                  <button>
                  </button>
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
                  <div>
                    <for {x in 0..=foo(10)}>
                      {x.to_string()}
                    </for>
                    {foo(10)}
                  </div>
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
                fn clamp(
                  value: Int,
                  low: Int,
                  high: Int,
                ) -> Int {
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
                error: Expected token '->' but got '{'
                1 | fn foo(x: Int) {
                  |                ^

                error: Unexpected text at top level
                1 | fn foo(x: Int) {
                2 |   x
                  |   ^
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

                error: Unexpected text at top level
                1 | fn foo -> Int {
                  |           ^^^
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
                view Foo {
                  <div>
                  </div>
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
                fn foo(
                  #[examples(min = 1)]
                  x: Int,
                ) -> Int {
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
                component Foo(
                  class: String,
                  ...rest,
                ) {
                  <div ...rest>
                  </div>
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
                  <div>
                    hello
                  </div>
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
                  <>
                  </>
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
                view Test {
                  <div>
                    {<span>
                      hello
                    </span>}
                  </div>
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
                view Test {
                  <div>
                    {<span>
                      {<b>
                        hello
                      </b>}
                    </span>}
                  </div>
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
                  <div>
                    {children}
                  </div>
                }

                fn card() -> Fragment {
                  wrap(<span>hello</span>)
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
                  [<div>a</div>, <div>b</div>]
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
                  match on {true => <b>yes</b>, false => <i>no</i>}
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
                  <div>
                    {slot}
                  </div>
                }

                view Test {
                  <Card slot={<span>hello</span>}/>
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
                  <if {on}>
                    <div>
                      hello
                    </div>
                  </if>
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
                  // a note
                  <div>
                    hello
                  </div>
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
                  <!-- a note -->
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
                fn check(
                  a: Int,
                  b: Int,
                  c: Int,
                  d: Int,
                ) -> Bool {
                  a < b && c > d
                }

                fn at_most(
                  a: Int,
                  b: Int,
                ) -> Bool {
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

                error: Unexpected text at top level
                1 | fn card() -> Fragment {
                2 |   < div
                  |     ^^^
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
                  <div>
                  </div>
                }
            "#]],
        );
    }
}
