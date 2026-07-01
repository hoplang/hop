use super::parsed_ast::{
    self, ParsedAst, ParsedAttribute, ParsedComponentDeclaration, ParsedDeclaration,
    ParsedEnumDeclaration, ParsedEnumDeclarationVariant, ParsedImportDeclaration,
    ParsedRecordDeclaration, ParsedRecordDeclarationField, ParsedViewDeclaration, RestSpreadTarget,
};
use super::parsed_node::{ParsedLetBinding, ParsedLoopSource, ParsedMatchCase, ParsedNode};
use super::token_tree::{TokenTree, parse_tree};
use super::tokenizer::Tokenizer;
use super::tokenizer::{self, Token};
use crate::document::{CheapString, Document, DocumentCursor, DocumentRange};
use crate::document_id::DocumentId;
use crate::expr::parsing::ParsedType;
use crate::expr::parsing::parse_type::parse_type;
use crate::expr::{self, ExamplesAnnotation};
use crate::html::HtmlElement;
use crate::parse_error::{ParseError, ParseErrorKind};
use crate::symbols::field_name::FieldName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::Peekable;

/// A `...name` spread attribute found in a body, with the target it lands on.
struct SpreadOccurrence {
    spread_name: VarName,
    target: RestSpreadTarget,
}

/// Parse a hop document into a ParsedAst.
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

    let mut defined_components = HashSet::new();
    let mut imported_components = HashMap::new();
    let mut defined_records = HashSet::new();
    let mut defined_enums = HashSet::new();
    let mut defined_views: HashSet<String> = HashSet::new();

    loop {
        let pub_range =
            expr::tokenizer::advance_if(&mut iter, &mut comments, errors, expr::Token::Pub);

        match expr::tokenizer::peek_past_comments(&iter) {
            Some((expr::Token::Import, _)) => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnexpectedPubKeyword {},
                        pub_r,
                    ));
                }
                if let Some(import) =
                    parse_import_declaration(&mut iter, &mut comments, errors, &document_range)
                {
                    let name_str = import.type_name.as_str();
                    if imported_components.contains_key(name_str) {
                        errors.push(ParseError::new(
                            ParseErrorKind::TypeNameIsAlreadyDefined {
                                name: import.type_name_range.to_cheap_string(),
                            },
                            import.type_name_range.clone(),
                        ));
                    } else {
                        imported_components
                            .insert(name_str.to_string(), import.module_name.to_document_id());
                    }
                    declarations.push(ParsedDeclaration::Import(import));
                }
            }
            Some((expr::Token::Record, _)) => {
                if let Some(record) = parse_record_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &document_range,
                    pub_range,
                ) {
                    let name = record.name();
                    if defined_records.contains(name)
                        || defined_enums.contains(name)
                        || defined_components.contains(name)
                        || imported_components.contains_key(name)
                    {
                        errors.push(ParseError::new(
                            ParseErrorKind::TypeNameIsAlreadyDefined {
                                name: record.name_range.to_cheap_string(),
                            },
                            record.name_range.clone(),
                        ));
                    } else {
                        defined_records.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::Record(record));
                }
            }
            Some((expr::Token::Enum, _)) => {
                if let Some(enum_decl) = parse_enum_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &document_range,
                    pub_range,
                ) {
                    let name = enum_decl.name();
                    if defined_enums.contains(name)
                        || defined_records.contains(name)
                        || defined_components.contains(name)
                        || imported_components.contains_key(name)
                    {
                        errors.push(ParseError::new(
                            ParseErrorKind::TypeNameIsAlreadyDefined {
                                name: enum_decl.name_range.to_cheap_string(),
                            },
                            enum_decl.name_range.clone(),
                        ));
                    } else {
                        defined_enums.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::Enum(enum_decl));
                }
            }
            Some((expr::Token::Component, _)) => {
                if let Some(component) = parse_component_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &document_id,
                    &defined_components,
                    &imported_components,
                    pub_range,
                ) {
                    let name = component.tag_name.as_str();
                    if defined_components.contains(name)
                        || imported_components.contains_key(name)
                        || defined_records.contains(name)
                        || defined_enums.contains(name)
                    {
                        errors.push(ParseError::new(
                            ParseErrorKind::TypeNameIsAlreadyDefined {
                                name: component.tag_name.to_cheap_string(),
                            },
                            component.tag_name.clone(),
                        ));
                    } else {
                        defined_components.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::Component(component));
                }
            }
            Some((expr::Token::View, _)) => {
                if let Some(view) = parse_view_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &document_id,
                    &defined_components,
                    &imported_components,
                    pub_range,
                ) {
                    let name = view.name.as_str();
                    if defined_views.contains(name)
                        || defined_components.contains(name)
                        || imported_components.contains_key(name)
                        || defined_records.contains(name)
                        || defined_enums.contains(name)
                    {
                        errors.push(ParseError::new(
                            ParseErrorKind::TypeNameIsAlreadyDefined {
                                name: view.name_range.to_cheap_string(),
                            },
                            view.name_range.clone(),
                        ));
                    } else {
                        defined_views.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::View(view));
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
        expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Import)?;
    let mut path_segments: Vec<DocumentRange> = Vec::new();
    let first_segment = match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
        Some((expr::Token::Identifier(_), seg_range))
        | Some((expr::Token::TypeName(_), seg_range)) => seg_range,
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
    while expr::tokenizer::advance_if(iter, comments, errors, expr::Token::ColonColon).is_some() {
        let segment = match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((expr::Token::Identifier(_), seg_range))
            | Some((expr::Token::TypeName(_), seg_range)) => seg_range,
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
    let path = module_path_range.to(type_name_range.clone());
    let full_import_range = import_range.to(type_name_range.clone());
    Some(ParsedImportDeclaration {
        type_name,
        type_name_range,
        path,
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
        expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Record)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let (name, name_range) = expr::tokenizer::expect_type_name(iter, comments, errors, range)?;
    let left_brace =
        expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::LeftBrace)?;
    let mut seen_names = HashSet::new();
    let (fields, right_brace) = expr::tokenizer::parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &expr::Token::LeftBrace,
        &left_brace,
        |iter, comments, errors, range| {
            let examples = parse_pattern_annotation(iter, comments, errors);
            let (field_name, field_name_range) =
                expr::tokenizer::expect_field_name(iter, comments, errors, range)?;
            expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Colon)?;
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
    let full_range = start_range.to(right_brace);
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
        expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Enum)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let (name, name_range) = expr::tokenizer::expect_type_name(iter, comments, errors, range)?;
    let left_brace =
        expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::LeftBrace)?;
    let mut seen_names = HashSet::new();
    let (variants, right_brace) = expr::tokenizer::parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &expr::Token::LeftBrace,
        &left_brace,
        |iter, comments, errors, range| {
            let (variant_name, variant_range) =
                expr::tokenizer::expect_type_name(iter, comments, errors, range)?;
            if !seen_names.insert(variant_range.to_cheap_string()) {
                errors.push(ParseError::new(
                    ParseErrorKind::DuplicateVariant {
                        name: variant_range.to_cheap_string(),
                    },
                    variant_range,
                ));
                return None;
            }
            let fields =
                if expr::tokenizer::advance_if(iter, comments, errors, expr::Token::LeftBrace)
                    .is_some()
                {
                    parse_enum_variant_fields(
                        iter,
                        comments,
                        errors,
                        range,
                        &expr::Token::RightBrace,
                    )?
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
    let full_range = start_range.to(right_brace);
    Some(ParsedEnumDeclaration {
        name,
        name_range,
        range: full_range,
        variants,
        pub_range,
    })
}

fn parse_enum_variant_fields(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
    closing: &expr::Token,
) -> Option<
    Vec<(
        FieldName,
        DocumentRange,
        ParsedType,
        Option<ExamplesAnnotation>,
    )>,
> {
    let mut fields = Vec::new();
    let mut seen_names = HashSet::new();
    if expr::tokenizer::advance_if(iter, comments, errors, closing.clone()).is_some() {
        return Some(fields);
    }
    loop {
        let examples = parse_pattern_annotation(iter, comments, errors);
        let (field_name, field_name_range) =
            expr::tokenizer::expect_field_name(iter, comments, errors, range)?;
        expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Colon)?;
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
        fields.push((field_name, field_name_range, field_type, examples));
        if expr::tokenizer::advance_if(iter, comments, errors, expr::Token::Comma).is_some() {
            if expr::tokenizer::advance_if(iter, comments, errors, closing.clone()).is_some() {
                break;
            }
        } else {
            expr::tokenizer::expect_token(iter, comments, errors, range, closing)?;
            break;
        }
    }
    Some(fields)
}

/// Parse a component declaration from a document cursor.
///
/// Syntax: `component Name(param: Type, ...) { ... }`
///
/// Returns `None` if parsing fails.
fn parse_component_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    document_id: &DocumentId,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, DocumentId>,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedComponentDeclaration> {
    // Consume the 'component' keyword
    let Some((expr::Token::Component, keyword_range)) =
        expr::tokenizer::next_collecting_comments(iter, comments, errors)
    else {
        return None;
    };

    // Parse the component name (must be PascalCase)
    let (name_str, name_range) =
        match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((expr::Token::TypeName(name_str), range)) => (name_str, range),
            Some((_, range)) => {
                errors.push(ParseError::new(
                ParseErrorKind::InvalidComponentName {
                    error:
                        crate::symbols::type_name::InvalidTypeNameError::DoesNotStartWithUppercase,
                },
                range,
            ));
                return None;
            }
            None => {
                errors.push(ParseError::new(
                ParseErrorKind::InvalidComponentName {
                    error:
                        crate::symbols::type_name::InvalidTypeNameError::DoesNotStartWithUppercase,
                },
                keyword_range,
            ));
                return None;
            }
        };

    let component_name = match TypeName::new(&name_str) {
        Ok(n) => n,
        Err(error) => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidComponentName { error },
                name_range,
            ));
            return None;
        }
    };

    // Parse parameters (parentheses are optional if no parameters)
    enum ParamItem {
        Param(Box<parsed_ast::ParsedParameter>),
        Rest {
            var_name: VarName,
            range: DocumentRange,
        },
    }

    let parsed_params = if let Some(left_paren) =
        expr::tokenizer::advance_if(iter, comments, errors, expr::Token::LeftParen)
    {
        let (items, right_paren) = expr::tokenizer::parse_delimited_list(
            iter,
            comments,
            errors,
            &left_paren,
            &expr::Token::LeftParen,
            &left_paren,
            |iter, comments, errors, range| {
                // A `...name` rest parameter.
                if let Some(dots_range) =
                    expr::tokenizer::advance_if(iter, comments, errors, expr::Token::DotDotDot)
                {
                    let (var_name, var_name_range) =
                        expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
                    return Some(ParamItem::Rest {
                        range: dots_range.to(var_name_range),
                        var_name,
                    });
                }
                // A regular parameter.
                let pattern = parse_pattern_annotation(iter, comments, errors);
                let (var_name, var_name_range) =
                    expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
                expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Colon)?;
                let var_type = parse_type(iter, comments, errors, range)?;
                let default_value =
                    if expr::tokenizer::advance_if(iter, comments, errors, expr::Token::Assign)
                        .is_some()
                    {
                        expr::parse_expr::parse_primary(iter, comments, errors, range)
                    } else {
                        None
                    };
                Some(ParamItem::Param(Box::new(parsed_ast::ParsedParameter {
                    var_name,
                    var_name_range,
                    var_type,
                    default_value,
                    examples: pattern,
                })))
            },
        )?;
        Some((items, left_paren.to(right_paren)))
    } else {
        None
    };

    let mut params = None;
    let mut rest_param: Option<(VarName, DocumentRange)> = None;
    if let Some((items, params_range)) = parsed_params {
        if let Some(first) = items
            .iter()
            .position(|i| matches!(i, ParamItem::Rest { .. }))
        {
            let after_rest = &items[first + 1..];
            for item in after_rest {
                if let ParamItem::Rest { range, .. } = item {
                    errors.push(ParseError::new(
                        ParseErrorKind::DuplicateRestParam {},
                        range.clone(),
                    ));
                }
            }
            if after_rest.iter().any(|i| matches!(i, ParamItem::Param(_))) {
                if let ParamItem::Rest { range, .. } = &items[first] {
                    errors.push(ParseError::new(
                        ParseErrorKind::RestParamMustBeLast {},
                        range.clone(),
                    ));
                }
            }
        }

        let mut regular = Vec::new();
        for item in items {
            match item {
                ParamItem::Param(p) => regular.push(*p),
                ParamItem::Rest {
                    var_name, range, ..
                } => {
                    rest_param.get_or_insert((var_name, range));
                }
            }
        }
        params = Some((regular, params_range));
    }

    let body_start = expr::tokenizer::expect_token(
        iter,
        comments,
        errors,
        &name_range,
        &expr::Token::LeftBrace,
    )?;

    // Parse the body - this contains HTML/component nodes
    let mut children = Vec::new();
    let mut spreads = Vec::new();
    let mut tokenizer = Tokenizer::new();

    while let Some(tree) = parse_tree(&mut tokenizer, iter, errors) {
        if let Some(node) = construct_node(
            tree,
            comments,
            errors,
            &mut spreads,
            document_id,
            defined_components,
            imported_components,
        ) {
            children.push(node);
        }
    }

    let body_end = expr::tokenizer::expect_opposite(
        iter,
        comments,
        errors,
        &expr::Token::LeftBrace,
        &body_start,
    )?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(body_end);

    // Validate the body's rest spreads against the declared rest parameter and select the
    // single target the rest forwards into. Spreads are collected as nodes finish parsing,
    // i.e. children before their parent, so order them by source position first.
    spreads.sort_by_key(|occ| occ.target.spread_range().start());
    let rest_name = rest_param.as_ref().map(|(n, _)| n);
    let mut valid: Vec<SpreadOccurrence> = Vec::new();
    for occ in spreads {
        match rest_name {
            Some(rn) if occ.spread_name == *rn => valid.push(occ),
            _ => errors.push(ParseError::new(
                ParseErrorKind::SpreadNotDeclaredRest {
                    name: occ.spread_name.clone(),
                },
                occ.target.spread_range().clone(),
            )),
        }
    }
    for occ in valid.iter().skip(1) {
        errors.push(ParseError::new(
            ParseErrorKind::RestSpreadMoreThanOnce {
                name: occ.spread_name.clone(),
            },
            occ.target.spread_range().clone(),
        ));
    }
    if let Some((name, range)) = rest_param.as_ref() {
        if valid.is_empty() {
            errors.push(ParseError::new(
                ParseErrorKind::RestNeverSpread {
                    component: component_name.clone(),
                    name: name.clone(),
                },
                range.clone(),
            ));
        }
    }
    let rest_target = valid.into_iter().next().map(|occ| occ.target);

    Some(ParsedComponentDeclaration {
        component_name,
        tag_name: name_range,
        closing_tag_name: None,
        params,
        rest_param,
        rest_target,
        range,
        children,
        pub_range,
    })
}

/// Parse an view declaration from a document cursor.
///
/// Syntax: `view Name(param: Type, ...) { ... }`
///
/// Returns `None` if parsing fails.
fn parse_view_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    document_id: &DocumentId,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, DocumentId>,
    pub_range: Option<DocumentRange>,
) -> Option<ParsedViewDeclaration> {
    // Consume the 'view' keyword
    let Some((expr::Token::View, keyword_range)) =
        expr::tokenizer::next_collecting_comments(iter, comments, errors)
    else {
        return None;
    };

    // Parse the view name (must be PascalCase)
    let (name_str, name_range) =
        match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((expr::Token::TypeName(name_str), range)) => (name_str, range),
            Some((expr::Token::Reserved(name), range)) => {
                errors.push(ParseError::new(
                    ParseErrorKind::ReservedViewName { name },
                    range,
                ));
                return None;
            }
            Some((_, range)) => {
                errors.push(ParseError::new(ParseErrorKind::InvalidViewName {}, range));
                return None;
            }
            None => {
                errors.push(ParseError::new(
                    ParseErrorKind::InvalidViewName {},
                    keyword_range,
                ));
                return None;
            }
        };

    let name = match TypeName::new(&name_str) {
        Ok(n) => n,
        Err(_) => {
            errors.push(ParseError::new(
                ParseErrorKind::InvalidViewName {},
                name_range,
            ));
            return None;
        }
    };

    // Parse parameters (parentheses are optional if no parameters)
    let (params, params_range) = if let Some(left_paren) =
        expr::tokenizer::advance_if(iter, comments, errors, expr::Token::LeftParen)
    {
        let (params, right_paren) = expr::tokenizer::parse_delimited_list(
            iter,
            comments,
            errors,
            &left_paren,
            &expr::Token::LeftParen,
            &left_paren,
            |iter, comments, errors, range| {
                let pattern = parse_pattern_annotation(iter, comments, errors);
                let (var_name, var_name_range) =
                    expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
                expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Colon)?;
                let var_type = parse_type(iter, comments, errors, range)?;
                if let Some(assign_range) =
                    expr::tokenizer::advance_if(iter, comments, errors, expr::Token::Assign)
                {
                    errors.push(ParseError::new(
                        ParseErrorKind::DefaultValueNotAllowedOnView {},
                        assign_range,
                    ));
                    expr::parse_expr::parse_primary(iter, comments, errors, range);
                }
                Some(parsed_ast::ParsedParameter {
                    var_name,
                    var_name_range,
                    var_type,
                    default_value: None,
                    examples: pattern,
                })
            },
        )?;
        (params, left_paren.to(right_paren))
    } else {
        (Vec::new(), name_range.clone())
    };

    let body_start = expr::tokenizer::expect_token(
        iter,
        comments,
        errors,
        &params_range,
        &expr::Token::LeftBrace,
    )?;

    let mut children = Vec::new();
    let mut spreads = Vec::new();
    let mut tokenizer = Tokenizer::new();

    while let Some(tree) = parse_tree(&mut tokenizer, iter, errors) {
        if let Some(node) = construct_node(
            tree,
            comments,
            errors,
            &mut spreads,
            document_id,
            defined_components,
            imported_components,
        ) {
            children.push(node);
        }
    }

    let body_end = expr::tokenizer::expect_opposite(
        iter,
        comments,
        errors,
        &expr::Token::LeftBrace,
        &body_start,
    )?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(body_end);
    // Views cannot declare a rest parameter, so any spread in the body fails to name one.
    for occ in &spreads {
        errors.push(ParseError::new(
            ParseErrorKind::SpreadNotDeclaredRest {
                name: occ.spread_name.clone(),
            },
            occ.target.spread_range().clone(),
        ));
    }
    Some(ParsedViewDeclaration {
        name,
        name_range,
        params,
        children,
        range,
        pub_range,
    })
}

/// Record a spread attribute on an HTML element, if present, as the body is parsed.
fn collect_element_spread(
    element: &HtmlElement,
    attributes: &[ParsedAttribute],
    out: &mut Vec<SpreadOccurrence>,
) {
    for attr in attributes {
        if let ParsedAttribute::Spread { name, range } = attr {
            let supplied_attrs = attributes
                .iter()
                .filter_map(|a| match a {
                    ParsedAttribute::Named { name, .. } => Some(name.to_cheap_string()),
                    ParsedAttribute::Spread { .. } => None,
                })
                .collect();
            out.push(SpreadOccurrence {
                spread_name: name.clone(),
                target: RestSpreadTarget::Element {
                    element: element.clone(),
                    supplied_attrs,
                    spread_range: range.clone(),
                },
            });
        }
    }
}

/// Record a spread attribute on a component invocation, if present, as the body is parsed.
fn collect_component_spread(
    callee: &TypeName,
    args: &[ParsedAttribute],
    has_children: bool,
    out: &mut Vec<SpreadOccurrence>,
) {
    for attr in args {
        if let ParsedAttribute::Spread { name, range } = attr {
            let supplied_attrs: Vec<CheapString> = args
                .iter()
                .filter_map(|a| match a {
                    ParsedAttribute::Named { name, .. } => Some(name.to_cheap_string()),
                    ParsedAttribute::Spread { .. } => None,
                })
                .collect();
            out.push(SpreadOccurrence {
                spread_name: name.clone(),
                target: RestSpreadTarget::Component {
                    callee: callee.clone(),
                    supplied_attrs,
                    has_children,
                    spread_range: range.clone(),
                },
            });
        }
    }
}

fn construct_node(
    tree: TokenTree,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    spreads: &mut Vec<SpreadOccurrence>,
    document_id: &DocumentId,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, DocumentId>,
) -> Option<ParsedNode> {
    match tree.token {
        Token::Comment { range } => Some(ParsedNode::Comment { range }),
        Token::ClosingTag { .. } => {
            // ClosingTags are not present in the token tree
            unreachable!()
        }
        Token::Doctype { range } => Some(ParsedNode::Doctype {
            value: range.to_cheap_string(),
            range,
        }),
        Token::Text { range } => Some(ParsedNode::Text { range }),
        Token::Newline { range } => Some(ParsedNode::Newline { range }),
        Token::TextExpression { content, range } => {
            let mut iter = content.cursor().peekable();
            expr::parse_expr::parse_expr(&mut iter, comments, errors, &content)
                .map(|expression| ParsedNode::TextExpression { expression, range })
        }
        Token::RawTextTag {
            tag_name,
            attributes,
            content,
            range,
            ..
        } => {
            let attributes = parse_attributes(&attributes, comments, errors);

            // Convert content to a Text child if present
            let children = content
                .map(|c| vec![ParsedNode::Text { range: c }])
                .unwrap_or_default();

            let element = match HtmlElement::parse(tag_name.as_str()) {
                Some(element) => element,
                None => {
                    errors.push(ParseError::new(
                        ParseErrorKind::UnknownHtmlElement {
                            tag: tag_name.to_cheap_string(),
                        },
                        tag_name.clone(),
                    ));
                    return None;
                }
            };

            collect_element_spread(&element, &attributes, spreads);
            Some(ParsedNode::Html {
                element,
                tag_name,
                closing_tag_name: None,
                attributes,
                range,
                children,
            })
        }
        Token::OpeningTag {
            tag_name,
            expression,
            attributes,
            range: opening_tag_range,
            ..
        } => {
            // Handle <match> specially - process children as <case> tags
            if tag_name.as_str() == "match" {
                errors.extend(disallow_attributes(&attributes, &tag_name));
                let subject = if let Some(e) = expression {
                    let mut iter = e.cursor().peekable();
                    expr::parse_expr::parse_expr(&mut iter, comments, errors, &e)
                } else {
                    errors.push(ParseError::new(
                        ParseErrorKind::MissingMatchExpression {},
                        opening_tag_range,
                    ));
                    None
                };
                let Some(subject) = subject else {
                    // Parse children to collect errors
                    for child in tree.children {
                        construct_node(
                            child,
                            comments,
                            errors,
                            spreads,
                            document_id,
                            defined_components,
                            imported_components,
                        );
                    }
                    return None;
                };

                // Process children as <case> tags
                let mut cases = Vec::new();
                for child_tree in tree.children {
                    match &child_tree.token {
                        // Ignore whitespace text and newlines
                        Token::Text { range, .. } if range.as_str().trim().is_empty() => {
                            continue;
                        }
                        Token::Newline { .. } => {
                            continue;
                        }
                        // Process <case> tags
                        Token::OpeningTag {
                            tag_name: case_tag_name,
                            expression: case_expression,
                            range: case_opening_range,
                            ..
                        } if case_tag_name.as_str() == "case" => {
                            let Some(pattern_range) = case_expression.clone() else {
                                errors.push(ParseError::new(
                                    ParseErrorKind::MissingCasePattern {},
                                    case_opening_range.clone(),
                                ));
                                continue;
                            };
                            let pattern = {
                                let mut iter = pattern_range.cursor().peekable();
                                expr::parse_expr::parse_match_pattern(
                                    &mut iter,
                                    comments,
                                    errors,
                                    &pattern_range,
                                )
                            };
                            let Some(pattern) = pattern else {
                                continue;
                            };
                            // Parse case children normally
                            let case_children: Vec<_> = child_tree
                                .children
                                .into_iter()
                                .filter_map(|c| {
                                    construct_node(
                                        c,
                                        comments,
                                        errors,
                                        spreads,
                                        document_id,
                                        defined_components,
                                        imported_components,
                                    )
                                })
                                .collect();
                            cases.push(ParsedMatchCase {
                                pattern,
                                children: case_children,
                            });
                        }
                        // Error on other nodes
                        _ => {
                            errors.push(ParseError::new(
                                ParseErrorKind::InvalidMatchChild {},
                                child_tree.range.clone(),
                            ));
                        }
                    }
                }

                return Some(ParsedNode::Match {
                    subject,
                    cases,
                    range: tree.range,
                });
            }

            let children: Vec<_> = tree
                .children
                .into_iter()
                .filter_map(|child| {
                    construct_node(
                        child,
                        comments,
                        errors,
                        spreads,
                        document_id,
                        defined_components,
                        imported_components,
                    )
                })
                .collect();

            match tag_name.as_str() {
                // <if {...}>
                "if" => {
                    errors.extend(disallow_attributes(&attributes, &tag_name));
                    let condition = if let Some(e) = expression {
                        let mut iter = e.cursor().peekable();
                        expr::parse_expr::parse_expr(&mut iter, comments, errors, &e)
                    } else {
                        errors.push(ParseError::new(
                            ParseErrorKind::MissingIfExpression {},
                            opening_tag_range,
                        ));
                        None
                    }?;
                    Some(ParsedNode::If {
                        condition,
                        range: tree.range.clone(),
                        children,
                    })
                }

                // <for {...}>
                "for" => {
                    errors.extend(disallow_attributes(&attributes, &tag_name));
                    let parse_result = if let Some(e) = expression {
                        let mut iter = e.cursor().peekable();
                        parse_loop_header(&mut iter, comments, errors, &e)
                    } else {
                        errors.push(ParseError::new(
                            ParseErrorKind::MissingForExpression {},
                            opening_tag_range,
                        ));
                        None
                    };
                    let loop_header = parse_result?;
                    Some(ParsedNode::For {
                        var_name: loop_header.var_name,
                        var_name_range: loop_header.var_name_range,
                        source: loop_header.loop_source,
                        range: tree.range.clone(),
                        children,
                    })
                }

                // <let {...}>
                "let" => {
                    errors.extend(disallow_attributes(&attributes, &tag_name));
                    let Some(bindings_range) = expression else {
                        errors.push(ParseError::new(
                            ParseErrorKind::MissingLetBinding {},
                            opening_tag_range,
                        ));
                        return None;
                    };
                    let parse_result = {
                        let mut iter = bindings_range.cursor().peekable();
                        parse_let_bindings(&mut iter, comments, errors, &bindings_range)
                    };
                    let parsed_bindings = parse_result?;
                    let bindings = parsed_bindings
                        .into_iter()
                        .map(
                            |(var_name, var_name_range, var_type, value_expr)| ParsedLetBinding {
                                var_name,
                                var_name_range,
                                var_type,
                                value_expr,
                            },
                        )
                        .collect();
                    Some(ParsedNode::Let {
                        bindings,
                        bindings_range,
                        range: tree.range.clone(),
                        children,
                    })
                }

                // <ComponentInvocation> - PascalCase indicates a component
                name if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
                    let component_name = match TypeName::new(name) {
                        Ok(name) => name,
                        Err(error) => {
                            errors.push(ParseError::new(
                                ParseErrorKind::InvalidComponentName { error },
                                tag_name.clone(),
                            ));
                            return None;
                        }
                    };

                    // Error if bare expression {..} is present on component invocation
                    if let Some(expr_range) = &expression {
                        errors.push(ParseError::new(
                            ParseErrorKind::UnexpectedComponentExpression {
                                tag_name: tag_name.to_cheap_string(),
                            },
                            expr_range.clone(),
                        ));
                    }

                    let parsed_args = parse_attributes(&attributes, comments, errors);

                    let declaring_module = if defined_components.contains(component_name.as_str()) {
                        Some(document_id.clone())
                    } else {
                        imported_components.get(component_name.as_str()).cloned()
                    };

                    let children = if tree.closing_tag_name.is_some() {
                        Some(children)
                    } else {
                        None
                    };

                    collect_component_spread(
                        &component_name,
                        &parsed_args,
                        children.is_some(),
                        spreads,
                    );
                    Some(ParsedNode::ComponentInvocation {
                        component_name,
                        component_name_opening_range: tag_name,
                        component_name_closing_range: tree.closing_tag_name,
                        declaring_module,
                        args: parsed_args,
                        range: tree.range,
                        children,
                    })
                }

                _ => {
                    // Default case: treat as HTML
                    let attributes = parse_attributes(&attributes, comments, errors);

                    let element = match HtmlElement::parse(tag_name.as_str()) {
                        Some(element) => element,
                        None if tag_name.as_str() == "case" => {
                            HtmlElement::Custom(tag_name.to_cheap_string())
                        }
                        None => {
                            errors.push(ParseError::new(
                                ParseErrorKind::UnknownHtmlElement {
                                    tag: tag_name.to_cheap_string(),
                                },
                                tag_name.clone(),
                            ));
                            return None;
                        }
                    };

                    collect_element_spread(&element, &attributes, spreads);
                    Some(ParsedNode::Html {
                        element,
                        tag_name,
                        closing_tag_name: tree.closing_tag_name,
                        attributes,
                        range: tree.range,
                        children,
                    })
                }
            }
        }
    }
}

struct ParsedLoopHeader {
    var_name: Option<VarName>,
    var_name_range: Option<DocumentRange>,
    loop_source: Box<ParsedLoopSource>,
}

fn parse_loop_header(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<ParsedLoopHeader> {
    let (var_name, var_name_range) = if let Some(underscore_range) =
        expr::tokenizer::advance_if(iter, comments, errors, expr::Token::Underscore)
    {
        (None, Some(underscore_range))
    } else {
        let (name, name_range) =
            expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
        (Some(name), Some(name_range))
    };
    expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::In)?;
    let start_expr = expr::parse_expr::parse_logical(iter, comments, errors, range)?;
    let source =
        if expr::tokenizer::advance_if(iter, comments, errors, expr::Token::DotDotEq).is_some() {
            let end_expr = expr::parse_expr::parse_logical(iter, comments, errors, range)?;
            ParsedLoopSource::RangeInclusive {
                start: start_expr,
                end: end_expr,
            }
        } else {
            ParsedLoopSource::Array(start_expr)
        };
    expr::tokenizer::expect_eof(iter, comments, errors)?;
    Some(ParsedLoopHeader {
        var_name,
        var_name_range,
        loop_source: Box::new(source),
    })
}

fn parse_let_bindings(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
    range: &DocumentRange,
) -> Option<Vec<(VarName, DocumentRange, Option<ParsedType>, expr::ParsedExpr)>> {
    let bindings = expr::tokenizer::parse_comma_separated(
        iter,
        comments,
        errors,
        range,
        |iter, comments, errors, range| {
            let (var_name, var_name_range) =
                expr::tokenizer::expect_variable_name(iter, comments, errors, range)?;
            let var_type = if let Some((expr::Token::Colon, _)) =
                expr::tokenizer::peek_past_comments(iter)
            {
                expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Colon)?;
                Some(parse_type(iter, comments, errors, range)?)
            } else {
                None
            };
            expr::tokenizer::expect_token(iter, comments, errors, range, &expr::Token::Assign)?;
            let value_expr = expr::parse_expr::parse_logical(iter, comments, errors, range)?;
            Some((var_name, var_name_range, var_type, value_expr))
        },
        None,
    )?;
    expr::tokenizer::expect_eof(iter, comments, errors)?;
    Some(bindings)
}

/// Parse a `#[examples(...)]` annotation using the expr tokenizer.
fn parse_pattern_annotation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<expr::ExamplesAnnotation> {
    if let Some((expr::Token::HashBracket, _)) = expr::tokenizer::peek_past_comments(iter) {
        expr::tokenizer::next_collecting_comments(iter, comments, errors);
        match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((expr::Token::Identifier(name), _)) if &*name == "examples" => {}
            _ => return None,
        }
        if expr::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
            != Some(expr::Token::LeftParen)
        {
            return None;
        }
        let mut annotation = expr::ExamplesAnnotation::default();
        loop {
            let key = match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
                Some((expr::Token::Identifier(name), _)) => name.to_string(),
                _ => return None,
            };
            if expr::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
                != Some(expr::Token::Assign)
            {
                return None;
            }
            match key.as_str() {
                "pattern" => {
                    let pattern =
                        match expr::tokenizer::next_collecting_comments(iter, comments, errors) {
                            Some((expr::Token::StringLiteral(s), _)) => s.to_string(),
                            _ => return None,
                        };
                    annotation.pattern = Some(pattern);
                }
                "min" | "max" | "min_len" | "max_len" => {
                    let (negative, token) = match expr::tokenizer::peek_past_comments(iter) {
                        Some((expr::Token::Minus, _)) => {
                            expr::tokenizer::next_collecting_comments(iter, comments, errors);
                            (
                                true,
                                expr::tokenizer::next_collecting_comments(iter, comments, errors),
                            )
                        }
                        _ => (
                            false,
                            expr::tokenizer::next_collecting_comments(iter, comments, errors),
                        ),
                    };
                    let n = match token {
                        Some((expr::Token::IntLiteral(n), _)) => {
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
            match expr::tokenizer::peek_past_comments(iter) {
                Some((expr::Token::Comma, _)) => {
                    expr::tokenizer::next_collecting_comments(iter, comments, errors);
                }
                _ => break,
            }
        }
        if expr::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
            != Some(expr::Token::RightParen)
        {
            return None;
        }
        if expr::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
            != Some(expr::Token::RightBracket)
        {
            return None;
        }
        Some(annotation)
    } else {
        None
    }
}

fn parse_attribute(
    item: &tokenizer::TokenizedAttribute,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<parsed_ast::ParsedAttribute> {
    match item {
        tokenizer::TokenizedAttribute::Named { name, value, .. } => {
            let value = match value {
                Some(tokenizer::TokenizedAttributeValue::String {
                    content,
                    quoted_range,
                }) => Some(parsed_ast::ParsedAttributeValue::String {
                    content: content.clone(),
                    quoted_range: quoted_range.clone(),
                }),
                Some(tokenizer::TokenizedAttributeValue::Expression(range)) => {
                    let mut iter = range.cursor().peekable();
                    let result = expr::parse_expr::parse_expr(&mut iter, comments, errors, range);
                    Some(result.map(parsed_ast::ParsedAttributeValue::Expression)?)
                }
                None => None,
            };
            Some(parsed_ast::ParsedAttribute::Named {
                name: name.clone(),
                value,
            })
        }
        tokenizer::TokenizedAttribute::Spread { name, range } => {
            match VarName::new(name.as_str()) {
                Ok(var_name) => Some(parsed_ast::ParsedAttribute::Spread {
                    name: var_name,
                    range: range.clone(),
                }),
                Err(error) => {
                    errors.push(ParseError::new(
                        ParseErrorKind::InvalidVariableName {
                            name: name.to_cheap_string(),
                            error,
                        },
                        name.clone(),
                    ));
                    None
                }
            }
        }
    }
}

fn parse_attributes(
    attributes: &[tokenizer::TokenizedAttribute],
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Vec<parsed_ast::ParsedAttribute> {
    attributes
        .iter()
        .filter_map(|item| parse_attribute(item, comments, errors))
        .collect()
}

fn disallow_attributes<'a>(
    attributes: &'a [tokenizer::TokenizedAttribute],
    tag_name: &'a DocumentRange,
) -> impl Iterator<Item = ParseError> + 'a {
    attributes.iter().map(move |item| {
        let (name, range) = (item.name().to_cheap_string(), item.range().clone());
        ParseError::new(
            ParseErrorKind::UnrecognizedAttribute {
                tag_name: tag_name.to_cheap_string(),
                attr_name: name,
            },
            range,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use crate::hop::parsing::formatter;
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
                .annotate(&DocumentId::new("test.hop").unwrap(), errors.to_vec())
                .render();
            panic!("expected no parse errors, got:\n{rendered}");
        }
        expected.assert_eq(&formatter::format(&module));
    }

    fn reject(input: &str, expected: Expect) {
        let mut errors = Vec::new();
        let document_id = DocumentId::new("test.hop").unwrap();
        parse(
            document_id.clone(),
            Document::new(document_id, input.to_string()),
            &mut errors,
        );
        if errors.is_empty() {
            panic!("expected parse errors but got none");
        }
        let actual = DocumentAnnotator::new()
            .with_label("error")
            .with_lines_before(1)
            .annotate(&DocumentId::new("test.hop").unwrap(), errors.to_vec())
            .render();
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
                error: 'pub' is not allowed here
                1 | pub import other::Foo
                  | ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_pub_at_end_of_file() {
        reject(
            "pub",
            expect![[r#"
                error: 'pub' is not allowed here
                1 | pub
                  | ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_comment_between_components() {
        accept(
            indoc! {"
                component First {}
                // This is a comment
                component Second {}
            "},
            expect![[r#"
                component First {
                }

                // This is a comment
                component Second {
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
                  {label}
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
                  {label}
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
    fn rejects_when_tags_are_not_closed() {
        reject(
            indoc! {"
                component Main {
                    <div>
                    <p>
                }
            "},
            expect![[r#"
                error: Unclosed <div>
                1 | component Main {
                2 |     <div>
                  |      ^^^

                error: Unclosed <p>
                2 |     <div>
                3 |     <p>
                  |      ^
            "#]],
        );
    }

    #[test]
    fn rejects_when_void_tag_is_closed_with_closing_tag() {
        reject(
            indoc! {"
                component Main {
                    <hr></hr>
                    <br></br>
                    <input></input>
                }
            "},
            expect![[r#"
                error: <hr> should not be closed using a closing tag
                1 | component Main {
                2 |     <hr></hr>
                  |         ^^^^^

                error: <br> should not be closed using a closing tag
                2 |     <hr></hr>
                3 |     <br></br>
                  |         ^^^^^

                error: <input> should not be closed using a closing tag
                3 |     <br></br>
                4 |     <input></input>
                  |            ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_void_tags_to_be_self_closing() {
        accept(
            indoc! {r#"
                import bar::Bar
                component Main {
                    <hr/>
                    <br/>
                    <input/>
                }
                component Foo {
                }
            "#},
            expect![[r#"
                import bar::Bar

                component Main {
                  <hr>
                  <br>
                  <input>
                }

                component Foo {
                }
            "#]],
        );
    }

    #[test]
    fn accepts_doctype_tags_inside_components() {
        accept(
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
                component Main(foo: String) {
                  <!DOCTYPE html>
                  <html>
                    <body>
                      <div>
                        hello world
                      </div>
                    </body>
                  </html>
                }
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
                error: Missing expression in <if> tag
                1 | component Main {
                2 |     <if>
                  |     ^^^^
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
                error: Missing loop generator expression in <for> tag
                1 | component Main {
                2 |     <for>
                  |     ^^^^^
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
                error: Expected token 'in' but got end of file
                1 | component Main {
                2 |     <for {foo}>
                  |           ^^^
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
                error: Unexpected character: '~'
                1 | component Main {
                2 |     <if {~}>
                  |          ^

                error: Unexpected end of expression
                1 | component Main {
                2 |     <if {~}>
                  |          ^
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
                error: Expected type name but got )
                1 | component Main(data: Array[) {
                  |                            ^

                error: Unexpected text at top level
                1 | component Main(data: Array[) {
                  |                              ^
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
    fn rejects_multiple_expressions_in_attribute() {
        reject(
            indoc! {r#"
                component Main(style1: String, style2: String, style3: String) {
                    <div class={style1, style2, style3}>Content</div>
                }
            "#},
            expect![[r#"
                error: Unexpected token ','
                1 | component Main(style1: String, style2: String, style3: String) {
                2 |     <div class={style1, style2, style3}>Content</div>
                  |                       ^
            "#]],
        );
    }

    #[test]
    fn rejects_when_an_import_is_imported_twice() {
        reject(
            indoc! {r#"
                import other::Foo
                import other::Foo

                component Main {
                	<Foo></Foo>
                }
            "#},
            expect![[r#"
                error: Foo is already defined
                1 | import other::Foo
                2 | import other::Foo
                  |               ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_is_defined_twice() {
        reject(
            indoc! {r#"
                component Foo {
                }

                component Foo {
                }
            "#},
            expect![[r#"
                error: Foo is already defined
                3 | 
                4 | component Foo {
                  |           ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_is_defined_with_the_same_name_as_an_import() {
        reject(
            indoc! {r#"
                import other::Foo

                component Foo {
                }

                component Bar {
                	<Foo/>
                }
            "#},
            expect![[r#"
                error: Foo is already defined
                2 | 
                3 | component Foo {
                  |           ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_component_is_defined_with_the_same_name_as_a_record() {
        reject(
            indoc! {r#"
                record User {
                  name: String,
                }

                component User {
                }
            "#},
            expect![[r#"
                error: User is already defined
                4 | 
                5 | component User {
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_a_record_is_defined_with_the_same_name_as_an_import() {
        reject(
            indoc! {r#"
                import other::User

                record User {
                  name: String
                }
            "#},
            expect![[r#"
                error: User is already defined
                2 | 
                3 | record User {
                  |        ^^^^
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
                error: Import path must have at least two segments: module::Component
                1 | import Foo
                  |        ^^^
            "#]],
        );
    }

    #[test]
    fn accepts_component_invocations() {
        accept(
            indoc! {"
                component Main(p: String) {
                    <Foo/>
                    <Foo/>
                }
            "},
            expect![[r#"
                component Main(p: String) {
                  <Foo/>
                  <Foo/>
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
                    <Foo a={data}/>
                    <Bar b={data.user}/>
                }
            "#},
            expect![[r#"
                import foo::Foo
                import bar::Bar

                record Data {
                  user: String,
                }

                component Main(data: Data) {
                  <Foo a={data}/>
                  <Bar b={data.user}/>
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
                error: Unknown HTML element <dvi>
                1 | component Main {
                2 |     <dvi>oops</dvi>
                  |      ^^^
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
                error: Unknown HTML element <math>
                1 | component Main {
                2 |     <math></math>
                  |      ^^^^
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
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                }
            "},
            expect![[r#"
                record Data {
                  message: String,
                }

                component Main(data: Data) {
                  <h1>
                    Hello World
                  </h1>
                  <p>
                    {data.message}
                  </p>
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
                    Hello {name}!
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
                    User {user.name} has {user.count} items
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
                    {greeting} world!
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
                    Price: {price}
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
                error: Empty expression
                1 | component Main {<div>Empty: {}</div>}
                  |                             ^^
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
                    Status: {user.profile.status == "active"}
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
                    {first}{second}
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
                error: Expected type name but got component
                1 | record
                2 | component Main {
                  | ^^^^^^^^^

                error: Unexpected text at top level
                1 | record
                2 | component Main {
                  |           ^^^^
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
                error: Unexpected text at top level
                1 | foo
                  | ^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_enum_is_defined_with_the_same_name_as_a_record() {
        reject(
            indoc! {"
                record Color {
                    name: String,
                }

                enum Color {Red, Green, Blue}
            "},
            expect![[r#"
                error: Color is already defined
                4 | 
                5 | enum Color {Red, Green, Blue}
                  |      ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_record_is_defined_with_the_same_name_as_an_enum() {
        reject(
            indoc! {"
                enum Color {Red, Green, Blue}

                record Color {
                    name: String,
                }
            "},
            expect![[r#"
                error: Color is already defined
                2 | 
                3 | record Color {
                  |        ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_component_is_defined_with_the_same_name_as_an_enum() {
        reject(
            indoc! {"
                enum Color {Red, Green, Blue}

                component Color {
                }
            "},
            expect![[r#"
                error: Color is already defined
                2 | 
                3 | component Color {
                  |           ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_when_enum_is_defined_with_the_same_name_as_an_import() {
        reject(
            indoc! {"
                import other::Color

                enum Color {Red, Green, Blue}
            "},
            expect![[r#"
                error: Color is already defined
                2 | 
                3 | enum Color {Red, Green, Blue}
                  |      ^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_match_expression_in_template() {
        accept(
            indoc! {r#"
                enum Color {Red, Green, Blue}

                component Main(color: Color) {
                    {match color {Color::Red => "red", Color::Blue => "blue"}}
                }
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                component Main(color: Color) {
                  {match color {Color::Red => "red", Color::Blue => "blue"}}
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
                    {match status {
                        Status::Active => "active",
                        Status::Inactive => "inactive",
                        Status::Pending => "pending",
                    }}
                }
            "#},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                component Main(status: Status) {
                  {match status {
                    Status::Active => "active",
                    Status::Inactive => "inactive",
                    Status::Pending => "pending",
                  }}
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
                      found {y}
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
                      Success: {v}
                    </case>
                    <case {Outcome::Failure {message: m}}>
                      Error: {m}
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
                error: Missing expression in <match> tag
                1 | component Main {
                2 |     <match>
                  |     ^^^^^^^
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
                error: Missing pattern in <case> tag
                2 |     <match {flag}>
                3 |         <case>yes</case>
                  |         ^^^^^^
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
                error: Only <case> tags are allowed inside <match>
                2 |     <match {flag}>
                3 |         <div>not allowed</div>
                  |         ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_case_outside_match_as_html() {
        accept(
            indoc! {r#"
                component Main {
                    <case {true}>standalone case</case>
                }
            "#},
            expect![[r#"
                component Main {
                  <case>
                    standalone case
                  </case>
                }
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
                      Hello {name}
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
                        {a} + {b}
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
                error: Missing binding in <let> tag
                1 | component Main {
                2 |     <let>
                  |     ^^^^^
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
                error: Expected token '=' but got end of file
                1 | component Main {
                2 |     <let {x: String}>
                  |           ^^^^^^^^^
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
                      {first} {second}
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
                      {a} + {b} + {c}
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
                      Hello {name}
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
                      {first} {second}
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
                error: Unexpected token 'second'
                1 | component Main {
                2 |     <let {first: String = "a" second: String = "b"}>
                  |                               ^^^^^^
            "#]],
        );
    }

    #[test]
    fn accepts_multiple_sibling_let_tags() {
        accept(
            indoc! {r#"
                component Main {
                    <let {a: String = "Hello"}>
                        {a}
                    </let>
                    <let {b: String = "World"}>
                        {b}
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {a: String = "Hello"}>
                    {a}
                  </let>
                  <let {b: String = "World"}>
                    {b}
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_after_html_element() {
        accept(
            indoc! {r#"
                component Main {
                    <div>First</div>
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                }
            "#},
            expect![[r#"
                component Main {
                  <div>
                    First
                  </div>
                  <let {name: String = "World"}>
                    <div>
                      Hello {name}
                    </div>
                  </let>
                }
            "#]],
        );
    }

    #[test]
    fn accepts_let_before_html_element() {
        accept(
            indoc! {r#"
                component Main {
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                    <div>Last</div>
                }
            "#},
            expect![[r#"
                component Main {
                  <let {name: String = "World"}>
                    <div>
                      Hello {name}
                    </div>
                  </let>
                  <div>
                    Last
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn rejects_top_level_html_element() {
        reject(
            "<div></div>",
            expect![[r#"
                error: Unexpected text at top level
                1 | <div></div>
                  | ^
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
                    {name}: {count}
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
    fn rejects_duplicate_view_names() {
        reject(
            indoc! {"
                view Index() {
                    <div>First</div>
                }

                view Index() {
                    <div>Second</div>
                }
            "},
            expect![[r#"
                error: Index is already defined
                4 | 
                5 | view Index() {
                  |      ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_same_name_as_component() {
        reject(
            indoc! {"
                component Index {
                    <div>Component</div>
                }

                view Index() {
                    <div>Entrypoint</div>
                }
            "},
            expect![[r#"
                error: Index is already defined
                4 | 
                5 | view Index() {
                  |      ^^^^^
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
                error: 'Error' is a reserved word and cannot be used as a view name
                1 | view Error() {
                  |      ^^^^^

                error: Unexpected text at top level
                1 | view Error() {
                  |           ^
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
                error: View name must start with an uppercase letter
                1 | view index() {
                  |      ^^^^^

                error: Unexpected text at top level
                1 | view index() {
                  |           ^
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
                error: Default values are not allowed on view parameters
                1 | view Index(name: String = "World") {
                  |                         ^
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_empty_body() {
        accept(
            indoc! {"
                view Index() {
                }
            "},
            expect![[r#"
                view Index {
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
                      Hello {name}
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
    fn rejects_view_with_same_name_as_record() {
        reject(
            indoc! {"
                record Index {
                    name: String
                }

                view Index() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                error: Index is already defined
                4 | 
                5 | view Index() {
                  |      ^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_view_with_same_name_as_enum() {
        reject(
            indoc! {"
                enum Index {
                    Page,
                    Home
                }

                view Index() {
                    <div>Hello</div>
                }
            "},
            expect![[r#"
                error: Index is already defined
                5 | 
                6 | view Index() {
                  |      ^^^^^
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
                error: Default values are not allowed on view parameters
                1 | view Index(required: String, optional: Int = 42) {
                  |                                            ^
            "#]],
        );
    }

    #[test]
    fn accepts_view_with_top_level_text() {
        accept(
            indoc! {"
                view Test {
                  hello world
                }
            "},
            expect![[r#"
                view Test {
                  hello world
                }
            "#]],
        );
    }

    #[test]
    fn accepts_escape_sequences_in_strings() {
        accept(
            indoc! {r#"
                component Test {
                    {"hello\nworld"}
                    {"tab\there"}
                    {"back\\slash"}
                    {"quote\"here"}
                }
            "#},
            expect![[r#"
                component Test {
                  {"hello\nworld"}
                  {"tab\there"}
                  {"back\\slash"}
                  {"quote\"here"}
                }
            "#]],
        );
    }

    #[test]
    fn rejects_invalid_escape_sequences_in_strings() {
        reject(
            r#"component Test {{"invalid\q"}}"#,
            expect![[r#"
                error: Invalid escape sequence '\q'
                1 | component Test {{"invalid\q"}}
                  |                          ^^
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
                error: Rest parameter must be the last parameter
                1 | component Foo(...rest, a: String, b: String) {
                  |               ^^^^^^^
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
                error: At most one rest parameter is allowed
                1 | component Foo(...a, ...b) {
                  |                     ^^^^
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
                error: Invalid variable name 'Bar': Variable name must be lowercase (found uppercase: 'B')
                1 | component Foo() {
                2 |   <button ...Bar></button>
                  |              ^^^
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
                error: Invalid variable name '_x': Variable name cannot start with underscore
                1 | component Foo() {
                2 |   <button ..._x></button>
                  |              ^^
            "#]],
        );
    }

    #[test]
    fn rejects_rest_param_never_spread() {
        reject(
            indoc! {r#"
                component Foo(...rest) {
                  <div></div>
                }
            "#},
            expect![[r#"
                error: Component Foo declares rest parameter 'rest' but never spreads it
                1 | component Foo(...rest) {
                  |               ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_spread_without_declared_rest() {
        reject(
            indoc! {r#"
                component Foo() {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                error: Spread '...rest' does not refer to a declared rest parameter
                1 | component Foo() {
                2 |   <div ...rest></div>
                  |        ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_rest_spread_more_than_once() {
        reject(
            indoc! {r#"
                component Foo(...rest) {
                  <div ...rest><span ...rest></span></div>
                }
            "#},
            expect![[r#"
                error: Rest parameter 'rest' is spread more than once
                1 | component Foo(...rest) {
                2 |   <div ...rest><span ...rest></span></div>
                  |                      ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn rejects_spread_in_view() {
        reject(
            indoc! {r#"
                view Main {
                  <div ...rest></div>
                }
            "#},
            expect![[r#"
                error: Spread '...rest' does not refer to a declared rest parameter
                1 | view Main {
                2 |   <div ...rest></div>
                  |        ^^^^^^^
            "#]],
        );
    }
}
