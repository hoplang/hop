use super::parsed_ast::{
    self, ParsedAst, ParsedComponentDeclaration, ParsedDeclaration, ParsedEnumDeclaration,
    ParsedEnumDeclarationVariant, ParsedImportDeclaration, ParsedRecordDeclaration,
    ParsedRecordDeclarationField, ParsedViewDeclaration,
};
use super::parsed_node::{ParsedLetBinding, ParsedLoopSource, ParsedMatchCase, ParsedNode};
use super::token_tree::{TokenTree, parse_tree};
use super::tokenizer::Tokenizer;
use super::tokenizer::{self, Token};
use crate::document::{Document, DocumentCursor, DocumentRange};
use crate::document_id::DocumentId;
use crate::dop::parsing::ParsedType;
use crate::dop::parsing::parse_type::parse_type;
use crate::dop::{self, ExamplesAnnotation};
use crate::html::HtmlElement;
use crate::parse_error::ParseError;
use crate::symbols::field_name::FieldName;
use crate::symbols::module_name::ModuleName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::Peekable;

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
            dop::tokenizer::advance_if(&mut iter, &mut comments, errors, dop::Token::Pub);

        match dop::tokenizer::peek_past_comments(&iter) {
            Some((dop::Token::Import, _)) => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::UnexpectedPubKeyword { range: pub_r });
                }
                if let Some(import) =
                    parse_import_declaration(&mut iter, &mut comments, errors, &document_range)
                {
                    let name_str = import.type_name.as_str();
                    if imported_components.contains_key(name_str) {
                        errors.push(ParseError::TypeNameIsAlreadyDefined {
                            name: import.type_name_range.to_cheap_string(),
                            range: import.type_name_range.clone(),
                        });
                    } else {
                        imported_components
                            .insert(name_str.to_string(), import.module_name.to_document_id());
                    }
                    declarations.push(ParsedDeclaration::Import(import));
                }
            }
            Some((dop::Token::Record, _)) => {
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
                        errors.push(ParseError::TypeNameIsAlreadyDefined {
                            name: record.name_range.to_cheap_string(),
                            range: record.name_range.clone(),
                        });
                    } else {
                        defined_records.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::Record(record));
                }
            }
            Some((dop::Token::Enum, _)) => {
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
                        errors.push(ParseError::TypeNameIsAlreadyDefined {
                            name: enum_decl.name_range.to_cheap_string(),
                            range: enum_decl.name_range.clone(),
                        });
                    } else {
                        defined_enums.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::Enum(enum_decl));
                }
            }
            Some((dop::Token::Component, _)) => {
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
                        errors.push(ParseError::TypeNameIsAlreadyDefined {
                            name: component.tag_name.to_cheap_string(),
                            range: component.tag_name.clone(),
                        });
                    } else {
                        defined_components.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::Component(component));
                }
            }
            Some((dop::Token::View, _)) => {
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
                        errors.push(ParseError::TypeNameIsAlreadyDefined {
                            name: view.name_range.to_cheap_string(),
                            range: view.name_range.clone(),
                        });
                    } else {
                        defined_views.insert(name.to_string());
                    }
                    declarations.push(ParsedDeclaration::View(view));
                }
            }
            Some((_, token_range)) => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::UnexpectedPubKeyword { range: pub_r });
                }
                // Unexpected token at top level
                errors.push(ParseError::UnexpectedTopLevelText { range: token_range });
                break;
            }
            None => {
                if let Some(pub_r) = pub_range {
                    errors.push(ParseError::UnexpectedPubKeyword { range: pub_r });
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
        dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Import)?;
    let mut path_segments: Vec<DocumentRange> = Vec::new();
    let first_segment = match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
        Some((dop::Token::Identifier(_), seg_range))
        | Some((dop::Token::TypeName(_), seg_range)) => seg_range,
        Some((_, seg_range)) => {
            errors.push(ParseError::ExpectedModulePath { range: seg_range });
            return None;
        }
        None => {
            errors.push(ParseError::ExpectedModulePath {
                range: range.clone(),
            });
            return None;
        }
    };
    path_segments.push(first_segment);
    while dop::tokenizer::advance_if(iter, comments, errors, dop::Token::ColonColon).is_some() {
        let segment = match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((dop::Token::Identifier(_), seg_range))
            | Some((dop::Token::TypeName(_), seg_range)) => seg_range,
            Some((_, seg_range)) => {
                errors.push(ParseError::ExpectedIdentifierAfterColonColon { range: seg_range });
                return None;
            }
            None => {
                errors.push(ParseError::ExpectedIdentifierAfterColonColon {
                    range: range.clone(),
                });
                return None;
            }
        };
        path_segments.push(segment);
    }
    if path_segments.len() < 2 {
        errors.push(ParseError::ImportPathTooShort {
            range: path_segments[0].clone(),
        });
        return None;
    }
    let type_name_range = path_segments.pop().unwrap();
    let type_name = match TypeName::from_cheap_string(type_name_range.to_cheap_string()) {
        Ok(name) => name,
        Err(e) => {
            errors.push(ParseError::InvalidTypeName {
                error: e,
                range: type_name_range.clone(),
            });
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
            errors.push(ParseError::InvalidModuleName {
                error: e,
                range: module_path_range.clone(),
            });
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
        dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Record)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let (name, name_range) = dop::tokenizer::expect_type_name(iter, comments, errors, range)?;
    let left_brace =
        dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::LeftBrace)?;
    let mut fields = Vec::new();
    let mut seen_names = HashSet::new();
    let right_brace = dop::tokenizer::parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &dop::Token::LeftBrace,
        &left_brace,
        |iter, comments, errors, range| {
            let examples = parse_pattern_annotation(iter, comments, errors);
            let (field_name, field_name_range) =
                dop::tokenizer::expect_field_name(iter, comments, errors, range)?;
            dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Colon)?;
            let field_type = parse_type(iter, comments, errors, range)?;
            if !seen_names.insert(field_name_range.to_cheap_string()) {
                errors.push(ParseError::DuplicateField {
                    name: field_name_range.to_cheap_string(),
                    range: field_name_range.clone(),
                });
                return None;
            }
            fields.push(ParsedRecordDeclarationField {
                name: field_name,
                name_range: field_name_range,
                field_type,
                examples,
            });
            Some(())
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
        dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Enum)?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let (name, name_range) = dop::tokenizer::expect_type_name(iter, comments, errors, range)?;
    let left_brace =
        dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::LeftBrace)?;
    let mut variants = Vec::new();
    let mut seen_names = HashSet::new();
    let right_brace = dop::tokenizer::parse_delimited_list(
        iter,
        comments,
        errors,
        range,
        &dop::Token::LeftBrace,
        &left_brace,
        |iter, comments, errors, range| {
            let (variant_name, variant_range) =
                dop::tokenizer::expect_type_name(iter, comments, errors, range)?;
            if !seen_names.insert(variant_range.to_cheap_string()) {
                errors.push(ParseError::DuplicateVariant {
                    name: variant_range.to_cheap_string(),
                    range: variant_range.clone(),
                });
                return None;
            }
            let fields =
                if dop::tokenizer::advance_if(iter, comments, errors, dop::Token::LeftBrace)
                    .is_some()
                {
                    parse_enum_variant_fields(
                        iter,
                        comments,
                        errors,
                        range,
                        &dop::Token::RightBrace,
                    )?
                } else {
                    Vec::new()
                };
            variants.push(ParsedEnumDeclarationVariant {
                name: variant_name,
                name_range: variant_range,
                fields,
            });
            Some(())
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
    closing: &dop::Token,
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
    if dop::tokenizer::advance_if(iter, comments, errors, closing.clone()).is_some() {
        return Some(fields);
    }
    loop {
        let examples = parse_pattern_annotation(iter, comments, errors);
        let (field_name, field_name_range) =
            dop::tokenizer::expect_field_name(iter, comments, errors, range)?;
        dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Colon)?;
        let field_type = parse_type(iter, comments, errors, range)?;
        if !seen_names.insert(field_name_range.to_cheap_string()) {
            errors.push(ParseError::DuplicateField {
                name: field_name_range.to_cheap_string(),
                range: field_name_range.clone(),
            });
            return None;
        }
        fields.push((field_name, field_name_range, field_type, examples));
        if dop::tokenizer::advance_if(iter, comments, errors, dop::Token::Comma).is_some() {
            if dop::tokenizer::advance_if(iter, comments, errors, closing.clone()).is_some() {
                break;
            }
        } else {
            dop::tokenizer::expect_token(iter, comments, errors, range, closing)?;
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
    let Some((dop::Token::Component, keyword_range)) =
        dop::tokenizer::next_collecting_comments(iter, comments, errors)
    else {
        return None;
    };

    // Parse the component name (must be PascalCase)
    let (name_str, name_range) =
        match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((dop::Token::TypeName(name_str), range)) => (name_str, range),
            Some((_, range)) => {
                errors.push(ParseError::InvalidComponentName {
                    error:
                        crate::symbols::type_name::InvalidTypeNameError::DoesNotStartWithUppercase,
                    range,
                });
                return None;
            }
            None => {
                errors.push(ParseError::InvalidComponentName {
                    error:
                        crate::symbols::type_name::InvalidTypeNameError::DoesNotStartWithUppercase,
                    range: keyword_range,
                });
                return None;
            }
        };

    let component_name = match TypeName::new(&name_str) {
        Ok(n) => n,
        Err(error) => {
            errors.push(ParseError::InvalidComponentName {
                error,
                range: name_range,
            });
            return None;
        }
    };

    // Parse parameters (parentheses are optional if no parameters)
    let params = if let Some(left_paren) =
        dop::tokenizer::advance_if(iter, comments, errors, dop::Token::LeftParen)
    {
        let mut params = Vec::new();
        let right_paren = dop::tokenizer::parse_delimited_list(
            iter,
            comments,
            errors,
            &left_paren,
            &dop::Token::LeftParen,
            &left_paren,
            |iter, comments, errors, range| {
                let pattern = parse_pattern_annotation(iter, comments, errors);
                let (var_name, var_name_range) =
                    dop::tokenizer::expect_variable_name(iter, comments, errors, range)?;
                dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Colon)?;
                let var_type = parse_type(iter, comments, errors, range)?;
                let default_value =
                    if dop::tokenizer::advance_if(iter, comments, errors, dop::Token::Assign)
                        .is_some()
                    {
                        dop::parse_expr::parse_primary(iter, comments, errors, range)
                    } else {
                        None
                    };
                params.push(parsed_ast::ParsedParameter {
                    var_name,
                    var_name_range,
                    var_type,
                    default_value,
                    examples: pattern,
                });
                Some(())
            },
        )?;
        Some((params, left_paren.to(right_paren)))
    } else {
        None
    };

    let body_start =
        dop::tokenizer::expect_token(iter, comments, errors, &name_range, &dop::Token::LeftBrace)?;

    // Parse the body - this contains HTML/component nodes
    let mut children = Vec::new();
    let mut tokenizer = Tokenizer::new();

    while let Some(tree) = parse_tree(&mut tokenizer, iter, errors) {
        if let Some(node) = construct_node(
            tree,
            comments,
            errors,
            document_id,
            defined_components,
            imported_components,
        ) {
            children.push(node);
        }
    }

    let body_end = dop::tokenizer::expect_opposite(
        iter,
        comments,
        errors,
        &dop::Token::LeftBrace,
        &body_start,
    )?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(body_end);
    Some(ParsedComponentDeclaration {
        component_name,
        tag_name: name_range,
        closing_tag_name: None,
        params,
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
    let Some((dop::Token::View, keyword_range)) =
        dop::tokenizer::next_collecting_comments(iter, comments, errors)
    else {
        return None;
    };

    // Parse the view name (must be PascalCase)
    let (name_str, name_range) =
        match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((dop::Token::TypeName(name_str), range)) => (name_str, range),
            Some((dop::Token::Reserved(name), range)) => {
                errors.push(ParseError::ReservedViewName { name, range });
                return None;
            }
            Some((_, range)) => {
                errors.push(ParseError::InvalidViewName { range });
                return None;
            }
            None => {
                errors.push(ParseError::InvalidViewName {
                    range: keyword_range,
                });
                return None;
            }
        };

    let name = match TypeName::new(&name_str) {
        Ok(n) => n,
        Err(_) => {
            errors.push(ParseError::InvalidViewName {
                range: name_range.clone(),
            });
            return None;
        }
    };

    // Parse parameters (parentheses are optional if no parameters)
    let (params, params_range) = if let Some(left_paren) =
        dop::tokenizer::advance_if(iter, comments, errors, dop::Token::LeftParen)
    {
        let mut params = Vec::new();
        let right_paren = dop::tokenizer::parse_delimited_list(
            iter,
            comments,
            errors,
            &left_paren,
            &dop::Token::LeftParen,
            &left_paren,
            |iter, comments, errors, range| {
                let pattern = parse_pattern_annotation(iter, comments, errors);
                let (var_name, var_name_range) =
                    dop::tokenizer::expect_variable_name(iter, comments, errors, range)?;
                dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Colon)?;
                let var_type = parse_type(iter, comments, errors, range)?;
                if let Some(assign_range) =
                    dop::tokenizer::advance_if(iter, comments, errors, dop::Token::Assign)
                {
                    errors.push(ParseError::DefaultValueNotAllowedOnView {
                        range: assign_range,
                    });
                    dop::parse_expr::parse_primary(iter, comments, errors, range);
                }
                params.push(parsed_ast::ParsedParameter {
                    var_name,
                    var_name_range,
                    var_type,
                    default_value: None,
                    examples: pattern,
                });
                Some(())
            },
        )?;
        (params, left_paren.to(right_paren))
    } else {
        (Vec::new(), name_range.clone())
    };

    let body_start = dop::tokenizer::expect_token(
        iter,
        comments,
        errors,
        &params_range,
        &dop::Token::LeftBrace,
    )?;

    let mut children = Vec::new();
    let mut tokenizer = Tokenizer::new();

    while let Some(tree) = parse_tree(&mut tokenizer, iter, errors) {
        if let Some(node) = construct_node(
            tree,
            comments,
            errors,
            document_id,
            defined_components,
            imported_components,
        ) {
            children.push(node);
        }
    }

    let body_end = dop::tokenizer::expect_opposite(
        iter,
        comments,
        errors,
        &dop::Token::LeftBrace,
        &body_start,
    )?;
    let start_range = pub_range.clone().unwrap_or_else(|| keyword_range.clone());
    let range = start_range.to(body_end);
    Some(ParsedViewDeclaration {
        name,
        name_range,
        params,
        children,
        range,
        pub_range,
    })
}

fn construct_node(
    tree: TokenTree,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
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
            dop::parse_expr::parse_expr(&mut iter, comments, errors, &content)
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
                    errors.push(ParseError::UnknownHtmlElement {
                        tag: tag_name.to_cheap_string(),
                        range: tag_name.clone(),
                    });
                    return None;
                }
            };

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
                    dop::parse_expr::parse_expr(&mut iter, comments, errors, &e)
                } else {
                    errors.push(ParseError::MissingMatchExpression {
                        range: opening_tag_range.clone(),
                    });
                    None
                };
                let Some(subject) = subject else {
                    // Parse children to collect errors
                    for child in tree.children {
                        construct_node(
                            child,
                            comments,
                            errors,
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
                                errors.push(ParseError::MissingCasePattern {
                                    range: case_opening_range.clone(),
                                });
                                continue;
                            };
                            let pattern = {
                                let mut iter = pattern_range.cursor().peekable();
                                dop::parse_expr::parse_match_pattern(
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
                            errors.push(ParseError::InvalidMatchChild {
                                range: child_tree.range.clone(),
                            });
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
                        dop::parse_expr::parse_expr(&mut iter, comments, errors, &e)
                    } else {
                        errors.push(ParseError::MissingIfExpression {
                            range: opening_tag_range.clone(),
                        });
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
                        errors.push(ParseError::MissingForExpression {
                            range: opening_tag_range.clone(),
                        });
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
                        errors.push(ParseError::MissingLetBinding {
                            range: opening_tag_range.clone(),
                        });
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
                            errors.push(ParseError::InvalidComponentName {
                                error,
                                range: tag_name.clone(),
                            });
                            return None;
                        }
                    };

                    // Error if bare expression {..} is present on component reference
                    if let Some(expr_range) = &expression {
                        errors.push(ParseError::UnexpectedComponentExpression {
                            tag_name: tag_name.to_cheap_string(),
                            range: expr_range.clone(),
                        });
                    }

                    // Parse arguments from attributes
                    let mut parsed_args: Vec<parsed_ast::ParsedAttribute> = Vec::new();

                    for attr in &attributes {
                        // Validate attribute name is a valid variable name
                        if VarName::new(attr.name.as_str()).is_err() {
                            errors.push(ParseError::InvalidArgumentName {
                                tag_name: tag_name.to_cheap_string(),
                                name: attr.name.to_cheap_string(),
                                range: attr.name.clone(),
                            });
                            continue;
                        }

                        let value = match &attr.value {
                            Some(tokenizer::TokenizedAttributeValue::Expression(range)) => {
                                let mut iter = range.cursor().peekable();
                                match dop::parse_expr::parse_expr(
                                    &mut iter, comments, errors, range,
                                ) {
                                    Some(expr) => {
                                        Some(parsed_ast::ParsedAttributeValue::Expression(expr))
                                    }
                                    None => {
                                        continue;
                                    }
                                }
                            }
                            Some(tokenizer::TokenizedAttributeValue::String { content }) => {
                                Some(parsed_ast::ParsedAttributeValue::String(content.clone()))
                            }
                            None => None,
                        };

                        parsed_args.push(parsed_ast::ParsedAttribute {
                            name: attr.name.clone(),
                            value,
                        });
                    }

                    let declaring_module = if defined_components.contains(component_name.as_str()) {
                        Some(document_id.clone())
                    } else {
                        imported_components.get(component_name.as_str()).cloned()
                    };

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
                            errors.push(ParseError::UnknownHtmlElement {
                                tag: tag_name.to_cheap_string(),
                                range: tag_name.clone(),
                            });
                            return None;
                        }
                    };

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
    let (var_name, var_name_range) =
        if let Some(underscore_range) =
            dop::tokenizer::advance_if(iter, comments, errors, dop::Token::Underscore)
        {
            (None, Some(underscore_range))
        } else {
            let (name, name_range) =
                dop::tokenizer::expect_variable_name(iter, comments, errors, range)?;
            (Some(name), Some(name_range))
        };
    dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::In)?;
    let start_expr = dop::parse_expr::parse_logical(iter, comments, errors, range)?;
    let source = if dop::tokenizer::advance_if(iter, comments, errors, dop::Token::DotDotEq)
        .is_some()
    {
        let end_expr = dop::parse_expr::parse_logical(iter, comments, errors, range)?;
        ParsedLoopSource::RangeInclusive {
            start: start_expr,
            end: end_expr,
        }
    } else {
        ParsedLoopSource::Array(start_expr)
    };
    dop::tokenizer::expect_eof(iter, comments, errors)?;
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
) -> Option<Vec<(VarName, DocumentRange, Option<ParsedType>, dop::ParsedExpr)>> {
    let mut bindings = Vec::new();
    dop::tokenizer::parse_comma_separated(
        iter,
        comments,
        errors,
        range,
        |iter, comments, errors, range| {
            let (var_name, var_name_range) =
                dop::tokenizer::expect_variable_name(iter, comments, errors, range)?;
            let var_type = if let Some((dop::Token::Colon, _)) =
                dop::tokenizer::peek_past_comments(iter)
            {
                dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Colon)?;
                Some(parse_type(iter, comments, errors, range)?)
            } else {
                None
            };
            dop::tokenizer::expect_token(iter, comments, errors, range, &dop::Token::Assign)?;
            let value_expr = dop::parse_expr::parse_logical(iter, comments, errors, range)?;
            bindings.push((var_name, var_name_range, var_type, value_expr));
            Some(())
        },
        None,
    )?;
    dop::tokenizer::expect_eof(iter, comments, errors)?;
    Some(bindings)
}

/// Parse a `#[examples(...)]` annotation using the dop tokenizer.
fn parse_pattern_annotation(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<dop::ExamplesAnnotation> {
    if let Some((dop::Token::HashBracket, _)) = dop::tokenizer::peek_past_comments(iter) {
        dop::tokenizer::next_collecting_comments(iter, comments, errors);
        match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
            Some((dop::Token::Identifier(name), _)) if &*name == "examples" => {}
            _ => return None,
        }
        if dop::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
            != Some(dop::Token::LeftParen)
        {
            return None;
        }
        let mut annotation = dop::ExamplesAnnotation::default();
        loop {
            let key = match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
                Some((dop::Token::Identifier(name), _)) => name.to_string(),
                _ => return None,
            };
            if dop::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
                != Some(dop::Token::Assign)
            {
                return None;
            }
            match key.as_str() {
                "pattern" => {
                    let pattern =
                        match dop::tokenizer::next_collecting_comments(iter, comments, errors) {
                            Some((dop::Token::StringLiteral(s), _)) => s.to_string(),
                            _ => return None,
                        };
                    annotation.pattern = Some(pattern);
                }
                "min" | "max" | "min_len" | "max_len" => {
                    let (negative, token) = match dop::tokenizer::peek_past_comments(iter) {
                        Some((dop::Token::Minus, _)) => {
                            dop::tokenizer::next_collecting_comments(iter, comments, errors);
                            (
                                true,
                                dop::tokenizer::next_collecting_comments(iter, comments, errors),
                            )
                        }
                        _ => (
                            false,
                            dop::tokenizer::next_collecting_comments(iter, comments, errors),
                        ),
                    };
                    let n = match token {
                        Some((dop::Token::IntLiteral(n), _)) => {
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
            match dop::tokenizer::peek_past_comments(iter) {
                Some((dop::Token::Comma, _)) => {
                    dop::tokenizer::next_collecting_comments(iter, comments, errors);
                }
                _ => break,
            }
        }
        if dop::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
            != Some(dop::Token::RightParen)
        {
            return None;
        }
        if dop::tokenizer::next_collecting_comments(iter, comments, errors).map(|(t, _)| t)
            != Some(dop::Token::RightBracket)
        {
            return None;
        }
        Some(annotation)
    } else {
        None
    }
}

fn parse_attribute(
    attr: &tokenizer::TokenizedAttribute,
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Option<parsed_ast::ParsedAttribute> {
    let value = match &attr.value {
        Some(tokenizer::TokenizedAttributeValue::String { content }) => {
            Some(parsed_ast::ParsedAttributeValue::String(content.clone()))
        }
        Some(tokenizer::TokenizedAttributeValue::Expression(range)) => {
            let mut iter = range.cursor().peekable();
            let result = dop::parse_expr::parse_expr(&mut iter, comments, errors, range);
            Some(result.map(parsed_ast::ParsedAttributeValue::Expression)?)
        }
        None => None,
    };
    Some(parsed_ast::ParsedAttribute {
        name: attr.name.clone(),
        value,
    })
}

fn parse_attributes(
    attributes: &[tokenizer::TokenizedAttribute],
    comments: &mut VecDeque<DocumentRange>,
    errors: &mut Vec<ParseError>,
) -> Vec<parsed_ast::ParsedAttribute> {
    attributes
        .iter()
        .filter_map(|attr| parse_attribute(attr, comments, errors))
        .collect()
}

fn disallow_attributes<'a>(
    attributes: &'a [tokenizer::TokenizedAttribute],
    tag_name: &'a DocumentRange,
) -> impl Iterator<Item = ParseError> + 'a {
    attributes
        .iter()
        .map(move |attr| ParseError::UnrecognizedAttribute {
            tag_name: tag_name.to_cheap_string(),
            attr_name: attr.name.to_cheap_string(),
            range: attr.range.clone(),
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
        expected.assert_eq(&formatter::format(module));
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
    fn should_accept_empty_file() {
        accept("", expect![[""]]);
    }

    #[test]
    fn should_accept_pub_on_record() {
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
    fn should_accept_pub_on_enum() {
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
    fn slot_round_trips() {
        accept(
            indoc! {"
                component Card(slot: Fragment) {
                  <div>{slot}</div>
                }
            "},
            expect![[r#"
                component Card(slot: Fragment) {
                  <div>
                    {slot}
                  </div>
                }
            "#]],
        );
    }

    #[test]
    fn should_accept_pub_on_component() {
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
    fn should_accept_pub_on_view() {
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
    fn should_reject_pub_on_import() {
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
    fn should_reject_pub_at_end_of_file() {
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
    fn should_accept_comment_between_components() {
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
    fn should_accept_keyword_component_syntax() {
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
    fn should_accept_keyword_component_with_params() {
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
    fn should_accept_keyword_component_with_trailing_comment() {
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
    fn should_accept_nested_for_loops() {
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
    fn should_accept_form_with_inputs() {
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
    fn should_reject_when_tags_are_not_closed() {
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
    fn should_reject_when_void_tag_is_closed_with_closing_tag() {
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
    fn should_accept_void_tags_to_be_self_closing() {
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
    fn should_accept_doctype_tags_inside_components() {
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
    fn should_reject_when_expression_is_missing_in_if_tag() {
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
    fn should_reject_when_expression_is_missing_in_for_tag() {
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
    fn should_reject_when_for_tag_has_invalid_expression() {
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
    fn should_reject_when_if_tag_has_invalid_expression() {
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
    fn should_reject_when_component_parameter_has_parse_error_in_type_name() {
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
    fn should_accept_field_access_on_record() {
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
    fn should_reject_multiple_expressions_in_attribute() {
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
    fn should_reject_when_an_import_is_imported_twice() {
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
    fn should_reject_when_a_component_is_defined_twice() {
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
    fn should_reject_when_a_component_is_defined_with_the_same_name_as_an_import() {
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
    fn should_reject_when_a_component_is_defined_with_the_same_name_as_a_record() {
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
    fn should_reject_when_a_record_is_defined_with_the_same_name_as_an_import() {
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
    fn should_reject_when_import_has_only_one_segment() {
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
    fn should_accept_component_references() {
        accept(
            indoc! {"
                component Main(p: String) {
                    <Foo></Foo>
                    <Foo></Foo>
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
    fn should_accept_component_references_with_params() {
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
    fn should_accept_for_loop() {
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
    fn should_accept_for_loop_with_text_expression() {
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
    fn should_accept_for_loop_with_inclusive_range() {
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
    fn should_accept_for_loop_with_variable_range_bounds() {
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
    fn should_accept_for_loop_with_expression_range_bounds() {
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
    fn should_accept_for_loop_with_underscore_binding() {
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
    fn should_accept_for_loop_with_underscore_and_range() {
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
    fn should_accept_for_loop_with_underscore_and_variable_range() {
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
    fn should_accept_if_statement() {
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
    fn should_accept_if_statement_with_nested_for_loop() {
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
    fn should_reject_unknown_html_element() {
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
    fn should_reject_mathml_element() {
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
    fn should_accept_custom_hyphenated_element() {
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
    fn should_accept_complex_svg_structure() {
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
    fn should_accept_component_parameter_with_string_type() {
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
    fn should_accept_component_parameter_with_record_type() {
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
    fn should_accept_component_parameter_with_array_type() {
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
    fn should_accept_component_parameter_with_array_of_record_type() {
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
    fn should_accept_text_with_single_expression() {
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
    fn should_accept_text_with_multiple_expressions() {
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
    fn should_accept_text_with_expression_at_start() {
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
    fn should_accept_text_with_expression_at_end() {
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
    fn should_accept_text_with_only_expression() {
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
    fn should_reject_empty_expression_in_text() {
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
    fn should_accept_complex_expression_in_text() {
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
    fn should_accept_adjacent_expressions_in_text() {
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
    fn should_accept_text_expression_with_string_containing_html() {
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
    fn should_reject_incomplete_record_declaration() {
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
    fn should_reject_unknown_text_before_component() {
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
    fn should_reject_when_enum_is_defined_with_the_same_name_as_a_record() {
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
    fn should_reject_when_record_is_defined_with_the_same_name_as_an_enum() {
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
    fn should_reject_when_component_is_defined_with_the_same_name_as_an_enum() {
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
    fn should_reject_when_enum_is_defined_with_the_same_name_as_an_import() {
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
    fn should_accept_match_expression_in_template() {
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
    fn should_accept_match_expression_in_attribute() {
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
    fn should_accept_match_expression_with_multiline_arms() {
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
    fn should_accept_parameter_with_default_string_value() {
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
    fn should_accept_parameter_with_default_int_value() {
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
    fn should_accept_parameter_with_default_bool_value() {
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
    fn should_accept_mixed_required_and_default_parameters() {
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
    fn should_accept_parameter_with_default_array_value() {
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
    fn should_accept_parameter_with_default_record_value() {
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
    fn should_accept_parameter_with_default_enum_value() {
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
    fn should_accept_parameter_with_option_type() {
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
    fn should_accept_parameter_with_default_none_value() {
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
    fn should_accept_parameter_with_default_some_value() {
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
    fn should_accept_match_with_option_cases() {
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
    fn should_accept_match_with_enum_cases() {
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
    fn should_accept_match_with_enum_variant_fields() {
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
    fn should_accept_match_on_enum_literal_expression() {
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
    fn should_accept_match_with_boolean_cases() {
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
    fn should_reject_on_match_without_expression() {
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
    fn should_reject_on_case_without_pattern() {
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
    fn should_reject_on_non_case_children_in_match() {
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
    fn should_treat_case_outside_match_as_html() {
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
    fn should_accept_let_with_string_value() {
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
    fn should_accept_let_with_int_value() {
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
    fn should_accept_let_with_expression_value() {
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
    fn should_accept_nested_let_tags() {
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
    fn should_reject_let_without_binding() {
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
    fn should_accept_let_with_omitted_type() {
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
    fn should_reject_let_with_missing_value() {
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
    fn should_accept_let_with_multiple_bindings() {
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
    fn should_accept_let_with_three_bindings() {
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
    fn should_accept_let_with_trailing_comma() {
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
    fn should_accept_let_with_multiple_bindings_and_trailing_comma() {
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
    fn should_accept_let_with_field_access_value() {
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
    fn should_reject_let_with_missing_comma_between_bindings() {
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
    fn should_accept_multiple_sibling_let_tags() {
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
    fn should_accept_let_after_html_element() {
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
    fn should_accept_let_before_html_element() {
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
    fn should_reject_top_level_html_element() {
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
    fn should_accept_view_declaration() {
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
    fn should_accept_view_without_parentheses() {
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
    fn should_accept_view_with_parameters() {
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
    fn should_accept_view_with_component_reference() {
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
    fn should_accept_multiple_views() {
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
    fn should_reject_duplicate_view_names() {
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
    fn should_reject_view_with_same_name_as_component() {
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
    fn should_reject_view_with_reserved_name() {
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
    fn should_reject_view_with_lowercase_name() {
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
    fn should_reject_view_with_default_parameter() {
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
    fn should_accept_view_with_empty_body() {
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
    fn should_accept_view_with_if_statement() {
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
    fn should_accept_view_with_for_loop() {
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
    fn should_accept_view_with_let_binding() {
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
    fn should_accept_view_with_void_elements() {
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
    fn should_accept_view_with_trailing_comma_in_params() {
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
    fn should_reject_view_with_same_name_as_record() {
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
    fn should_reject_view_with_same_name_as_enum() {
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
    fn should_accept_view_with_match_expression() {
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
    fn should_accept_view_with_nested_components() {
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
    fn should_accept_view_between_components() {
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
    fn should_reject_view_with_multiple_params_mixed_defaults() {
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
    fn should_accept_view_with_top_level_text() {
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
    fn should_accept_escape_sequences_in_strings() {
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
    fn should_reject_invalid_escape_sequences_in_strings() {
        reject(
            r#"component Test {{"invalid\q"}}"#,
            expect![[r#"
                error: Invalid escape sequence '\q'
                1 | component Test {{"invalid\q"}}
                  |                          ^^
            "#]],
        );
    }
}
