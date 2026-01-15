use std::collections::{HashMap, HashSet, VecDeque};
use std::iter::Peekable;

use super::parsed_ast::{
    self, ParsedAst, ParsedComponentDeclaration, ParsedDeclaration, ParsedEnumDeclaration,
    ParsedEnumDeclarationVariant, ParsedImportDeclaration, ParsedRecordDeclaration,
    ParsedRecordDeclarationField,
};
use super::parsed_node::{ParsedLetBinding, ParsedMatchCase, ParsedNode};
use super::token_tree::{TokenTree, parse_tree};
use crate::document::{CheapString, Document, DocumentCursor, DocumentRange};
use crate::dop;
use crate::dop::ParsedDeclaration as DopParsedDeclaration;
use crate::dop::VarName;
use crate::error_collector::ErrorCollector;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::parse_error::ParseError;
use super::tokenizer::{self, Token};

struct AttributeValidator {
    attributes: Vec<tokenizer::TokenizedAttribute>,
    tag_name: DocumentRange,
    handled_attributes: HashSet<String>,
}

impl AttributeValidator {
    fn new(attributes: Vec<tokenizer::TokenizedAttribute>, tag_name: DocumentRange) -> Self {
        Self {
            attributes,
            tag_name,
            handled_attributes: HashSet::new(),
        }
    }

    fn parse_attribute_value(
        value: &tokenizer::TokenizedAttributeValue,
        comments: &mut VecDeque<(CheapString, DocumentRange)>,
    ) -> Result<parsed_ast::ParsedAttributeValue, ParseError> {
        match value {
            tokenizer::TokenizedAttributeValue::String { content } => {
                Ok(parsed_ast::ParsedAttributeValue::String(content.clone()))
            }
            tokenizer::TokenizedAttributeValue::Expression(range) => {
                let mut iter = range.cursor().peekable();
                match dop::parser::parse_expr(&mut iter, comments, range) {
                    Ok(expr) => Ok(parsed_ast::ParsedAttributeValue::Expression(expr)),
                    Err(err) => Err(err.into()),
                }
            }
        }
    }

    // Parse an attribute or return an error.
    fn parse(
        attr: &tokenizer::TokenizedAttribute,
        comments: &mut VecDeque<(CheapString, DocumentRange)>,
    ) -> Result<parsed_ast::ParsedAttribute, ParseError> {
        match &attr.value {
            Some(val) => Ok(parsed_ast::ParsedAttribute {
                name: attr.name.clone(),
                value: Some(Self::parse_attribute_value(val, comments)?),
            }),
            None => Ok(parsed_ast::ParsedAttribute {
                name: attr.name.clone(),
                value: None,
            }),
        }
    }

    fn disallow_unrecognized(&self) -> impl Iterator<Item = ParseError> + '_ {
        self.attributes
            .iter()
            .filter(|attr| !self.handled_attributes.contains(attr.name.as_str()))
            .map(move |attr| ParseError::UnrecognizedAttribute {
                tag_name: self.tag_name.to_cheap_string(),
                attr_name: attr.name.to_cheap_string(),
                range: attr.range.clone(),
            })
    }

    fn parse_unrecognized(
        &self,
        comments: &mut VecDeque<(CheapString, DocumentRange)>,
    ) -> Vec<Result<parsed_ast::ParsedAttribute, ParseError>> {
        self.attributes
            .iter()
            .filter(|attr| !self.handled_attributes.contains(attr.name.as_str()))
            .map(|attr| Self::parse(attr, comments))
            .collect()
    }

    fn get_all_attributes(&self) -> impl Iterator<Item = &tokenizer::TokenizedAttribute> {
        self.attributes.iter()
    }
}

/// Parse a hop document into a ParsedAst.
pub fn parse(
    module_name: ModuleName,
    document: Document,
    errors: &mut ErrorCollector<ParseError>,
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

    loop {
        match dop::tokenizer::peek_past_comments(&iter) {
            Some(Ok((dop::Token::Import, _))) => {
                match dop::parser::parse_import_declaration(
                    &mut iter,
                    &mut comments,
                    &document_range,
                ) {
                    Ok(DopParsedDeclaration::Import {
                        name,
                        name_range,
                        path,
                        module_name,
                        ..
                    }) => {
                        let import = ParsedImportDeclaration {
                            type_name: name,
                            type_name_range: name_range.clone(),
                            path,
                            module_name,
                        };
                        let name_str = import.type_name.as_str();
                        if imported_components.contains_key(name_str) {
                            errors.push(ParseError::TypeNameIsAlreadyDefined {
                                name: name_range.to_cheap_string(),
                                range: name_range,
                            });
                        } else {
                            imported_components
                                .insert(name_str.to_string(), import.module_name.clone());
                        }
                        declarations.push(ParsedDeclaration::Import(import));
                    }
                    Ok(_) => unreachable!("parse_import_declaration returned non-Import"),
                    Err(err) => {
                        errors.push(err.into());
                        break;
                    }
                }
            }
            Some(Ok((dop::Token::Record, _))) => {
                match dop::parser::parse_record_declaration(
                    &mut iter,
                    &mut comments,
                    &document_range,
                ) {
                    Ok(DopParsedDeclaration::Record {
                        name,
                        name_range,
                        fields,
                        range,
                    }) => {
                        let record = ParsedRecordDeclaration {
                            name: name.clone(),
                            name_range: name_range.clone(),
                            range: range.clone(),
                            fields: fields
                                .iter()
                                .map(|(field_name, field_name_range, field_type)| {
                                    ParsedRecordDeclarationField {
                                        name: field_name.clone(),
                                        name_range: field_name_range.clone(),
                                        field_type: field_type.clone(),
                                    }
                                })
                                .collect(),
                        };
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
                    Ok(_) => unreachable!("parse_record_declaration returned non-Record"),
                    Err(err) => {
                        errors.push(err.into());
                        break;
                    }
                }
            }
            Some(Ok((dop::Token::Enum, _))) => {
                match dop::parser::parse_enum_declaration(&mut iter, &mut comments, &document_range)
                {
                    Ok(DopParsedDeclaration::Enum {
                        name,
                        name_range,
                        variants,
                        range,
                    }) => {
                        let enum_decl = ParsedEnumDeclaration {
                            name: name.clone(),
                            name_range: name_range.clone(),
                            range: range.clone(),
                            variants: variants
                                .iter()
                                .map(|(name, name_range, fields)| ParsedEnumDeclarationVariant {
                                    name: name.clone(),
                                    name_range: name_range.clone(),
                                    fields: fields.clone(),
                                })
                                .collect(),
                        };
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
                    Ok(_) => unreachable!("parse_enum_declaration returned non-Enum"),
                    Err(err) => {
                        errors.push(err.into());
                        break;
                    }
                }
            }
            Some(Ok((dop::Token::LessThan, _))) => {
                // Component declaration - delegate to hop parser
                // First, consume whitespace and comments using dop tokenizer
                // (peek_past_comments discards them, so we need to actually consume them)
                loop {
                    while iter.peek().is_some_and(|s| s.ch().is_whitespace()) {
                        iter.next();
                    }
                    match dop::tokenizer::peek(&iter) {
                        Some(Ok((dop::Token::Comment(_), _))) => {
                            if let Some(Ok((dop::Token::Comment(text), range))) =
                                dop::tokenizer::next(&mut iter)
                            {
                                comments.push_back((text, range));
                            }
                        }
                        _ => break,
                    }
                }
                if let Some(component) = parse_component_declaration(
                    &mut iter,
                    &mut comments,
                    errors,
                    &module_name,
                    &defined_components,
                    &imported_components,
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
            Some(Ok((_, token_range))) => {
                // Unexpected token at top level
                errors.push(ParseError::UnexpectedTopLevelText { range: token_range });
                break;
            }
            Some(Err(err)) => {
                errors.push(err.into());
                break;
            }
            None => break, // EOF
        }
    }

    ParsedAst::new(module_name, declarations, comments)
}

/// Parse a component declaration from a document cursor.
///
/// Returns `None` if:
/// - The cursor is exhausted
/// - The next token is not an opening tag
/// - The tag name is not a valid component name (PascalCase)
fn parse_component_declaration(
    iter: &mut Peekable<DocumentCursor>,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    errors: &mut ErrorCollector<ParseError>,
    module_name: &ModuleName,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, ModuleName>,
) -> Option<ParsedComponentDeclaration> {
    let tree = parse_tree(iter, errors)?;

    let TokenTree {
        token,
        closing_tag_name,
        children: tree_children,
        range,
    } = tree;

    let Token::OpeningTag {
        tag_name,
        attributes,
        expression,
        ..
    } = token
    else {
        return None;
    };

    let component_name = match ComponentName::new(tag_name.to_string()) {
        Ok(name) => name,
        Err(error) => {
            errors.push(ParseError::InvalidComponentName {
                error,
                range: tag_name.clone(),
            });
            return None;
        }
    };

    let validator = AttributeValidator::new(attributes, tag_name.clone());

    let params = expression.as_ref().and_then(|expr| {
        let mut expr_iter = expr.cursor().peekable();
        errors.ok_or_add(
            dop::parser::parse_parameters(&mut expr_iter, comments, expr)
                .map(|parsed_params| {
                    let params = parsed_params
                        .into_iter()
                        .map(|((var_name, var_name_range), var_type, default_value)| {
                            parsed_ast::ParsedParameter {
                                var_name,
                                var_name_range,
                                var_type,
                                default_value,
                            }
                        })
                        .collect();
                    (params, expr.clone())
                })
                .map_err(|err| err.into()),
        )
    });

    for error in validator.disallow_unrecognized() {
        errors.push(error);
    }

    let children: Vec<ParsedNode> = tree_children
        .into_iter()
        .flat_map(|child| {
            construct_nodes(
                child,
                comments,
                errors,
                module_name,
                defined_components,
                imported_components,
            )
        })
        .collect();

    Some(ParsedComponentDeclaration {
        component_name,
        tag_name,
        closing_tag_name,
        params,
        range,
        children,
    })
}

/// Check if a tag name is one that should have raw text content (no expression parsing).
fn is_raw_text_element(tag_name: &str) -> bool {
    matches!(tag_name, "script" | "style")
}

fn construct_nodes(
    tree: TokenTree,
    comments: &mut VecDeque<(CheapString, DocumentRange)>,
    errors: &mut ErrorCollector<ParseError>,
    module_name: &ModuleName,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, ModuleName>,
) -> Vec<ParsedNode> {
    match tree.token {
        Token::Comment { .. } => {
            // Skip comments
            vec![]
        }
        Token::ClosingTag { .. } => {
            // ClosingTags are not present in the token tree
            unreachable!()
        }
        Token::Doctype { range } => {
            vec![ParsedNode::Doctype {
                value: range.to_cheap_string(),
                range,
            }]
        }
        Token::Text { range } => {
            vec![ParsedNode::Text {
                value: range.to_cheap_string(),
                range,
            }]
        }
        Token::TextExpression { content, range } => {
            let mut iter = content.cursor().peekable();
            match dop::parser::parse_expr(&mut iter, comments, &content) {
                Ok(expression) => {
                    vec![ParsedNode::TextExpression { expression, range }]
                }
                Err(err) => {
                    errors.push(err.into());
                    vec![]
                }
            }
        }
        Token::RawTextTag {
            tag_name,
            attributes,
            content,
            range,
            ..
        } => {
            let validator = AttributeValidator::new(attributes, tag_name.clone());
            let attributes = validator
                .parse_unrecognized(comments)
                .into_iter()
                .filter_map(|attr| errors.ok_or_add(attr))
                .collect();

            // Convert content to a Text child if present
            let children = content
                .map(|c| {
                    vec![ParsedNode::Text {
                        value: c.to_cheap_string(),
                        range: c,
                    }]
                })
                .unwrap_or_default();

            vec![ParsedNode::Html {
                tag_name,
                closing_tag_name: None,
                attributes,
                range,
                children,
            }]
        }
        Token::OpeningTag {
            tag_name,
            expression,
            attributes,
            range: opening_tag_range,
            ..
        } => {
            let validator = AttributeValidator::new(attributes, tag_name.clone());

            // Handle <match> specially - process children as <case> tags
            if tag_name.as_str() == "match" {
                errors.extend(validator.disallow_unrecognized());
                let expr = expression.ok_or_else(|| ParseError::MissingMatchExpression {
                    range: opening_tag_range.clone(),
                });
                let Some(subject) = errors.ok_or_add(expr.and_then(|e| {
                    let mut iter = e.cursor().peekable();
                    dop::parser::parse_expr(&mut iter, comments, &e).map_err(|err| err.into())
                })) else {
                    // Parse children normally for error recovery
                    let children: Vec<_> = tree
                        .children
                        .into_iter()
                        .flat_map(|child| {
                            construct_nodes(
                                child,
                                comments,
                                errors,
                                module_name,
                                defined_components,
                                imported_components,
                            )
                        })
                        .collect();
                    return vec![ParsedNode::Placeholder {
                        range: tree.range.clone(),
                        children,
                    }];
                };

                // Process children as <case> tags
                let mut cases = Vec::new();
                for child_tree in tree.children {
                    match &child_tree.token {
                        // Ignore whitespace text
                        Token::Text { range, .. } if range.as_str().trim().is_empty() => {
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
                            let Some(pattern) = errors.ok_or_add({
                                let mut iter = pattern_range.cursor().peekable();
                                dop::parser::parse_match_pattern(
                                    &mut iter,
                                    comments,
                                    &pattern_range,
                                )
                                .map_err(|err| err.into())
                            }) else {
                                continue;
                            };
                            // Parse case children normally
                            let case_children: Vec<_> = child_tree
                                .children
                                .into_iter()
                                .flat_map(|c| {
                                    construct_nodes(
                                        c,
                                        comments,
                                        errors,
                                        module_name,
                                        defined_components,
                                        imported_components,
                                    )
                                })
                                .collect();
                            cases.push(ParsedMatchCase {
                                pattern,
                                pattern_range,
                                children: case_children,
                                range: child_tree.range,
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

                return vec![ParsedNode::Match {
                    subject,
                    cases,
                    range: tree.range,
                }];
            }

            // Process children - raw text elements (script/style) don't parse expressions
            let children: Vec<_> = if is_raw_text_element(tag_name.as_str()) {
                tree.children
                    .into_iter()
                    .filter_map(|child| match child.token {
                        Token::Text { range, .. } => Some(ParsedNode::Text {
                            value: range.to_cheap_string(),
                            range,
                        }),
                        _ => unreachable!(),
                    })
                    .collect()
            } else {
                tree.children
                    .into_iter()
                    .flat_map(|child| {
                        construct_nodes(
                            child,
                            comments,
                            errors,
                            module_name,
                            defined_components,
                            imported_components,
                        )
                    })
                    .collect()
            };

            match tag_name.as_str() {
                // <if {...}>
                "if" => {
                    errors.extend(validator.disallow_unrecognized());
                    let expr = expression.ok_or_else(|| ParseError::MissingIfExpression {
                        range: opening_tag_range.clone(),
                    });
                    let Some(condition) = errors.ok_or_add(expr.and_then(|e| {
                        let mut iter = e.cursor().peekable();
                        dop::parser::parse_expr(&mut iter, comments, &e).map_err(|err| err.into())
                    })) else {
                        return vec![];
                    };
                    vec![ParsedNode::If {
                        condition,
                        range: tree.range.clone(),
                        children,
                    }]
                }

                // <for {...}>
                "for" => {
                    errors.extend(validator.disallow_unrecognized());
                    let parse_result = expression
                        .ok_or_else(|| ParseError::MissingForExpression {
                            range: opening_tag_range.clone(),
                        })
                        .and_then(|e| {
                            let mut iter = e.cursor().peekable();
                            dop::parser::parse_loop_header(&mut iter, comments, &e)
                                .map_err(|err| err.into())
                        });
                    let Some((var_name, var_name_range, array_expr)) =
                        errors.ok_or_add(parse_result)
                    else {
                        return vec![ParsedNode::Placeholder {
                            range: tree.range.clone(),
                            children,
                        }];
                    };
                    vec![ParsedNode::For {
                        var_name,
                        var_name_range,
                        array_expr,
                        range: tree.range.clone(),
                        children,
                    }]
                }

                // <let {...}>
                "let" => {
                    errors.extend(validator.disallow_unrecognized());
                    let Some(bindings_range) = expression else {
                        errors.push(ParseError::MissingLetBinding {
                            range: opening_tag_range.clone(),
                        });
                        return vec![ParsedNode::Placeholder {
                            range: tree.range.clone(),
                            children,
                        }];
                    };
                    let parse_result = {
                        let mut iter = bindings_range.cursor().peekable();
                        dop::parser::parse_let_bindings(&mut iter, comments, &bindings_range)
                            .map_err(|err| err.into())
                    };
                    let Some(parsed_bindings) = errors.ok_or_add(parse_result) else {
                        return vec![ParsedNode::Placeholder {
                            range: tree.range.clone(),
                            children,
                        }];
                    };
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
                    vec![ParsedNode::Let {
                        bindings,
                        bindings_range,
                        range: tree.range.clone(),
                        children,
                    }]
                }

                // <ComponentReference> - PascalCase indicates a component
                name if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) => {
                    let component_name = match ComponentName::new(tag_name.as_str().to_string()) {
                        Ok(name) => name,
                        Err(error) => {
                            errors.push(ParseError::InvalidComponentName {
                                error,
                                range: tag_name.clone(),
                            });
                            return vec![];
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

                    for attr in validator.get_all_attributes() {
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
                                match dop::parser::parse_expr(&mut iter, comments, range) {
                                    Ok(expr) => {
                                        Some(parsed_ast::ParsedAttributeValue::Expression(expr))
                                    }
                                    Err(err) => {
                                        errors.push(err.into());
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
                        Some(module_name.clone())
                    } else {
                        imported_components.get(component_name.as_str()).cloned()
                    };

                    vec![ParsedNode::ComponentReference {
                        component_name,
                        component_name_opening_range: tag_name,
                        component_name_closing_range: tree.closing_tag_name,
                        declaring_module,
                        args: parsed_args,
                        range: tree.range,
                        children,
                    }]
                }

                _ => {
                    // Default case: treat as HTML
                    let attributes = validator
                        .parse_unrecognized(comments)
                        .into_iter()
                        .filter_map(|attr| errors.ok_or_add(attr))
                        .collect();

                    vec![ParsedNode::Html {
                        tag_name,
                        closing_tag_name: tree.closing_tag_name,
                        attributes,
                        range: tree.range,
                        children,
                    }]
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::error_collector::ErrorCollector;
    use crate::hop::syntax::transform::whitespace_removal::remove_whitespace;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check(input: &str, expected: Expect) {
        let mut errors = ErrorCollector::new();
        let module = parse(
            ModuleName::new("test").unwrap(),
            Document::new(input.to_string()),
            &mut errors,
        );

        let actual = if !errors.is_empty() {
            DocumentAnnotator::new()
                .with_label("error")
                .with_lines_before(1)
                .annotate(None, errors.to_vec())
        } else {
            remove_whitespace(module).to_string()
        };

        expected.assert_eq(&actual);
    }

    #[test]
    fn should_accept_empty_file() {
        check("", expect![[""]]);
    }

    #[test]
    fn should_accept_comment_between_components() {
        check(
            indoc! {"
                <First></First>
                // This is a comment
                <Second></Second>
            "},
            expect![[r#"
                <First></First>

                <Second></Second>
            "#]],
        );
    }

    #[test]
    fn should_accept_nested_for_loops() {
        check(
            indoc! {"
                record T {
                  t: Array[String],
                }
                record S {
                  s: T,
                }
                <Main {i: Array[S]}>
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
                </Main>
            "},
            expect![[r#"
                record T {
                  t: Array[String],
                }

                record S {
                  s: T,
                }

                <Main {i: Array[S]}>
                  <for {j in i}>
                    <for {k in j.s.t}>
                      <if {k}></if>
                    </for>
                  </for>
                  <for {p in i}>
                    <for {k in p.s.t}>
                      <for {item in k}></for>
                    </for>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_script_and_style_tag_content_as_raw_text() {
        check(
            indoc! {r#"
                <Main>
                    <script>
                        // note that the <div> inside here is not
                        // parsed as html
                        console.log("<div>test</div>");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <script>
                    // note that the <div> inside here is not
                    // parsed as html
                    console.log("<div>test</div>");
                  </script>
                  <style>
                    body { color: red; }
                  </style>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_form_with_inputs() {
        check(
            indoc! {r#"
                <Main>
                    <form id="form">
                        <input type="text" required>
                        <button type="submit">Send</button>
                    </form>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <form id="form">
                    <input type="text" required>
                    <button type="submit">
                      Send
                    </button>
                  </form>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_when_tags_are_not_closed() {
        check(
            indoc! {"
                <Main>
                    <div>
                    <p>
                </Main>

                <Foo>
            "},
            // TODO: It would make more sense if these were reported in the opposite
            // order.
            expect![[r#"
                error: Unclosed <p>
                2 |     <div>
                3 |     <p>
                  |      ^

                error: Unclosed <div>
                1 | <Main>
                2 |     <div>
                  |      ^^^

                error: Unclosed <Foo>
                5 | 
                6 | <Foo>
                  |  ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_void_tag_is_closed_with_closing_tag() {
        check(
            indoc! {"
                <Main>
                    <hr></hr>
                    <br></br>
                    <input></input>
                </Main>
            "},
            expect![[r#"
                error: <hr> should not be closed using a closing tag
                1 | <Main>
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
        check(
            indoc! {r#"
                import bar::Bar
                <Main>
                    <hr/>
                    <br/>
                    <input/>
                </Main>
                <Foo/>
            "#},
            expect![[r#"
                import bar::Bar

                <Main>
                  <hr>
                  <br>
                  <input>
                </Main>

                <Foo></Foo>
            "#]],
        );
    }

    #[test]
    fn should_accept_doctype_tags_inside_components() {
        check(
            indoc! {"
                <Main {foo: String}>
                    <!DOCTYPE html>
                    <html>
                        <body>
                            <div>hello world</div>
                        </body>
                    </html>
                </Main>
            "},
            expect![[r#"
                <Main {foo: String}>
                  <!DOCTYPE html>
                  <html>
                    <body>
                      <div>
                        hello world
                      </div>
                    </body>
                  </html>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_when_closing_tag_does_not_have_matching_opening_tag() {
        check(
            indoc! {"
                <Main>
                    </div>
                    </p>
                </Main>
                </Main>
            "},
            expect![[r#"
                error: Unmatched </div>
                1 | <Main>
                2 |     </div>
                  |     ^^^^^^

                error: Unmatched </p>
                2 |     </div>
                3 |     </p>
                  |     ^^^^

                error: Unmatched </Main>
                4 | </Main>
                5 | </Main>
                  | ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_component_name_contains_dash() {
        check(
            indoc! {"
                <Foo-bar>
                </Foo-bar>
            "},
            expect![[r#"
                error: Component name contains invalid character: '-'. Only alphanumeric characters are allowed
                1 | <Foo-bar>
                  |  ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_expression_is_missing_in_if_tag() {
        check(
            indoc! {"
                <Main>
                    <if>
                        <div>Content</div>
                    </if>
                </Main>
            "},
            expect![[r#"
                error: Missing expression in <if> tag
                1 | <Main>
                2 |     <if>
                  |     ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_expression_is_missing_in_for_tag() {
        check(
            indoc! {"
                <Main>
                    <for>
                        <div>Content</div>
                    </for>
                </Main>
            "},
            expect![[r#"
                error: Missing loop generator expression in <for> tag
                1 | <Main>
                2 |     <for>
                  |     ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_component_parameter_name_starts_with_uppercase_letter() {
        check(
            indoc! {"
                <Main {Data: String}>
                    <div></div>
                </Main>
            "},
            expect![[r#"
                error: Expected variable name but got Data
                1 | <Main {Data: String}>
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_for_tag_has_invalid_expression() {
        check(
            indoc! {"
                <Main>
                    <for {foo}>
                        <div>Content</div>
                    </for>
                </Main>
            "},
            expect![[r#"
                error: Expected token 'in' but got end of file
                1 | <Main>
                2 |     <for {foo}>
                  |           ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_if_tag_has_invalid_expression() {
        check(
            indoc! {"
                <Main>
                    <if {~}>
                        <div>Content</div>
                    </if>
                </Main>
            "},
            expect![[r#"
                error: Unexpected character: '~'
                1 | <Main>
                2 |     <if {~}>
                  |          ^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_component_parameter_has_invalid_type_name() {
        check(
            indoc! {"
                <Main {data: invalid}>
                    <div>{data}</div>
                </Main>
            "},
            expect![[r#"
                error: Expected type name but got invalid
                1 | <Main {data: invalid}>
                  |              ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_component_parameter_has_parse_error_in_type_name() {
        check(
            indoc! {"
                <Main {data: Array[}>
                    <div>{data}</div>
                </Main>
            "},
            expect![[r#"
                error: Expected type name but got end of file
                1 | <Main {data: Array[}>
                  |        ^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_field_access_on_record() {
        check(
            indoc! {r#"
                record User {
                  url: String,
                  theme: String,
                }
                <Main {user: User}>
                    <a href={user.url} class={user.theme}>Link</a>
                </Main>
            "#},
            expect![[r#"
                record User {
                  url: String,
                  theme: String,
                }

                <Main {user: User}>
                  <a href={user.url} class={user.theme}>
                    Link
                  </a>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_multiple_expressions_in_attribute() {
        check(
            indoc! {r#"
                <Main {style1: String, style2: String, style3: String}>
                    <div class={style1, style2, style3}>Content</div>
                </Main>
            "#},
            expect![[r#"
                error: Unexpected token ','
                1 | <Main {style1: String, style2: String, style3: String}>
                2 |     <div class={style1, style2, style3}>Content</div>
                  |                       ^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_an_import_is_imported_twice() {
        check(
            indoc! {r#"
                import other::Foo
                import other::Foo

                <Main>
                	<Foo></Foo>
                </Main>
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
        check(
            indoc! {r#"
                <Foo>
                </Foo>

                <Foo>
                </Foo>
            "#},
            expect![[r#"
                error: Foo is already defined
                3 | 
                4 | <Foo>
                  |  ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_component_is_defined_with_the_same_name_as_an_import() {
        check(
            indoc! {r#"
                import other::Foo

                <Foo>
                </Foo>

                <Bar>
                	<Foo/>
                </Bar>
            "#},
            expect![[r#"
                error: Foo is already defined
                2 | 
                3 | <Foo>
                  |  ^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_component_is_defined_with_the_same_name_as_a_record() {
        check(
            indoc! {r#"
                record User {
                  name: String,
                }

                <User>
                </User>
            "#},
            expect![[r#"
                error: User is already defined
                4 | 
                5 | <User>
                  |  ^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_a_record_is_defined_with_the_same_name_as_an_import() {
        check(
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
        check(
            indoc! {r#"
                import Foo

                <Main>
                	<Foo/>
                </Main>
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
        check(
            indoc! {"
                <Main {p: String}>
                    <Foo></Foo>
                    <Foo></Foo>
                </Main>
            "},
            expect![[r#"
                <Main {p: String}>
                  <Foo/>
                  <Foo/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_references_with_params() {
        check(
            indoc! {r#"
                import foo::Foo
                import bar::Bar
                record Data {
                  user: String,
                }
                <Main {data: Data}>
                    <Foo a={data}/>
                    <Bar b={data.user}/>
                </Main>
            "#},
            expect![[r#"
                import foo::Foo
                import bar::Bar

                record Data {
                  user: String,
                }

                <Main {data: Data}>
                  <Foo a={data}/>
                  <Bar b={data.user}/>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_for_loop() {
        check(
            indoc! {"
                <Main {item: Array[String]}>
                    <for {item in items}>
                        <div>Item content</div>
                    </for>
                </Main>
            "},
            expect![[r#"
                <Main {item: Array[String]}>
                  <for {item in items}>
                    <div>
                      Item content
                    </div>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_for_loop_with_text_expression() {
        check(
            indoc! {"
                <Main {foo: Array[String]}>
                    <for {v in foo}>
                        <div>{v}</div>
                    </for>
                </Main>
            "},
            expect![[r#"
                <Main {foo: Array[String]}>
                  <for {v in foo}>
                    <div>
                      {v}
                    </div>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_if_statement() {
        check(
            indoc! {"
                <Main {x: Int, y: Int}>
                    <if {x == y}>
                        <div>Equal</div>
                    </if>
                </Main>
            "},
            expect![[r#"
                <Main {x: Int, y: Int}>
                  <if {x == y}>
                    <div>
                      Equal
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_if_statement_with_nested_for_loop() {
        check(
            indoc! {"
                <Main {x: Bool, data: Array[String]}>
	                <if {x}>
		                <for {d in data}>
                          {d}
		                </for>
	                </if>
                </Main>
            "},
            expect![[r#"
                <Main {x: Bool, data: Array[String]}>
                  <if {x}>
                    <for {d in data}>
                      {d}
                    </for>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_complex_svg_structure() {
        check(
            indoc! {r#"
                <Main>
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
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div class="navbar">
                    <svg
                      xmlns="http://www.w3.org/2000/svg"
                      width="128"
                      height="128"
                      version="1.1"
                      viewBox="0 0 128 128"
                      class="size-12"
                    >
                      <g
                        style="fill: none; stroke: currentcolor; stroke-width: 5px; stroke-linecap: round; stroke-linejoin: round;"
                      >
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
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_parameter_with_string_type() {
        check(
            indoc! {"
                <Main {data: String}>
                    <div>{data}</div>
                </Main>
            "},
            expect![[r#"
                <Main {data: String}>
                  <div>
                    {data}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_parameter_with_record_type() {
        check(
            indoc! {"
                record Data {
                  message: String,
                }

                <Main {data: Data}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </Main>
            "},
            expect![[r#"
                record Data {
                  message: String,
                }

                <Main {data: Data}>
                  <h1>
                    Hello World
                  </h1>
                  <p>
                    {data.message}
                  </p>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_parameter_with_array_type() {
        check(
            indoc! {"
                <Main {items: Array[String]}>
                    <for {item in items}>
                        <div>{item}</div>
                    </for>
                </Main>
            "},
            expect![[r#"
                <Main {items: Array[String]}>
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_component_parameter_with_array_of_record_type() {
        check(
            indoc! {"
                record Section {
                  title: String,
                  items: Array[String],
                }

                <Main {data: Array[Section]}>
                    <for {section in data}>
                        <h1>{section.title}</h1>
                        <for {item in section.items}>
                            <div>{item}</div>
                        </for>
                    </for>
                </Main>
            "},
            expect![[r#"
                record Section {
                  title: String,
                  items: Array[String],
                }

                <Main {data: Array[Section]}>
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
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_single_expression() {
        check(
            "<Main><h1>Hello {name}!</h1></Main>",
            expect![[r#"
                <Main>
                  <h1>
                    Hello
                    {name}
                    !
                  </h1>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_multiple_expressions() {
        check(
            "<Main><p>User {user.name} has {user.count} items</p></Main>",
            expect![[r#"
                <Main>
                  <p>
                    User
                    {user.name}
                    has
                    {user.count}
                    items
                  </p>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_expression_at_start() {
        check(
            "<Main><span>{greeting} world!</span></Main>",
            expect![[r#"
                <Main>
                  <span>
                    {greeting}
                    world!
                  </span>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_expression_at_end() {
        check(
            "<Main><div>Price: {price}</div></Main>",
            expect![[r#"
                <Main>
                  <div>
                    Price:
                    {price}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_only_expression() {
        check(
            "<Main><h2>{title}</h2></Main>",
            expect![[r#"
                <Main>
                  <h2>
                    {title}
                  </h2>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_not_parse_expressions_in_script_tags() {
        check(
            indoc! {r#"
                <Main>
                    <script>
                        const x = "{not_an_expression}";
                        const obj = {key: "value"};
                    </script>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <script>
                    const x = "{not_an_expression}";
                    const obj = {key: "value"};
                  </script>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_not_parse_expressions_in_style_tags() {
        check(
            indoc! {r#"
                <Main>
                    <style>
                        body { color: red; }
                        .class { font-size: 12px; }
                    </style>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <style>
                    body { color: red; }
                    .class { font-size: 12px; }
                  </style>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_empty_expression_in_text() {
        check(
            "<Main><div>Empty: {}</div></Main>",
            expect![[r#"
                error: Empty expression
                1 | <Main><div>Empty: {}</div></Main>
                  |                   ^^
            "#]],
        );
    }

    #[test]
    fn should_reject_unterminated_expression_in_text() {
        check(
            "<Main><div>Broken: {name</div></Main>",
            expect![[r#"
                error: Unmatched {
                1 | <Main><div>Broken: {name</div></Main>
                  |                    ^
            "#]],
        );
    }

    #[test]
    fn should_parse_complex_expression_in_text() {
        check(
            r#"<Main><p>Status: {user.profile.status == "active"}</p></Main>"#,
            expect![[r#"
                <Main>
                  <p>
                    Status:
                    {user.profile.status == "active"}
                  </p>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_adjacent_expressions_in_text() {
        check(
            "<Main><span>{first}{second}</span></Main>",
            expect![[r#"
                <Main>
                  <span>
                    {first}
                    {second}
                  </span>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_text_expression_with_string_containing_html() {
        check(
            r#"<Main><div>{"<div></div>"}</div></Main>"#,
            expect![[r#"
                <Main>
                  <div>
                    {"<div></div>"}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_incomplete_record_declaration() {
        check(
            indoc! {"
                record
                <Main>
                </Main>
            "},
            expect![[r#"
                error: Expected type name but got <
                1 | record
                2 | <Main>
                  | ^
            "#]],
        );
    }

    #[test]
    fn should_reject_unknown_text_before_component() {
        check(
            indoc! {"
                foo
                <Main>
                </Main>
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
        check(
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
        check(
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
        check(
            indoc! {"
                enum Color {Red, Green, Blue}

                <Color>
                </Color>
            "},
            expect![[r#"
                error: Color is already defined
                2 | 
                3 | <Color>
                  |  ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_when_enum_is_defined_with_the_same_name_as_an_import() {
        check(
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
        check(
            indoc! {r#"
                enum Color {Red, Green, Blue}

                <Main {color: Color}>
                    {match color {Color::Red => "red", Color::Blue => "blue"}}
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {color: Color}>
                  {match color {Color::Red => "red", Color::Blue => "blue"}}
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_in_attribute() {
        check(
            indoc! {r#"
                enum Color {Red, Green, Blue}

                <Main {color: Color}>
                    <div class={match color {Color::Red => "text-red", Color::Blue => "text-blue"}}></div>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {color: Color}>
                  <div
                    class={match color {Color::Red => "text-red", Color::Blue => "text-blue"}}
                  >
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_match_expression_with_multiline_arms() {
        check(
            indoc! {r#"
                enum Status {Active, Inactive, Pending}

                <Main {status: Status}>
                    {match status {
                        Status::Active => "active",
                        Status::Inactive => "inactive",
                        Status::Pending => "pending",
                    }}
                </Main>
            "#},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                <Main {status: Status}>
                  {match status {
                    Status::Active => "active",
                    Status::Inactive => "inactive",
                    Status::Pending => "pending",
                  }}
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_string_value() {
        check(
            indoc! {r#"
                <Main {name: String = "World"}>
                    <div>{name}</div>
                </Main>
            "#},
            expect![[r#"
                <Main {name: String = "World"}>
                  <div>
                    {name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_int_value() {
        check(
            indoc! {"
                <Main {count: Int = 42}>
                    <span>{count}</span>
                </Main>
            "},
            expect![[r#"
                <Main {count: Int = 42}>
                  <span>
                    {count}
                  </span>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_bool_value() {
        check(
            indoc! {"
                <Main {enabled: Bool = true}>
                    <div></div>
                </Main>
            "},
            expect![[r#"
                <Main {enabled: Bool = true}>
                  <div>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_mixed_required_and_default_parameters() {
        check(
            indoc! {r#"
                <Main {name: String, role: String = "user", active: Bool = true}>
                    <div>{name}</div>
                </Main>
            "#},
            expect![[r#"
                <Main {name: String, role: String = "user", active: Bool = true}>
                  <div>
                    {name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_array_value() {
        check(
            indoc! {r#"
                <Main {items: Array[String] = ["a", "b"]}>
                    <for {item in items}>
                        {item}
                    </for>
                </Main>
            "#},
            expect![[r#"
                <Main {items: Array[String] = ["a", "b"]}>
                  <for {item in items}>
                    {item}
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_record_value() {
        check(
            indoc! {r#"
                record Config { debug: Bool, timeout: Int }
                <Main {config: Config = Config(debug: false, timeout: 30)}>
                    <div></div>
                </Main>
            "#},
            expect![[r#"
                record Config {
                  debug: Bool,
                  timeout: Int,
                }

                <Main {config: Config = Config(debug: false, timeout: 30)}>
                  <div>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_enum_value() {
        check(
            indoc! {"
                enum Status { Active, Inactive, Pending }
                <Main {status: Status = Status::Active}>
                    <div></div>
                </Main>
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                  Pending,
                }

                <Main {status: Status = Status::Active}>
                  <div>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_option_type() {
        check(
            indoc! {"
                <Main {name: Option[String]}>
                    <div></div>
                </Main>
            "},
            expect![[r#"
                <Main {name: Option[String]}>
                  <div>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_none_value() {
        check(
            indoc! {"
                <Main {name: Option[String] = None}>
                    <div></div>
                </Main>
            "},
            expect![[r#"
                <Main {name: Option[String] = None}>
                  <div>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_parameter_with_default_some_value() {
        check(
            indoc! {r#"
                <Main {name: Option[String] = Some("default")}>
                    <div></div>
                </Main>
            "#},
            expect![[r#"
                <Main {name: Option[String] = Some("default")}>
                  <div>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_match_with_option_cases() {
        check(
            indoc! {r#"
                <Main {x: Option[String]}>
                    <match {x}>
                        <case {Some(y)}>
                            found {y}
                        </case>
                        <case {None}>
                            nothing
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                <Main {x: Option[String]}>
                  <match {x}>
                    <case {Some(y)}>
                      found
                      {y}
                    </case>
                    <case {None}>
                      nothing
                    </case>
                  </match>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_match_with_enum_cases() {
        check(
            indoc! {r#"
                enum Color { Red, Green, Blue }
                <Main {c: Color}>
                    <match {c}>
                        <case {Color::Red}>red</case>
                        <case {Color::Green}>green</case>
                        <case {Color::Blue}>blue</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                enum Color {
                  Red,
                  Green,
                  Blue,
                }

                <Main {c: Color}>
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
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_match_with_enum_variant_fields() {
        check(
            indoc! {r#"
                enum Result { Ok(value: Int), Err(message: String) }
                <Main {r: Result}>
                    <match {r}>
                        <case {Result::Ok(value: v)}>
                            Success: {v}
                        </case>
                        <case {Result::Err(message: m)}>
                            Error: {m}
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                enum Result {
                  Ok(value: Int),
                  Err(message: String),
                }

                <Main {r: Result}>
                  <match {r}>
                    <case {Result::Ok(value: v)}>
                      Success:
                      {v}
                    </case>
                    <case {Result::Err(message: m)}>
                      Error:
                      {m}
                    </case>
                  </match>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_match_on_enum_literal_expression() {
        check(
            indoc! {r#"
                enum Status { Active(name: String), Inactive }
                <Main>
                    <match {Status::Active(name: "test")}>
                        <case {Status::Active(name: n)}>
                            {n}
                        </case>
                        <case {Status::Inactive}>
                            none
                        </case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                enum Status {
                  Active(name: String),
                  Inactive,
                }

                <Main>
                  <match {Status::Active(name: "test")}>
                    <case {Status::Active(name: n)}>
                      {n}
                    </case>
                    <case {Status::Inactive}>
                      none
                    </case>
                  </match>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_parse_match_with_boolean_cases() {
        check(
            indoc! {r#"
                <Main {flag: Bool}>
                    <match {flag}>
                        <case {true}>yes</case>
                        <case {false}>no</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                <Main {flag: Bool}>
                  <match {flag}>
                    <case {true}>
                      yes
                    </case>
                    <case {false}>
                      no
                    </case>
                  </match>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_error_on_match_without_expression() {
        check(
            indoc! {r#"
                <Main>
                    <match>
                        <case {true}>yes</case>
                    </match>
                </Main>
            "#},
            expect![[r#"
                error: Missing expression in <match> tag
                1 | <Main>
                2 |     <match>
                  |     ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_error_on_case_without_pattern() {
        check(
            indoc! {r#"
                <Main {flag: Bool}>
                    <match {flag}>
                        <case>yes</case>
                    </match>
                </Main>
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
    fn should_error_on_non_case_children_in_match() {
        check(
            indoc! {r#"
                <Main {flag: Bool}>
                    <match {flag}>
                        <div>not allowed</div>
                    </match>
                </Main>
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
        check(
            indoc! {r#"
                <Main>
                    <case {true}>standalone case</case>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <case>
                    standalone case
                  </case>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_string_value() {
        check(
            indoc! {r#"
                <Main>
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_int_value() {
        check(
            indoc! {"
                <Main>
                    <let {count: Int = 42}>
                        <span>{count}</span>
                    </let>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <let {count: Int = 42}>
                    <span>
                      {count}
                    </span>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_expression_value() {
        check(
            indoc! {r#"
                record User { name: String }
                <Main {user: User}>
                    <let {greeting: String = user.name}>
                        <div>{greeting}</div>
                    </let>
                </Main>
            "#},
            expect![[r#"
                record User {
                  name: String,
                }

                <Main {user: User}>
                  <let {greeting: String = user.name}>
                    <div>
                      {greeting}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_nested_let_tags() {
        check(
            indoc! {r#"
                <Main>
                    <let {a: Int = 1}>
                        <let {b: Int = 2}>
                            <div>{a} + {b}</div>
                        </let>
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: Int = 1}>
                    <let {b: Int = 2}>
                      <div>
                        {a}
                        +
                        {b}
                      </div>
                    </let>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_let_without_binding() {
        check(
            indoc! {"
                <Main>
                    <let>
                        <div>Content</div>
                    </let>
                </Main>
            "},
            expect![[r#"
                error: Missing binding in <let> tag
                1 | <Main>
                2 |     <let>
                  |     ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_with_missing_type() {
        check(
            indoc! {"
                <Main>
                    <let {x = 1}>
                        <div>Content</div>
                    </let>
                </Main>
            "},
            expect![[r#"
                error: Expected token ':' but got '='
                1 | <Main>
                2 |     <let {x = 1}>
                  |             ^
            "#]],
        );
    }

    #[test]
    fn should_reject_let_with_missing_value() {
        check(
            indoc! {"
                <Main>
                    <let {x: String}>
                        <div>Content</div>
                    </let>
                </Main>
            "},
            expect![[r#"
                error: Expected token '=' but got end of file
                1 | <Main>
                2 |     <let {x: String}>
                  |           ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_multiple_bindings() {
        check(
            indoc! {r#"
                <Main>
                    <let {first: String = "Hello", second: String = "World"}>
                        <div>{first} {second}</div>
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {first: String = "Hello", second: String = "World"}>
                    <div>
                      {first}
                      {second}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_three_bindings() {
        check(
            indoc! {r#"
                <Main>
                    <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                        <div>{a} + {b} + {c}</div>
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: Int = 1, b: Int = 2, c: Int = 3}>
                    <div>
                      {a}
                      +
                      {b}
                      +
                      {c}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_with_trailing_comma() {
        check(
            indoc! {r#"
                <Main>
                    <let {name: String = "World",}>
                        <div>Hello {name}</div>
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_sibling_let_tags() {
        check(
            indoc! {r#"
                <Main>
                    <let {a: String = "Hello"}>
                        {a}
                    </let>
                    <let {b: String = "World"}>
                        {b}
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {a: String = "Hello"}>
                    {a}
                  </let>
                  <let {b: String = "World"}>
                    {b}
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_after_html_element() {
        check(
            indoc! {r#"
                <Main>
                    <div>First</div>
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <div>
                    First
                  </div>
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {name}
                    </div>
                  </let>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_accept_let_before_html_element() {
        check(
            indoc! {r#"
                <Main>
                    <let {name: String = "World"}>
                        <div>Hello {name}</div>
                    </let>
                    <div>Last</div>
                </Main>
            "#},
            expect![[r#"
                <Main>
                  <let {name: String = "World"}>
                    <div>
                      Hello
                      {name}
                    </div>
                  </let>
                  <div>
                    Last
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn should_reject_top_level_html_element() {
        check(
            "<div></div>",
            expect![[r#"
                error: Component name must start with an uppercase letter
                1 | <div></div>
                  |  ^^^
            "#]],
        );
    }
}
