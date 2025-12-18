use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::Peekable;

use super::ast::{self, Ast, ComponentDefinition, Enum, EnumVariant, Import, Record, RecordField};
use super::node::{ParsedArgument, ParsedNode};
use super::token_tree::{TokenTree, build_tree};
use crate::document::document_cursor::{DocumentCursor, DocumentRange, StringSpan};
use crate::dop;
use crate::dop::ParsedDeclaration;
use crate::dop::Parser;
use crate::error_collector::ErrorCollector;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

use super::parse_error::ParseError;
use super::tokenizer::{self, Token, Tokenizer};

struct AttributeValidator {
    attributes: BTreeMap<StringSpan, tokenizer::TokenizedAttribute>,
    tag_name: DocumentRange,
    handled_attributes: HashSet<String>,
}

impl AttributeValidator {
    fn new(
        attributes: BTreeMap<StringSpan, tokenizer::TokenizedAttribute>,
        tag_name: DocumentRange,
    ) -> Self {
        Self {
            attributes,
            tag_name,
            handled_attributes: HashSet::new(),
        }
    }

    fn parse_attribute_value(
        value: &tokenizer::TokenizedAttributeValue,
    ) -> Result<ast::AttributeValue, ParseError> {
        match value {
            tokenizer::TokenizedAttributeValue::String(range) => {
                Ok(ast::AttributeValue::String(range.clone()))
            }
            tokenizer::TokenizedAttributeValue::Expression(range) => {
                match Parser::from(range.clone()).parse_exprs() {
                    Ok(exprs) => Ok(ast::AttributeValue::Expressions(exprs)),
                    Err(err) => Err(err.into()),
                }
            }
        }
    }

    // Parse an attribute or return an error.
    fn parse(attr: &tokenizer::TokenizedAttribute) -> Result<ast::Attribute, ParseError> {
        match &attr.value {
            Some(val) => Ok(ast::Attribute {
                name: attr.name.clone(),
                value: Some(Self::parse_attribute_value(val)?),
            }),
            None => Ok(ast::Attribute {
                name: attr.name.clone(),
                value: None,
            }),
        }
    }

    fn disallow_unrecognized(&self) -> impl Iterator<Item = ParseError> {
        self.attributes
            .values()
            .filter(|attr| !self.handled_attributes.contains(attr.name.as_str()))
            .map(move |attr| ParseError::UnrecognizedAttribute {
                tag_name: self.tag_name.to_string_span(),
                attr_name: attr.name.to_string_span(),
                range: attr.range.clone(),
            })
    }

    fn get_unrecognized(&self) -> impl Iterator<Item = Result<ast::Attribute, ParseError>> {
        self.attributes
            .values()
            .filter(|attr| !self.handled_attributes.contains(attr.name.as_str()))
            .map(AttributeValidator::parse)
    }
}

/// Find the end of an expression using the dop tokenizer.
///
/// Expects the current char iterator be on the first character
/// of a dop expression.
///
/// E.g. {x + 2}
///       ^
///
/// Returns None if we reached EOF before finding the closing '}'.
fn find_expression_end(iter: Peekable<DocumentCursor>) -> Option<DocumentRange> {
    let mut dop_tokenizer = dop::Tokenizer::from(iter);
    let mut open_braces = 1;
    loop {
        let token = dop_tokenizer.next()?;
        match token {
            Ok((dop::Token::LeftBrace, _)) => {
                open_braces += 1;
            }
            Ok((dop::Token::RightBrace, range)) => {
                open_braces -= 1;
                if open_braces == 0 {
                    return Some(range);
                }
            }
            _ => {}
        }
    }
}

pub fn parse(
    module_name: ModuleName,
    source: String,
    errors: &mut ErrorCollector<ParseError>,
) -> Ast {
    let source_range = DocumentCursor::new(source).range();

    // Build the token tree
    let tokenizer = Tokenizer::from_range(source_range);
    let trees = build_tree(tokenizer, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut records = Vec::new();
    let mut enums = Vec::new();

    let mut defined_components = HashSet::new();
    let mut imported_components = HashMap::new();
    let mut defined_records = HashSet::new();
    let mut defined_enums = HashSet::new();

    // Process token trees
    for mut tree in trees {
        match &tree.token {
            Token::Text { range } => {
                let mut decl_errors = ErrorCollector::new();
                for decl in Parser::from(range.clone()).parse_declarations(&mut decl_errors) {
                    match decl {
                        ParsedDeclaration::Import {
                            name,
                            name_range,
                            path,
                            module_name,
                            ..
                        } => {
                            let import = Import {
                                type_name: name,
                                type_name_range: name_range.clone(),
                                path,
                                module_name,
                            };
                            let name_str = import.type_name.as_str();
                            if imported_components.contains_key(name_str) {
                                errors.push(ParseError::TypeNameIsAlreadyDefined {
                                    name: name_range.to_string_span(),
                                    range: name_range,
                                });
                            } else {
                                imported_components
                                    .insert(name_str.to_string(), import.module_name.clone());
                            }
                            imports.push(import);
                        }
                        ParsedDeclaration::Record {
                            name,
                            name_range,
                            fields,
                            range: _,
                        } => {
                            let record = Record {
                                name: name.clone(),
                                name_range: name_range.clone(),
                                fields: fields
                                    .iter()
                                    .map(|(field_name, _field_name_range, field_type)| {
                                        RecordField {
                                            name: field_name.clone(),
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
                                    name: record.name_range.to_string_span(),
                                    range: record.name_range.clone(),
                                });
                            } else {
                                defined_records.insert(name.to_string());
                            }
                            records.push(record);
                        }
                        ParsedDeclaration::Enum {
                            name,
                            name_range,
                            variants,
                            range: _,
                        } => {
                            let enum_decl = Enum {
                                name: name.clone(),
                                name_range: name_range.clone(),
                                variants: variants
                                    .iter()
                                    .map(|(name, _range)| EnumVariant { name: name.clone() })
                                    .collect(),
                            };
                            let name = enum_decl.name();
                            if defined_enums.contains(name)
                                || defined_records.contains(name)
                                || defined_components.contains(name)
                                || imported_components.contains_key(name)
                            {
                                errors.push(ParseError::TypeNameIsAlreadyDefined {
                                    name: enum_decl.name_range.to_string_span(),
                                    range: enum_decl.name_range.clone(),
                                });
                            } else {
                                defined_enums.insert(name.to_string());
                            }
                            enums.push(enum_decl);
                        }
                    }
                }
                // Convert dop parse errors to hop parse errors
                for err in decl_errors.iter().cloned() {
                    errors.push(err.into());
                }
            }
            _ => {
                let children: Vec<ParsedNode> = std::mem::take(&mut tree.children)
                    .into_iter()
                    .flat_map(|child| {
                        construct_nodes(
                            child,
                            errors,
                            &module_name,
                            &defined_components,
                            &imported_components,
                        )
                    })
                    .collect();

                if let Some(component) = parse_component_definition(tree, children, errors) {
                    let name = component.tag_name.as_str();
                    if defined_components.contains(name)
                        || imported_components.contains_key(name)
                        || defined_records.contains(name)
                        || defined_enums.contains(name)
                    {
                        errors.push(ParseError::TypeNameIsAlreadyDefined {
                            name: component.tag_name.to_string_span(),
                            range: component.tag_name.clone(),
                        });
                    } else {
                        defined_components.insert(name.to_string());
                    }
                    components.push(component);
                }
            }
        }
    }

    Ast::new(module_name, components, imports, records, enums)
}

/// Try to parse a token tree as a component definition.
///
/// Returns `Some(component)` if the tree is an opening tag (component definition),
/// or `None` for other token types (text, comments, etc.).
fn parse_component_definition(
    tree: TokenTree,
    children: Vec<ParsedNode>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<ComponentDefinition> {
    let Token::OpeningTag {
        tag_name,
        attributes,
        expression,
        ..
    } = tree.token
    else {
        return None;
    };

    let validator = AttributeValidator::new(attributes, tag_name.clone());

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

    // Parse parameters
    let params = expression.as_ref().and_then(|expr| {
        errors.ok_or_add(
            Parser::from(expr.clone())
                .parse_parameters()
                .map(|parsed_params| {
                    let params = parsed_params
                        .into_iter()
                        .map(|((var_name, var_name_range), var_type)| ast::Parameter {
                            var_name,
                            var_name_range,
                            var_type,
                        })
                        .collect();
                    (params, expr.clone())
                })
                .map_err(|err| err.into()),
        )
    });

    // Disallow any unrecognized attributes on component definitions
    for error in validator.disallow_unrecognized() {
        errors.push(error);
    }

    Some(ComponentDefinition {
        component_name,
        tag_name: tag_name.clone(),
        closing_tag_name: tree.closing_tag_name,
        params,
        range: tree.range.clone(),
        children,
    })
}

/// Check if a tag name is one that should have raw text content (no expression parsing).
fn is_raw_text_element(tag_name: &str) -> bool {
    matches!(tag_name, "script" | "style")
}

fn construct_nodes(
    tree: TokenTree,
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
                value: range.to_string_span(),
                range,
            }]
        }
        Token::Text { range, .. } => {
            // Parse text content, splitting into Text and TextExpression nodes
            let mut nodes = Vec::new();
            let mut iter = range.cursor().peekable();
            let mut text_start: Option<DocumentRange> = None;

            while iter.peek().is_some() {
                let ch = iter.peek().unwrap();
                if ch.ch() == '{' {
                    // Flush accumulated text
                    if let Some(text_range) = text_start.take() {
                        nodes.push(ParsedNode::Text {
                            value: text_range.to_string_span(),
                            range: text_range,
                        });
                    }

                    // Parse expression
                    let left_brace = iter.next().unwrap();

                    // Check for empty expression
                    if iter.peek().map(|s| s.ch()) == Some('}') {
                        let right_brace = iter.next().unwrap();
                        errors.push(ParseError::new(
                            "Empty expression".to_string(),
                            left_brace.to(right_brace),
                        ));
                        continue;
                    }

                    // Find the end of the expression
                    if let Some(right_brace) = find_expression_end(iter.clone()) {
                        // Collect the expression content (everything between braces)
                        let mut expr_range: Option<DocumentRange> = None;
                        while iter.peek().map(|s| s.start()) != Some(right_brace.start()) {
                            let ch_range = iter.next().unwrap();
                            expr_range = Some(
                                expr_range
                                    .map(|r| r.to(ch_range.clone()))
                                    .unwrap_or(ch_range),
                            );
                        }
                        // Consume the closing brace
                        iter.next();

                        if let Some(expr_range) = expr_range {
                            // Parse the expression
                            match Parser::from(expr_range.clone()).parse_expr() {
                                Ok(expression) => {
                                    nodes.push(ParsedNode::TextExpression {
                                        expression,
                                        range: left_brace.to(right_brace),
                                    });
                                }
                                Err(err) => {
                                    errors.push(err.into());
                                }
                            }
                        }
                    } else {
                        // Unmatched brace - report error, treat as text
                        errors.push(ParseError::UnmatchedCharacter {
                            ch: '{',
                            range: left_brace.clone(),
                        });
                        text_start = Some(
                            text_start
                                .map(|r| r.to(left_brace.clone()))
                                .unwrap_or(left_brace),
                        );
                    }
                } else {
                    // Accumulate text
                    let ch_range = iter.next().unwrap();
                    text_start = Some(
                        text_start
                            .map(|r| r.to(ch_range.clone()))
                            .unwrap_or(ch_range),
                    );
                }
            }

            // Flush remaining text
            if let Some(text_range) = text_start {
                nodes.push(ParsedNode::Text {
                    value: text_range.to_string_span(),
                    range: text_range,
                });
            }

            nodes
        }
        Token::OpeningTag {
            tag_name,
            expression,
            attributes,
            range: opening_tag_range,
            ..
        } => {
            let validator = AttributeValidator::new(attributes, tag_name.clone());

            // Process children - raw text elements (script/style) don't parse expressions
            let children: Vec<_> = if is_raw_text_element(tag_name.as_str()) {
                tree.children
                    .into_iter()
                    .filter_map(|child| match child.token {
                        Token::Text { range, .. } => Some(ParsedNode::Text {
                            value: range.to_string_span(),
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
                    let expr = expression.ok_or_else(|| {
                        ParseError::new(
                            "Missing expression in <if> tag".to_string(),
                            opening_tag_range.clone(),
                        )
                    });
                    let Some(condition) = errors.ok_or_add(
                        expr.and_then(|e| Parser::from(e).parse_expr().map_err(|err| err.into())),
                    ) else {
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
                        .ok_or_else(|| {
                            ParseError::new(
                                "Missing loop generator expression in <for> tag".to_string(),
                                opening_tag_range.clone(),
                            )
                        })
                        .and_then(|e| {
                            Parser::from(e.clone())
                                .parse_loop_header()
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

                    let args = expression.as_ref().and_then(|expr| {
                        errors.ok_or_add(
                            Parser::from(expr.clone())
                                .parse_arguments()
                                .map(|parsed_args| {
                                    let named_args = parsed_args
                                        .into_iter()
                                        .map(|((var_name, var_name_range), var_expr)| {
                                            ParsedArgument {
                                                var_name,
                                                var_name_range,
                                                var_expr,
                                            }
                                        })
                                        .collect();
                                    (named_args, expr.clone())
                                })
                                .map_err(|err| err.into()),
                        )
                    });

                    let definition_module = if defined_components.contains(component_name.as_str())
                    {
                        Some(module_name.clone())
                    } else {
                        imported_components.get(component_name.as_str()).cloned()
                    };

                    // Disallow any unrecognized attributes on component references
                    for error in validator.disallow_unrecognized() {
                        errors.push(error);
                    }

                    vec![ParsedNode::ComponentReference {
                        component_name,
                        component_name_opening_range: tag_name,
                        component_name_closing_range: tree.closing_tag_name,
                        definition_module,
                        args,
                        range: tree.range,
                        children,
                    }]
                }

                _ => {
                    // Default case: treat as HTML
                    let attributes = validator
                        .get_unrecognized()
                        .filter_map(|attr| errors.ok_or_add(attr))
                        .map(|attr| (attr.name.to_string_span(), attr))
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
    use super::ast::ComponentDefinition;
    use super::*;
    use crate::document::{DocumentAnnotator, document_cursor::Ranged};
    use crate::error_collector::ErrorCollector;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn node_name(node: &ParsedNode) -> &str {
        match node {
            ParsedNode::Doctype { .. } => "doctype",
            ParsedNode::ComponentReference { .. } => "component_reference",
            ParsedNode::If { .. } => "if",
            ParsedNode::For { .. } => "for",
            ParsedNode::Html { tag_name, .. } => tag_name.as_str(),
            ParsedNode::Text { .. } => "text",
            ParsedNode::TextExpression { .. } => "text_expression",
            ParsedNode::Placeholder { .. } => "error",
        }
    }

    fn write_node(node: &ParsedNode, depth: usize, lines: &mut Vec<String>) {
        if matches!(node, ParsedNode::Text { .. }) {
            return;
        }
        let left = format!("{}{}", "    ".repeat(depth).as_str(), node_name(node));
        let right = format!(
            "{}-{}",
            node.range().start_utf32(),
            node.range().end_utf32()
        );
        lines.push(format!("{:<50}{}", left, right));
        for child in node.children() {
            write_node(child, depth + 1, lines);
        }
    }

    pub fn format_component_definition(d: &ComponentDefinition) -> String {
        let mut lines = Vec::new();
        for child in &d.children {
            write_node(child, 0, &mut lines);
        }
        lines.join("\n") + "\n"
    }

    fn check(input: &str, expected: Expect) {
        let mut errors = ErrorCollector::new();
        let module = parse(
            ModuleName::new("test").unwrap(),
            input.to_string(),
            &mut errors,
        );

        let actual = if !errors.is_empty() {
            DocumentAnnotator::new()
                .with_label("error")
                .with_lines_before(1)
                .annotate(None, errors.to_vec())
        } else {
            for component in module.get_component_definitions() {
                if component.tag_name.as_str() == "Main" {
                    return expected.assert_eq(&format_component_definition(component));
                }
            }
            String::new()
        };

        println!("{}", actual);

        expected.assert_eq(&actual);
    }

    #[test]
    fn should_accept_empty_file() {
        check("", expect![[""]]);
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
                for                                               7:4-12:10
                    for                                           8:8-11:14
                        if                                        9:12-10:17
                for                                               13:4-18:10
                    for                                           14:8-17:14
                        for                                       15:12-16:18
            "#]],
        );
    }

    #[test]
    fn should_accept_script_and_style_tag_content_as_raw_text() {
        check(
            indoc! {r#"
                <Main>
                    <script>
                        // note that the <div> inside here is note
                        // parsed as html
                        console.log("<div>test</div>");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </Main>
            "#},
            expect![[r#"
                script                                            1:4-5:13
                style                                             6:4-8:12
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
                form                                              1:4-4:11
                    input                                         2:8-2:36
                    button                                        3:8-3:43
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
                hr                                                2:4-2:9
                br                                                3:4-3:9
                input                                             4:4-4:12
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
                doctype                                           1:4-1:19
                html                                              2:4-6:11
                    body                                          3:8-5:15
                        div                                       4:12-4:34
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
                error: Unexpected end of expression
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
                error: Unexpected end of expression
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
                a                                                 5:4-5:50
            "#]],
        );
    }

    #[test]
    fn should_accept_multiple_expressions_in_class_attribute() {
        check(
            indoc! {r#"
                <Main {style1: String, style2: String, style3: String}>
                    <div class={style1, style2, style3}>Content</div>
                </Main>
            "#},
            expect![[r#"
                div                                               1:4-1:53
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
                component_reference                               1:4-1:15
                component_reference                               2:4-2:15
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
                    <Foo {a: data}/>
                    <Bar {b: data.user}/>
                </Main>
            "#},
            expect![[r#"
                component_reference                               6:4-6:20
                component_reference                               7:4-7:25
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
                for                                               1:4-3:10
                    div                                           2:8-2:31
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
                for                                               1:4-3:10
                    div                                           2:8-2:22
                        text_expression                           2:13-2:16
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
                if                                                1:4-3:9
                    div                                           2:8-2:24
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
                if                                                1:1-5:6
                    for                                           2:2-4:8
                        text_expression                           3:10-3:13
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
                div                                               1:4-12:10
                    svg                                           2:8-8:14
                        g                                         3:12-7:16
                            path                                  4:16-4:65
                            path                                  5:16-5:63
                            path                                  6:16-6:73
                    ul                                            9:8-11:13
                        li                                        10:12-10:41
                            a                                     10:16-10:36
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
                div                                               1:4-1:21
                    text_expression                               1:9-1:15
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
                h1                                                5:4-5:24
                p                                                 6:4-6:25
                    text_expression                               6:7-6:21
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
                for                                               1:4-3:10
                    div                                           2:8-2:25
                        text_expression                           2:13-2:19
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
                for                                               6:4-11:10
                    h1                                            7:8-7:32
                        text_expression                           7:12-7:27
                    for                                           8:8-10:14
                        div                                       9:12-9:29
                            text_expression                       9:17-9:23
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_single_expression() {
        check(
            "<Main><h1>Hello {name}!</h1></Main>",
            expect![[r#"
                h1                                                0:6-0:28
                    text_expression                               0:16-0:22
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_multiple_expressions() {
        check(
            "<Main><p>User {user.name} has {user.count} items</p></Main>",
            expect![[r#"
                p                                                 0:6-0:52
                    text_expression                               0:14-0:25
                    text_expression                               0:30-0:42
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_expression_at_start() {
        check(
            "<Main><span>{greeting} world!</span></Main>",
            expect![[r#"
                span                                              0:6-0:36
                    text_expression                               0:12-0:22
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_expression_at_end() {
        check(
            "<Main><div>Price: {price}</div></Main>",
            expect![[r#"
                div                                               0:6-0:31
                    text_expression                               0:18-0:25
            "#]],
        );
    }

    #[test]
    fn should_parse_text_with_only_expression() {
        check(
            "<Main><h2>{title}</h2></Main>",
            expect![[r#"
                h2                                                0:6-0:22
                    text_expression                               0:10-0:17
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
                script                                            1:4-4:13
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
                style                                             1:4-4:12
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
                p                                                 0:6-0:54
                    text_expression                               0:17-0:50
            "#]],
        );
    }

    #[test]
    fn should_parse_adjacent_expressions_in_text() {
        check(
            "<Main><span>{first}{second}</span></Main>",
            expect![[r#"
                span                                              0:6-0:34
                    text_expression                               0:12-0:19
                    text_expression                               0:19-0:27
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
                error: Unexpected end of expression
                1 | record
                  | ^^^^^^
                2 | <Main>
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
                error: Expected declaration (import, record, or enum)
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
                text_expression                                   3:4-3:62
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
                div                                               3:4-3:90
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
                text_expression                                   3:4-7:6
            "#]],
        );
    }
}
