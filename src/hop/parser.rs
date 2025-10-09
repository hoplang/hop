use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::Parser;
use crate::error_collector::ErrorCollector;
use crate::hop::ast::{Ast, ComponentDefinition, Import};
use crate::hop::parse_error::ParseError;
use crate::hop::token_tree::{TokenTree, build_tree};
use crate::hop::tokenizer::{Token, Tokenizer};
use std::collections::{BTreeMap, HashMap, HashSet};

use super::ast::{self, UntypedAst, UntypedComponentDefinition};
use super::module_name::ModuleName;
use super::node::{Node, UntypedNode};
use super::tokenizer;

struct AttributeValidator {
    attributes: BTreeMap<StringSpan, tokenizer::Attribute>,
    tag_name: DocumentRange,
    handled_attributes: HashSet<String>,
}

impl AttributeValidator {
    fn new(
        attributes: BTreeMap<StringSpan, tokenizer::Attribute>,
        tag_name: DocumentRange,
    ) -> Self {
        Self {
            attributes,
            tag_name,
            handled_attributes: HashSet::new(),
        }
    }

    fn parse_attribute_value(
        value: &tokenizer::AttributeValue,
    ) -> Result<ast::AttributeValue, ParseError> {
        match value {
            tokenizer::AttributeValue::String(range) => {
                Ok(ast::AttributeValue::String(range.clone()))
            }
            tokenizer::AttributeValue::Expression(expr) => {
                match Parser::from(expr.clone()).parse_expr() {
                    Ok(expr) => Ok(ast::AttributeValue::Expression(expr)),
                    Err(err) => Err(err.into()),
                }
            }
        }
    }

    fn allow_static(&mut self, key: &str) -> Option<Result<ast::StaticAttribute, ParseError>> {
        self.handled_attributes.insert(key.to_string());
        self.attributes.get(key).map(Self::parse_as_static)
    }

    fn allow_boolean(&mut self, key: &str) -> Result<bool, ParseError> {
        self.handled_attributes.insert(key.to_string());
        match self.attributes.get(key) {
            None => Ok(false),
            Some(attr) => Self::parse_as_boolean(attr),
        }
    }

    // Require an attribute.
    //
    // Checks that the attributes is present and returns it or returns an error.
    fn require(&mut self, key: &str) -> Result<&tokenizer::Attribute, ParseError> {
        self.handled_attributes.insert(key.to_string());
        self.attributes
            .get(key)
            .ok_or_else(|| ParseError::MissingRequiredAttribute {
                tag_name: self.tag_name.to_string_span(),
                attr: key.to_string(),
                range: self.tag_name.clone(),
            })
    }

    // Require an attribute and parse it as static.
    //
    // Checks that the attributes is present and returns it or returns an error.
    fn require_static(&mut self, key: &str) -> Result<ast::StaticAttribute, ParseError> {
        self.require(key).and_then(Self::parse_as_static)
    }

    // Parse an attribute or return an error.
    fn parse(attr: &tokenizer::Attribute) -> Result<ast::Attribute, ParseError> {
        match &attr.value {
            Some(val) => Ok(ast::Attribute {
                name: attr.name.clone(),
                value: Some(Self::parse_attribute_value(val)?),
                range: attr.range.clone(),
            }),
            None => Ok(ast::Attribute {
                name: attr.name.clone(),
                value: None,
                range: attr.range.clone(),
            }),
        }
    }

    // Parse an attribute as static or return an error.
    fn parse_as_static(attr: &tokenizer::Attribute) -> Result<ast::StaticAttribute, ParseError> {
        match &attr.value {
            Some(tokenizer::AttributeValue::String(s)) => {
                Ok(ast::StaticAttribute { value: s.clone() })
            }
            Some(tokenizer::AttributeValue::Expression(expr_range)) => {
                Err(ParseError::AttributeMustBeStaticallyKnown {
                    attr_name: attr.name.to_string_span(),
                    range: expr_range.clone(),
                })
            }
            None => Err(ParseError::AttributeMissingValue {
                attr_name: attr.name.to_string_span(),
                range: attr.name.clone(),
            }),
        }
    }

    // Parse an attribute as boolean return an error.
    fn parse_as_boolean(attr: &tokenizer::Attribute) -> Result<bool, ParseError> {
        match &attr.value {
            Some(tokenizer::AttributeValue::String(s)) => {
                // TODO: Better error
                Err(ParseError::AttributeMustBeStaticallyKnown {
                    attr_name: attr.name.to_string_span(),
                    range: s.clone(),
                })
            }
            Some(tokenizer::AttributeValue::Expression(expr_range)) => {
                // TODO: Better error
                Err(ParseError::AttributeMustBeStaticallyKnown {
                    attr_name: attr.name.to_string_span(),
                    range: expr_range.clone(),
                })
            }
            None => Ok(true),
        }
    }

    fn parse_import_name_from_attr(
        attr: ast::StaticAttribute,
    ) -> Result<(ast::StaticAttribute, ModuleName), ParseError> {
        // Strip the @/ prefix for internal module resolution
        let module_name_input = match attr.value.as_str().strip_prefix("@/") {
            Some(n) => n,
            None => {
                return Err(ParseError::MissingAtPrefixInImportPath {
                    range: attr.value.clone(),
                });
            }
        };
        match ModuleName::new(module_name_input.to_string()) {
            Ok(name) => Ok((attr.clone(), name)),
            Err(e) => Err(ParseError::InvalidModuleName {
                error: e,
                range: attr.value.clone(),
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

enum TopLevelNode {
    Import(Import),
    ComponentDefinition(UntypedComponentDefinition),
}

pub fn parse(
    module_name: ModuleName,
    tokenizer: Tokenizer,
    errors: &mut ErrorCollector<ParseError>,
) -> UntypedAst {
    let trees = build_tree(tokenizer, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();

    let mut defined_components = HashSet::new();
    let mut imported_components = HashMap::new();

    for mut tree in trees {
        let children: Vec<UntypedNode> = std::mem::take(&mut tree.children)
            .into_iter()
            .filter_map(|child| {
                construct_node(
                    child,
                    errors,
                    &module_name,
                    &defined_components,
                    &imported_components,
                )
            })
            .collect();

        if let Some(node) = parse_top_level_node(tree, children, errors) {
            match node {
                TopLevelNode::Import(import) => {
                    let component_name = import.component_attr.value.as_str();
                    if imported_components.contains_key(component_name) {
                        errors.push(ParseError::ComponentIsAlreadyDefined {
                            component_name: import.component_attr.value.to_string_span(),
                            range: import.component_attr.value.clone(),
                        });
                    } else {
                        imported_components
                            .insert(component_name.to_string(), import.module_name.clone());
                    }
                    imports.push(import);
                }
                TopLevelNode::ComponentDefinition(component) => {
                    let name = component.tag_name.as_str();
                    if defined_components.contains(name) || imported_components.contains_key(name) {
                        errors.push(ParseError::ComponentIsAlreadyDefined {
                            component_name: component.tag_name.to_string_span(),
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

    Ast::new(module_name, components, imports)
}

fn parse_top_level_node(
    tree: TokenTree,
    children: Vec<UntypedNode>,
    errors: &mut ErrorCollector<ParseError>,
) -> Option<TopLevelNode> {
    match tree.token {
        Token::Text { .. } => None,
        Token::ClosingTag { .. } => None,
        Token::TextExpression { .. } => None,
        Token::Comment { .. } => None,
        Token::Doctype { .. } => None,
        Token::OpeningTag {
            tag_name,
            attributes,
            expression,
            ..
        } => {
            let mut validator = AttributeValidator::new(attributes, tag_name.clone());
            //
            match tag_name.as_str() {
                // <import ...>
                "import" => {
                    let (from_attr, module_name) = errors.ok_or_add(
                        validator
                            .require_static("from")
                            .and_then(AttributeValidator::parse_import_name_from_attr),
                    )?;
                    let component_attr = errors.ok_or_add(validator.require_static("component"))?;
                    errors.extend(validator.disallow_unrecognized());
                    Some(TopLevelNode::Import(Import {
                        component_attr,
                        module_name,
                        from_attr,
                    }))
                }

                // <component-definition ...>
                name => {
                    // Validate component name
                    if !is_valid_component_name(name) {
                        errors.push(ParseError::InvalidComponentName {
                            tag_name: tag_name.to_string_span(),
                            range: tag_name.clone(),
                        });
                        return None;
                    }

                    // Parse parameters
                    let params = expression.as_ref().and_then(|expr| {
                        errors.ok_or_add(
                            Parser::from(expr.clone())
                                .parse_parameters()
                                .map(|params| (params, expr.clone()))
                                .map_err(|err| err.into()),
                        )
                    });

                    // TODO: Here we iterate over the whole subtree to check
                    // if it contains a slot. There should be a better way
                    // to do this.
                    let mut has_slot = false;
                    for child in &children {
                        for node in child.iter_depth_first() {
                            if let Node::SlotDefinition { range, .. } = node {
                                if has_slot {
                                    errors.push(ParseError::SlotIsAlreadyDefined {
                                        range: range.clone(),
                                    });
                                }
                                has_slot = true;
                            }
                        }
                    }

                    // Parse attributes
                    let is_entrypoint = errors
                        .ok_or_add(validator.allow_boolean("entrypoint"))
                        .is_some_and(|v| v);
                    let as_attr = validator
                        .allow_static("as")
                        .and_then(|attr| errors.ok_or_add(attr));
                    let attributes = validator
                        .get_unrecognized()
                        .filter_map(|attr| errors.ok_or_add(attr))
                        .map(|attr| (attr.name.to_string_span(), attr))
                        .collect();

                    Some(TopLevelNode::ComponentDefinition(ComponentDefinition {
                        tag_name: tag_name.clone(),
                        closing_tag_name: tree.closing_tag_name,
                        params,
                        is_entrypoint,
                        as_attr,
                        attributes,
                        range: tree.range.clone(),
                        children,
                        has_slot,
                    }))
                }
            }
        }
    }
}

fn is_valid_component_name(name: &str) -> bool {
    if name.is_empty() || name.starts_with('-') || name.ends_with('-') {
        return false;
    }
    if name.starts_with("hop-") {
        return false;
    }
    name.contains('-')
}

fn construct_node(
    tree: TokenTree,
    errors: &mut ErrorCollector<ParseError>,
    module_name: &ModuleName,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, ModuleName>,
) -> Option<UntypedNode> {
    let children: Vec<_> = tree
        .children
        .into_iter()
        .filter_map(|child| {
            construct_node(
                child,
                errors,
                module_name,
                defined_components,
                imported_components,
            )
        })
        .collect();

    match tree.token {
        Token::Comment { .. } => {
            // Skip comments
            None
        }
        Token::ClosingTag { .. } => {
            // ClosingTags are not present in the token tree
            unreachable!()
        }
        Token::Doctype { range } => {
            //
            Some(Node::Doctype {
                value: range.to_string_span(),
                range,
            })
        }
        Token::Text { range, .. } => {
            //
            Some(Node::Text {
                value: range.to_string_span(),
                range,
            })
        }
        Token::TextExpression {
            expression: expr, ..
        } => {
            let expression = errors.ok_or_add(
                Parser::from(expr.clone())
                    .parse_expr()
                    .map_err(|err| err.into()),
            )?;
            Some(Node::TextExpression {
                expression,
                range: tree.range.clone(),
            })
        }
        Token::OpeningTag {
            tag_name,
            expression,
            attributes,
            range: opening_tag_range,
            ..
        } => {
            let validator = AttributeValidator::new(attributes, tag_name.clone());
            //
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
                    let condition =
                        errors.ok_or_add(expr.and_then(|e| {
                            Parser::from(e).parse_expr().map_err(|err| err.into())
                        }))?;
                    Some(Node::If {
                        condition,
                        range: tree.range.clone(),
                        children,
                    })
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
                        return Some(Node::Placeholder {
                            range: tree.range.clone(),
                            children,
                        });
                    };
                    Some(Node::For {
                        var_name,
                        var_name_range,
                        array_expr,
                        range: tree.range.clone(),
                        children,
                    })
                }

                // <slot-default>
                "slot-default" => {
                    errors.extend(validator.disallow_unrecognized());
                    Some(Node::SlotDefinition {
                        range: tree.range.clone(),
                    })
                }

                // <hop-x-raw>
                "hop-x-raw" => {
                    errors.extend(validator.disallow_unrecognized());

                    // XRaw should contain either no children or a single Text node
                    match children.into_iter().next() {
                        None => None,
                        Some(text_node @ Node::Text { .. }) => Some(text_node),
                        _ => panic!(
                            "hop-x-raw should contain either no children or a single Text node"
                        ),
                    }
                }

                name if name.starts_with("hop-") => {
                    errors.push(ParseError::UnrecognizedHopTag {
                        tag: tag_name.to_string_span(),
                        range: tag_name.clone(),
                    });
                    Some(Node::Placeholder {
                        range: tree.range.clone(),
                        children: vec![],
                    })
                }

                // <component-reference>
                name if name.contains('-') => {
                    if !is_valid_component_name(tag_name.as_str()) {
                        errors.push(ParseError::InvalidComponentName {
                            tag_name: tag_name.to_string_span(),
                            range: tag_name.clone(),
                        });
                    }

                    let args = expression.as_ref().and_then(|expr| {
                        errors.ok_or_add(
                            Parser::from(expr.clone())
                                .parse_arguments()
                                .map(|named_args| (named_args, expr.clone()))
                                .map_err(|err| err.into()),
                        )
                    });

                    let definition_module = if defined_components.contains(tag_name.as_str()) {
                        Some(module_name.clone())
                    } else {
                        imported_components.get(tag_name.as_str()).cloned()
                    };

                    let attributes = validator
                        .get_unrecognized()
                        .filter_map(|attr| errors.ok_or_add(attr))
                        .map(|attr| (attr.name.to_string_span(), attr))
                        .collect();

                    Some(Node::ComponentReference {
                        tag_name,
                        closing_tag_name: tree.closing_tag_name,
                        definition_module,
                        args,
                        attributes,
                        range: tree.range,
                        children,
                    })
                }

                _ => {
                    // Default case: treat as HTML
                    let attributes = validator
                        .get_unrecognized()
                        .filter_map(|attr| errors.ok_or_add(attr))
                        .map(|attr| (attr.name.to_string_span(), attr))
                        .collect();

                    Some(Node::Html {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::{DocumentAnnotator, document_cursor::Ranged};
    use crate::error_collector::ErrorCollector;
    use crate::hop::ast::UntypedComponentDefinition;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn node_name(node: &UntypedNode) -> &str {
        match node {
            Node::Doctype { .. } => "doctype",
            Node::ComponentReference { .. } => "component_reference",
            Node::If { .. } => "if",
            Node::For { .. } => "for",
            Node::Html { tag_name, .. } => tag_name.as_str(),
            Node::SlotDefinition { .. } => "slot-definition",
            Node::Text { .. } => "text",
            Node::TextExpression { .. } => "text_expression",
            Node::Placeholder { .. } => "error",
        }
    }

    fn write_node(node: &UntypedNode, depth: usize, lines: &mut Vec<String>) {
        if matches!(node, Node::Text { .. }) {
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

    pub fn format_component_definition(d: &UntypedComponentDefinition) -> String {
        let mut lines = Vec::new();
        for child in &d.children {
            write_node(child, 0, &mut lines);
        }
        lines.join("\n") + "\n"
    }

    fn check(input: &str, expected: Expect) {
        let mut errors = ErrorCollector::new();
        let module = parse(
            ModuleName::new("test".to_string()).unwrap(),
            Tokenizer::new(input.to_string()),
            &mut errors,
        );

        let actual = if !errors.is_empty() {
            DocumentAnnotator::new()
                .with_label("error")
                .with_lines_before(1)
                .annotate(None, errors.to_vec())
        } else {
            for component in module.get_component_definitions() {
                if component.tag_name.as_str() == "main-comp" {
                    return expected.assert_eq(&format_component_definition(component));
                }
            }
            String::new()
        };

        println!("{}", actual);

        expected.assert_eq(&actual);
    }

    // The parser allows empty file and the resulting output is empty.
    #[test]
    fn test_parser_empty_file() {
        check("", expect![[""]]);
    }

    // When a tag is not properly closed the parser outputs an error.
    #[test]
    fn test_parser_unclosed_tag() {
        check(
            indoc! {"
                <main-comp>
                    <div>
                    <p>
                </main-comp>

                <foo-comp>
            "},
            // TODO: It would make more sense if these were reported in the opposite
            // order.
            expect![[r#"
                error: Unclosed <p>
                2 |     <div>
                3 |     <p>
                  |      ^

                error: Unclosed <div>
                1 | <main-comp>
                2 |     <div>
                  |      ^^^

                error: Unclosed <foo-comp>
                5 | 
                6 | <foo-comp>
                  |  ^^^^^^^^
            "#]],
        );
    }

    // When a void tag is closed with an end tag the parser outputs an error.
    #[test]
    fn test_parser_void_tag_closed() {
        check(
            indoc! {"
                <main-comp>
                    <hr></hr>
                    <br></br>
                    <input></input>
                </main-comp>
            "},
            expect![[r#"
                error: <hr> should not be closed using a closing tag
                1 | <main-comp>
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

    // Import tags are treated as a void tag.
    #[test]
    fn test_parser_import_self_closing() {
        check(
            indoc! {r#"
                <import component="foo" from="@/bar"></import>
            "#},
            expect![[r#"
                error: <import> should not be closed using a closing tag
                1 | <import component="foo" from="@/bar"></import>
                  |                                      ^^^^^^^^^
            "#]],
        );
    }

    // Void tags are allowed to be self-closing.
    #[test]
    fn test_parser_void_tag_may_be_self_closing() {
        check(
            indoc! {r#"
                <import component="foo" from="@/bar">
                <main-comp>
                    <hr/>
                    <br/>
                    <input/>
                </main-comp>
                <foo-comp/>
            "#},
            expect![[r#"
                hr                                                2:4-2:9
                br                                                3:4-3:9
                input                                             4:4-4:12
            "#]],
        );
    }

    // When a component reference has an invalid tag name, the parser outputs an error.
    #[test]
    fn test_parser_invalid_tag_name_in_component_reference() {
        check(
            indoc! {"
                <main-comp>
                    <foo-></foo->
                    <foo-bar-></foo-bar->
                    <X-BAR-></X-BAR->
                </main-comp>

                <foo-></foo->
            "},
            expect![[r#"
                error: Invalid component name 'foo-'. Component names must contain a dash and not start or end with one
                1 | <main-comp>
                2 |     <foo-></foo->
                  |      ^^^^

                error: Invalid component name 'foo-bar-'. Component names must contain a dash and not start or end with one
                2 |     <foo-></foo->
                3 |     <foo-bar-></foo-bar->
                  |      ^^^^^^^^

                error: Invalid component name 'X-BAR-'. Component names must contain a dash and not start or end with one
                3 |     <foo-bar-></foo-bar->
                4 |     <X-BAR-></X-BAR->
                  |      ^^^^^^

                error: Invalid component name 'foo-'. Component names must contain a dash and not start or end with one
                6 | 
                7 | <foo-></foo->
                  |  ^^^^
            "#]],
        );
    }

    // When a closing tag does not have a matching opening tag, the parser outputs an error.
    #[test]
    fn test_parser_unmatched_closing_tag() {
        check(
            indoc! {"
                <main-comp>
                    </div>
                    </p>
                </main-comp>
                </main-comp>
            "},
            expect![[r#"
                error: Unmatched </div>
                1 | <main-comp>
                2 |     </div>
                  |     ^^^^^^

                error: Unmatched </p>
                2 |     </div>
                3 |     </p>
                  |     ^^^^

                error: Unmatched </main-comp>
                4 | </main-comp>
                5 | </main-comp>
                  | ^^^^^^^^^^^^
            "#]],
        );
    }

    // To declare a component at the top level scope it must have a valid name.
    #[test]
    fn test_parser_invalid_component_name() {
        check(
            indoc! {"
                <div>
                </div>
                <p></p>
            "},
            expect![[r#"
                error: Invalid component name 'div'. Component names must contain a dash and not start or end with one
                1 | <div>
                  |  ^^^

                error: Invalid component name 'p'. Component names must contain a dash and not start or end with one
                2 | </div>
                3 | <p></p>
                  |  ^
            "#]],
        );
    }

    // An unrecognized hop tag should produce an error.
    #[test]
    fn test_parser_unrecognized_hop_tag() {
        check(
            indoc! {"
                <main-comp>
                    <hop-whatever>Content</hop-whatever>
                </main-comp>
            "},
            expect![[r#"
                error: Unrecognized hop tag: <hop-whatever>
                1 | <main-comp>
                2 |     <hop-whatever>Content</hop-whatever>
                  |      ^^^^^^^^^^^^
            "#]],
        );
    }

    // An if tag without expression should produce error.
    #[test]
    fn test_parser_if_no_expression_error() {
        check(
            indoc! {"
                <main-comp>
                    <if>
                        <div>Content</div>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                error: Missing expression in <if> tag
                1 | <main-comp>
                2 |     <if>
                  |     ^^^^
            "#]],
        );
    }

    #[test]
    fn test_parser_uppercase_variable_name() {
        check(
            indoc! {"
                <main-comp {Data: String}>
                    <div></div>
                </main-comp>
            "},
            expect![[r#"
                div                                               1:4-1:15
            "#]],
        );
    }

    // A for tag without expression should produce error.
    #[test]
    fn test_parser_for_no_expression_error() {
        check(
            indoc! {"
                <main-comp>
                    <for>
                        <div>Content</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                error: Missing loop generator expression in <for> tag
                1 | <main-comp>
                2 |     <for>
                  |     ^^^^^
            "#]],
        );
    }

    // A for tag with non-loop-generator expression should produce error.
    #[test]
    fn test_parser_for_invalid_expression_error() {
        check(
            indoc! {"
                <main-comp>
                    <for {foo}>
                        <div>Content</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                error: Unexpected end of expression
                1 | <main-comp>
                2 |     <for {foo}>
                  |           ^^^
            "#]],
        );
    }

    // An if expression without valid tokens should produce an error.
    #[test]
    fn test_parser_dop_tokenization_error() {
        check(
            indoc! {"
                <main-comp>
                    <if {~}>
                        <div>Content</div>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                error: Unexpected character: '~'
                1 | <main-comp>
                2 |     <if {~}>
                  |          ^
            "#]],
        );
    }

    // Component parameter with invalid type should produce an error.
    #[test]
    fn test_parser_param_invalid_type_error() {
        check(
            indoc! {"
                <main-comp {data: invalid}>
                    <div>{data}</div>
                </main-comp>
            "},
            expect![[r#"
                error: Expected type name
                1 | <main-comp {data: invalid}>
                  |                   ^^^^^^^
            "#]],
        );
    }

    // Component parameter with malformed type should produce error.
    #[test]
    fn test_parser_param_malformed_type_error() {
        check(
            indoc! {"
                <main-comp {data: Array[}>
                    <div>{data}</div>
                </main-comp>
            "},
            // TODO: Improve error message
            expect![[r#"
                error: Unexpected end of expression
                1 | <main-comp {data: Array[}>
                  |             ^^^^^^^^^^^^
            "#]],
        );
    }

    // When slot-default is defined twice in a component, the parser outputs an error.
    #[test]
    fn test_slot_default_defined_twice() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <slot-default/>
                    <slot-default/>
                </main-comp>
            "#},
            expect![[r#"
                error: slot-default is already defined
                3 |     <slot-default/>
                4 |     <slot-default/>
                  |     ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_parser_nested_loops_complex_types() {
        check(
            indoc! {"
                <main-comp {sections: Array[{title: String, items: Array[String]}]}>
                    <div>
                        <for {section in sections}>
                            <div>
                                <h2>{section.title}</h2>
                                <for {item in section.items}>
                                    <p>{item}</p>
                                </for>
                            </div>
                        </for>
                    </div>
                </main-comp>
            "},
            expect![[r#"
                div                                               1:4-10:10
                    for                                           2:8-9:14
                        div                                       3:12-8:18
                            h2                                    4:16-4:40
                                text_expression                   4:20-4:35
                            for                                   5:16-7:22
                                p                                 6:20-6:33
                                    text_expression               6:23-6:29
            "#]],
        );
    }

    #[test]
    fn test_parser_doctype_html_structure() {
        check(
            indoc! {"
                <main-comp {foo: String}>
                    <!DOCTYPE html>
                    <html>
                        <body>
                            <div>hello world</div>
                        </body>
                    </html>
                </main-comp>
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
    fn test_parser_entrypoint_with_script_style() {
        check(
            indoc! {r#"
                <main-comp entrypoint>
                    <script>
                        // note that the <div> inside here is note
                        // parsed as html
                        console.log("<div>test</div>");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </main-comp>
            "#},
            expect![[r#"
                script                                            1:4-5:13
                style                                             6:4-8:12
            "#]],
        );
    }

    #[test]
    fn test_parser_entrypoint_with_data_param() {
        check(
            indoc! {"
                <main-comp entrypoint {data: {message: String}}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </main-comp>
            "},
            expect![[r#"
                h1                                                1:4-1:24
                p                                                 2:4-2:25
                    text_expression                               2:7-2:21
            "#]],
        );
    }

    #[test]
    fn test_parser_if_conditions_various() {
        check(
            indoc! {r#"
                <main-comp>
                    <if {user.name == other_user.name}>
                        <div>Same name</div>
                    </if>
                    <if {(data.x == data.y)}>
                        <div>Parentheses work</div>
                    </if>
                    <if {a == b == c}>
                        <div>Chained equality</div>
                    </if>
                </main-comp>
            "#},
            expect![[r#"
                if                                                1:4-3:9
                    div                                           2:8-2:28
                if                                                4:4-6:9
                    div                                           5:8-5:35
                if                                                7:4-9:9
                    div                                           8:8-8:35
            "#]],
        );
    }

    #[test]
    fn test_parser_simple_if_condition() {
        check(
            indoc! {"
                <main-comp>
                    <if {x == y}>
                        <div>Equal</div>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                if                                                1:4-3:9
                    div                                           2:8-2:24
            "#]],
        );
    }

    #[test]
    fn test_parser_complex_nested_loops() {
        check(
            indoc! {"
                <main-comp {i: Array[{s: {t: Array[String]}}]}>
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
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-6:10
                    for                                           2:8-5:14
                        if                                        3:12-4:17
                for                                               7:4-12:10
                    for                                           8:8-11:14
                        for                                       9:12-10:18
            "#]],
        );
    }

    #[test]
    fn test_parser_if_with_for_nested() {
        check(
            indoc! {"
                <main-comp {data: Array[String]}>
	                <if {data}>
		                <for {d in data}>
		                </for>
	                </if>
                </main-comp>
            "},
            expect![[r#"
                if                                                1:1-4:6
                    for                                           2:2-3:8
            "#]],
        );
    }

    #[test]
    fn test_parser_simple_for_loop() {
        check(
            indoc! {"
                <main-comp {foo: Array[String]}>
                    <for {bar in foo}>
                        <div></div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-3:10
                    div                                           2:8-2:19
            "#]],
        );
    }

    #[test]
    fn test_parser_component_references() {
        check(
            indoc! {"
                <main-comp {p: String}>
                    <foo-comp></foo-comp>
                    <foo-comp></foo-comp>
                </main-comp>
            "},
            expect![[r#"
                component_reference                               1:4-1:25
                component_reference                               2:4-2:25
            "#]],
        );
    }

    #[test]
    fn test_parser_component_with_params() {
        check(
            indoc! {"
                <main-comp {data: {user: String}}>
                    <foo-comp {a: data}/>
                    <bar-comp {b: data.user}/>
                </main-comp>
            "},
            expect![[r#"
                component_reference                               1:4-1:25
                component_reference                               2:4-2:30
            "#]],
        );
    }

    #[test]
    fn test_parser_for_loop_with_text_expression() {
        check(
            indoc! {"
                <main-comp {foo: Array[String]}>
                    <for {v in foo}>
                        <div>{v}</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-3:10
                    div                                           2:8-2:22
                        text_expression                           2:13-2:16
            "#]],
        );
    }

    #[test]
    fn test_parser_script_tag_with_html_content() {
        check(
            indoc! {r#"
                <main-comp {foo: String}>
                    <script>
                        const x = "<div></div>";
                    </script>
                </main-comp>
            "#},
            expect![[r#"
                script                                            1:4-3:13
            "#]],
        );
    }

    #[test]
    fn test_parser_expression_attributes() {
        check(
            indoc! {r#"
                <main-comp {user: {url: String, theme: String}}>
                    <a href={user.url} class={user.theme}>Link</a>
                </main-comp>
            "#},
            expect![[r#"
                a                                                 1:4-1:50
            "#]],
        );
    }

    #[test]
    fn test_parser_slots_default() {
        check(
            indoc! {r#"
                <main-comp>
                    <slot-default/>
                    <button-comp>Custom Button</button-comp>
                </main-comp>
            "#},
            expect![[r#"
                slot-definition                                   1:4-1:19
                component_reference                               2:4-2:44
            "#]],
        );
    }

    // When a component is imported twice, the parser outputs an error.
    #[test]
    fn test_parser_component_imported_twice() {
        check(
            indoc! {r#"
                <import component="foo-comp" from="@/other">
                <import component="foo-comp" from="@/other">

                <main-comp>
                	<foo-comp></foo-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                1 | <import component="foo-comp" from="@/other">
                2 | <import component="foo-comp" from="@/other">
                  |                    ^^^^^^^^
            "#]],
        );
    }

    // When a component is defined twice, the parser outputs an error.
    #[test]
    fn test_parser_duplicate_component_definition() {
        check(
            indoc! {r#"
                <foo-comp>
                </foo-comp>

                <foo-comp>
                </foo-comp>

                <foo-comp>
                </foo-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                3 | 
                4 | <foo-comp>
                  |  ^^^^^^^^

                error: Component foo-comp is already defined
                6 | 
                7 | <foo-comp>
                  |  ^^^^^^^^
            "#]],
        );
    }

    // When a component is defined with the same name as an imported component, the parser
    // outputs an error.
    #[test]
    fn test_component_name_conflicts_with_import() {
        check(
            indoc! {r#"
                <import component="foo-comp" from="@/other">

                <foo-comp>
                </foo-comp>

                <bar-comp>
                	<foo-comp/>
                </bar-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                2 | 
                3 | <foo-comp>
                  |  ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_import_without_at_prefix_is_rejected() {
        check(
            indoc! {r#"
                <import component="foo-comp" from="other">

                <main-comp>
                	<foo-comp/>
                </main-comp>
            "#},
            expect![[r#"
                error: Import paths must start with '@/' where '@' indicates the root directory
                1 | <import component="foo-comp" from="other">
                  |                                    ^^^^^
            "#]],
        );
    }

    #[test]
    fn test_import_with_invalid_module_name() {
        check(
            indoc! {r#"
                <import component="foo-comp" from="@/../foo">

                <main-comp>
                	<foo-comp/>
                </main-comp>
            "#},
            expect![[r#"
                error: Module name cannot contain '..'
                1 | <import component="foo-comp" from="@/../foo">
                  |                                    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_import_with_invalid_characters_in_module_name() {
        check(
            indoc! {r#"
                <import component="bar-comp" from="@/foo/bar!baz">

                <main-comp>
                	<bar-comp/>
                </main-comp>
            "#},
            expect![[r#"
                error: Module name contains invalid character: '!'
                1 | <import component="bar-comp" from="@/foo/bar!baz">
                  |                                    ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_parser_svg_complex_structure() {
        check(
            indoc! {r#"
                <main-comp>
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
                </main-comp>
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
    fn test_parser_form_with_inputs() {
        check(
            indoc! {r#"
                <main-comp>
                    <form id="form">
                        <input type="text" required>
                        <button type="submit">Send</button>
                    </form>
                </main-comp>
            "#},
            expect![[r#"
                form                                              1:4-4:11
                    input                                         2:8-2:36
                    button                                        3:8-3:43
            "#]],
        );
    }

    // Test basic <if> tag with simple variable expression.
    #[test]
    fn test_parser_if_simple_variable() {
        check(
            indoc! {"
                <main-comp>
                    <if {is_visible}>
                        <div>This is visible</div>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                if                                                1:4-3:9
                    div                                           2:8-2:34
            "#]],
        );
    }

    // Test <if> tag with complex expression.
    #[test]
    fn test_parser_if_complex_expression() {
        check(
            indoc! {r#"
                <main-comp>
                    <if {user.name == "admin"}>
                        <div>Admin panel</div>
                        <button>Settings</button>
                    </if>
                </main-comp>
            "#},
            expect![[r#"
                if                                                1:4-4:9
                    div                                           2:8-2:30
                    button                                        3:8-3:33
            "#]],
        );
    }

    // Test basic <for> tag with simple loop generator expression.
    #[test]
    fn test_parser_for_simple_loop() {
        check(
            indoc! {"
                <main-comp>
                    <for {item in items}>
                        <div>Item content</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-3:10
                    div                                           2:8-2:31
            "#]],
        );
    }

    // Test <for> tag with complex array expression.
    #[test]
    fn test_parser_for_complex_array() {
        check(
            indoc! {"
                <main-comp>
                    <for {user in users.active}>
                        <div>User: {user.name}</div>
                        <p>Role: {user.role}</p>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-4:10
                    div                                           2:8-2:36
                        text_expression                           2:19-2:30
                    p                                             3:8-3:32
                        text_expression                           3:17-3:28
            "#]],
        );
    }

    // Component parameter can have a simple type annotation.
    #[test]
    fn test_parser_param_simple_type() {
        check(
            indoc! {"
                <main-comp {data: String}>
                    <div>{data}</div>
                </main-comp>
            "},
            expect![[r#"
                div                                               1:4-1:21
                    text_expression                               1:9-1:15
            "#]],
        );
    }

    // Component parameter can have an array type annotation.
    #[test]
    fn test_parser_param_array_type() {
        check(
            indoc! {"
                <main-comp {items: Array[String]}>
                    <for {item in items}>
                        <div>{item}</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-3:10
                    div                                           2:8-2:25
                        text_expression                           2:13-2:19
            "#]],
        );
    }

    // Component parameter can have an object type annotation.
    #[test]
    fn test_parser_param_object_type() {
        check(
            indoc! {"
                <main-comp {user: {name: String, age: Float}}>
                    <div>{user.name} is {user.age} years old</div>
                </main-comp>
            "},
            expect![[r#"
                div                                               1:4-1:50
                    text_expression                               1:9-1:20
                    text_expression                               1:24-1:34
            "#]],
        );
    }

    // Component parameter can have nested type annotations.
    #[test]
    fn test_parser_param_nested_types() {
        check(
            indoc! {"
                <main-comp {data: Array[{title: String, items: Array[String]}]}>
                    <for {section in data}>
                        <h1>{section.title}</h1>
                        <for {item in section.items}>
                            <div>{item}</div>
                        </for>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               1:4-6:10
                    h1                                            2:8-2:32
                        text_expression                           2:12-2:27
                    for                                           3:8-5:14
                        div                                       4:12-4:29
                            text_expression                       4:17-4:23
            "#]],
        );
    }

    // Test <import> tag without required attributes produces error.
    #[test]
    fn test_parser_import_missing_attributes() {
        check(
            indoc! {r#"
                <import>
                <main-comp>
                    <div>Content</div>
                </main-comp>
            "#},
            // TODO: Make this a single error message
            expect![[r#"
                error: <import> is missing required attribute from
                1 | <import>
                  |  ^^^^^^
            "#]],
        );
    }

    // Test <import> tag with unrecognized attribute produces error.
    #[test]
    fn test_parser_import_unrecognized_attribute() {
        check(
            indoc! {r#"
                <import component="button-cmp" from="@/components/button" extra="value" unknown>
                <main-comp>
                    <button-cmp/>
                </main-comp>
            "#},
            expect![[r#"
                error: Unrecognized attribute 'extra' on <import>
                1 | <import component="button-cmp" from="@/components/button" extra="value" unknown>
                  |                                                           ^^^^^^^^^^^^^

                error: Unrecognized attribute 'unknown' on <import>
                1 | <import component="button-cmp" from="@/components/button" extra="value" unknown>
                  |                                                                         ^^^^^^^
            "#]],
        );
    }
}
