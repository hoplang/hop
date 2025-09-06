use crate::common::ParseError;
use crate::dop::{self, DopTokenizer};
use crate::hop::ast::{ComponentDefinition, DopExprAttribute, HopAst, HopNode, Import, Render};
use crate::hop::token_tree::{TokenTree, build_tree};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;
use crate::range::RangedChars;
use std::collections::{BTreeMap, HashMap, HashSet};

use super::ast::PresentAttribute;

pub fn parse(module_name: String, tokenizer: Tokenizer, errors: &mut Vec<ParseError>) -> HopAst {
    let trees = build_tree(tokenizer, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut renders = Vec::new();

    let mut defined_components = HashSet::new();
    let mut imported_components = HashMap::new();

    for tree in trees {
        let children: Vec<HopNode> = tree
            .children
            .into_iter()
            .map(|child| {
                construct_node(
                    child,
                    errors,
                    &module_name,
                    &defined_components,
                    &imported_components,
                )
            })
            .collect();

        match tree.token {
            Token::Text { .. } => {}
            Token::ClosingTag { .. } => {}
            Token::Expression { .. } => {}
            Token::Comment { .. } => {}
            Token::Doctype { .. } => {}
            Token::OpeningTag {
                tag_name: (tag_name, tag_name_range),
                attributes,
                expression,
                range,
                ..
            } => match tag_name.as_str() {
                "import" => {
                    let mut component_attr = None;
                    let mut from_attr = None;

                    for (key, attr) in attributes {
                        match key.as_str() {
                            "component" => component_attr = Some(attr),
                            "from" => from_attr = Some(attr),
                            _ => {
                                // TODO: Check for unrecognized attributes
                            }
                        }
                    }

                    match (
                        component_attr.and_then(|attr| attr.value),
                        from_attr.and_then(|attr| attr.value),
                    ) {
                        (Some((cmp_attr, cmp_attr_range)), Some((from_attr, from_attr_range))) => {
                            if imported_components.contains_key(&cmp_attr) {
                                errors.push(ParseError::component_is_already_defined(
                                    &cmp_attr,
                                    cmp_attr_range,
                                ));
                            } else {
                                imported_components.insert(cmp_attr.clone(), from_attr.clone());
                            }
                            imports.push(Import {
                                component_attr: PresentAttribute {
                                    value: cmp_attr,
                                    range: cmp_attr_range,
                                },
                                from_attr: PresentAttribute {
                                    value: from_attr,
                                    range: from_attr_range,
                                },
                                range: tree.range,
                            });
                        }
                        (component, from) => {
                            if component.is_none() {
                                errors.push(ParseError::missing_required_attribute(
                                    &tag_name,
                                    "component",
                                    range,
                                ));
                            }
                            if from.is_none() {
                                errors.push(ParseError::missing_required_attribute(
                                    &tag_name, "from", range,
                                ));
                            }
                        }
                    }
                }
                "render" => {
                    let mut file_attr = None;

                    for (key, attr) in attributes {
                        match key.as_str() {
                            "file" => file_attr = Some(attr),
                            _ => {
                                // TODO: Check for unrecognized attributes
                            }
                        }
                    }

                    match file_attr.and_then(|attr| attr.value) {
                        Some((file_attr, file_attr_range)) => {
                            renders.push(Render {
                                file_attr: PresentAttribute {
                                    value: file_attr,
                                    range: file_attr_range,
                                },
                                range: tree.range,
                                children,
                            });
                        }
                        None => errors.push(ParseError::missing_required_attribute(
                            &tag_name, "file", range,
                        )),
                    }
                }
                // Treat as ComponentDefinition
                name => {
                    if !is_valid_component_name(name) {
                        errors.push(ParseError::invalid_component_name(name, tag_name_range));
                    } else {
                        let params = expression.as_ref().and_then(|(expr_string, expr_range)| {
                            let mut tokenizer = DopTokenizer::from(RangedChars::with_position(
                                expr_string,
                                expr_range.start(),
                            ))
                            .peekable();
                            match dop::parse_parameters(&mut tokenizer) {
                                Ok(params) => Some((params, *expr_range)),
                                Err(dop::parser::ParseError::UnexpectedEof) => {
                                    errors.push(ParseError::unexpected_end_of_expression(
                                        *expr_range,
                                    ));
                                    None
                                }
                                Err(dop::parser::ParseError::Ranged { message, range }) => {
                                    errors.push(ParseError::new(message, range));
                                    None
                                }
                            }
                        });

                        // TODO: Here we iterate over the whole subtree to check
                        // if it contains a slot. There should be a better way
                        // to do this.
                        let mut has_slot = false;
                        for child in &children {
                            for node in child.iter_depth_first() {
                                if let HopNode::SlotDefinition { range, .. } = node {
                                    if has_slot {
                                        errors.push(ParseError::slot_is_already_defined(*range));
                                    }
                                    has_slot = true;
                                }
                            }
                        }

                        if defined_components.contains(name)
                            || imported_components.contains_key(name)
                        {
                            errors.push(ParseError::component_is_already_defined(
                                name,
                                tag_name_range,
                            ));
                        } else {
                            defined_components.insert(name.to_string());
                        }

                        let mut is_entrypoint = false;
                        let mut as_attr = None;
                        let mut unhandled_attributes = BTreeMap::new();

                        for (key, attr) in attributes.into_iter() {
                            match key.as_str() {
                                "as" => {
                                    as_attr = attr.value;
                                }
                                "entrypoint" => {
                                    is_entrypoint = true;
                                }
                                _ => {
                                    // Here we keep the unhandled attributes
                                    // since they should be rendered in the
                                    // resulting HTML.
                                    unhandled_attributes.insert(key, attr);
                                }
                            }
                        }

                        components.push(ComponentDefinition {
                            tag_name: name.to_string(),
                            opening_tag_name_range: tag_name_range,
                            closing_tag_name_range: tree.closing_tag_name_range,
                            params,
                            is_entrypoint,
                            as_attr: as_attr.map(|(v, r)| PresentAttribute { value: v, range: r }),
                            attributes: unhandled_attributes,
                            range: tree.range,
                            children,
                            has_slot,
                        });
                    }
                }
            },
        }
    }

    HopAst::new(module_name, components, imports, renders)
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
    errors: &mut Vec<ParseError>,
    module_name: &str,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, String>,
) -> HopNode {
    let children: Vec<HopNode> = tree
        .children
        .into_iter()
        .map(|child| {
            construct_node(
                child,
                errors,
                module_name,
                defined_components,
                imported_components,
            )
        })
        .collect();

    let t = tree.token;

    match t {
        Token::Doctype { range } => HopNode::Doctype {
            value: "".to_string(),
            range,
        },
        Token::Text { value, .. } => HopNode::Text {
            value,
            range: tree.range,
        },
        Token::Expression {
            expression: (expression, expression_range),
            ..
        } => {
            let mut tokenizer = DopTokenizer::from(RangedChars::with_position(
                &expression,
                expression_range.start(),
            ))
            .peekable();
            match dop::parse_expr(&mut tokenizer) {
                Ok(expression) => HopNode::TextExpression {
                    expression,
                    range: tree.range,
                },
                Err(dop::parser::ParseError::UnexpectedEof) => {
                    errors.push(ParseError::unexpected_end_of_expression(expression_range));
                    HopNode::Error {
                        range: tree.range,
                        children: vec![],
                    }
                }
                Err(dop::parser::ParseError::Ranged { message, range }) => {
                    errors.push(ParseError::new(message, range));
                    HopNode::Error {
                        range: tree.range,
                        children: vec![],
                    }
                }
            }
        }
        Token::OpeningTag {
            tag_name: (tag_name, tag_name_range),
            expression,
            attributes,
            range: opening_tag_range,
            ..
        } => {
            match tag_name.as_str() {
                "if" => match expression {
                    // TODO: Check for unrecognized attributes
                    Some((expr_string, expr_range)) => {
                        let mut tokenizer = DopTokenizer::from(RangedChars::with_position(
                            &expr_string,
                            expr_range.start(),
                        ))
                        .peekable();
                        match dop::parse_expr(&mut tokenizer) {
                            Ok(condition) => HopNode::If {
                                condition,
                                range: tree.range,
                                children,
                            },
                            Err(dop::parser::ParseError::UnexpectedEof) => {
                                errors.push(ParseError::unexpected_end_of_expression(expr_range));
                                HopNode::Error {
                                    range: tree.range,
                                    children,
                                }
                            }
                            Err(dop::parser::ParseError::Ranged { message, range }) => {
                                errors.push(ParseError::new(message, range));
                                HopNode::Error {
                                    range: tree.range,
                                    children,
                                }
                            }
                        }
                    }
                    None => {
                        errors.push(ParseError::new(
                            "Missing expression in <if> tag".to_string(),
                            opening_tag_range,
                        ));
                        HopNode::Error {
                            range: tree.range,
                            children,
                        }
                    }
                },
                "for" => match expression {
                    // TODO: Check for unrecognized attributes
                    Some((expr_string, expr_range)) => {
                        let mut tokenizer = DopTokenizer::from(RangedChars::with_position(
                            &expr_string,
                            expr_range.start(),
                        ))
                        .peekable();
                        match dop::parse_loop_header(&mut tokenizer) {
                            Ok((var_name, array_expr)) => HopNode::For {
                                var_name,
                                array_expr,
                                range: tree.range,
                                children,
                            },
                            Err(dop::parser::ParseError::UnexpectedEof) => {
                                errors.push(ParseError::unexpected_end_of_expression(expr_range));
                                HopNode::Error {
                                    range: tree.range,
                                    children,
                                }
                            }
                            Err(dop::parser::ParseError::Ranged { message, range }) => {
                                errors.push(ParseError::new(message, range));
                                HopNode::Error {
                                    range: tree.range,
                                    children,
                                }
                            }
                        }
                    }
                    None => {
                        errors.push(ParseError::new(
                            "Missing loop generator expression in <for> tag".to_string(),
                            opening_tag_range,
                        ));
                        HopNode::Error {
                            range: tree.range,
                            children,
                        }
                    }
                },
                "slot-default" => HopNode::SlotDefinition { range: tree.range },
                tag_name if tag_name.starts_with("hop-") => match tag_name {
                    "hop-x-exec" => {
                        let mut cmd_attr = None;

                        for (key, attr) in attributes {
                            match key.as_str() {
                                "cmd" => cmd_attr = Some(attr),
                                _ => {
                                    // TODO: Check for unrecognized attributes
                                }
                            }
                        }

                        match cmd_attr.and_then(|attr| attr.value) {
                            Some((cmd_attr, cmd_attr_range)) => HopNode::XExec {
                                cmd_attr: PresentAttribute {
                                    value: cmd_attr,
                                    range: cmd_attr_range,
                                },
                                range: tree.range,
                                children,
                            },
                            None => {
                                errors.push(ParseError::missing_required_attribute(
                                    tag_name,
                                    "cmd",
                                    opening_tag_range,
                                ));
                                HopNode::Error {
                                    range: tree.range,
                                    children,
                                }
                            }
                        }
                    }
                    "hop-x-raw" => HopNode::XRaw {
                        trim: attributes.contains_key("trim"),
                        range: tree.range,
                        children,
                    },
                    _ => {
                        errors.push(ParseError::unrecognized_hop_tag(tag_name, tag_name_range));
                        HopNode::Error {
                            range: tree.range,
                            children: vec![],
                        }
                    }
                },
                tag_name if tag_name.contains('-') => {
                    if !is_valid_component_name(tag_name) {
                        errors.push(ParseError::invalid_component_name(tag_name, tag_name_range));
                    }
                    let args = match &expression {
                        Some((expr_string, expr_range)) => {
                            let mut tokenizer = DopTokenizer::from(RangedChars::with_position(
                                expr_string,
                                expr_range.start(),
                            ))
                            .peekable();
                            match dop::parse_arguments(&mut tokenizer) {
                                Ok(named_args) => Some((named_args, *expr_range)),
                                Err(dop::parser::ParseError::UnexpectedEof) => {
                                    errors.push(ParseError::unexpected_end_of_expression(
                                        *expr_range,
                                    ));
                                    return HopNode::Error {
                                        range: tree.range,
                                        children: vec![],
                                    };
                                }
                                Err(dop::parser::ParseError::Ranged { message, range }) => {
                                    errors.push(ParseError::new(message, range));
                                    return HopNode::Error {
                                        range: tree.range,
                                        children: vec![],
                                    };
                                }
                            }
                        }
                        None => None,
                    };

                    let definition_module = if defined_components.contains(tag_name) {
                        Some(module_name.to_string())
                    } else {
                        imported_components.get(tag_name).cloned()
                    };

                    HopNode::ComponentReference {
                        component: tag_name.to_string(),
                        opening_name_range: tag_name_range,
                        closing_name_range: tree.closing_tag_name_range,
                        definition_module,
                        args,
                        attributes,
                        range: tree.range,
                        children,
                    }
                }
                // Treat as HTML
                _ => {
                    let mut set_attributes = Vec::new();
                    for (name, attr) in &attributes {
                        if name.starts_with("set-") {
                            let (attr_val, attr_val_range) = match &attr.value {
                                None => {
                                    errors.push(ParseError::missing_attribute_value(attr.range));
                                    continue;
                                }
                                Some(val) => val,
                            };
                            let mut tokenizer = DopTokenizer::from(RangedChars::with_position(
                                attr_val,
                                attr_val_range.start(),
                            ))
                            .peekable();
                            match dop::parse_expr(&mut tokenizer) {
                                Ok(expression) => set_attributes.push(DopExprAttribute {
                                    name: name.to_string(),
                                    expression,
                                    range: attr.range,
                                }),
                                Err(dop::parser::ParseError::UnexpectedEof) => {
                                    errors.push(ParseError::unexpected_end_of_expression(
                                        *attr_val_range,
                                    ));
                                }
                                Err(dop::parser::ParseError::Ranged { message, range }) => {
                                    errors.push(ParseError::new(message, range));
                                }
                            };
                        }
                    }

                    HopNode::Html {
                        tag_name,
                        opening_name_range: tag_name_range,
                        closing_name_range: tree.closing_tag_name_range,
                        attributes: attributes.clone(),
                        range: tree.range,
                        children,
                        set_attributes,
                    }
                }
            }
        }
        _ => {
            panic!("Unexpected token type in construct_node");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::range::{Ranged, SourceAnnotator};
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn node_name(node: &HopNode) -> &str {
        match node {
            HopNode::Doctype { .. } => "doctype",
            HopNode::ComponentReference { .. } => "component_reference",
            HopNode::If { .. } => "if",
            HopNode::For { .. } => "for",
            HopNode::Html { tag_name, .. } => tag_name,
            HopNode::SlotDefinition { .. } => "slot-definition",
            HopNode::XExec { .. } => "hop-x-exec",
            HopNode::XRaw { .. } => "hop-x-raw",
            HopNode::Text { .. } => "text",
            HopNode::TextExpression { .. } => "text_expression",
            HopNode::Error { .. } => "error",
        }
    }

    fn write_node(node: &HopNode, depth: usize, lines: &mut Vec<String>) {
        if matches!(node, HopNode::Text { .. }) {
            return;
        }
        let left = format!("{}{}", "    ".repeat(depth).as_str(), node_name(node));
        let right = format!("{}", node.range());
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
        let mut errors = Vec::new();
        let module = parse("test".to_string(), Tokenizer::new(input), &mut errors);

        let actual = if !errors.is_empty() {
            SourceAnnotator::new()
                .with_label("error")
                .with_lines_before(1)
                .annotate(None, input, errors)
        } else {
            for component in module.get_component_definitions() {
                if component.tag_name == "main-comp" {
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
                  |     ^^^

                error: Unclosed <div>
                1 | <main-comp>
                2 |     <div>
                  |     ^^^^^

                error: Unclosed <foo-comp>
                5 | 
                6 | <foo-comp>
                  | ^^^^^^^^^^
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
                <import component="foo" from="bar"></import>
            "#},
            expect![[r#"
                error: <import> should not be closed using a closing tag
                1 | <import component="foo" from="bar"></import>
                  |                                    ^^^^^^^^^
            "#]],
        );
    }

    // Void tags are allowed to be self-closing.
    #[test]
    fn test_parser_void_tag_may_be_self_closing() {
        check(
            indoc! {r#"
                <import component="foo" from="bar">
                <main-comp>
                    <hr/>
                    <br/>
                    <input/>
                </main-comp>
                <foo-comp/>
            "#},
            expect![[r#"
                hr                                                3:5-3:10
                br                                                4:5-4:10
                input                                             5:5-5:13
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
    fn test_parser_invalid_variable_name() {
        check(
            indoc! {"
                <main-comp {Data: string}>
                    <div></div>
                </main-comp>
            "},
            expect![[r#"
                error: Invalid variable name 'Data'. Variable names must match [a-z][a-z0-9_]*
                1 | <main-comp {Data: string}>
                  |             ^^^^
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
                <main-comp {data: array[}>
                    <div>{data}</div>
                </main-comp>
            "},
            // TODO: Improve error message
            expect![[r#"
                error: Unexpected end of expression
                1 | <main-comp {data: array[}>
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
                <main-comp {sections: array[{title: string, items: array[string]}]}>
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
                div                                               2:5-11:11
                    for                                           3:9-10:15
                        div                                       4:13-9:19
                            h2                                    5:17-5:41
                                text_expression                   5:21-5:36
                            for                                   6:17-8:23
                                p                                 7:21-7:34
                                    text_expression               7:24-7:30
            "#]],
        );
    }

    #[test]
    fn test_parser_doctype_html_structure() {
        check(
            indoc! {"
                <main-comp {foo: string}>
                    <!DOCTYPE html>
                    <html>
                        <body>
                            <div>hello world</div>
                        </body>
                    </html>
                </main-comp>
            "},
            expect![[r#"
                doctype                                           2:5-2:20
                html                                              3:5-7:12
                    body                                          4:9-6:16
                        div                                       5:13-5:35
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
                script                                            2:5-6:14
                style                                             7:5-9:13
            "#]],
        );
    }

    #[test]
    fn test_parser_entrypoint_with_data_param() {
        check(
            indoc! {"
                <main-comp entrypoint {data: {message: string}}>
                    <h1>Hello World</h1>
                    <p>{data.message}</p>
                </main-comp>
            "},
            expect![[r#"
                h1                                                2:5-2:25
                p                                                 3:5-3:26
                    text_expression                               3:8-3:22
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
                if                                                2:5-4:10
                    div                                           3:9-3:29
                if                                                5:5-7:10
                    div                                           6:9-6:36
                if                                                8:5-10:10
                    div                                           9:9-9:36
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
                if                                                2:5-4:10
                    div                                           3:9-3:25
            "#]],
        );
    }

    #[test]
    fn test_parser_complex_nested_loops() {
        check(
            indoc! {"
                <main-comp {i: array[{s: {t: array[string]}}]}>
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
                for                                               2:5-7:11
                    for                                           3:9-6:15
                        if                                        4:13-5:18
                for                                               8:5-13:11
                    for                                           9:9-12:15
                        for                                       10:13-11:19
            "#]],
        );
    }

    #[test]
    fn test_parser_if_with_for_nested() {
        check(
            indoc! {"
                <main-comp {data: array[string]}>
	                <if {data}>
		                <for {d in data}>
		                </for>
	                </if>
                </main-comp>
            "},
            expect![[r#"
                if                                                2:2-5:7
                    for                                           3:3-4:9
            "#]],
        );
    }

    #[test]
    fn test_parser_simple_for_loop() {
        check(
            indoc! {"
                <main-comp {foo: array[string]}>
                    <for {bar in foo}>
                        <div></div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               2:5-4:11
                    div                                           3:9-3:20
            "#]],
        );
    }

    #[test]
    fn test_parser_component_references() {
        check(
            indoc! {"
                <main-comp {p: string}>
                    <foo-comp></foo-comp>
                    <foo-comp></foo-comp>
                </main-comp>
            "},
            expect![[r#"
                component_reference                               2:5-2:26
                component_reference                               3:5-3:26
            "#]],
        );
    }

    #[test]
    fn test_parser_component_with_params() {
        check(
            indoc! {"
                <main-comp {data: {user: string}}>
                    <foo-comp {a: data}/>
                    <bar-comp {b: data.user}/>
                </main-comp>
            "},
            expect![[r#"
                component_reference                               2:5-2:26
                component_reference                               3:5-3:31
            "#]],
        );
    }

    #[test]
    fn test_parser_for_loop_with_text_expression() {
        check(
            indoc! {"
                <main-comp {foo: array[string]}>
                    <for {v in foo}>
                        <div>{v}</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               2:5-4:11
                    div                                           3:9-3:23
                        text_expression                           3:14-3:17
            "#]],
        );
    }

    #[test]
    fn test_parser_script_tag_with_html_content() {
        check(
            indoc! {r#"
                <main-comp {foo: string}>
                    <script>
                        const x = "<div></div>";
                    </script>
                </main-comp>
            "#},
            expect![[r#"
                script                                            2:5-4:14
            "#]],
        );
    }

    #[test]
    fn test_parser_set_attributes() {
        check(
            indoc! {r#"
                <main-comp {user: {url: string, theme: string}}>
                    <a set-href="user.url" set-class="user.theme">Link</a>
                </main-comp>
            "#},
            expect![[r#"
                a                                                 2:5-2:59
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
                slot-definition                                   2:5-2:20
                component_reference                               3:5-3:45
            "#]],
        );
    }

    // When a component is imported twice, the parser outputs an error.
    #[test]
    fn test_parser_component_imported_twice() {
        check(
            indoc! {r#"
                <import component="foo-comp" from="other">
                <import component="foo-comp" from="other">

                <main-comp>
                	<foo-comp></foo-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                1 | <import component="foo-comp" from="other">
                2 | <import component="foo-comp" from="other">
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
                <import component="foo-comp" from="other">

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
                div                                               2:5-13:11
                    svg                                           3:9-9:15
                        g                                         4:13-8:17
                            path                                  5:17-5:66
                            path                                  6:17-6:64
                            path                                  7:17-7:74
                    ul                                            10:9-12:14
                        li                                        11:13-11:42
                            a                                     11:17-11:37
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
                form                                              2:5-5:12
                    input                                         3:9-3:37
                    button                                        4:9-4:44
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
                if                                                2:5-4:10
                    div                                           3:9-3:35
            "#]],
        );
    }

    // Test <if> tag with complex expression.
    #[test]
    fn test_parser_if_complex_expression() {
        check(
            indoc! {r#"
                <main-comp>
                    <if {user.name == 'admin'}>
                        <div>Admin panel</div>
                        <button>Settings</button>
                    </if>
                </main-comp>
            "#},
            expect![[r#"
                if                                                2:5-5:10
                    div                                           3:9-3:31
                    button                                        4:9-4:34
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
                for                                               2:5-4:11
                    div                                           3:9-3:32
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
                for                                               2:5-5:11
                    div                                           3:9-3:37
                        text_expression                           3:20-3:31
                    p                                             4:9-4:33
                        text_expression                           4:18-4:29
            "#]],
        );
    }

    // Component parameter can have a simple type annotation.
    #[test]
    fn test_parser_param_simple_type() {
        check(
            indoc! {"
                <main-comp {data: string}>
                    <div>{data}</div>
                </main-comp>
            "},
            expect![[r#"
                div                                               2:5-2:22
                    text_expression                               2:10-2:16
            "#]],
        );
    }

    // Component parameter can have an array type annotation.
    #[test]
    fn test_parser_param_array_type() {
        check(
            indoc! {"
                <main-comp {items: array[string]}>
                    <for {item in items}>
                        <div>{item}</div>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               2:5-4:11
                    div                                           3:9-3:26
                        text_expression                           3:14-3:20
            "#]],
        );
    }

    // Component parameter can have an object type annotation.
    #[test]
    fn test_parser_param_object_type() {
        check(
            indoc! {"
                <main-comp {user: {name: string, age: number}}>
                    <div>{user.name} is {user.age} years old</div>
                </main-comp>
            "},
            expect![[r#"
                div                                               2:5-2:51
                    text_expression                               2:10-2:21
                    text_expression                               2:25-2:35
            "#]],
        );
    }

    // Component parameter can have nested type annotations.
    #[test]
    fn test_parser_param_nested_types() {
        check(
            indoc! {"
                <main-comp {data: array[{title: string, items: array[string]}]}>
                    <for {section in data}>
                        <h1>{section.title}</h1>
                        <for {item in section.items}>
                            <div>{item}</div>
                        </for>
                    </for>
                </main-comp>
            "},
            expect![[r#"
                for                                               2:5-7:11
                    h1                                            3:9-3:33
                        text_expression                           3:13-3:28
                    for                                           4:9-6:15
                        div                                       5:13-5:30
                            text_expression                       5:18-5:24
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
                error: <import> is missing required attribute component
                1 | <import>
                  | ^^^^^^^^

                error: <import> is missing required attribute from
                1 | <import>
                  | ^^^^^^^^
            "#]],
        );
    }

    // Test <render> tag without required file attribute produces error.
    #[test]
    fn test_parser_render_missing_file_attribute() {
        check(
            indoc! {r#"
                <render>
                    <div>Content</div>
                </render>
            "#},
            expect![[r#"
                error: <render> is missing required attribute file
                1 | <render>
                  | ^^^^^^^^
            "#]],
        );
    }
}
