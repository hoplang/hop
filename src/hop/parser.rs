use crate::common::RangeError;
use crate::dop::{self, DopTokenizer};
use crate::hop::ast::{
    ComponentDefinitionNode, DopExprAttribute, HopAST, HopNode, ImportNode, RenderNode,
    TopLevelHopNode,
};
use crate::hop::token_tree::{TokenTree, build_tree};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;
use std::collections::{HashMap, HashSet};

pub fn parse(module_name: String, tokenizer: Tokenizer, errors: &mut Vec<RangeError>) -> HopAST {
    let trees = build_tree(tokenizer, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut renders = Vec::new();

    // Track component definitions and imports as we go
    let mut defined_components = HashSet::new();
    let mut imported_components = HashMap::new();

    for tree in &trees {
        if let Some(toplevel_node) = construct_top_level_node(
            tree,
            errors,
            &module_name,
            &defined_components,
            &imported_components,
        ) {
            match toplevel_node {
                TopLevelHopNode::Import(import_data) => {
                    if imported_components.contains_key(&import_data.component_attr.value) {
                        errors.push(RangeError::component_is_already_defined(
                            &import_data.component_attr.value,
                            import_data.component_attr.value_range,
                        ));
                    } else {
                        imported_components.insert(
                            import_data.component_attr.value.clone(),
                            import_data.from_attr.value.clone(),
                        );
                    }
                    imports.push(import_data);
                }
                TopLevelHopNode::ComponentDefinition(component_data) => {
                    // Check if component is already defined or imported
                    if defined_components.contains(&component_data.name)
                        || imported_components.contains_key(&component_data.name)
                    {
                        errors.push(RangeError::component_is_already_defined(
                            &component_data.name,
                            component_data.opening_name_range,
                        ));
                    } else {
                        // Add to component definitions set
                        defined_components.insert(component_data.name.clone());
                    }
                    components.push(component_data);
                }
                TopLevelHopNode::Render(render_data) => renders.push(render_data),
            }
        }
    }

    HopAST::new(module_name, components, imports, renders)
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

fn construct_top_level_node(
    tree: &TokenTree,
    errors: &mut Vec<RangeError>,
    module_name: &str,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, String>,
) -> Option<TopLevelHopNode> {
    let t = &tree.opening_token;

    match t {
        Token::OpeningTag {
            value,
            attributes,
            expression,
            name_range,
            ..
        } => {
            match value.as_str() {
                "import" => {
                    let component_attr = t.find_attribute("component").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "component",
                            tree.opening_token.range(),
                        ));
                        None
                    });
                    let from_attr = t.find_attribute("from").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "from",
                            tree.opening_token.range(),
                        ));
                        None
                    });

                    match (component_attr, from_attr) {
                        (Some(component_attr), Some(from_attr)) => {
                            Some(TopLevelHopNode::Import(ImportNode {
                                component_attr,
                                from_attr,
                                range: tree.range(),
                            }))
                        }
                        _ => None,
                    }
                }
                "render" => {
                    let children: Vec<HopNode> = tree
                        .children
                        .iter()
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

                    let file_attr = t.find_attribute("file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "file",
                            tree.opening_token.range(),
                        ));
                        None
                    });

                    file_attr.map(|file_attr| {
                        TopLevelHopNode::Render(RenderNode {
                            file_attr,
                            range: tree.range(),
                            children,
                        })
                    })
                }
                name => {
                    // Handle as component definition
                    if !is_valid_component_name(name) {
                        errors.push(RangeError::invalid_component_name(
                            name,
                            tree.opening_token.range(),
                        ));
                        return None;
                    }

                    let children: Vec<_> = tree
                        .children
                        .iter()
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

                    let as_attr = t.find_attribute("as");
                    let entrypoint = t.find_attribute("entrypoint").is_some();
                    let params_as_attrs = expression
                        .as_ref()
                        .map(|(expr_string, range)| {
                            let mut tokenizer = match DopTokenizer::new(expr_string, range.start) {
                                Ok(tokenizer) => tokenizer,
                                Err(err) => {
                                    errors.push(err);
                                    return Vec::new();
                                }
                            };
                            match dop::parse_parameters(&mut tokenizer) {
                                Ok(params) => params,
                                Err(error) => {
                                    errors.push(error);
                                    Vec::new()
                                }
                            }
                        })
                        .unwrap_or_else(Vec::new);

                    let mut has_default_slot = false;
                    for child in &children {
                        for node in child.iter_depth_first() {
                            if let HopNode::SlotDefinition { range, .. } = node {
                                if has_default_slot {
                                    errors.push(RangeError::slot_already_defined(*range));
                                } else {
                                    has_default_slot = true;
                                }
                            }
                        }
                    }

                    Some(TopLevelHopNode::ComponentDefinition(
                        ComponentDefinitionNode {
                            name: name.to_string(),
                            opening_name_range: *name_range,
                            closing_name_range: tree.closing_token.as_ref().and_then(|tag| {
                                if let Token::ClosingTag { name_range, .. } = tag {
                                    Some(*name_range)
                                } else {
                                    None
                                }
                            }),
                            params: params_as_attrs,
                            as_attr,
                            attributes: attributes.clone(),
                            range: tree.range(),
                            children,
                            entrypoint,
                            has_slot: has_default_slot,
                        },
                    ))
                }
            }
        }
        _ => None,
    }
}

fn construct_node(
    tree: &TokenTree,
    errors: &mut Vec<RangeError>,
    module_name: &str,
    defined_components: &HashSet<String>,
    imported_components: &HashMap<String, String>,
) -> HopNode {
    let children: Vec<HopNode> = tree
        .children
        .iter()
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

    let t = &tree.opening_token;

    match t {
        Token::Doctype { range } => HopNode::Doctype {
            value: "".to_string(),
            range: *range,
        },
        Token::Text { value, range } => HopNode::Text {
            value: value.clone(),
            range: *range,
        },
        Token::Expression {
            value,
            expression_range,
            range,
        } => {
            // Expression tokens represent {expression} in text content
            let mut tokenizer = match DopTokenizer::new(value, expression_range.start) {
                Ok(tokenizer) => tokenizer,
                Err(err) => {
                    errors.push(err);
                    return HopNode::Error {
                        range: *range,
                        children: vec![],
                    };
                }
            };
            match dop::parse_expr(&mut tokenizer) {
                Ok(expr) => HopNode::TextExpression {
                    expression: expr,
                    range: *range,
                },
                Err(err) => {
                    errors.push(err);
                    HopNode::Error {
                        range: tree.range(),
                        children: vec![],
                    }
                }
            }
        }
        Token::OpeningTag {
            value,
            expression,
            attributes,
            name_range,
            ..
        } => {
            match value.as_str() {
                "if" => match &expression {
                    Some((expr_string, expr_range)) => {
                        let mut tokenizer = match DopTokenizer::new(expr_string, expr_range.start) {
                            Ok(tokenizer) => tokenizer,
                            Err(err) => {
                                errors.push(err);
                                return HopNode::Error {
                                    range: *expr_range,
                                    children: vec![],
                                };
                            }
                        };
                        match dop::parse_expr(&mut tokenizer) {
                            Ok(condition) => HopNode::If {
                                condition,
                                range: tree.range(),
                                children,
                            },
                            Err(err) => {
                                errors.push(err);
                                HopNode::Error {
                                    range: tree.range(),
                                    children,
                                }
                            }
                        }
                    }
                    None => {
                        errors.push(RangeError::new(
                            "Missing expression in <if> tag".to_string(),
                            tree.opening_token.range(),
                        ));
                        HopNode::Error {
                            range: tree.range(),
                            children,
                        }
                    }
                },
                "for" => match expression {
                    Some((expr_string, expr_range)) => {
                        let mut tokenizer = match DopTokenizer::new(expr_string, expr_range.start) {
                            Ok(tokenizer) => tokenizer,
                            Err(err) => {
                                errors.push(err);
                                return HopNode::Error {
                                    range: *expr_range,
                                    children: vec![],
                                };
                            }
                        };
                        match dop::parse_loop_header(&mut tokenizer) {
                            Ok((var_name, array_expr)) => HopNode::For {
                                var_name,
                                array_expr,
                                range: tree.range(),
                                children,
                            },
                            Err(error) => {
                                errors.push(error);
                                HopNode::Error {
                                    range: tree.range(),
                                    children,
                                }
                            }
                        }
                    }
                    None => {
                        errors.push(RangeError::new(
                            "Missing loop generator expression in <for> tag".to_string(),
                            tree.opening_token.range(),
                        ));
                        HopNode::Error {
                            range: tree.range(),
                            children,
                        }
                    }
                },
                "hop-x-exec" => {
                    let cmd_attr = t.find_attribute("cmd").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "cmd",
                            tree.opening_token.range(),
                        ));
                        None
                    });

                    match cmd_attr {
                        Some(cmd_attr) => HopNode::XExec {
                            cmd_attr,
                            range: tree.range(),
                            children,
                        },
                        None => HopNode::Error {
                            range: tree.range(),
                            children,
                        },
                    }
                }
                "hop-x-raw" => {
                    let has_trim = attributes.iter().any(|attr| attr.name == "trim");
                    HopNode::XRaw {
                        trim: has_trim,
                        range: tree.range(),
                        children,
                    }
                }
                "hop-x-load-json" => {
                    let file_attr = t.find_attribute("file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "file",
                            tree.opening_token.range(),
                        ));
                        None
                    });

                    let as_attr = t.find_attribute("as").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "as",
                            tree.opening_token.range(),
                        ));
                        None
                    });

                    match (file_attr, as_attr) {
                        (Some(file_attr), Some(as_attr)) => HopNode::XLoadJson {
                            file_attr,
                            as_attr,
                            range: tree.range(),
                            children,
                        },
                        _ => HopNode::Error {
                            range: tree.range(),
                            children,
                        },
                    }
                }
                "slot-default" => HopNode::SlotDefinition {
                    range: tree.range(),
                },
                tag_name if is_valid_component_name(tag_name) => {
                    // This is a component render (contains dash)
                    let params_attrs = match &expression {
                        Some((expr_string, range)) => {
                            let mut tokenizer = match DopTokenizer::new(expr_string, range.start) {
                                Ok(tokenizer) => tokenizer,
                                Err(err) => {
                                    errors.push(err);
                                    return HopNode::Error {
                                        range: *range,
                                        children: vec![],
                                    };
                                }
                            };
                            match dop::parse_arguments(&mut tokenizer) {
                                Ok(named_args) => named_args,
                                Err(err) => {
                                    errors.push(err);
                                    Vec::new()
                                }
                            }
                        }
                        None => Vec::new(),
                    };

                    let definition_location = if defined_components.contains(tag_name) {
                        Some(module_name.to_string())
                    } else {
                        imported_components.get(tag_name).cloned()
                    };

                    HopNode::ComponentReference {
                        component: tag_name.to_string(),
                        opening_name_range: *name_range,
                        closing_name_range: tree.closing_token.as_ref().and_then(|tag| {
                            if let Token::ClosingTag { name_range, .. } = tag {
                                Some(*name_range)
                            } else {
                                None
                            }
                        }),
                        definition_module: definition_location,
                        args: params_attrs,
                        attributes: attributes.clone(),
                        range: tree.range(),
                        children,
                    }
                }
                _ => {
                    let mut set_attributes = Vec::new();
                    for attr in attributes {
                        if attr.name.starts_with("set-") {
                            if let Some(expr_attr) = {
                                let mut tokenizer =
                                    match DopTokenizer::new(&attr.value, attr.range.start) {
                                        Ok(tokenizer) => tokenizer,
                                        Err(err) => {
                                            errors.push(err);
                                            continue;
                                        }
                                    };
                                match dop::parse_expr(&mut tokenizer) {
                                    Ok(expression) => Some(DopExprAttribute {
                                        name: attr.name.to_string(),
                                        expression,
                                        range: attr.range,
                                    }),
                                    Err(err) => {
                                        errors.push(err);
                                        None
                                    }
                                }
                            } {
                                set_attributes.push(expr_attr);
                            }
                        }
                    }

                    HopNode::NativeHTML {
                        tag_name: value.clone(),
                        opening_name_range: *name_range,
                        closing_name_range: tree.closing_token.as_ref().and_then(|tag| {
                            if let Token::ClosingTag { name_range, .. } = tag {
                                Some(*name_range)
                            } else {
                                None
                            }
                        }),
                        attributes: attributes.clone(),
                        range: tree.range(),
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
    use crate::tui::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    pub fn format_component_definition(d: &ComponentDefinitionNode) -> String {
        let mut lines = Vec::new();
        for child in &d.children {
            let s = format_tree(child, 0);
            if !s.is_empty() {
                lines.push(s);
            }
        }
        lines.join("\n") + "\n"
    }

    pub fn format_tree(root: &HopNode, depth: usize) -> String {
        let mut lines = Vec::new();

        fn format_node(node: &HopNode, depth: usize, lines: &mut Vec<String>) {
            let indent = "\t".repeat(depth);

            match node {
                HopNode::Doctype { .. } => {
                    lines.push(format!("{}doctype", indent));
                }
                HopNode::ComponentReference { children, .. } => {
                    lines.push(format!("{}render", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                HopNode::If { children, .. } => {
                    lines.push(format!("{}if", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                HopNode::For { children, .. } => {
                    lines.push(format!("{}for", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                HopNode::NativeHTML {
                    tag_name, children, ..
                } => {
                    lines.push(format!("{}{}", indent, tag_name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                HopNode::SlotDefinition { .. } => {
                    lines.push(format!("{}slot-default", indent));
                }
                HopNode::XExec { children, .. } => {
                    lines.push(format!("{}hop-x-exec", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                HopNode::XRaw { children, .. } => {
                    lines.push(format!("{}hop-x-raw", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                _ => {}
            }
        }

        format_node(root, depth, &mut lines);
        lines.join("\n")
    }

    fn check(input: &str, expected: Expect) {
        let mut errors = Vec::new();
        let module = parse("test".to_string(), Tokenizer::new(input), &mut errors);

        let actual = if !errors.is_empty() {
            let annotator = SourceAnnotator::new()
                .with_label("error")
                .with_underline('^')
                .with_lines_before(1);
            annotator.add_annotations(input, &errors)
        } else {
            for component in module.get_component_definition_nodes() {
                if component.name == "main-comp" {
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
                </main-comp>
            "},
            expect![[r#"
                error: Unclosed <div>
                1 | <main-comp>
                2 |     <div>
                  |     ^^^^^
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
                </main-comp>
            "},
            expect![[r#"
                error: <hr> should not be closed using a closing tag
                1 | <main-comp>
                2 |     <hr></hr>
                  |         ^^^^^
            "#]],
        );
    }

    // Import tags are treated as a void tag.
    #[test]
    fn test_parser_import_self_closing() {
        check(
            indoc! {r#"
                <import component="foo" from="bar"></import>
                <main-comp>
                </main-comp>
            "#},
            expect![[r#"
                error: <import> should not be closed using a closing tag
                1 | <import component="foo" from="bar"></import>
                  |                                    ^^^^^^^^^
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
                </main-comp>
            "},
            expect![[r#"
                error: Unmatched </div>
                1 | <main-comp>
                2 |     </div>
                  |     ^^^^^^
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
            "},
            expect![[r#"
                error: Invalid component name 'div'. Component names must contain a dash and not start or end with one
                1 | <div>
                  | ^^^^^
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
                error: Expected token 'in'
                1 | <main-comp>
                2 |     <for {foo}>
                  |              ^
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
            expect![[r#"
                error: Expected type name
                1 | <main-comp {data: array[}>
                  |                         ^
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
                div
                	for
                		div
                			h2
                			for
                				p
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
                doctype
                html
                	body
                		div
            "#]],
        );
    }

    #[test]
    fn test_parser_entrypoint_with_script_style() {
        check(
            indoc! {r#"
                <main-comp entrypoint>
                    <script>
                        console.log("test");
                    </script>
                    <style>
                        body { color: red; }
                    </style>
                </main-comp>
            "#},
            expect![[r#"
                script
                style
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
                h1
                p
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
                if
                	div
                if
                	div
                if
                	div
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
                if
                	div
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
                for
                	for
                		if
                for
                	for
                		for
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
                if
                	for
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
                for
                	div
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
                render
                render
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
                render
                render
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
                for
                	div
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
                script
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
                a
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
                slot-default
                render
            "#]],
        );
    }

    // When a component is imported twice, the typechecker outputs an error.
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
            "#},
            expect![[r#"
                error: Component foo-comp is already defined
                3 | 
                4 | <foo-comp>
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
                div
                	svg
                		g
                			path
                			path
                			path
                	ul
                		li
                			a
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
                form
                	input
                	button
            "#]],
        );
    }

    // Test basic <if> tag with simple variable expression.
    #[test]
    fn test_parser_if_simple_variable() {
        check(
            indoc! {"
                <main-comp>
                    <if {isVisible}>
                        <div>This is visible</div>
                    </if>
                </main-comp>
            "},
            expect![[r#"
                if
                	div
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
                if
                	div
                	button
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
                for
                	div
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
                for
                	div
                	p
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
                div
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
                for
                	div
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
                div
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
                for
                	h1
                	for
                		div
            "#]],
        );
    }
}
