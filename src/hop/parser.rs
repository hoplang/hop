use crate::common::{Range, RangeError, is_void_element};
use crate::dop::{self, DopTokenizer};
use crate::hop::ast::{ComponentDefinitionNode, DopExprAttribute, HopNode, ImportNode, RenderNode};
use crate::hop::tokenizer::Token;
use crate::hop::tokenizer::Tokenizer;
use std::collections::HashSet;

use super::ast::TopLevelHopNode;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub components: Vec<ComponentDefinitionNode>,
    pub imports: Vec<ImportNode>,
    pub renders: Vec<RenderNode>,
}

#[derive(Debug, Clone)]
struct TokenTree {
    token: Token,
    children: Vec<TokenTree>,
    opening_tag_range: Range,
    closing_tag_name_range: Option<Range>,
}

impl TokenTree {
    fn new(token: Token, range: Range) -> Self {
        TokenTree {
            token,
            children: Vec::new(),
            opening_tag_range: range,
            closing_tag_name_range: None,
        }
    }

    fn append_node(&mut self, token: Token, range: Range) {
        self.children.push(TokenTree::new(token, range));
    }

    fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    fn set_closing_tag_name_range(&mut self, range: Range) {
        self.closing_tag_name_range = Some(range);
    }
}

pub fn parse(module_name: String, tokenizer: Tokenizer, errors: &mut Vec<RangeError>) -> Module {
    let tree = build_tree(tokenizer, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut renders = Vec::new();

    for child in &tree.children {
        if let Some(toplevel_node) = construct_top_level_node(child, errors) {
            match toplevel_node {
                TopLevelHopNode::Import(import_data) => imports.push(import_data),
                TopLevelHopNode::ComponentDefinition(component_data) => {
                    components.push(component_data)
                }
                TopLevelHopNode::Render(render_data) => renders.push(render_data),
            }
        }
    }

    Module {
        name: module_name,
        components,
        imports,
        renders,
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

fn build_tree(tokenizer: Tokenizer, errors: &mut Vec<RangeError>) -> TokenTree {
    struct StackElement {
        tree: TokenTree,
        tag_name: String,
    }

    let mut stack: Vec<StackElement> = Vec::new();

    let root_token = Token::StartTag {
        self_closing: false,
        value: "root".to_string(),
        attributes: Vec::new(),
        name_range: Range::default(),
        expression: None,
    };
    stack.push(StackElement {
        tree: TokenTree::new(root_token, Range::default()),
        tag_name: "root".to_string(),
    });

    for t in tokenizer {
        match t {
            Err(err) => errors.push(err),
            Ok((token, range)) => {
                match token {
                    Token::Comment => {
                        // skip comments
                        continue;
                    }
                    Token::Doctype | Token::Text { .. } | Token::Expression { .. } => {
                        stack.last_mut().unwrap().tree.append_node(token, range);
                    }
                    Token::StartTag {
                        ref value,
                        self_closing,
                        ..
                    } => {
                        if is_void_element(value) || self_closing {
                            stack.last_mut().unwrap().tree.append_node(token, range);
                        } else {
                            stack.push(StackElement {
                                tree: TokenTree::new(token.clone(), range),
                                tag_name: value.clone(),
                            });
                        }
                    }
                    Token::EndTag {
                        ref value,
                        name_range,
                        ..
                    } => {
                        if is_void_element(value) {
                            errors.push(RangeError::closed_void_tag(value, range));
                        } else if !stack.iter().any(|el| el.tag_name == *value) {
                            errors.push(RangeError::unmatched_closing_tag(value, range));
                        } else {
                            while stack.last().unwrap().tag_name != *value {
                                let unclosed = stack.pop().unwrap();
                                errors.push(RangeError::unclosed_tag(
                                    &unclosed.tag_name,
                                    unclosed.tree.opening_tag_range,
                                ));
                                stack.last_mut().unwrap().tree.append_tree(unclosed.tree);
                            }
                            let mut completed = stack.pop().unwrap();
                            completed.tree.set_closing_tag_name_range(name_range);
                            stack.last_mut().unwrap().tree.append_tree(completed.tree);
                        }
                    }
                    Token::Eof => {
                        break;
                    }
                }
            }
        }
    }

    while stack.len() > 1 {
        let unclosed = stack.pop().unwrap();
        errors.push(RangeError::unclosed_tag(
            &unclosed.tag_name,
            unclosed.tree.opening_tag_range,
        ));
    }

    stack.pop().unwrap().tree
}

fn construct_top_level_node(
    tree: &TokenTree,
    errors: &mut Vec<RangeError>,
) -> Option<TopLevelHopNode> {
    let t = &tree.token;

    match t {
        Token::StartTag {
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
                            tree.opening_tag_range,
                        ));
                        None
                    });
                    let from_attr = t.find_attribute("from").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "from",
                            tree.opening_tag_range,
                        ));
                        None
                    });

                    match (component_attr, from_attr) {
                        (Some(component_attr), Some(from_attr)) => {
                            Some(TopLevelHopNode::Import(ImportNode {
                                component_attr,
                                from_attr,
                                range: tree.opening_tag_range,
                            }))
                        }
                        _ => None,
                    }
                }
                "render" => {
                    let children: Vec<HopNode> = tree
                        .children
                        .iter()
                        .map(|child| construct_node(child, errors))
                        .collect();

                    let file_attr = t.find_attribute("file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "file",
                            tree.opening_tag_range,
                        ));
                        None
                    });

                    file_attr.map(|file_attr| {
                        TopLevelHopNode::Render(RenderNode {
                            file_attr,
                            range: tree.opening_tag_range,
                            children,
                        })
                    })
                }
                name => {
                    // Handle as component definition
                    if !is_valid_component_name(name) {
                        errors.push(RangeError::invalid_component_name(
                            name,
                            tree.opening_tag_range,
                        ));
                        return None;
                    }

                    // Separate preview content from main children
                    let mut main_children = Vec::new();
                    let mut preview_children = None;

                    for child in &tree.children {
                        if let Token::StartTag { value, .. } = &child.token {
                            if value == "hop-x-preview" {
                                preview_children = Some(
                                    child
                                        .children
                                        .iter()
                                        .map(|c| construct_node(c, errors))
                                        .collect(),
                                );
                                continue; // Don't add to main children
                            }
                        }
                        main_children.push(construct_node(child, errors));
                    }

                    let children = main_children;

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

                    let mut slots = HashSet::new();
                    for child in &children {
                        for node in child.iter_depth_first() {
                            if let HopNode::SlotDefinition { name, range, .. } = node {
                                if slots.contains(name) {
                                    errors.push(RangeError::slot_already_defined(name, *range));
                                } else {
                                    // Check if trying to mix default slot with other slots
                                    if (name == "default" && !slots.is_empty())
                                        || (slots.contains("default") && name != "default")
                                    {
                                        errors.push(RangeError::default_slot_with_other_slots(
                                            *range,
                                        ));
                                    } else {
                                        slots.insert(name.clone());
                                    }
                                }
                            }
                        }
                    }

                    Some(TopLevelHopNode::ComponentDefinition(
                        ComponentDefinitionNode {
                            name: name.to_string(),
                            opening_name_range: *name_range,
                            closing_name_range: tree.closing_tag_name_range,
                            params: params_as_attrs,
                            as_attr,
                            attributes: attributes.clone(),
                            range: tree.opening_tag_range,
                            children,
                            preview: preview_children,
                            entrypoint,
                            slots: slots.into_iter().collect(),
                        },
                    ))
                }
            }
        }
        _ => None,
    }
}

fn construct_node(tree: &TokenTree, errors: &mut Vec<RangeError>) -> HopNode {
    let children: Vec<HopNode> = tree
        .children
        .iter()
        .map(|child| construct_node(child, errors))
        .collect();

    let t = &tree.token;

    match t {
        Token::Doctype => HopNode::Doctype {
            value: "".to_string(),
            range: tree.opening_tag_range,
        },
        Token::Text { value } => HopNode::Text {
            value: value.clone(),
            range: tree.opening_tag_range,
        },
        Token::Expression { value, range } => {
            // Expression tokens represent {expression} in text content
            let mut tokenizer = match DopTokenizer::new(value, range.start) {
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
                        range: tree.opening_tag_range,
                        children: vec![],
                    }
                }
            }
        }
        Token::StartTag {
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
                                range: *expr_range,
                                children,
                            },
                            Err(err) => {
                                errors.push(err);
                                HopNode::Error {
                                    range: tree.opening_tag_range,
                                    children,
                                }
                            }
                        }
                    }
                    None => {
                        errors.push(RangeError::new(
                            "Missing expression in <if> tag".to_string(),
                            tree.opening_tag_range,
                        ));
                        HopNode::Error {
                            range: tree.opening_tag_range,
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
                                range: tree.opening_tag_range,
                                children,
                            },
                            Err(error) => {
                                errors.push(error);
                                HopNode::Error {
                                    range: tree.opening_tag_range,
                                    children,
                                }
                            }
                        }
                    }
                    None => {
                        errors.push(RangeError::new(
                            "Missing loop generator expression in <for> tag".to_string(),
                            tree.opening_tag_range,
                        ));
                        HopNode::Error {
                            range: tree.opening_tag_range,
                            children,
                        }
                    }
                },
                "hop-x-exec" => {
                    let cmd_attr = t.find_attribute("cmd").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "cmd",
                            tree.opening_tag_range,
                        ));
                        None
                    });

                    match cmd_attr {
                        Some(cmd_attr) => HopNode::XExec {
                            cmd_attr,
                            range: tree.opening_tag_range,
                            children,
                        },
                        None => HopNode::Error {
                            range: tree.opening_tag_range,
                            children,
                        },
                    }
                }
                "hop-x-raw" => {
                    let has_trim = attributes.iter().any(|attr| attr.name == "trim");
                    HopNode::XRaw {
                        trim: has_trim,
                        range: tree.opening_tag_range,
                        children,
                    }
                }
                "hop-x-load-json" => {
                    let file_attr = t.find_attribute("file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "file",
                            tree.opening_tag_range,
                        ));
                        None
                    });

                    let as_attr = t.find_attribute("as").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            value,
                            "as",
                            tree.opening_tag_range,
                        ));
                        None
                    });

                    match (file_attr, as_attr) {
                        (Some(file_attr), Some(as_attr)) => HopNode::XLoadJson {
                            file_attr,
                            as_attr,
                            range: tree.opening_tag_range,
                            children,
                        },
                        _ => HopNode::Error {
                            range: tree.opening_tag_range,
                            children,
                        },
                    }
                }
                tag_name if tag_name.starts_with("slot-") => {
                    let slot_name = &tag_name[5..]; // Remove "slot-" prefix
                    HopNode::SlotDefinition {
                        name: slot_name.to_string(),
                        range: tree.opening_tag_range,
                        children,
                    }
                }
                tag_name if tag_name.starts_with("with-") => {
                    let slot_name = &tag_name[5..]; // Remove "with-" prefix
                    HopNode::SlotReference {
                        name: slot_name.to_string(),
                        range: tree.opening_tag_range,
                        children,
                    }
                }
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

                    HopNode::ComponentReference {
                        component: tag_name.to_string(),
                        opening_name_range: *name_range,
                        closing_name_range: tree.closing_tag_name_range,
                        args: params_attrs,
                        attributes: attributes.clone(),
                        range: tree.opening_tag_range,
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
                        attributes: attributes.clone(),
                        range: tree.opening_tag_range,
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
    use crate::tui::error_formatter::ErrorFormatter;
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
                HopNode::SlotDefinition { name, children, .. } => {
                    lines.push(format!("{}slot-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                HopNode::SlotReference { name, children, .. } => {
                    lines.push(format!("{}with-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
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
            let mut efmt = ErrorFormatter::new().without_location_info();
            efmt.add_errors("test".to_string(), input.to_string(), errors);
            efmt.format_all_errors()
        } else {
            for component in module.components {
                if component.name == "main-comp" {
                    return expected.assert_eq(&format_component_definition(&component));
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
    fn test_parser_slots_basic() {
        check(
            indoc! {r#"
                <main-comp>
                    <slot-content>
                        Default content
                    </slot-content>
                    <other-comp>
                        <with-data>
                            Custom content
                        </with-data>
                    </other-comp>
                </main-comp>

                <other-comp>
                    <slot-data>
                        Other content
                    </slot-data>
                </other-comp>
            "#},
            expect![[r#"
                slot-content
                render
                	with-data
            "#]],
        );
    }

    // When slot-default is defined, it must be the only slot, otherwise the parser outputs an error.
    #[test]
    fn test_slot_default_must_be_only_slot() {
        check(
            indoc! {r#"
                -- main.hop --
                <mixed-comp>
                    <slot-default>Default slot</slot-default>
                    <slot-other>Other slot</slot-other>
                </mixed-comp>
            "#},
            expect![[r#"
                error: When using slot-default, it must be the only slot in the component
                3 |     <slot-default>Default slot</slot-default>
                4 |     <slot-other>Other slot</slot-other>
                  |     ^^^^^^^^^^^^
            "#]],
        );
    }

    // When a slot is defined twice in a component, the parser outputs an error.
    #[test]
    fn test_slot_defined_twice() {
        check(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <slot-content>
                        First definition
                    </slot-content>
                    <slot-content>
                        Second definition
                    </slot-content>
                </main-comp>
            "#},
            expect![[r#"
                error: Slot 'content' is already defined
                5 |     </slot-content>
                6 |     <slot-content>
                  |     ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_parser_slots_default() {
        check(
            indoc! {r#"
                <main-comp>
                    <slot-default>
                        Default content
                    </slot-default>
                    <button-comp>
                        <with-default>Custom Button</with-default>
                    </button-comp>
                </main-comp>
            "#},
            expect![[r#"
                slot-default
                render
                	with-default
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
