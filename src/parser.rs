use crate::common::{
    ComponentDefinitionNode, ComponentReferenceNode, DoctypeNode, DopExprAttribute,
    DopVarNameAttribute, ErrorNode, ForNode, IfNode, ImportNode, NativeHTMLNode, Node, Position,
    Range, RangeError, RenderNode, SlotDefinitionNode, SlotReferenceNode, TextExpressionNode,
    TextNode, Token, TokenKind, XExecNode, XRawNode, is_void_element,
};
use crate::dop;
use std::collections::HashSet;

fn is_valid_component_name(name: &str) -> bool {
    if name.is_empty() || name.starts_with('-') || name.ends_with('-') {
        return false;
    }
    if name.starts_with("hop-") {
        return false;
    }
    name.contains('-')
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub components: Vec<ComponentDefinitionNode>,
    pub imports: Vec<ImportNode>,
    pub renders: Vec<RenderNode>,
}

#[derive(Debug, Clone, PartialEq)]
enum ToplevelNode {
    Import(ImportNode),
    ComponentDefinition(ComponentDefinitionNode),
    Render(RenderNode),
}

pub fn parse(module_name: String, tokens: Vec<Token>, errors: &mut Vec<RangeError>) -> Module {
    let tree = build_tree(tokens, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut renders = Vec::new();

    for child in &tree.children {
        if let Some(toplevel_node) = construct_toplevel_node(child, errors) {
            match toplevel_node {
                ToplevelNode::Import(import_data) => imports.push(import_data),
                ToplevelNode::ComponentDefinition(component_data) => {
                    components.push(component_data)
                }
                ToplevelNode::Render(render_data) => renders.push(render_data),
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

#[derive(Debug, Clone)]
struct TokenTree {
    token: Token,
    children: Vec<TokenTree>,
    end_token: Option<Token>,
}

impl TokenTree {
    fn new(token: Token) -> Self {
        TokenTree {
            token,
            children: Vec::new(),
            end_token: None,
        }
    }

    fn append_node(&mut self, token: Token) {
        self.children.push(TokenTree::new(token));
    }

    fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    fn set_end_token(&mut self, token: Token) {
        self.end_token = Some(token);
    }
}

fn build_tree(tokens: Vec<Token>, errors: &mut Vec<RangeError>) -> TokenTree {
    let mut stack: Vec<TokenTree> = Vec::new();

    let root_token = Token {
        kind: TokenKind::StartTag,
        value: "root".to_string(),
        attributes: Vec::new(),
        expression: None,
        range: Range {
            start: Position { line: 0, column: 0 },
            end: Position { line: 0, column: 0 },
        },
    };
    stack.push(TokenTree::new(root_token));

    for token in tokens {
        match token.kind {
            TokenKind::Comment => {
                // skip comments
                continue;
            }
            TokenKind::Doctype
            | TokenKind::Text
            | TokenKind::SelfClosingTag
            | TokenKind::Expression => {
                stack.last_mut().unwrap().append_node(token);
            }
            TokenKind::StartTag => {
                if is_void_element(&token.value) {
                    stack.last_mut().unwrap().append_node(token);
                } else {
                    stack.push(TokenTree::new(token));
                }
            }
            TokenKind::EndTag => {
                if is_void_element(&token.value) {
                    errors.push(RangeError::closed_void_tag(&token.value, token.range));
                } else if !stack.iter().any(|t| t.token.value == token.value) {
                    errors.push(RangeError::unmatched_closing_tag(&token.value, token.range));
                } else {
                    while stack.last().unwrap().token.value != token.value {
                        let unclosed = stack.pop().unwrap();
                        errors.push(RangeError::unclosed_tag(
                            &unclosed.token.value,
                            unclosed.token.range,
                        ));
                        stack.last_mut().unwrap().append_tree(unclosed);
                    }
                    let mut completed = stack.pop().unwrap();
                    completed.set_end_token(token);
                    stack.last_mut().unwrap().append_tree(completed);
                }
            }
        }
    }

    while stack.len() > 1 {
        let unclosed_token = stack.pop().unwrap().token;
        errors.push(RangeError::unclosed_tag(
            &unclosed_token.value,
            unclosed_token.range,
        ));
    }

    stack.pop().unwrap()
}

fn collect_slots_from_children(
    children: &[Node],
    slots: &mut HashSet<String>,
    errors: &mut Vec<RangeError>,
) {
    for child in children {
        match child {
            Node::Text(_) => {}
            Node::TextExpression(_) => {}
            Node::Doctype(_) => {}
            Node::SlotDefinition(SlotDefinitionNode { name, range, .. }) => {
                if slots.contains(name) {
                    errors.push(RangeError::slot_already_defined(name, *range));
                } else {
                    // Check if trying to mix default slot with other slots
                    if (name == "default" && !slots.is_empty())
                        || (slots.contains("default") && name != "default")
                    {
                        errors.push(RangeError::default_slot_with_other_slots(*range));
                    } else {
                        slots.insert(name.clone());
                    }
                }
            }
            Node::XExec(XExecNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::If(IfNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::For(ForNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::NativeHTML(NativeHTMLNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::ComponentReference(ComponentReferenceNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::SlotReference(SlotReferenceNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::Error(ErrorNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::XRaw(XRawNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
        }
    }
}

fn construct_toplevel_node(tree: &TokenTree, errors: &mut Vec<RangeError>) -> Option<ToplevelNode> {
    let t = &tree.token;

    match t.kind {
        TokenKind::SelfClosingTag | TokenKind::StartTag => {
            match t.value.as_str() {
                "import" => {
                    let component_attr = t.get_attribute("component").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value,
                            "component",
                            t.range,
                        ));
                        None
                    });
                    let from_attr = t.get_attribute("from").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "from", t.range,
                        ));
                        None
                    });

                    match (component_attr, from_attr) {
                        (Some(component_attr), Some(from_attr)) => {
                            Some(ToplevelNode::Import(ImportNode {
                                component_attr,
                                from_attr,
                                range: t.range,
                            }))
                        }
                        _ => None,
                    }
                }
                "render" => {
                    let children: Vec<Node> = tree
                        .children
                        .iter()
                        .map(|child| construct_node(child, errors))
                        .collect();

                    let file_attr = t.get_attribute("file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "file", t.range,
                        ));
                        None
                    });

                    file_attr.map(|file_attr| {
                        ToplevelNode::Render(RenderNode {
                            file_attr,
                            range: t.range,
                            children,
                        })
                    })
                }
                name => {
                    // Handle as component definition
                    if !is_valid_component_name(name) {
                        errors.push(RangeError::invalid_component_name(name, t.range));
                        return None;
                    }

                    // Separate preview content from main children
                    let mut main_children = Vec::new();
                    let mut preview_children = None;

                    for child in &tree.children {
                        if let TokenKind::StartTag = child.token.kind {
                            if child.token.value == "hop-x-preview" {
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

                    let as_attr = t.get_attribute("as");
                    let entrypoint = t.get_attribute("entrypoint").is_some();
                    let params_as_attr = t.expression.as_ref().and_then(|expr_string| {
                        dop::parse_variable_name(expr_string, t.range, errors).map(|var_name| {
                            DopVarNameAttribute {
                                var_name,
                                range: t.range,
                            }
                        })
                    });

                    let mut slots = HashSet::new();
                    collect_slots_from_children(&children, &mut slots, errors);

                    Some(ToplevelNode::ComponentDefinition(ComponentDefinitionNode {
                        name: name.to_string(),
                        params_as_attr,
                        as_attr,
                        attributes: t.attributes.clone(),
                        range: t.range,
                        children,
                        preview: preview_children,
                        entrypoint,
                        slots: slots.into_iter().collect(),
                    }))
                }
            }
        }
        _ => None,
    }
}

fn construct_node(tree: &TokenTree, errors: &mut Vec<RangeError>) -> Node {
    let children: Vec<Node> = tree
        .children
        .iter()
        .map(|child| construct_node(child, errors))
        .collect();

    let t = &tree.token;

    match t.kind {
        TokenKind::Doctype => Node::Doctype(DoctypeNode {
            value: t.value.clone(),
            range: t.range,
        }),
        TokenKind::Text => Node::Text(TextNode {
            value: t.value.clone(),
            range: t.range,
        }),
        TokenKind::Expression => {
            // Expression tokens represent {expression} in text content
            match &t.expression {
                Some(expr_string) => match dop::parse_expr(expr_string) {
                    Ok(expression) => Node::TextExpression(TextExpressionNode {
                        expression,
                        range: t.range,
                    }),
                    Err(err) => {
                        errors.push(RangeError::new(
                            format!("Invalid expression: {}", err),
                            t.range,
                        ));
                        Node::Error(ErrorNode {
                            range: t.range,
                            children: vec![],
                        })
                    }
                },
                None => {
                    errors.push(RangeError::new(
                        "Missing expression in Expression token".to_string(),
                        t.range,
                    ));
                    Node::Error(ErrorNode {
                        range: t.range,
                        children: vec![],
                    })
                }
            }
        }
        TokenKind::SelfClosingTag | TokenKind::StartTag => {
            match t.value.as_str() {
                "if" => match &t.expression {
                    Some(expr_string) => match dop::parse_expr(expr_string) {
                        Ok(condition) => Node::If(IfNode {
                            condition,
                            range: t.range,
                            children,
                        }),
                        Err(err) => {
                            errors.push(RangeError::new(
                                format!("Invalid expression in <if> tag: {}", err),
                                t.range,
                            ));
                            Node::Error(ErrorNode {
                                range: t.range,
                                children,
                            })
                        }
                    },
                    None => {
                        errors.push(RangeError::new(
                            "Missing expression in <if> tag".to_string(),
                            t.range,
                        ));
                        Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        })
                    }
                },
                "for" => match &t.expression {
                    Some(expr_string) => match dop::parse_loop_header(expr_string, t.range, errors)
                    {
                        Some((var_name, array_expr)) => Node::For(ForNode {
                            var_name,
                            array_expr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    },
                    None => {
                        errors.push(RangeError::new(
                            "Missing loop generator expression in <for> tag".to_string(),
                            t.range,
                        ));
                        Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        })
                    }
                },
                "hop-x-exec" => {
                    let cmd_attr = t.get_attribute("cmd").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "cmd", t.range,
                        ));
                        None
                    });

                    match cmd_attr {
                        Some(cmd_attr) => Node::XExec(XExecNode {
                            cmd_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
                "hop-x-raw" => {
                    let has_trim = t.attributes.iter().any(|attr| attr.name == "trim");
                    Node::XRaw(XRawNode {
                        trim: has_trim,
                        range: t.range,
                        children,
                    })
                }
                tag_name if tag_name.starts_with("slot-") => {
                    let slot_name = &tag_name[5..]; // Remove "slot-" prefix
                    Node::SlotDefinition(SlotDefinitionNode {
                        name: slot_name.to_string(),
                        range: t.range,
                        children,
                    })
                }
                tag_name if tag_name.starts_with("with-") => {
                    let slot_name = &tag_name[5..]; // Remove "with-" prefix
                    Node::SlotReference(SlotReferenceNode {
                        name: slot_name.to_string(),
                        range: t.range,
                        children,
                    })
                }
                tag_name if is_valid_component_name(tag_name) => {
                    // This is a component render (contains dash)
                    let params_attr = match &t.expression {
                        Some(expr_string) => match dop::parse_expr(expr_string) {
                            Ok(expression) => Some(DopExprAttribute::new(
                                "params".to_string(),
                                expression,
                                t.range,
                            )),
                            Err(err) => {
                                errors.push(RangeError::new(
                                    format!("Invalid expression in <{}> tag: {}", tag_name, err),
                                    t.range,
                                ));
                                None
                            }
                        },
                        None => None,
                    };

                    Node::ComponentReference(ComponentReferenceNode {
                        component: tag_name.to_string(),
                        params_attr,
                        range: t.range,
                        children,
                    })
                }
                _ => {
                    let mut set_attributes = Vec::new();
                    for attr in &t.attributes {
                        if attr.name.starts_with("set-") {
                            if let Some(expr_attr) = {
                                match dop::parse_expr(&attr.value) {
                                    Ok(expression) => Some(DopExprAttribute::new(
                                        attr.name.to_string(),
                                        expression,
                                        attr.range,
                                    )),
                                    Err(err) => {
                                        errors.push(RangeError::new(err, attr.range));
                                        None
                                    }
                                }
                            } {
                                set_attributes.push(expr_attr);
                            }
                        }
                    }

                    Node::NativeHTML(NativeHTMLNode {
                        tag_name: t.value.clone(),
                        attributes: t.attributes.clone(),
                        range: t.range,
                        children,
                        set_attributes,
                    })
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
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::PathBuf;

    use crate::tokenizer::tokenize;

    pub fn format_component_definition(d: &ComponentDefinitionNode) -> String {
        let mut lines = Vec::new();
        for child in &d.children {
            let s = format_tree(&child, 0);
            if s != "" {
                lines.push(s);
            }
        }
        lines.join("\n")
    }

    pub fn format_tree(root: &Node, depth: usize) -> String {
        let mut lines = Vec::new();

        fn format_node(node: &Node, depth: usize, lines: &mut Vec<String>) {
            let indent = "\t".repeat(depth);

            match node {
                Node::Doctype(_) => {
                    lines.push(format!("{}doctype", indent));
                }
                Node::ComponentReference(ComponentReferenceNode { children, .. }) => {
                    lines.push(format!("{}render", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::If(IfNode { children, .. }) => {
                    lines.push(format!("{}if", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::For(ForNode { children, .. }) => {
                    lines.push(format!("{}for", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::NativeHTML(NativeHTMLNode {
                    tag_name, children, ..
                }) => {
                    lines.push(format!("{}{}", indent, tag_name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::SlotDefinition(SlotDefinitionNode { name, children, .. }) => {
                    lines.push(format!("{}slot-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::SlotReference(SlotReferenceNode { name, children, .. }) => {
                    lines.push(format!("{}with-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::XExec(XExecNode { children, .. }) => {
                    lines.push(format!("{}hop-x-exec", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::XRaw(XRawNode { children, .. }) => {
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

    #[test]
    fn test_parser() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/parser.cases");

        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let input = archive
                .get("main.hop")
                .expect("Missing 'main.hop' section in test case")
                .content
                .trim();
            let expected = archive
                .get("out")
                .expect("Missing 'out' section in test case")
                .content
                .trim();

            println!("Test case {} (line {})", case_num + 1, line_number);

            let mut errors = Vec::new();
            let tokens = tokenize(input, &mut errors);
            assert!(errors.is_empty());
            let module = parse("test".to_string(), tokens, &mut errors);

            if !errors.is_empty() {
                let output = errors
                    .iter()
                    .map(|e| e.message.clone())
                    .collect::<Vec<_>>()
                    .join(" ");
                assert_eq!(
                    output,
                    expected,
                    "Mismatch in test case {} (line {})",
                    case_num + 1,
                    line_number
                );
            } else {
                for component in module.components {
                    if component.name == "main-comp" {
                        let output = format_component_definition(&component);
                        assert_eq!(
                            output.trim(),
                            expected.trim(),
                            "Mismatch in test case {} (line {})",
                            case_num + 1,
                            line_number
                        );
                    }
                }
            }
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            if line == "## BEGIN" {
                assert!(
                    !in_case,
                    "Found '## BEGIN' at line {} while already inside a test case",
                    line_number
                );
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(
                    in_case,
                    "Found '## END' at line {} without matching '## BEGIN'",
                    line_number
                );
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(
            !in_case,
            "Reached end of file while inside a test case (missing '## END')"
        );

        test_cases
    }
}
