use crate::common::{
    ComponentDefinitionNode, ComponentReferenceNode, CondNode, DefineSlotNode, DoctypeNode,
    ErrorNode, ExprAttribute, ForNode, ImportNode, NativeHTMLNode, Node, Position, Range,
    RangeError, RenderNode, SupplySlotNode, TextNode, Token, TokenKind, VarNameAttr,
    is_void_element,
};
use crate::expression_parser::parse_expression;
use std::collections::HashSet;

fn is_valid_component_name(name: &str) -> bool {
    if name.is_empty() || name.starts_with('-') || name.ends_with('-') {
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

pub fn parse(module_name: String, tokens: Vec<Token>, errors: &mut Vec<RangeError>) -> Module {
    let tree = build_tree(tokens, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();
    let mut renders = Vec::new();

    for child in &tree.children {
        let node = construct_node(child, 0, errors);
        match node {
            Node::Import(import_data) => imports.push(import_data),
            Node::ComponentDefinition(component_data) => components.push(component_data),
            Node::Render(render_data) => renders.push(render_data),
            _ => {} // ignore other node types at root level
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
            TokenKind::Doctype | TokenKind::Text | TokenKind::SelfClosingTag => {
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

fn parse_expr_attribute(
    name: &str,
    value: &str,
    range: Range,
    errors: &mut Vec<RangeError>,
) -> Option<ExprAttribute> {
    match parse_expression(value) {
        Ok(expression) => Some(ExprAttribute::new(name.to_string(), expression, range)),
        Err(err) => {
            errors.push(RangeError::new(err, range));
            None
        }
    }
}

fn collect_slots_from_children(
    children: &[Node],
    slots: &mut HashSet<String>,
    errors: &mut Vec<RangeError>,
) {
    for child in children {
        match child {
            Node::DefineSlot(DefineSlotNode { name, range, .. }) => {
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
            Node::ComponentDefinition(ComponentDefinitionNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::For(ForNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::Cond(CondNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::NativeHTML(NativeHTMLNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::ComponentReference(ComponentReferenceNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::SupplySlot(SupplySlotNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::Render(RenderNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::Error(ErrorNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            _ => {}
        }
    }
}

fn construct_node(tree: &TokenTree, depth: usize, errors: &mut Vec<RangeError>) -> Node {
    let children: Vec<Node> = tree
        .children
        .iter()
        .map(|child| construct_node(child, depth + 1, errors))
        .collect();

    let t = &tree.token;

    match t.kind {
        TokenKind::Doctype => {
            if depth == 0 {
                errors.push(RangeError::unexpected_doctype_at_root(t.range));
            }
            Node::Doctype(DoctypeNode {
                value: t.value.clone(),
                range: t.range,
            })
        }
        TokenKind::Text => Node::Text(TextNode {
            value: t.value.clone(),
            range: t.range,
        }),
        TokenKind::SelfClosingTag | TokenKind::StartTag => {
            if t.value == "import" || t.value == "render" {
                if depth > 0 {
                    errors.push(RangeError::unexpected_tag_outside_root(&t.value, t.range));
                }
            } else if depth == 0 && t.value != "import" && t.value != "render" {
                // At root level, non-import/non-render tags are treated as components
                // Validate component name format
                if !is_valid_component_name(&t.value) {
                    errors.push(RangeError::invalid_component_name(&t.value, t.range));
                }
            }

            match t.value.as_str() {
                name if depth == 0 && name != "import" && name != "render" => {
                    // Handle component at root level
                    let as_attr = t.get_attribute("as");
                    let entrypoint = t.get_attribute("entrypoint").is_some();
                    let params_as_attr = t.get_attribute("params-as").and_then(|attr| {
                        match VarNameAttr::new(&attr) {
                            Some(var_attr) => Some(var_attr),
                            None => {
                                errors.push(RangeError::invalid_variable_name(
                                    &attr.value,
                                    attr.range,
                                ));
                                None
                            }
                        }
                    });

                    let mut slots = HashSet::new();
                    collect_slots_from_children(&children, &mut slots, errors);
                    Node::ComponentDefinition(ComponentDefinitionNode {
                        name: name.to_string(),
                        params_as_attr,
                        as_attr,
                        attributes: t.attributes.clone(),
                        range: t.range,
                        children,
                        entrypoint,
                        slots: slots.into_iter().collect(),
                    })
                }
                "for" => {
                    let each_attr = t
                        .get_attribute("each")
                        .or_else(|| {
                            errors.push(RangeError::missing_required_attribute(
                                &t.value, "each", t.range,
                            ));
                            None
                        })
                        .and_then(|attr| {
                            parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                        });
                    let as_attr =
                        t.get_attribute("as")
                            .and_then(|attr| match VarNameAttr::new(&attr) {
                                Some(var_attr) => Some(var_attr),
                                None => {
                                    errors.push(RangeError::invalid_variable_name(
                                        &attr.value,
                                        attr.range,
                                    ));
                                    None
                                }
                            });
                    match each_attr {
                        Some(each_attr) => Node::For(ForNode {
                            each_attr,
                            as_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
                "cond" => {
                    let if_attr = t
                        .get_attribute("if")
                        .or_else(|| {
                            errors.push(RangeError::missing_required_attribute(
                                &t.value, "if", t.range,
                            ));
                            None
                        })
                        .and_then(|attr| {
                            parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                        });

                    match if_attr {
                        Some(if_attr) => Node::Cond(CondNode {
                            if_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
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
                        (Some(component_attr), Some(from_attr)) => Node::Import(ImportNode {
                            component_attr,
                            from_attr,
                            range: t.range,
                        }),
                        _ => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
                "render" => {
                    let file_attr = t.get_attribute("file").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "file", t.range,
                        ));
                        None
                    });

                    match file_attr {
                        Some(file_attr) => Node::Render(RenderNode {
                            file_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
                tag_name if tag_name.starts_with("slot-") => {
                    let slot_name = &tag_name[5..]; // Remove "slot-" prefix
                    Node::DefineSlot(DefineSlotNode {
                        name: slot_name.to_string(),
                        range: t.range,
                        children,
                    })
                }
                tag_name if tag_name.starts_with("with-") => {
                    let slot_name = &tag_name[5..]; // Remove "with-" prefix
                    Node::SupplySlot(SupplySlotNode {
                        name: slot_name.to_string(),
                        range: t.range,
                        children,
                    })
                }
                tag_name if is_valid_component_name(tag_name) => {
                    // This is a component render (contains dash)
                    let params_attr = t.get_attribute("params").and_then(|attr| {
                        parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                    });

                    Node::ComponentReference(ComponentReferenceNode {
                        component: tag_name.to_string(),
                        params_attr,
                        range: t.range,
                        children,
                    })
                }
                _ => {
                    let inner_text_attr = t.get_attribute("set-inner-text").and_then(|attr| {
                        parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                    });

                    let mut set_attributes = Vec::new();
                    for attr in &t.attributes {
                        if attr.name.starts_with("set-") && attr.name != "set-inner-text" {
                            if let Some(expr_attr) =
                                parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                            {
                                set_attributes.push(expr_attr);
                            }
                        }
                    }

                    Node::NativeHTML(NativeHTMLNode {
                        tag_name: t.value.clone(),
                        attributes: t.attributes.clone(),
                        range: t.range,
                        children,
                        inner_text_attr,
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

    pub fn format_tree(root: &Node) -> String {
        let mut lines = Vec::new();

        fn format_node(node: &Node, depth: usize, lines: &mut Vec<String>) {
            let indent = "\t".repeat(depth);

            match node {
                Node::Import(_) => {
                    lines.push(format!("{}import", indent));
                }
                Node::Doctype(_) => {
                    lines.push(format!("{}doctype", indent));
                }
                Node::ComponentReference(ComponentReferenceNode { children, .. }) => {
                    lines.push(format!("{}render", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::Cond(CondNode { children, .. }) => {
                    lines.push(format!("{}cond", indent));
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
                Node::ComponentDefinition(ComponentDefinitionNode { children, .. }) => {
                    lines.push(format!("{}component", indent));
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
                Node::DefineSlot(DefineSlotNode { name, children, .. }) => {
                    lines.push(format!("{}slot-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::SupplySlot(SupplySlotNode { name, children, .. }) => {
                    lines.push(format!("{}with-{}", indent, name));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::Render(RenderNode { children, .. }) => {
                    lines.push(format!("{}build-render", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                _ => {}
            }
        }

        format_node(root, 0, &mut lines);
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
                        let output = format_tree(&Node::ComponentDefinition(component));
                        assert_eq!(
                            output,
                            expected,
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
