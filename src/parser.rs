use crate::common::{
    ComponentNode, CondNode, DoctypeNode, ErrorNode, ExprAttribute, ForNode,
    ImportNode, NativeHTMLNode, Node, Position, Range, RangeError, RenderNode, TextNode, Token,
    TokenKind, VarNameAttr, DefineSlotNode, SupplySlotNode, is_void_element,
};
use crate::expression_parser::parse_expression;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub components: Vec<ComponentNode>,
    pub imports: Vec<ImportNode>,
}

pub fn parse(tokens: Vec<Token>, errors: &mut Vec<RangeError>) -> Module {
    let tree = build_tree(tokens, errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();

    for child in &tree.children {
        let node = construct_node(child, 0, errors);
        match node {
            Node::Import(import_data) => imports.push(import_data),
            Node::Component(component_data) => components.push(component_data),
            _ => {} // ignore other node types at root level
        }
    }

    Module {
        components,
        imports,
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

fn collect_slots_from_children(children: &[Node], slots: &mut HashSet<String>, errors: &mut Vec<RangeError>) {
    for child in children {
        match child {
            Node::DefineSlot(DefineSlotNode { name_attr, range, .. }) => {
                if slots.contains(&name_attr.value) {
                    errors.push(RangeError::slot_already_defined(&name_attr.value, *range));
                } else {
                    slots.insert(name_attr.value.clone());
                }
            }
            Node::Component(ComponentNode { children, .. }) => {
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
            Node::Render(RenderNode { children, .. }) => {
                collect_slots_from_children(children, slots, errors);
            }
            Node::SupplySlot(SupplySlotNode { children, .. }) => {
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
            if t.value == "import" || t.value == "component" {
                if depth > 0 {
                    errors.push(RangeError::unexpected_tag_outside_root(&t.value, t.range));
                }
            } else if depth == 0 {
                errors.push(RangeError::unexpected_tag_at_root(&t.value, t.range));
            }

            match t.value.as_str() {
                "render" => {
                    let params_attr = t.get_attribute("params").and_then(|attr| {
                        parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                    });
                    let component_attr = t.get_attribute("component").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value,
                            "component",
                            t.range,
                        ));
                        None
                    });

                    match component_attr {
                        Some(component_attr) => Node::Render(RenderNode {
                            component_attr,
                            params_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
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
                "component" => {
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
                    let name_attr = t.get_attribute("name").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "name", t.range,
                        ));
                        None
                    });

                    match name_attr {
                        Some(name_attr) => {
                            let mut slots = HashSet::new();
                            collect_slots_from_children(&children, &mut slots, errors);
                            Node::Component(ComponentNode {
                                name_attr,
                                params_as_attr,
                                as_attr,
                                attributes: t.attributes.clone(),
                                range: t.range,
                                children,
                                entrypoint,
                                slots: slots.into_iter().collect(),
                            })
                        }
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
                "define-slot" => {
                    let name_attr = t.get_attribute("name").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "name", t.range,
                        ));
                        None
                    });
                    match name_attr {
                        Some(name_attr) => Node::DefineSlot(DefineSlotNode {
                            name_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
                }
                "supply-slot" => {
                    let name_attr = t.get_attribute("name").or_else(|| {
                        errors.push(RangeError::missing_required_attribute(
                            &t.value, "name", t.range,
                        ));
                        None
                    });
                    match name_attr {
                        Some(name_attr) => Node::SupplySlot(SupplySlotNode {
                            name_attr,
                            range: t.range,
                            children,
                        }),
                        None => Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        }),
                    }
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
                Node::Render(RenderNode { children, .. }) => {
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
                Node::Component(ComponentNode { children, .. }) => {
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
                Node::DefineSlot(DefineSlotNode { children, .. }) => {
                    lines.push(format!("{}define-slot", indent));
                    for child in children {
                        format_node(child, depth + 1, lines);
                    }
                }
                Node::SupplySlot(SupplySlotNode { children, .. }) => {
                    lines.push(format!("{}supply-slot", indent));
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
        d.push("test_data/parser");
        let entries = fs::read_dir(d).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let input = archive.get("main.hop").unwrap().content.trim();
            let expected = archive.get("output.txt").unwrap().content.trim();

            let mut errors = Vec::new();
            let tokens = tokenize(input, &mut errors);
            assert!(errors.is_empty());
            let module = parse(tokens, &mut errors);

            if !errors.is_empty() {
                let output = errors
                    .iter()
                    .map(|e| e.message.clone())
                    .collect::<Vec<_>>()
                    .join(" ");
                assert_eq!(output, expected, "Mismatch in file: {}", file_name);
            } else {
                for component in module.components {
                    if component.name_attr.value == "main" {
                        let output = format_tree(&Node::Component(component));
                        assert_eq!(output, expected, "Mismatch in file: {}", file_name);
                    }
                }
            }
        }
    }
}
