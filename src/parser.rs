use crate::common::{
    is_void_element, ComponentNode, CondNode, DoctypeNode, ErrorNode, ForNode, ImportNode,
    NativeHTMLNode, Node, Position, Range, RangeError, RenderNode, TextNode, Token, TokenKind,
};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseResult {
    pub components: Vec<ComponentNode>,
    pub imports: Vec<ImportNode>,
    pub errors: Vec<RangeError>,
}

// Error constructors
fn err_unmatched(t: &Token) -> RangeError {
    RangeError {
        message: format!("Unmatched </{}>", t.value),
        range: t.range,
    }
}

fn err_unclosed(t: &Token) -> RangeError {
    RangeError {
        message: format!("Unclosed <{}>", t.value),
        range: t.range,
    }
}

fn err_closed_void(t: &Token) -> RangeError {
    RangeError {
        message: format!("<{}> should not be closed using a closing tag", t.value),
        range: t.range,
    }
}

fn err_unexpected_outside_root(t: &Token) -> RangeError {
    RangeError {
        message: format!("<{}> must be placed at module root", t.value),
        range: t.range,
    }
}

fn err_unexpected_at_root(t: &Token) -> RangeError {
    RangeError {
        message: format!("Unexpected <{}> at module root", t.value),
        range: t.range,
    }
}

fn err_doctype_at_root(t: &Token) -> RangeError {
    RangeError {
        message: "Unexpected doctype at module root".to_string(),
        range: t.range,
    }
}

fn err_missing_required_attr(t: &Token, attr: &str) -> RangeError {
    RangeError {
        message: format!("<{}> is missing required attribute {}", t.value, attr),
        range: t.range,
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

    fn append_child(&mut self, token: Token) {
        self.children.push(TokenTree::new(token));
    }

    fn append_tree(&mut self, tree: TokenTree) {
        self.children.push(tree);
    }

    fn set_end_token(&mut self, token: Token) {
        self.end_token = Some(token);
    }
}

pub fn parse(tokens: Vec<Token>) -> ParseResult {
    let mut errors = Vec::new();
    let tree = build_tree(tokens, &mut errors);

    let mut components = Vec::new();
    let mut imports = Vec::new();

    for child in &tree.children {
        let node = construct_node(child, 0, &mut errors);
        match node {
            Node::Import(import_data) => imports.push(import_data),
            Node::Component(component_data) => components.push(component_data),
            _ => {} // ignore other node types at root level
        }
    }

    ParseResult {
        components,
        imports,
        errors,
    }
}

fn build_tree(tokens: Vec<Token>, errors: &mut Vec<RangeError>) -> TokenTree {
    let mut stack: Vec<TokenTree> = Vec::new();

    // Push a root token
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
            TokenKind::Error => {
                errors.push(RangeError {
                    message: token.value.clone(),
                    range: token.range,
                });
            }
            TokenKind::Doctype | TokenKind::Text | TokenKind::SelfClosingTag => {
                stack.last_mut().unwrap().append_child(token);
            }
            TokenKind::StartTag => {
                if is_void_element(&token.value) {
                    stack.last_mut().unwrap().append_child(token);
                } else {
                    stack.push(TokenTree::new(token));
                }
            }
            TokenKind::EndTag => {
                if is_void_element(&token.value) {
                    errors.push(err_closed_void(&token));
                } else if stack
                    .iter()
                    .find(|t| t.token.value == token.value)
                    .is_none()
                {
                    errors.push(err_unmatched(&token));
                } else {
                    while stack.last().unwrap().token.value != token.value {
                        errors.push(err_unclosed(&stack.pop().unwrap().token));
                    }
                    let mut completed = stack.pop().unwrap();
                    completed.set_end_token(token);
                    stack.last_mut().unwrap().append_tree(completed);
                }
            }
        }
    }

    while stack.len() > 1 {
        errors.push(err_unclosed(&stack.pop().unwrap().token));
    }

    stack.pop().unwrap_or_else(|| {
        let root_token = Token {
            kind: TokenKind::StartTag,
            value: "root".to_string(),
            attributes: Vec::new(),
            range: Range {
                start: Position { line: 0, column: 0 },
                end: Position { line: 0, column: 0 },
            },
        };
        TokenTree::new(root_token)
    })
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
                errors.push(err_doctype_at_root(t));
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
                    errors.push(err_unexpected_outside_root(t));
                }
            } else if depth == 0 {
                errors.push(err_unexpected_at_root(t));
            }

            match t.value.as_str() {
                "render" => {
                    let params_attr = t.get_attribute("params");
                    let component_attr = t.get_attribute("component");

                    if component_attr.is_none() {
                        errors.push(err_missing_required_attr(t, "component"));
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::Render(RenderNode {
                        component_attr: component_attr.unwrap(),
                        params_attr,
                        range: t.range,
                        children,
                    })
                }
                "for" => {
                    let each_attr = t.get_attribute("each");
                    let as_attr = t.get_attribute("as");

                    if each_attr.is_none() {
                        errors.push(err_missing_required_attr(t, "each"));
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::For(ForNode {
                        each_attr: each_attr.unwrap(),
                        as_attr,
                        range: t.range,
                        children,
                    })
                }
                "cond" => {
                    let if_attr = t.get_attribute("if");

                    if if_attr.is_none() {
                        errors.push(err_missing_required_attr(t, "if"));
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::Cond(CondNode {
                        if_attr: if_attr.unwrap(),
                        range: t.range,
                        children,
                    })
                }
                "import" => {
                    let component_attr = t.get_attribute("component");
                    let from_attr = t.get_attribute("from");

                    if component_attr.is_none() || from_attr.is_none() {
                        if component_attr.is_none() {
                            errors.push(err_missing_required_attr(t, "component"));
                        }
                        if from_attr.is_none() {
                            errors.push(err_missing_required_attr(t, "from"));
                        }
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::Import(ImportNode {
                        component_attr: component_attr.unwrap(),
                        from_attr: from_attr.unwrap(),
                        range: t.range,
                    })
                }
                "component" => {
                    let as_attr = t.get_attribute("as");
                    let params_as_attr = t.get_attribute("params-as");
                    let name_attr = t.get_attribute("name");

                    if name_attr.is_none() {
                        errors.push(err_missing_required_attr(t, "name"));
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::Component(ComponentNode {
                        name_attr: name_attr.unwrap(),
                        params_as_attr,
                        as_attr,
                        attributes: t.attributes.clone(),
                        range: t.range,
                        children,
                    })
                }
                _ => {
                    let inner_text_attr = t.get_attribute("inner-text");
                    Node::NativeHTML(NativeHTMLNode {
                        value: t.value.clone(),
                        attributes: t.attributes.clone(),
                        range: t.range,
                        children,
                        inner_text_attr,
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
    use std::path::Path;

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
                    value, children, ..
                }) => {
                    lines.push(format!("{}{}", indent, value));
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
        let entries = fs::read_dir(Path::new("test_data/parser")).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let input = archive.get("main.hop").unwrap().content.trim();
            let expected = archive.get("output.txt").unwrap().content.trim();

            let result = parse(tokenize(input.to_string()));

            if !result.errors.is_empty() {
                let output = result
                    .errors
                    .iter()
                    .map(|e| e.message.clone())
                    .collect::<Vec<_>>()
                    .join(" ");
                assert_eq!(output, expected, "Mismatch in file: {}", file_name);
            } else {
                for component in result.components {
                    if component.name_attr.value == "main" {
                        let output = format_tree(&Node::Component(component));
                        assert_eq!(output, expected, "Mismatch in file: {}", file_name);
                    }
                }
            }
        }
    }
}
