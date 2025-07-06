use crate::common::{
    ComponentNode, CondNode, DoctypeNode, ErrorNode, ForNode, ImportNode, NativeHTMLNode, Node,
    Position, Range, RangeError, RenderNode, TextNode, Token, TokenType, is_void_element,
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
        token_type: TokenType::StartTag,
        value: "root".to_string(),
        attributes: Vec::new(),
        range: Range {
            start: Position { line: 0, column: 0 },
            end: Position { line: 0, column: 0 },
        },
    };
    stack.push(TokenTree::new(root_token));

    for token in tokens {
        match token.token_type {
            TokenType::Comment => {
                // skip comments
                continue;
            }
            TokenType::Error => {
                errors.push(RangeError {
                    message: token.value.clone(),
                    range: token.range,
                });
            }
            TokenType::Doctype | TokenType::Text | TokenType::SelfClosingTag => {
                if let Some(top) = stack.last_mut() {
                    top.append_child(token);
                }
            }
            TokenType::StartTag => {
                if is_void_element(&token.value) {
                    if let Some(top) = stack.last_mut() {
                        top.append_child(token);
                    }
                } else {
                    stack.push(TokenTree::new(token));
                }
            }
            TokenType::EndTag => {
                if is_void_element(&token.value) {
                    errors.push(err_closed_void(&token));
                } else {
                    // Check if we can find a matching start tag
                    let mut found = false;
                    for tree in &stack {
                        if tree.token.value == token.value {
                            found = true;
                            break;
                        }
                    }

                    if !found {
                        errors.push(err_unmatched(&token));
                    } else {
                        // Pop until we find the matching tag
                        while stack.len() > 1 {
                            if let Some(last) = stack.last() {
                                if last.token.value == token.value {
                                    break;
                                }
                            }
                            if let Some(unclosed) = stack.pop() {
                                errors.push(err_unclosed(&unclosed.token));
                            }
                        }

                        if stack.len() > 1 {
                            if let Some(mut completed) = stack.pop() {
                                completed.set_end_token(token);
                                if let Some(top) = stack.last_mut() {
                                    top.append_tree(completed);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Close any remaining unclosed tags
    while stack.len() > 1 {
        if let Some(unclosed) = stack.pop() {
            errors.push(err_unclosed(&unclosed.token));
        }
    }

    // Return the root, or create an empty one if stack is empty (shouldn't happen)
    stack.pop().unwrap_or_else(|| {
        let root_token = Token {
            token_type: TokenType::StartTag,
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

    match t.token_type {
        TokenType::Doctype => {
            if depth == 0 {
                errors.push(err_doctype_at_root(t));
            }
            Node::Doctype(DoctypeNode {
                value: t.value.clone(),
                range: t.range,
            })
        }
        TokenType::Text => Node::Text(TextNode {
            value: t.value.clone(),
            range: t.range,
        }),
        TokenType::SelfClosingTag | TokenType::StartTag => {
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
