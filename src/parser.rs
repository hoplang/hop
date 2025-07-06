use crate::common::{
    Attribute, ComponentNode, CondNode, DoctypeNode, ForNode, ImportNode, NativeHTMLNode, Node,
    Position, Range, RangeError, RenderNode, TextNode, Token, TokenType, is_void_element,
};

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
pub struct ParseResult {
    pub components: Vec<ComponentNode>,
    pub imports: Vec<ImportNode>,
    pub errors: Vec<RangeError>,
}

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
            Node::Import(import_node) => imports.push(import_node),
            Node::Component(component_node) => components.push(component_node),
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
    let mut stack = Vec::new();

    // Push root token
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
                    // Check if matching start tag exists
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
                        while stack.len() > 1 && stack.last().unwrap().token.value != token.value {
                            let unclosed = stack.pop().unwrap();
                            errors.push(err_unclosed(&unclosed.token));
                        }

                        if stack.len() > 1 {
                            let mut completed = stack.pop().unwrap();
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

    // Handle remaining unclosed tags
    while stack.len() > 1 {
        let unclosed = stack.pop().unwrap();
        errors.push(err_unclosed(&unclosed.token));
    }

    stack.pop().unwrap()
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
                    if !t.has_attribute("component") {
                        errors.push(err_missing_required_attr(t, "component"));
                    }
                    let component_attr = t.get_attribute("component").unwrap_or(Attribute {
                        name: "component".to_string(),
                        value: "".to_string(),
                        range: t.range,
                    });
                    let params_attr = t.get_attribute("params");
                    Node::Render(RenderNode {
                        component_attr,
                        params_attr,
                        range: t.range,
                        children,
                    })
                }
                "for" => {
                    if !t.has_attribute("each") {
                        errors.push(err_missing_required_attr(t, "each"));
                    }
                    let each_attr = t.get_attribute("each").unwrap_or(Attribute {
                        name: "each".to_string(),
                        value: "".to_string(),
                        range: t.range,
                    });
                    let as_attr = t.get_attribute("as");
                    Node::For(ForNode {
                        each_attr,
                        as_attr,
                        range: t.range,
                        children,
                    })
                }
                "cond" => {
                    if !t.has_attribute("if") {
                        errors.push(err_missing_required_attr(t, "if"));
                    }
                    let if_attr = t.get_attribute("if").unwrap_or(Attribute {
                        name: "if".to_string(),
                        value: "".to_string(),
                        range: t.range,
                    });
                    Node::Cond(CondNode {
                        if_attr: if_attr,
                        range: t.range,
                        children,
                    })
                }
                "import" => {
                    if !t.has_attribute("component") {
                        errors.push(err_missing_required_attr(t, "component"));
                    }
                    if !t.has_attribute("from") {
                        errors.push(err_missing_required_attr(t, "from"));
                    }
                    let component_attr = t.get_attribute("component").unwrap_or(Attribute {
                        name: "component".to_string(),
                        value: "".to_string(),
                        range: t.range,
                    });
                    let from_attr = t.get_attribute("from").unwrap_or(Attribute {
                        name: "from".to_string(),
                        value: "".to_string(),
                        range: t.range,
                    });
                    Node::Import(ImportNode {
                        component_attr: component_attr,
                        from_attr: from_attr,
                        range: t.range,
                    })
                }
                "component" => {
                    if !t.has_attribute("name") {
                        errors.push(err_missing_required_attr(t, "name"));
                    }
                    let name_attr = t.get_attribute("name").unwrap_or(Attribute {
                        name: "name".to_string(),
                        value: "".to_string(),
                        range: t.range,
                    });
                    let params_as_attr = t.get_attribute("params-as");
                    let as_attr = t.get_attribute("as");
                    Node::Component(ComponentNode {
                        name_attr,
                        params_as_attr: params_as_attr,
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
