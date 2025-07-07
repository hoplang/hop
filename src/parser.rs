use crate::common::{
    is_void_element, ComponentNode, CondNode, DoctypeNode, ErrorNode, ExprAttribute, ForNode,
    ImportNode, NativeHTMLNode, Node, Position, Range, RenderNode, TextNode, Token, TokenKind,
};
use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq)]
pub enum ParseError {
    #[error("Unmatched </{value}>")]
    UnmatchedTag { value: String, range: Range },

    #[error("Unclosed <{value}>")]
    UnclosedTag { value: String, range: Range },

    #[error("<{value}> should not be closed using a closing tag")]
    ClosedVoidElement { value: String, range: Range },

    #[error("<{value}> must be placed at module root")]
    UnexpectedOutsideRoot { value: String, range: Range },

    #[error("Unexpected <{value}> at module root")]
    UnexpectedAtRoot { value: String, range: Range },

    #[error("Unexpected doctype at module root")]
    DoctypeAtRoot { range: Range },

    #[error("<{tag}> is missing required attribute {attr}")]
    MissingRequiredAttribute {
        tag: String,
        attr: String,
        range: Range,
    },

    #[error("Empty expression")]
    EmptyExpression { range: Range },

    #[error("Tokenizer error: {message}")]
    TokenizerError { message: String, range: Range },
}

impl ParseError {
    pub fn range(&self) -> Range {
        match self {
            ParseError::UnmatchedTag { range, .. } => *range,
            ParseError::UnclosedTag { range, .. } => *range,
            ParseError::ClosedVoidElement { range, .. } => *range,
            ParseError::UnexpectedOutsideRoot { range, .. } => *range,
            ParseError::UnexpectedAtRoot { range, .. } => *range,
            ParseError::DoctypeAtRoot { range } => *range,
            ParseError::MissingRequiredAttribute { range, .. } => *range,
            ParseError::EmptyExpression { range } => *range,
            ParseError::TokenizerError { range, .. } => *range,
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseResult {
    pub components: Vec<ComponentNode>,
    pub imports: Vec<ImportNode>,
    pub errors: Vec<ParseError>,
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

fn build_tree(tokens: Vec<Token>, errors: &mut Vec<ParseError>) -> TokenTree {
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
                errors.push(ParseError::TokenizerError {
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
                    errors.push(ParseError::ClosedVoidElement {
                        value: token.value.clone(),
                        range: token.range,
                    });
                } else if stack
                    .iter()
                    .find(|t| t.token.value == token.value)
                    .is_none()
                {
                    errors.push(ParseError::UnmatchedTag {
                        value: token.value.clone(),
                        range: token.range,
                    });
                } else {
                    while stack.last().unwrap().token.value != token.value {
                        let unclosed_token = stack.pop().unwrap().token;
                        errors.push(ParseError::UnclosedTag {
                            value: unclosed_token.value.clone(),
                            range: unclosed_token.range,
                        });
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
        errors.push(ParseError::UnclosedTag {
            value: unclosed_token.value.clone(),
            range: unclosed_token.range,
        });
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

fn parse_expr(expr: &str) -> Vec<String> {
    expr.trim().split('.').map(|s| s.to_string()).collect()
}

fn parse_expr_attribute(
    name: &str,
    value: &str,
    range: Range,
    errors: &mut Vec<ParseError>,
) -> Option<ExprAttribute> {
    let segments = parse_expr(value);
    if segments.is_empty() {
        errors.push(ParseError::EmptyExpression { range });
        return None;
    }
    Some(ExprAttribute::new(name.to_string(), segments, range))
}

fn construct_node(tree: &TokenTree, depth: usize, errors: &mut Vec<ParseError>) -> Node {
    let children: Vec<Node> = tree
        .children
        .iter()
        .map(|child| construct_node(child, depth + 1, errors))
        .collect();

    let t = &tree.token;

    match t.kind {
        TokenKind::Doctype => {
            if depth == 0 {
                errors.push(ParseError::DoctypeAtRoot { range: t.range });
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
                    errors.push(ParseError::UnexpectedOutsideRoot {
                        value: t.value.clone(),
                        range: t.range,
                    });
                }
            } else if depth == 0 {
                errors.push(ParseError::UnexpectedAtRoot {
                    value: t.value.clone(),
                    range: t.range,
                });
            }

            match t.value.as_str() {
                "render" => {
                    let params_attr = t.get_attribute("params").and_then(|attr| {
                        parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                    });
                    let component_attr = t.get_attribute("component");

                    if component_attr.is_none() {
                        errors.push(ParseError::MissingRequiredAttribute {
                            tag: t.value.clone(),
                            attr: "component".to_string(),
                            range: t.range,
                        });
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
                        errors.push(ParseError::MissingRequiredAttribute {
                            tag: t.value.clone(),
                            attr: "each".to_string(),
                            range: t.range,
                        });
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    let each_attr = each_attr.unwrap();
                    let parsed_each_attr = parse_expr_attribute(
                        &each_attr.name,
                        &each_attr.value,
                        each_attr.range,
                        errors,
                    );

                    if parsed_each_attr.is_none() {
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::For(ForNode {
                        each_attr: parsed_each_attr.unwrap(),
                        as_attr,
                        range: t.range,
                        children,
                    })
                }
                "cond" => {
                    let if_attr = t.get_attribute("if");

                    if if_attr.is_none() {
                        errors.push(ParseError::MissingRequiredAttribute {
                            tag: t.value.clone(),
                            attr: "if".to_string(),
                            range: t.range,
                        });
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    let if_attr = if_attr.unwrap();
                    let parsed_if_attr =
                        parse_expr_attribute(&if_attr.name, &if_attr.value, if_attr.range, errors);

                    if parsed_if_attr.is_none() {
                        return Node::Error(ErrorNode {
                            range: t.range,
                            children,
                        });
                    }

                    Node::Cond(CondNode {
                        if_attr: parsed_if_attr.unwrap(),
                        range: t.range,
                        children,
                    })
                }
                "import" => {
                    let component_attr = t.get_attribute("component");
                    let from_attr = t.get_attribute("from");

                    if component_attr.is_none() || from_attr.is_none() {
                        if component_attr.is_none() {
                            errors.push(ParseError::MissingRequiredAttribute {
                                tag: t.value.clone(),
                                attr: "component".to_string(),
                                range: t.range,
                            });
                        }
                        if from_attr.is_none() {
                            errors.push(ParseError::MissingRequiredAttribute {
                                tag: t.value.clone(),
                                attr: "from".to_string(),
                                range: t.range,
                            });
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
                        errors.push(ParseError::MissingRequiredAttribute {
                            tag: t.value.clone(),
                            attr: "name".to_string(),
                            range: t.range,
                        });
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
                    let inner_text_attr = t.get_attribute("inner-text").and_then(|attr| {
                        parse_expr_attribute(&attr.name, &attr.value, attr.range, errors)
                    });
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
                    .map(|e| e.to_string())
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
