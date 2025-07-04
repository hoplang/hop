use crate::common::{
    is_void_element, Node, NodeType, Position, Range, RangeError, Token, TokenType,
};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseResult {
    pub root: Node,
    pub errors: Vec<RangeError>,
}

impl ParseResult {
    pub fn new(root: Node, errors: Vec<RangeError>) -> Self {
        Self { root, errors }
    }
}

struct NodeBuilder {
    nodes: Vec<Node>,
    children: HashMap<usize, Vec<usize>>,
    root: usize,
}

impl NodeBuilder {
    fn new() -> Self {
        let mut nodes = Vec::new();
        let mut children = HashMap::new();

        // Create root node
        let root_node = Node::new(
            NodeType::RootNode,
            String::new(),
            Vec::new(),
            Range::new(Position::new(0, 0), Position::new(0, 0)),
            Vec::new(),
        );
        nodes.push(root_node);
        children.insert(0, Vec::new());

        Self {
            nodes,
            children,
            root: 0,
        }
    }

    fn append_child(&mut self, parent_id: usize, node: Node) -> usize {
        let idx = self.nodes.len();
        self.nodes.push(node);
        self.children
            .entry(parent_id)
            .or_insert_with(Vec::new)
            .push(idx);
        self.children.insert(idx, Vec::new());
        idx
    }

    fn get(&self, idx: usize) -> &Node {
        &self.nodes[idx]
    }

    fn set_range_end(&mut self, idx: usize, pos: Position) {
        let current = &self.nodes[idx];
        let new_range = Range::new(current.range.start, pos);
        self.nodes[idx] = Node::new(
            current.node_type,
            current.value.clone(),
            current.attributes.clone(),
            new_range,
            current.children.clone(),
        );
    }

    fn finalize(&self, idx: usize) -> Node {
        let current = &self.nodes[idx];
        let child_nodes: Vec<Node> = self.children[&idx]
            .iter()
            .map(|&child_idx| self.finalize(child_idx))
            .collect();

        Node::new(
            current.node_type,
            current.value.clone(),
            current.attributes.clone(),
            current.range,
            child_nodes,
        )
    }
}

fn construct_node_type(token: &Token) -> NodeType {
    match token.value.as_str() {
        "render" => NodeType::RenderNode,
        "for" => NodeType::ForNode,
        "cond" => NodeType::CondNode,
        "import" => NodeType::ImportNode,
        "component" => NodeType::ComponentNode,
        _ => NodeType::NativeHTMLNode,
    }
}

fn validate_level(token: &Token, level: usize) -> Option<RangeError> {
    if token.value == "import" || token.value == "component" {
        if level != 0 {
            return Some(RangeError::new(
                format!("Tag <{}> must be placed at module root", token.value),
                token.range,
            ));
        }
    } else if level == 0 {
        return Some(RangeError::new(
            format!("Unexpected tag <{}> at module root", token.value),
            token.range,
        ));
    }
    None
}

fn validate_attributes(token: &Token) -> Vec<RangeError> {
    let required_attrs = match token.value.as_str() {
        "render" => vec!["component"],
        "for" => vec!["each"],
        "cond" => vec!["if"],
        "import" => vec!["component", "from"],
        "component" => vec!["name"],
        _ => vec![],
    };

    let mut errors = Vec::new();
    for attr in required_attrs {
        if !token.has_attribute(attr) {
            errors.push(RangeError::new(
                format!("{} is missing required attribute {}", token.value, attr),
                token.range,
            ));
        }
    }
    errors
}

pub fn parse(tokens: Vec<Token>) -> ParseResult {
    let mut builder = NodeBuilder::new();
    let mut errors = Vec::new();
    let mut stack = vec![builder.root];

    for token in tokens {
        let level = stack.len() - 1;

        match token.token_type {
            TokenType::Error => {
                errors.push(RangeError::new(token.value.clone(), token.range));
            }

            TokenType::Doctype => {
                if level == 0 {
                    errors.push(RangeError::new(
                        "Unexpected doctype tag at module root".to_string(),
                        token.range,
                    ));
                } else {
                    let node = Node::new(
                        NodeType::DoctypeNode,
                        token.value,
                        Vec::new(),
                        token.range,
                        Vec::new(),
                    );
                    builder.append_child(*stack.last().unwrap(), node);
                }
            }

            TokenType::Text => {
                if level != 0 {
                    let node = Node::new(
                        NodeType::TextNode,
                        token.value,
                        Vec::new(),
                        token.range,
                        Vec::new(),
                    );
                    builder.append_child(*stack.last().unwrap(), node);
                }
            }

            TokenType::Comment => {
                // Skip comments
                continue;
            }

            TokenType::SelfClosingTag | TokenType::StartTag => {
                if let Some(err) = validate_level(&token, level) {
                    errors.push(err);
                }

                for err in validate_attributes(&token) {
                    errors.push(err);
                }

                let node = Node::new(
                    construct_node_type(&token),
                    token.value.clone(),
                    token.attributes,
                    token.range,
                    Vec::new(),
                );

                let idx = builder.append_child(*stack.last().unwrap(), node);

                if token.token_type == TokenType::StartTag && !is_void_element(&token.value) {
                    stack.push(idx);
                }
            }

            TokenType::EndTag => {
                if is_void_element(&token.value) {
                    errors.push(RangeError::new(
                        format!(
                            "Tag <{}> should not be closed using a closing tag",
                            token.value
                        ),
                        token.range,
                    ));
                } else if builder.get(*stack.last().unwrap()).node_type == NodeType::RootNode {
                    errors.push(RangeError::new(
                        "Unexpected closing tag".to_string(),
                        token.range,
                    ));
                } else if token.value != builder.get(*stack.last().unwrap()).value {
                    // Recover by finding a matching tag on the stack
                    let mut found_match = false;
                    for (_i, &stack_idx) in stack.iter().enumerate().rev() {
                        if builder.get(stack_idx).value == token.value {
                            // Find first matching tag
                            while builder.get(*stack.last().unwrap()).value != token.value {
                                let unclosed_idx = stack.pop().unwrap();
                                let unclosed_node = builder.get(unclosed_idx);
                                errors.push(RangeError::new(
                                    format!("Unclosed tag <{}>", unclosed_node.value),
                                    unclosed_node.range,
                                ));
                            }
                            builder.set_range_end(*stack.last().unwrap(), token.range.end);
                            stack.pop();
                            found_match = true;
                            break;
                        }
                    }

                    if !found_match {
                        errors.push(RangeError::new(
                            format!(
                                "Expected closing tag for <{}>",
                                builder.get(*stack.last().unwrap()).value
                            ),
                            token.range,
                        ));
                    }
                } else {
                    builder.set_range_end(*stack.last().unwrap(), token.range.end);
                    stack.pop();
                }
            }
        }
    }

    // Handle unclosed tags
    while stack.len() > 1 {
        let unclosed_idx = stack.pop().unwrap();
        let unclosed_node = builder.get(unclosed_idx);
        errors.push(RangeError::new(
            format!("Unclosed tag <{}>", unclosed_node.value),
            unclosed_node.range,
        ));
    }

    ParseResult::new(builder.finalize(builder.root), errors)
}
