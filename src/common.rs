use std::collections::HashMap;
use std::fmt;

// Type system
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Object(HashMap<String, Type>),
    Array(Box<Type>),
    Bool,
    String,
    Void,
    TypeVar(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Object(props) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in props {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Type::Array(inner) => write!(f, "{}[]", inner),
            Type::Bool => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::TypeVar(id) => write!(f, "?{}", id),
        }
    }
}

// Position and Range
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl Position {
    pub fn new(line: i32, column: i32) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn contains_position(&self, position: Position) -> bool {
        (position.line > self.start.line || 
         (position.line == self.start.line && position.column >= self.start.column)) &&
        (position.line < self.end.line || 
         (position.line == self.end.line && position.column < self.end.column))
    }
}

// Error types
#[derive(Debug, Clone, PartialEq)]
pub struct RangeError {
    pub message: String,
    pub range: Range,
}

impl RangeError {
    pub fn new(message: String, range: Range) -> Self {
        Self { message, range }
    }
}

// Attribute
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
}

impl Attribute {
    pub fn new(name: String, value: String, range: Range) -> Self {
        Self { name, value, range }
    }
}

// Token types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Doctype,
    StartTag,
    EndTag,
    SelfClosingTag,
    Text,
    Comment,
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
}

impl Token {
    pub fn new(token_type: TokenType, value: String, attributes: Vec<Attribute>, range: Range) -> Self {
        Self {
            token_type,
            value,
            attributes,
            range,
        }
    }
}

// Node types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeType {
    RootNode,
    DoctypeNode,
    TextNode,
    CommentNode,
    RenderNode,
    ForNode,
    CondNode,
    ImportNode,
    ComponentNode,
    NativeHTMLNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub value: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<Node>,
}

impl Node {
    pub fn new(
        node_type: NodeType,
        value: String,
        attributes: Vec<Attribute>,
        range: Range,
        children: Vec<Node>,
    ) -> Self {
        Self {
            node_type,
            value,
            attributes,
            range,
            children,
        }
    }

    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|attr| attr.name == name)
    }

    pub fn get_attribute(&self, name: &str) -> &Attribute {
        self.attributes
            .iter()
            .find(|attr| attr.name == name)
            .unwrap_or_else(|| panic!("Attribute '{}' not found", name))
    }

    pub fn get_attribute_value(&self, name: &str) -> &str {
        &self.get_attribute(name).value
    }

    pub fn get_children_of_type(&self, node_type: NodeType) -> Vec<&Node> {
        self.children
            .iter()
            .filter(|child| child.node_type == node_type)
            .collect()
    }

}

// Utility functions
pub fn escape_html(text: &str) -> String {
    text.chars()
        .map(|c| match c {
            '&' => "&amp;".to_string(),
            '<' => "&lt;".to_string(),
            '>' => "&gt;".to_string(),
            '"' => "&quot;".to_string(),
            '\'' => "&#39;".to_string(),
            _ => c.to_string(),
        })
        .collect()
}

pub fn format_range_errors(message: &str, errors: &[RangeError]) -> String {
    let mut result = message.to_string();
    result.push('\n');
    
    for error in errors {
        result.push_str(&format!(
            "  {}:{}:{} - {}\n",
            error.range.start.line,
            error.range.start.column,
            error.range.end.column,
            error.message
        ));
    }
    
    result
}

pub fn is_void_element(el: &str) -> bool {
    matches!(
        el,
        "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "param" | "source" | "track" | "wbr" | "import"
    )
}
