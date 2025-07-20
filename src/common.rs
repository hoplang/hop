use serde_json::{Value, json};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Object(HashMap<String, Type>, i32),
    Array(Box<Type>),
    Bool,
    String,
    Void,
    TypeVar(i32),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Object(properties, _rest) => {
                write!(f, "{{")?;
                let mut first = true;

                // Collect and sort properties by key
                let mut sorted_props: Vec<_> = properties.iter().collect();
                sorted_props.sort_by_key(|(key, _)| *key);

                for (key, value) in sorted_props {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Type::Array(inner_type) => write!(f, "{}[]", inner_type),
            Type::Bool => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::TypeVar(id) => write!(f, "?t{}", id),
        }
    }
}

impl Type {
    /// Convert a Type to a JSON Schema representation
    pub fn to_json_schema(&self) -> Value {
        match self {
            Type::Object(properties, _rest) => {
                let mut schema_properties = serde_json::Map::new();
                let mut required = Vec::new();

                for (key, value) in properties {
                    schema_properties.insert(key.clone(), value.to_json_schema());
                    required.push(key.clone());
                }

                json!({
                    "type": "object",
                    "properties": schema_properties,
                    "required": required,
                    "additionalProperties": false
                })
            }
            Type::Array(inner_type) => {
                json!({
                    "type": "array",
                    "items": inner_type.to_json_schema()
                })
            }
            Type::Bool => {
                json!({
                    "type": "boolean"
                })
            }
            Type::String => {
                json!({
                    "type": "string"
                })
            }
            Type::Void => {
                json!({
                    "type": "null"
                })
            }
            Type::TypeVar(_id) => {
                // Type variables are placeholders during type inference
                // In JSON Schema, we represent them as allowing any type
                json!({})
            }
        }
    }
}

/// Represents a position in source code with UTF-8 byte-based tracking
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position {
    /// Line number (1-based)
    pub line: usize,
    /// Byte column within the line (1-based, incremented by UTF-8 byte length)
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        Range { start, end }
    }

    // returns true if the position lies in the range
    // where start is inclusive and end is exclusive
    pub fn contains_position(&self, position: Position) -> bool {
        (position.line > self.start.line
            || (position.line == self.start.line && position.column >= self.start.column))
            && (position.line < self.end.line
                || (position.line == self.end.line && position.column < self.end.column))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeError {
    pub message: String,
    pub range: Range,
}

impl RangeError {
    pub fn new(message: String, range: Range) -> Self {
        RangeError { message, range }
    }

    // Parser error functions
    pub fn unmatched_closing_tag(tag: &str, range: Range) -> Self {
        Self::new(format!("Unmatched </{tag}>"), range)
    }

    pub fn unclosed_tag(tag: &str, range: Range) -> Self {
        Self::new(format!("Unclosed <{tag}>"), range)
    }

    pub fn closed_void_tag(tag: &str, range: Range) -> Self {
        Self::new(
            format!("<{tag}> should not be closed using a closing tag"),
            range,
        )
    }

    pub fn unexpected_tag_outside_root(tag: &str, range: Range) -> Self {
        Self::new(format!("<{tag}> must be placed at module root"), range)
    }

    pub fn unexpected_tag_at_root(tag: &str, range: Range) -> Self {
        Self::new(format!("Unexpected <{tag}> at module root"), range)
    }

    pub fn unexpected_doctype_at_root(range: Range) -> Self {
        Self::new("Unexpected doctype at module root".to_string(), range)
    }

    pub fn missing_required_attribute(tag: &str, attr: &str, range: Range) -> Self {
        Self::new(
            format!("<{tag}> is missing required attribute {attr}"),
            range,
        )
    }

    pub fn empty_expression(range: Range) -> Self {
        Self::new("Empty expression".to_string(), range)
    }

    pub fn invalid_variable_name(name: &str, range: Range) -> Self {
        Self::new(
            format!("Invalid variable name '{name}'. Variable names must match [a-z][a-z0-9]*"),
            range,
        )
    }

    // Typechecker error functions
    pub fn undefined_component(component: &str, range: Range) -> Self {
        Self::new(format!("Component {component} not found"), range)
    }

    pub fn component_already_defined(component: &str, range: Range) -> Self {
        Self::new(format!("Component {component} is already defined"), range)
    }

    pub fn undefined_variable(var: &str, range: Range) -> Self {
        Self::new(format!("Undefined variable: {var}"), range)
    }

    pub fn unused_variable(var: &str, range: Range) -> Self {
        Self::new(format!("Unused variable {var}"), range)
    }

    pub fn variable_already_defined(var: &str, range: Range) -> Self {
        Self::new(format!("Variable {var} is already defined"), range)
    }

    pub fn unification_error(message: &str, range: Range) -> Self {
        Self::new(message.to_string(), range)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarNameAttr {
    pub value: String,
    pub range: Range,
}

impl VarNameAttr {
    pub fn new(attr: &Attribute) -> Option<Self> {
        let mut chars = attr.value.chars();
        if let Some(first_char) = chars.next() {
            if !first_char.is_ascii_lowercase() {
                return None;
            }
            if !chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit()) {
                return None;
            }
        } else {
            return None;
        }
        Some(VarNameAttr {
            value: attr.value.clone(),
            range: attr.range,
        })
    }
}

impl Attribute {
    pub fn new(name: String, value: String, range: Range) -> Self {
        Attribute { name, value, range }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(String),
    PropertyAccess(Box<Expression>, String),
    StringLiteral(String),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprAttribute {
    pub name: String,
    pub expression: Expression,
    pub range: Range,
}

impl ExprAttribute {
    pub fn new(name: String, expression: Expression, range: Range) -> Self {
        ExprAttribute {
            name,
            expression,
            range,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Doctype,
    StartTag,
    EndTag,
    SelfClosingTag,
    Text,
    Comment,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, attributes: Vec<Attribute>, range: Range) -> Self {
        Token {
            kind,
            value,
            attributes,
            range,
        }
    }

    pub fn get_attribute(&self, name: &str) -> Option<Attribute> {
        self.attributes
            .iter()
            .find(|attr| attr.name == name)
            .cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoctypeNode {
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextNode {
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderNode {
    pub component_attr: Attribute,
    pub params_attr: Option<ExprAttribute>,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub each_attr: ExprAttribute,
    pub as_attr: Option<VarNameAttr>,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CondNode {
    pub if_attr: ExprAttribute,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportNode {
    pub component_attr: Attribute,
    pub from_attr: Attribute,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentNode {
    pub name_attr: Attribute,
    pub params_as_attr: Option<VarNameAttr>,
    pub as_attr: Option<Attribute>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EntrypointNode {
    pub name_attr: Attribute,
    pub params_as_attr: Option<VarNameAttr>,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeHTMLNode {
    pub tag_name: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<Node>,
    pub inner_text_attr: Option<ExprAttribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorNode {
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Doctype(DoctypeNode),
    Text(TextNode),
    Render(RenderNode),
    For(ForNode),
    Cond(CondNode),
    Import(ImportNode),
    Component(ComponentNode),
    Entrypoint(EntrypointNode),
    NativeHTML(NativeHTMLNode),
    Error(ErrorNode),
}

// Environment entry that holds both value and access status
#[derive(Debug, Clone)]
struct EnvironmentEntry<T> {
    value: T,
    accessed: bool,
}

// Environment class for managing variable scope
#[derive(Debug, Clone)]
pub struct Environment<T> {
    entries: HashMap<String, EnvironmentEntry<T>>,
    operations: Vec<String>,
}

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Environment {
            entries: HashMap::new(),
            operations: Vec::new(),
        }
    }

    // Bind the key to the given value
    // Returns true if successful, false if the variable already exists (shadowing not allowed)
    pub fn push(&mut self, key: String, value: T) -> bool {
        if self.entries.contains_key(&key) {
            return false;
        }
        self.entries.insert(
            key.clone(),
            EnvironmentEntry {
                value,
                accessed: false,
            },
        );
        self.operations.push(key);
        true
    }

    // Undo the latest push operation and return whether the variable was accessed
    pub fn pop(&mut self) -> bool {
        if let Some(key) = self.operations.pop() {
            self.entries
                .remove(&key)
                .map(|entry| entry.accessed)
                .unwrap_or(false)
        } else {
            false
        }
    }

    // Look up a key in the environment
    pub fn lookup(&mut self, key: &str) -> Option<&T> {
        if let Some(entry) = self.entries.get_mut(key) {
            entry.accessed = true;
            Some(&entry.value)
        } else {
            None
        }
    }
}

// Escape HTML special characters to prevent XSS attacks
// Converts &, <, >, ", and ' to their HTML entity equivalents
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

// Return true if the string represents a void element
// The void elements are `area`, `base`, `br`, `col`, `embed`, `hr`,
// `img`, `input`, `link`, `meta`, `param`, `source`, `track` and `wbr`
// (native HTML nodes) as well as `import` (defined by hop).
pub fn is_void_element(el: &str) -> bool {
    matches!(
        el,
        "area"
            | "base"
            | "br"
            | "col"
            | "embed"
            | "hr"
            | "img"
            | "input"
            | "link"
            | "meta"
            | "param"
            | "source"
            | "track"
            | "wbr"
            | "import"
    )
}
