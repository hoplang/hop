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

    pub fn tokenizer_error(message: &str, range: Range) -> Self {
        Self::new(format!("Tokenizer error: {}", message), range)
    }

    // Typechecker error functions
    pub fn component_not_found(component: &str, range: Range) -> Self {
        Self::new(format!("Component {component} not found"), range)
    }

    pub fn undefined_variable(var: &str, range: Range) -> Self {
        Self::new(format!("Undefined variable: {var}"), range)
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

impl Attribute {
    pub fn new(name: String, value: String, range: Range) -> Self {
        Attribute { name, value, range }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprAttribute {
    pub name: String,
    pub segments: Vec<String>,
    pub range: Range,
}

impl ExprAttribute {
    pub fn new(name: String, segments: Vec<String>, range: Range) -> Self {
        ExprAttribute {
            name,
            segments,
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
    Error,
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

    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes.iter().any(|attr| attr.name == name)
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
    pub as_attr: Option<Attribute>,
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
    pub params_as_attr: Option<Attribute>,
    pub as_attr: Option<Attribute>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeHTMLNode {
    pub value: String,
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
    NativeHTML(NativeHTMLNode),
    Error(ErrorNode),
}

// Environment class for managing variable scope
#[derive(Debug, Clone)]
pub struct Environment<T> {
    values: HashMap<String, Vec<T>>,
    operations: Vec<String>,
}

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            operations: Vec::new(),
        }
    }

    // Bind the key to the given value
    pub fn push(&mut self, key: String, value: T) {
        self.values.entry(key.clone()).or_default().push(value);
        self.operations.push(key);
    }

    // Undo the latest push operation
    pub fn pop(&mut self) {
        if let Some(key) = self.operations.pop() {
            if let Some(stack) = self.values.get_mut(&key) {
                stack.pop();
                if stack.is_empty() {
                    self.values.remove(&key);
                }
            }
        }
    }

    // Returns true if the environment contains an entry with the given key
    pub fn has(&self, key: &str) -> bool {
        self.values.get(key).is_some_and(|stack| !stack.is_empty())
    }

    // Look up a key in the environment
    pub fn lookup(&self, key: &str) -> Option<&T> {
        self.values.get(key)?.last()
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

// Format a collection of range errors into a readable error message
// NOTE: This function is deprecated. Use ErrorFormatter::format_range_errors instead.
pub fn format_range_errors(message: &str, errors: &[RangeError]) -> String {
    let mut result = message.to_string();
    for error in errors {
        result.push_str(&format!(
            "\n  {}:{}:{}: {}",
            error.range.start.line, error.range.start.column, error.range.end.column, error.message
        ));
    }
    result
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
