use std::collections::HashMap;

/// HopMode influences the runtime value of the global variable HOP_MODE which
/// will be set to 'build' when running `hop build` and 'dev' when running
/// `hop dev`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HopMode {
    Build,
    Dev,
}

impl HopMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            HopMode::Build => "build",
            HopMode::Dev => "dev",
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

    pub fn missing_required_attribute(tag: &str, attr: &str, range: Range) -> Self {
        Self::new(
            format!("<{tag}> is missing required attribute {attr}"),
            range,
        )
    }

    pub fn invalid_variable_name(name: &str, range: Range) -> Self {
        Self::new(
            format!("Invalid variable name '{name}'. Variable names must match [a-z][a-z0-9]*"),
            range,
        )
    }
    pub fn invalid_component_name(name: &str, range: Range) -> Self {
        Self::new(
            format!(
                "Invalid component name '{name}'. Component names must contain a dash and not start or end with one"
            ),
            range,
        )
    }

    // Typechecker error functions
    pub fn undefined_component(component: &str, range: Range) -> Self {
        Self::new(format!("Component {component} is not defined"), range)
    }
    pub fn undeclared_component(module: &str, component: &str, range: Range) -> Self {
        Self::new(
            format!("Module {module} does not declare a component named {component}"),
            range,
        )
    }
    pub fn undefined_module(module: &str, range: Range) -> Self {
        Self::new(format!("Module {module} is not defined"), range)
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

    pub fn undefined_slot(slot: &str, component: &str, range: Range) -> Self {
        Self::new(
            format!("Slot '{slot}' is not defined in component {component}"),
            range,
        )
    }

    pub fn slot_already_defined(slot: &str, range: Range) -> Self {
        Self::new(format!("Slot '{slot}' is already defined"), range)
    }
    pub fn default_slot_with_other_slots(range: Range) -> Self {
        Self::new(
            "When using slot-default, it must be the only slot in the component".to_string(),
            range,
        )
    }

    pub fn unused_import(component: &str, range: Range) -> Self {
        Self::new(format!("Unused import: {component}"), range)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: String,
    pub value: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopVarName {
    pub value: String,
}

impl DopVarName {
    pub fn new(value: String) -> Option<Self> {
        let mut chars = value.chars();
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
        Some(DopVarName { value })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarNameAttr {
    pub var_name: DopVarName,
    pub range: Range,
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
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DopExpr {
    Variable(String),
    PropertyAccess(Box<DopExpr>, String),
    StringLiteral(String),
    BooleanLiteral(bool),
    BinaryOp(Box<DopExpr>, BinaryOp, Box<DopExpr>),
    UnaryOp(UnaryOp, Box<DopExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopAttribute {
    pub name: String,
    pub expression: DopExpr,
    pub range: Range,
}

impl DopAttribute {
    pub fn new(name: String, expression: DopExpr, range: Range) -> Self {
        DopAttribute {
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
    Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub attributes: Vec<Attribute>,
    pub expression: Option<String>,
    pub range: Range,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, attributes: Vec<Attribute>, range: Range) -> Self {
        Token {
            kind,
            value,
            attributes,
            expression: None,
            range,
        }
    }

    pub fn new_with_expression(
        kind: TokenKind,
        value: String,
        attributes: Vec<Attribute>,
        expression: Option<String>,
        range: Range,
    ) -> Self {
        Token {
            kind,
            value,
            attributes,
            expression,
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
pub struct ComponentReferenceNode {
    pub component: String,
    pub params_attr: Option<DopAttribute>,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfNode {
    pub condition: DopExpr,
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
pub struct ComponentDefinitionNode {
    pub name: String,
    pub params_as_attr: Option<VarNameAttr>,
    pub as_attr: Option<Attribute>,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<Node>,
    pub preview: Option<Vec<Node>>,
    pub entrypoint: bool,
    pub slots: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeHTMLNode {
    pub tag_name: String,
    pub attributes: Vec<Attribute>,
    pub range: Range,
    pub children: Vec<Node>,
    pub set_attributes: Vec<DopAttribute>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorNode {
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SlotDefinitionNode {
    pub name: String,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SlotReferenceNode {
    pub name: String,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderNode {
    pub file_attr: Attribute,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XExecNode {
    pub cmd_attr: Attribute,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XRawNode {
    pub trim: bool,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub var_name: DopVarName,
    pub array_expr: DopExpr,
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextExpressionNode {
    pub expression: DopExpr,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Doctype(DoctypeNode),
    Text(TextNode),
    TextExpression(TextExpressionNode),
    ComponentReference(ComponentReferenceNode),
    SlotDefinition(SlotDefinitionNode),
    SlotReference(SlotReferenceNode),
    If(IfNode),
    For(ForNode),
    NativeHTML(NativeHTMLNode),
    Error(ErrorNode),
    XExec(XExecNode),
    XRaw(XRawNode),
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

impl<V> Environment<V> {
    pub fn new() -> Self {
        Environment {
            entries: HashMap::new(),
            operations: Vec::new(),
        }
    }

    // Bind the key to the given value
    // Returns true if successful, false if the variable already exists (shadowing not allowed)
    pub fn push(&mut self, key: String, value: V) -> bool {
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
    pub fn lookup(&mut self, key: &str) -> Option<&V> {
        if let Some(entry) = self.entries.get_mut(key) {
            entry.accessed = true;
            Some(&entry.value)
        } else {
            None
        }
    }
}

impl<V> Default for Environment<V> {
    fn default() -> Self {
        Self::new()
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
