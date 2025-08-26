use crate::dop::{DopExpr, parser::DopVarName};
use std::collections::HashMap;
use std::fmt;

/// Represents a position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    /// Line number (1-based)
    pub line: usize,
    /// Byte column within the line (1-based)
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }
}

impl Default for Position {
    fn default() -> Self {
        Position { line: 1, column: 1 }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
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

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
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

    pub fn component_is_already_defined(component: &str, range: Range) -> Self {
        Self::new(format!("Component {component} is already defined"), range)
    }

    pub fn unused_variable(var: &str, range: Range) -> Self {
        Self::new(format!("Unused variable {var}"), range)
    }

    pub fn variable_is_already_defined(var: &str, range: Range) -> Self {
        Self::new(format!("Variable {var} is already defined"), range)
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
    pub value_range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopVarNameAttribute {
    pub var_name: DopVarName,
    pub type_annotation: crate::dop::DopType,
    pub range: Range,
}

impl Attribute {
    pub fn new(name: String, value: String, range: Range, value_range: Range) -> Self {
        Attribute {
            name,
            value,
            range,
            value_range,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DopExprAttribute {
    pub name: String,
    pub expression: DopExpr,
    pub range: Range,
}

impl DopExprAttribute {
    pub fn new(name: String, expression: DopExpr, range: Range) -> Self {
        DopExprAttribute {
            name,
            expression,
            range,
        }
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
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub params: Option<(DopExpr, Range)>,
    pub attributes: Vec<Attribute>,
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
    pub opening_name_range: Range,
    pub closing_name_range: Option<Range>,
    pub params: Vec<DopVarNameAttribute>,
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
    pub set_attributes: Vec<DopExprAttribute>,
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
    pub var_name: (DopVarName, Range),
    pub array_expr: (DopExpr, Range),
    pub range: Range,
    pub children: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct XLoadJsonNode {
    pub file_attr: Attribute,
    pub as_attr: Attribute,
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
    XLoadJson(XLoadJsonNode),
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
