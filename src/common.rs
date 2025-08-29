use crate::{dop::tokenizer::DopToken, tui::source_annotator::Annotation};
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

#[derive(Clone, Copy, PartialEq, Eq, Default, PartialOrd, Ord)]
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

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Trait for types that have a Range
pub trait Ranged {
    /// Returns the range of this item
    fn range(&self) -> Range;
    
    /// Returns true if the position lies within this item's range
    /// (start inclusive, end exclusive)
    fn contains_position(&self, position: Position) -> bool {
        self.range().contains_position(position)
    }
}

impl Ranged for Range {
    fn range(&self) -> Range {
        *self
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
            format!("Invalid variable name '{name}'. Variable names must match [a-z][a-z0-9_]*"),
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
    pub fn import_from_undefined_module(module: &str, range: Range) -> Self {
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

    pub fn expected_token(token: &DopToken, range: Range) -> Self {
        Self::new(format!("Expected token '{token}'"), range)
    }

    pub fn unexpected_token(token: &DopToken, range: Range) -> Self {
        Self::new(format!("Unexpected token '{token}'"), range)
    }

    pub fn expected_variable_name(range: Range) -> Self {
        Self::new("Expected variable name".to_string(), range)
    }

    pub fn expected_property_name(range: Range) -> Self {
        Self::new("Expected property name".to_string(), range)
    }

    pub fn duplicate_argument(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate argument '{name}'"), range)
    }

    pub fn duplicate_property(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate property '{name}'"), range)
    }
}

impl Ranged for RangeError {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotation for RangeError {
    fn message(&self) -> String {
        self.message.clone()
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
