use crate::{
    dop::{DopParameter, tokenizer::DopToken},
    tui::source_annotator::Annotated,
};
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
    pub fn contains(&self, position: Position) -> bool {
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
    fn contains(&self, position: Position) -> bool {
        self.range().contains(position)
    }
}

impl Ranged for Range {
    fn range(&self) -> Range {
        *self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeError {
    pub message: String,
    pub range: Range,
}

impl TypeError {
    pub fn new(message: String, range: Range) -> Self {
        TypeError { message, range }
    }

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

    pub fn unused_variable(var: &str, range: Range) -> Self {
        Self::new(format!("Unused variable {var}"), range)
    }

    pub fn variable_is_already_defined(var: &str, range: Range) -> Self {
        Self::new(format!("Variable {var} is already defined"), range)
    }

    pub fn undefined_slot(component: &str, range: Range) -> Self {
        Self::new(
            format!("Component {component} does not have a slot-default"),
            range,
        )
    }

    pub fn circular_import(importer: &str, imported: &str, cycle: &[String], range: Range) -> Self {
        let cycle_display = if let Some(first) = cycle.first() {
            format!("{} → {}", cycle.join(" → "), first)
        } else {
            cycle.join(" → ")
        };

        Self::new(
            format!(
                "Circular import detected: {} imports {} which creates a dependency cycle: {}",
                importer, imported, cycle_display
            ),
            range,
        )
    }

    pub fn expected_boolean_condition(found: &str, range: Range) -> Self {
        Self::new(format!("Expected boolean condition, got {}", found), range)
    }

    pub fn missing_required_parameter(param: &str, range: Range) -> Self {
        Self::new(format!("Missing required parameter '{}'", param), range)
    }

    pub fn missing_arguments(params: &[DopParameter], range: Range) -> Self {
        Self::new(
            format!(
                "Component requires arguments: {}",
                params
                    .iter()
                    .map(|p| p.var_name.value.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            range,
        )
    }

    pub fn unexpected_arguments(range: Range) -> Self {
        Self::new("Component does not accept arguments".to_string(), range)
    }

    pub fn unexpected_argument(arg: &str, range: Range) -> Self {
        Self::new(format!("Unexpected argument '{}'", arg), range)
    }

    pub fn argument_is_incompatible(
        expected: &str,
        found: &str,
        arg_name: &str,
        range: Range,
    ) -> Self {
        Self::new(
            format!(
                "Argument '{}' of type {} is incompatible with expected type {}",
                arg_name, found, expected
            ),
            range,
        )
    }

    pub fn expected_string_attribute(found: &str, range: Range) -> Self {
        Self::new(format!("Expected string attribute, got {}", found), range)
    }

    pub fn cannot_iterate_empty_array(range: Range) -> Self {
        Self::new(
            "Cannot iterate over an empty array with unknown element type".to_string(),
            range,
        )
    }

    pub fn cannot_iterate_over(typ: &str, range: Range) -> Self {
        Self::new(format!("Can not iterate over {}", typ), range)
    }

    pub fn expected_string_expression(found: &str, range: Range) -> Self {
        Self::new(
            format!("Expected string for text expression, got {}", found),
            range,
        )
    }

    pub fn undefined_variable(name: &str, range: Range) -> Self {
        Self::new(format!("Undefined variable: {}", name), range)
    }

    pub fn property_not_found_in_object(property: &str, range: Range) -> Self {
        Self::new(format!("Property {} not found in object", property), range)
    }

    pub fn cannot_use_as_object(typ: &str, range: Range) -> Self {
        Self::new(format!("{} can not be used as an object", typ), range)
    }

    pub fn cannot_compare_types(left: &str, right: &str, range: Range) -> Self {
        Self::new(format!("Can not compare {} to {}", left, right), range)
    }

    pub fn negation_requires_boolean(range: Range) -> Self {
        Self::new(
            "Negation operator can only be applied to boolean values".to_string(),
            range,
        )
    }

    pub fn array_type_mismatch(expected: &str, found: &str, range: Range) -> Self {
        Self::new(
            format!(
                "Array elements must all have the same type, found {} and {}",
                expected, found
            ),
            range,
        )
    }
}

impl Ranged for TypeError {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for TypeError {
    fn message(&self) -> String {
        self.message.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub range: Range,
}

impl ParseError {
    pub fn new(message: String, range: Range) -> Self {
        ParseError { message, range }
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

    pub fn duplicate_parameter(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate parameter '{name}'"), range)
    }

    pub fn duplicate_property(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate property '{name}'"), range)
    }
}

impl Ranged for ParseError {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for ParseError {
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
// (native HTML nodes) as well as `import` and `slot-default` (defined by hop).
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
            | "slot-default"
    )
}
