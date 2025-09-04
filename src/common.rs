use crate::{dop::DopParameter, tui::source_annotator::Annotated};
use std::{cmp, collections::BTreeMap, fmt, iter::Peekable, str::Chars};

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

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        debug_assert!(start < end, "start must be less than end in Range::new");
        Range { start, end }
    }

    pub fn extend_to(&self, range: Range) -> Range {
        Range::new(self.start, range.end)
    }

    pub fn contains(&self, position: Position) -> bool {
        position >= self.start && position < self.end
    }

    // returns the intersection of two ranges, or None if they don't overlap
    pub fn intersection(&self, other: &Range) -> Option<Range> {
        let start = cmp::max(self.start, other.start);
        let end = cmp::min(self.end, other.end);

        // If start >= end, there's no intersection
        if start >= end {
            None
        } else {
            Some(Range::new(start, end))
        }
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

#[derive(Clone)]
struct PositionedChars<'a> {
    chars: Chars<'a>,
    position: Position,
}

impl<'a> PositionedChars<'a> {
    fn new(input: &'a str, start_pos: Position) -> Self {
        Self {
            chars: input.chars(),
            position: start_pos,
        }
    }
}

impl Iterator for PositionedChars<'_> {
    type Item = (char, Range);
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.position;
        self.chars.next().map(|ch| {
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += ch.len_utf8();
            }
            (ch, Range::new(start, self.position))
        })
    }
}

pub struct StrCursor<'a> {
    chars: Peekable<PositionedChars<'a>>,
}

impl<'a> StrCursor<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: PositionedChars::new(input, Position::default()).peekable(),
        }
    }
    pub fn new_with_position(input: &'a str, pos: Position) -> Self {
        Self {
            chars: PositionedChars::new(input, pos).peekable(),
        }
    }
    pub fn peek(&mut self) -> Option<(char, Range)> {
        self.chars.peek().cloned()
    }

    pub fn peek_n(&mut self, n: usize) -> Option<(String, Range)> {
        let mut clone = self.chars.clone();
        let mut result: Option<(String, Range)> = None;
        for _ in 0..n {
            let (ch, range) = clone.next()?;
            match &mut result {
                Some(result) => {
                    result.0.push(ch);
                    result.1 = result.1.extend_to(range);
                }
                None => {
                    result = Some((String::from(ch), range));
                }
            }
        }
        result
    }
    pub fn next_if(&mut self, func: impl FnOnce(&(char, Range)) -> bool) -> Option<(char, Range)> {
        self.chars.next_if(func)
    }
    pub fn next_n(&mut self, n: usize) -> Option<(String, Range)> {
        let mut result: Option<(String, Range)> = None;
        for _ in 0..n {
            let (ch, range) = self.next()?;
            match &mut result {
                Some(result) => {
                    result.0.push(ch);
                    result.1 = result.1.extend_to(range);
                }
                None => {
                    result = Some((String::from(ch), range));
                }
            }
        }
        result
    }
    pub fn next_while(&mut self, func: impl Fn(&(char, Range)) -> bool) -> Option<(String, Range)> {
        let mut result: Option<(String, Range)> = None;
        loop {
            match self.chars.peek() {
                Some(matched) if func(matched) => {
                    let (ch, range) = self.next().unwrap();
                    match &mut result {
                        Some(result) => {
                            result.0.push(ch);
                            result.1 = result.1.extend_to(range);
                        }
                        None => {
                            result = Some((String::from(ch), range));
                        }
                    }
                }
                _ => {
                    return result;
                }
            };
        }
    }
    pub fn matches_str(&mut self, expected: &str) -> bool {
        let mut actual_chars = self.chars.clone();
        for expected_char in expected.chars() {
            match actual_chars.peek() {
                None => return false,
                Some((actual_char, _)) => {
                    if *actual_char != expected_char {
                        return false;
                    }
                }
            }
            actual_chars.next();
        }
        true
    }
}

impl Iterator for StrCursor<'_> {
    type Item = (char, Range);
    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
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

    pub fn import_cycle(importer: &str, imported: &str, cycle: &[String], range: Range) -> Self {
        let cycle_display = if let Some(first) = cycle.first() {
            format!("{} → {}", cycle.join(" → "), first)
        } else {
            cycle.join(" → ")
        };

        Self::new(
            format!(
                "Import cycle: {} imports from {} which creates a dependency cycle: {}",
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

    pub fn missing_arguments(params: &BTreeMap<String, DopParameter>, range: Range) -> Self {
        Self::new(
            format!(
                "Component requires arguments: {}",
                params
                    .iter()
                    .map(|(_, p)| p.var_name.value.clone())
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

    pub fn slot_is_already_defined(range: Range) -> Self {
        Self::new("slot-default is already defined".to_string(), range)
    }

    pub fn unmatched_closing_tag(tag: &str, range: Range) -> Self {
        Self::new(format!("Unmatched </{tag}>"), range)
    }

    pub fn unexpected_end_of_expression(range: Range) -> Self {
        Self::new("Unexpected end of expression".to_string(), range)
    }

    pub fn missing_attribute_value(range: Range) -> Self {
        Self::new("Missing attribute value".to_string(), range)
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

    pub fn unrecognized_hop_tag(tag: &str, range: Range) -> Self {
        Self::new(format!("Unrecognized hop tag: <{tag}>"), range)
    }

    pub fn missing_required_attribute(tag: &str, attr: &str, range: Range) -> Self {
        Self::new(
            format!("<{tag}> is missing required attribute {attr}"),
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

    pub fn component_is_already_defined(name: &str, range: Range) -> Self {
        Self::new(format!("Component {name} is already defined"), range)
    }

    pub fn duplicate_attribute(name: &str, range: Range) -> Self {
        Self::new(format!("Duplicate attribute '{name}'"), range)
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

// Escape HTML special characters to prevent XSS
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
