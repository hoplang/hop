use crate::dop::DopParameter;
use crate::range::{Range, Ranged};
use std::collections::BTreeMap;
use std::fmt::{self, Display};

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
                    .map(|(_, p)| p.var_name.as_str())
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

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
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

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
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
