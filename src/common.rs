use crate::dop::DopParameter;
use crate::range::string_cursor::{Spanned, StringSpan};
use std::collections::BTreeMap;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: StringSpan,
}

impl TypeError {
    pub fn new(message: String, span: StringSpan) -> Self {
        TypeError { message, span }
    }

    pub fn undefined_component(component: &str, span: StringSpan) -> Self {
        Self::new(format!("Component {component} is not defined"), span)
    }

    pub fn undeclared_component(module: &str, component: &str, span: StringSpan) -> Self {
        Self::new(
            format!("Module {module} does not declare a component named {component}"),
            span,
        )
    }

    pub fn import_from_undefined_module(module: &str, span: StringSpan) -> Self {
        Self::new(format!("Module {module} is not defined"), span)
    }

    pub fn unused_variable(var: &str, span: StringSpan) -> Self {
        Self::new(format!("Unused variable {var}"), span)
    }

    pub fn variable_is_already_defined(var: &str, span: StringSpan) -> Self {
        Self::new(format!("Variable {var} is already defined"), span)
    }

    pub fn undefined_slot(component: &str, span: StringSpan) -> Self {
        Self::new(
            format!("Component {component} does not have a slot-default"),
            span,
        )
    }

    pub fn import_cycle(
        importer: &str,
        imported: &str,
        cycle: &[String],
        span: StringSpan,
    ) -> Self {
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
            span,
        )
    }

    pub fn expected_boolean_condition(found: &str, span: StringSpan) -> Self {
        Self::new(format!("Expected boolean condition, got {}", found), span)
    }

    pub fn missing_required_parameter(param: &str, span: StringSpan) -> Self {
        Self::new(format!("Missing required parameter '{}'", param), span)
    }

    pub fn missing_arguments(params: &BTreeMap<String, DopParameter>, span: StringSpan) -> Self {
        Self::new(
            format!(
                "Component requires arguments: {}",
                params
                    .iter()
                    .map(|(_, p)| p.var_name.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            span,
        )
    }

    pub fn unexpected_arguments(span: StringSpan) -> Self {
        Self::new("Component does not accept arguments".to_string(), span)
    }

    pub fn unexpected_argument(arg: &str, span: StringSpan) -> Self {
        Self::new(format!("Unexpected argument '{}'", arg), span)
    }

    pub fn argument_is_incompatible(
        expected: &str,
        found: &str,
        arg_name: &str,
        span: StringSpan,
    ) -> Self {
        Self::new(
            format!(
                "Argument '{}' of type {} is incompatible with expected type {}",
                arg_name, found, expected
            ),
            span,
        )
    }

    pub fn expected_string_attribute(found: &str, span: StringSpan) -> Self {
        Self::new(format!("Expected string attribute, got {}", found), span)
    }

    pub fn cannot_iterate_empty_array(span: StringSpan) -> Self {
        Self::new(
            "Cannot iterate over an empty array with unknown element type".to_string(),
            span,
        )
    }

    pub fn cannot_iterate_over(typ: &str, span: StringSpan) -> Self {
        Self::new(format!("Can not iterate over {}", typ), span)
    }

    pub fn expected_string_expression(found: &str, span: StringSpan) -> Self {
        Self::new(
            format!("Expected string for text expression, got {}", found),
            span,
        )
    }

    pub fn undefined_variable(name: &str, span: StringSpan) -> Self {
        Self::new(format!("Undefined variable: {}", name), span)
    }

    pub fn property_not_found_in_object(property: &str, span: StringSpan) -> Self {
        Self::new(format!("Property {} not found in object", property), span)
    }

    pub fn cannot_use_as_object(typ: &str, span: StringSpan) -> Self {
        Self::new(format!("{} can not be used as an object", typ), span)
    }

    pub fn cannot_compare_types(left: &str, right: &str, span: StringSpan) -> Self {
        Self::new(format!("Can not compare {} to {}", left, right), span)
    }

    pub fn negation_requires_boolean(span: StringSpan) -> Self {
        Self::new(
            "Negation operator can only be applied to boolean values".to_string(),
            span,
        )
    }

    pub fn array_type_mismatch(expected: &str, found: &str, span: StringSpan) -> Self {
        Self::new(
            format!(
                "Array elements must all have the same type, found {} and {}",
                expected, found
            ),
            span,
        )
    }
}

impl Spanned for TypeError {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}


impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: StringSpan,
}

impl ParseError {
    pub fn new(message: String, span: StringSpan) -> Self {
        ParseError { message, span }
    }

    pub fn slot_is_already_defined(span: StringSpan) -> Self {
        Self::new("slot-default is already defined".to_string(), span)
    }

    pub fn unmatched_closing_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(format!("Unmatched </{tag}>"), span)
    }

    pub fn unexpected_end_of_expression(span: StringSpan) -> Self {
        Self::new("Unexpected end of expression".to_string(), span)
    }

    pub fn missing_attribute_value(span: StringSpan) -> Self {
        Self::new("Missing attribute value".to_string(), span)
    }

    pub fn unclosed_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(format!("Unclosed <{tag}>"), span)
    }

    pub fn closed_void_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(
            format!("<{tag}> should not be closed using a closing tag"),
            span,
        )
    }

    pub fn unrecognized_hop_tag(tag: &str, span: StringSpan) -> Self {
        Self::new(format!("Unrecognized hop tag: <{tag}>"), span)
    }

    pub fn missing_required_attribute(tag: &str, attr: &str, span: StringSpan) -> Self {
        Self::new(
            format!("<{tag}> is missing required attribute {attr}"),
            span,
        )
    }

    pub fn invalid_component_name(name: &str, span: StringSpan) -> Self {
        Self::new(
            format!(
                "Invalid component name '{name}'. Component names must contain a dash and not start or end with one"
            ),
            span,
        )
    }

    pub fn component_is_already_defined(name: &str, span: StringSpan) -> Self {
        Self::new(format!("Component {name} is already defined"), span)
    }

    pub fn duplicate_attribute(name: &str, span: StringSpan) -> Self {
        Self::new(format!("Duplicate attribute '{name}'"), span)
    }
}

impl Spanned for ParseError {
    fn span(&self) -> &StringSpan {
        &self.span
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
