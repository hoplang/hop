use crate::span::string_cursor::{Spanned, StringSpan};
use std::fmt::{self, Display};

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

    pub fn missing_required_attribute(tag_name: StringSpan, attr: &str) -> Self {
        Self::new(
            format!("<{tag_name}> is missing required attribute {attr}"),
            tag_name,
        )
    }

    pub fn invalid_component_name(tag_name: StringSpan) -> Self {
        Self::new(
            format!(
                "Invalid component name '{tag_name}'. Component names must contain a dash and not start or end with one"
            ),
            tag_name,
        )
    }

    pub fn component_is_already_defined(component_attr_value: StringSpan) -> Self {
        Self::new(
            format!("Component {component_attr_value} is already defined"),
            component_attr_value,
        )
    }

    pub fn duplicate_attribute(name: &str, span: StringSpan) -> Self {
        Self::new(format!("Duplicate attribute '{name}'"), span)
    }

    pub fn invalid_import_path(span: StringSpan) -> Self {
        Self::new(
            format!("Import paths must start with '@/' where '@' indicates the root directory"),
            span,
        )
    }

    pub fn invalid_module_name(
        span: StringSpan,
        error: crate::hop::module_name::InvalidModuleNameError,
    ) -> Self {
        Self::new(format!("{}", error), span)
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