use crate::span::string_cursor::{Spanned, StringSpan};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("slot-default is already defined")]
    SlotIsAlreadyDefined { span: StringSpan },

    #[error("Unmatched </{tag}>")]
    UnmatchedClosingTag { tag: String, span: StringSpan },

    #[error("Unexpected end of expression")]
    UnexpectedEndOfExpression { span: StringSpan },

    #[error("Missing attribute value")]
    MissingAttributeValue { span: StringSpan },

    #[error("Unclosed <{tag}>")]
    UnclosedTag { tag: String, span: StringSpan },

    #[error("<{tag}> should not be closed using a closing tag")]
    ClosedVoidTag { tag: String, span: StringSpan },

    #[error("Unrecognized hop tag: <{tag}>")]
    UnrecognizedHopTag { tag: String, span: StringSpan },

    #[error("<{tag_name}> is missing required attribute {attr}")]
    MissingRequiredAttribute {
        tag_name: String,
        attr: String,
        span: StringSpan,
    },

    #[error(
        "Invalid component name '{tag_name}'. Component names must contain a dash and not start or end with one"
    )]
    InvalidComponentName { tag_name: String, span: StringSpan },

    #[error("Component {component_name} is already defined")]
    ComponentIsAlreadyDefined {
        component_name: String,
        span: StringSpan,
    },

    #[error("Duplicate attribute '{name}'")]
    DuplicateAttribute { name: String, span: StringSpan },

    #[error("Import paths must start with '@/' where '@' indicates the root directory")]
    InvalidImportPath { span: StringSpan },

    #[error("{error}")]
    InvalidModuleName {
        error: crate::hop::module_name::InvalidModuleNameError,
        span: StringSpan,
    },

    #[error("{message}")]
    GenericError { message: String, span: StringSpan },
}

impl ParseError {
    pub fn new(message: String, span: StringSpan) -> Self {
        ParseError::GenericError { message, span }
    }

    pub fn slot_is_already_defined(span: StringSpan) -> Self {
        ParseError::SlotIsAlreadyDefined { span }
    }

    pub fn unmatched_closing_tag(tag: &str, span: StringSpan) -> Self {
        ParseError::UnmatchedClosingTag {
            tag: tag.to_string(),
            span,
        }
    }

    pub fn unexpected_end_of_expression(span: StringSpan) -> Self {
        ParseError::UnexpectedEndOfExpression { span }
    }

    pub fn missing_attribute_value(span: StringSpan) -> Self {
        ParseError::MissingAttributeValue { span }
    }

    pub fn unclosed_tag(tag: &str, span: StringSpan) -> Self {
        ParseError::UnclosedTag {
            tag: tag.to_string(),
            span,
        }
    }

    pub fn closed_void_tag(tag: &str, span: StringSpan) -> Self {
        ParseError::ClosedVoidTag {
            tag: tag.to_string(),
            span,
        }
    }

    pub fn unrecognized_hop_tag(tag: &str, span: StringSpan) -> Self {
        ParseError::UnrecognizedHopTag {
            tag: tag.to_string(),
            span,
        }
    }

    pub fn missing_required_attribute(tag_name: StringSpan, attr: &str) -> Self {
        ParseError::MissingRequiredAttribute {
            tag_name: tag_name.to_string(),
            attr: attr.to_string(),
            span: tag_name,
        }
    }

    pub fn invalid_component_name(tag_name: StringSpan) -> Self {
        ParseError::InvalidComponentName {
            tag_name: tag_name.to_string(),
            span: tag_name,
        }
    }

    pub fn component_is_already_defined(component_attr_value: StringSpan) -> Self {
        ParseError::ComponentIsAlreadyDefined {
            component_name: component_attr_value.to_string(),
            span: component_attr_value,
        }
    }

    pub fn duplicate_attribute(name: &str, span: StringSpan) -> Self {
        ParseError::DuplicateAttribute {
            name: name.to_string(),
            span,
        }
    }

    pub fn invalid_import_path(span: StringSpan) -> Self {
        ParseError::InvalidImportPath { span }
    }

    pub fn invalid_module_name(
        span: StringSpan,
        error: crate::hop::module_name::InvalidModuleNameError,
    ) -> Self {
        ParseError::InvalidModuleName { error, span }
    }
}

impl Spanned for ParseError {
    fn span(&self) -> &StringSpan {
        match self {
            ParseError::SlotIsAlreadyDefined { span }
            | ParseError::UnmatchedClosingTag { span, .. }
            | ParseError::UnexpectedEndOfExpression { span }
            | ParseError::MissingAttributeValue { span }
            | ParseError::UnclosedTag { span, .. }
            | ParseError::ClosedVoidTag { span, .. }
            | ParseError::UnrecognizedHopTag { span, .. }
            | ParseError::MissingRequiredAttribute { span, .. }
            | ParseError::InvalidComponentName { span, .. }
            | ParseError::ComponentIsAlreadyDefined { span, .. }
            | ParseError::DuplicateAttribute { span, .. }
            | ParseError::InvalidImportPath { span }
            | ParseError::InvalidModuleName { span, .. }
            | ParseError::GenericError { span, .. } => span,
        }
    }
}
