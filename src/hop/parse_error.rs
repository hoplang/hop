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
