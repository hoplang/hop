use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("slot-default is already defined")]
    SlotIsAlreadyDefined { range: DocumentRange },

    #[error("Unmatched </{tag}>")]
    UnmatchedClosingTag {
        tag: StringSpan,
        range: DocumentRange,
    },

    #[error("Unclosed <{tag}>")]
    UnclosedTag {
        tag: StringSpan,
        range: DocumentRange,
    },

    #[error("<{tag}> should not be closed using a closing tag")]
    ClosedVoidTag {
        tag: StringSpan,
        range: DocumentRange,
    },

    #[error("Unrecognized hop tag: <{tag}>")]
    UnrecognizedHopTag {
        tag: StringSpan,
        range: DocumentRange,
    },

    #[error("<{tag_name}> is missing required attribute {attr}")]
    MissingRequiredAttribute {
        tag_name: StringSpan,
        attr: String,
        range: DocumentRange,
    },

    #[error(
        "Invalid component name '{tag_name}'. Component names must contain a dash and not start or end with one"
    )]
    InvalidComponentName {
        tag_name: StringSpan,
        range: DocumentRange,
    },

    #[error("Component {component_name} is already defined")]
    ComponentIsAlreadyDefined {
        component_name: StringSpan,
        range: DocumentRange,
    },

    #[error("Duplicate attribute '{name}'")]
    DuplicateAttribute {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Import paths must start with '@/' where '@' indicates the root directory")]
    InvalidImportPath { range: DocumentRange },

    #[error("{error}")]
    InvalidModuleName {
        error: crate::hop::module_name::InvalidModuleNameError,
        range: DocumentRange,
    },

    #[error("Unmatched {ch}")]
    UnmatchedCharacter { ch: char, range: DocumentRange },

    #[error("Attribute {attr_name} must be statically known")]
    AttributeMustBeStaticallyKnown {
        attr_name: StringSpan,
        range: DocumentRange,
    },

    #[error("{message}")]
    GenericError {
        message: String,
        range: DocumentRange,
    },
}

impl ParseError {
    pub fn new(message: String, range: DocumentRange) -> Self {
        ParseError::GenericError { message, range }
    }
}

impl Ranged for ParseError {
    fn range(&self) -> &DocumentRange {
        match self {
            ParseError::SlotIsAlreadyDefined { range }
            | ParseError::UnmatchedClosingTag { range, .. }
            | ParseError::UnmatchedCharacter { range, .. }
            | ParseError::UnclosedTag { range, .. }
            | ParseError::ClosedVoidTag { range, .. }
            | ParseError::UnrecognizedHopTag { range, .. }
            | ParseError::MissingRequiredAttribute { range, .. }
            | ParseError::InvalidComponentName { range, .. }
            | ParseError::ComponentIsAlreadyDefined { range, .. }
            | ParseError::DuplicateAttribute { range, .. }
            | ParseError::InvalidImportPath { range }
            | ParseError::InvalidModuleName { range, .. }
            | ParseError::AttributeMustBeStaticallyKnown { range, .. }
            | ParseError::GenericError { range, .. } => range,
        }
    }
}
