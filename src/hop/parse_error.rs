use crate::{
    document::document_cursor::{DocumentRange, Ranged, StringSpan},
    dop,
};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
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

    #[error("{error}")]
    InvalidComponentName {
        error: crate::hop::component_name::InvalidComponentNameError,
        range: DocumentRange,
    },

    #[error("{name} is already defined")]
    TypeNameIsAlreadyDefined {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Duplicate attribute '{name}'")]
    DuplicateAttribute {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Unmatched {ch}")]
    UnmatchedCharacter { ch: char, range: DocumentRange },

    #[error("Unrecognized attribute '{attr_name}' on <{tag_name}>")]
    UnrecognizedAttribute {
        tag_name: StringSpan,
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

impl From<dop::ParseError> for ParseError {
    fn from(err: dop::ParseError) -> Self {
        Self::GenericError {
            message: err.to_string(),
            range: err.range().clone(),
        }
    }
}

impl Ranged for ParseError {
    fn range(&self) -> &DocumentRange {
        match self {
            ParseError::UnmatchedClosingTag { range, .. }
            | ParseError::UnmatchedCharacter { range, .. }
            | ParseError::UnclosedTag { range, .. }
            | ParseError::ClosedVoidTag { range, .. }
            | ParseError::InvalidComponentName { range, .. }
            | ParseError::TypeNameIsAlreadyDefined { range, .. }
            | ParseError::DuplicateAttribute { range, .. }
            | ParseError::UnrecognizedAttribute { range, .. }
            | ParseError::GenericError { range, .. } => range,
        }
    }
}
