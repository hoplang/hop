use crate::{
    document::document_cursor::{CheapString, DocumentRange, Ranged},
    dop,
};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("Unmatched </{tag}>")]
    UnmatchedClosingTag {
        tag: CheapString,
        range: DocumentRange,
    },

    #[error("Unclosed <{tag}>")]
    UnclosedTag {
        tag: CheapString,
        range: DocumentRange,
    },

    #[error("<{tag}> should not be closed using a closing tag")]
    ClosedVoidTag {
        tag: CheapString,
        range: DocumentRange,
    },

    #[error("{error}")]
    InvalidComponentName {
        error: crate::hop::symbols::component_name::InvalidComponentNameError,
        range: DocumentRange,
    },

    #[error("{name} is already defined")]
    TypeNameIsAlreadyDefined {
        name: CheapString,
        range: DocumentRange,
    },

    #[error("Duplicate attribute '{name}'")]
    DuplicateAttribute {
        name: CheapString,
        range: DocumentRange,
    },

    #[error("Unmatched {ch}")]
    UnmatchedCharacter { ch: char, range: DocumentRange },

    #[error("Unrecognized attribute '{attr_name}' on <{tag_name}>")]
    UnrecognizedAttribute {
        tag_name: CheapString,
        attr_name: CheapString,
        range: DocumentRange,
    },

    #[error("{message}")]
    GenericError {
        message: String,
        range: DocumentRange,
    },

    #[error(
        "Invalid argument name '{name}' on <{tag_name}>: argument names cannot contain hyphens"
    )]
    InvalidArgumentName {
        tag_name: CheapString,
        name: CheapString,
        range: DocumentRange,
    },

    #[error(
        "Unexpected expression on <{tag_name}>: use attribute syntax instead (e.g. attr={{value}})"
    )]
    UnexpectedComponentExpression {
        tag_name: CheapString,
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
            | ParseError::GenericError { range, .. }
            | ParseError::InvalidArgumentName { range, .. }
            | ParseError::UnexpectedComponentExpression { range, .. } => range,
        }
    }
}
