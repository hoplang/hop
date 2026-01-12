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

    #[error("{err}")]
    DopError { err: dop::ParseError },

    #[error("Empty expression")]
    EmptyExpression { range: DocumentRange },

    #[error("Missing expression in <match> tag")]
    MissingMatchExpression { range: DocumentRange },

    #[error("Missing pattern in <case> tag")]
    MissingCasePattern { range: DocumentRange },

    #[error("Only <case> tags are allowed inside <match>")]
    InvalidMatchChild { range: DocumentRange },

    #[error("Missing expression in <if> tag")]
    MissingIfExpression { range: DocumentRange },

    #[error("Missing loop generator expression in <for> tag")]
    MissingForExpression { range: DocumentRange },

    #[error("Missing binding in <let> tag")]
    MissingLetBinding { range: DocumentRange },

    #[error("Invalid markup declaration")]
    InvalidMarkupDeclaration { range: DocumentRange },

    #[error("Unterminated comment")]
    UnterminatedComment { range: DocumentRange },

    #[error("Expected quoted attribute value or expression")]
    ExpectedQuotedAttributeValue { range: DocumentRange },

    #[error("Unterminated opening tag")]
    UnterminatedOpeningTag { range: DocumentRange },

    #[error("Unterminated closing tag")]
    UnterminatedClosingTag { range: DocumentRange },

    #[error("Unterminated tag start")]
    UnterminatedTagStart { range: DocumentRange },

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

impl From<dop::ParseError> for ParseError {
    fn from(err: dop::ParseError) -> Self {
        Self::DopError { err }
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
            | ParseError::InvalidArgumentName { range, .. }
            | ParseError::UnexpectedComponentExpression { range, .. }
            | ParseError::EmptyExpression { range }
            | ParseError::MissingMatchExpression { range }
            | ParseError::MissingCasePattern { range }
            | ParseError::InvalidMatchChild { range }
            | ParseError::MissingIfExpression { range }
            | ParseError::MissingForExpression { range }
            | ParseError::MissingLetBinding { range }
            | ParseError::InvalidMarkupDeclaration { range }
            | ParseError::UnterminatedComment { range }
            | ParseError::ExpectedQuotedAttributeValue { range }
            | ParseError::UnterminatedOpeningTag { range }
            | ParseError::UnterminatedClosingTag { range }
            | ParseError::UnterminatedTagStart { range } => range,
            ParseError::DopError { err } => err.range(),
        }
    }
}
