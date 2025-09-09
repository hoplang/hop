use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::dop::tokenizer::DopToken;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected end of expression")]
    UnexpectedEof { range: DocumentRange },

    #[error("Unexpected end of property access")]
    UnexpectedEndOfPropertyAccess { range: DocumentRange },

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral { range: DocumentRange },

    #[error("Unmatched '{token}'")]
    UnmatchedToken {
        token: DopToken,
        range: DocumentRange,
    },

    #[error(
        "Invalid variable name '{name}'. Variable names must start with a letter and contain only letters, digits, and underscores"
    )]
    InvalidVariableName {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Expected token '{expected}' but got '{actual}'")]
    ExpectedTokenButGot {
        expected: DopToken,
        actual: DopToken,
        range: DocumentRange,
    },

    #[error("Unexpected token '{token}'")]
    UnexpectedToken {
        token: DopToken,
        range: DocumentRange,
    },

    #[error("Unexpected character: '{ch}'")]
    UnexpectedCharacter { ch: char, range: DocumentRange },

    #[error("Expected variable name but got {actual}")]
    ExpectedVariableNameButGot {
        actual: DopToken,
        range: DocumentRange,
    },

    #[error("Expected property name but got {actual}")]
    ExpectedPropertyNameButGot {
        actual: DopToken,
        range: DocumentRange,
    },

    #[error("Expected identifier after '.'")]
    ExpectedIdentifierAfterDot { range: DocumentRange },

    #[error("Duplicate argument '{name}'")]
    DuplicateArgument {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Expected '==' but got '='")]
    ExpectedDoubleEqButGotSingleEq { range: DocumentRange },

    #[error("Duplicate parameter '{name}'")]
    DuplicateParameter {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Duplicate property '{name}'")]
    DuplicateProperty {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Expected type name")]
    ExpectedTypeName { range: DocumentRange },

    #[error("Invalid number format")]
    InvalidNumberFormat { range: DocumentRange },
}

impl Ranged for ParseError {
    fn range(&self) -> &DocumentRange {
        match self {
            ParseError::UnexpectedEof { range, .. }
            | ParseError::UnterminatedStringLiteral { range }
            | ParseError::UnmatchedToken { range, .. }
            | ParseError::UnexpectedCharacter { range, .. }
            | ParseError::InvalidNumberFormat { range, .. }
            | ParseError::ExpectedTokenButGot { range, .. }
            | ParseError::ExpectedDoubleEqButGotSingleEq { range, .. }
            | ParseError::UnexpectedToken { range, .. }
            | ParseError::ExpectedVariableNameButGot { range, .. }
            | ParseError::ExpectedPropertyNameButGot { range, .. }
            | ParseError::DuplicateArgument { range, .. }
            | ParseError::InvalidVariableName { range, .. }
            | ParseError::ExpectedTypeName { range, .. }
            | ParseError::UnexpectedEndOfPropertyAccess { range, .. }
            | ParseError::DuplicateParameter { range, .. }
            | ParseError::DuplicateProperty { range, .. }
            | ParseError::ExpectedIdentifierAfterDot { range } => range,
        }
    }
}
