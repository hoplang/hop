use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use thiserror::Error;

use super::field_name::InvalidFieldNameError;
use super::token::Token;
use super::var_name::InvalidVarNameError;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected end of expression")]
    UnexpectedEof { range: DocumentRange },

    #[error("Unexpected end of field access")]
    UnexpectedEndOfFieldAccess { range: DocumentRange },

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral { range: DocumentRange },

    #[error("Unmatched '{token}'")]
    UnmatchedToken { token: Token, range: DocumentRange },

    #[error("Invalid variable name '{name}': {error}")]
    InvalidVariableName {
        name: StringSpan,
        error: InvalidVarNameError,
        range: DocumentRange,
    },

    #[error("Invalid field name '{name}': {error}")]
    InvalidFieldName {
        name: StringSpan,
        error: InvalidFieldNameError,
        range: DocumentRange,
    },

    #[error("Expected token '{expected}' but got '{actual}'")]
    ExpectedTokenButGot {
        expected: Token,
        actual: Token,
        range: DocumentRange,
    },

    #[error("Unexpected token '{token}'")]
    UnexpectedToken { token: Token, range: DocumentRange },

    #[error("Unexpected character: '{ch}'")]
    UnexpectedCharacter { ch: char, range: DocumentRange },

    #[error("Expected variable name but got {actual}")]
    ExpectedVariableNameButGot { actual: Token, range: DocumentRange },

    #[error("Expected field name but got {actual}")]
    ExpectedFieldNameButGot { actual: Token, range: DocumentRange },

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

    #[error("Duplicate field '{name}'")]
    DuplicateField {
        name: StringSpan,
        range: DocumentRange,
    },

    #[error("Expected type name but got {actual}")]
    ExpectedTypeNameButGot { actual: Token, range: DocumentRange },

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
            | ParseError::ExpectedFieldNameButGot { range, .. }
            | ParseError::DuplicateArgument { range, .. }
            | ParseError::InvalidVariableName { range, .. }
            | ParseError::InvalidFieldName { range, .. }
            | ParseError::ExpectedTypeNameButGot { range, .. }
            | ParseError::UnexpectedEndOfFieldAccess { range, .. }
            | ParseError::DuplicateParameter { range, .. }
            | ParseError::DuplicateField { range, .. }
            | ParseError::ExpectedIdentifierAfterDot { range } => range,
        }
    }
}
