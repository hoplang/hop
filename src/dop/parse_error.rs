use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::tokenizer::DopToken;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected end of expression")]
    UnexpectedEof { span: DocumentRange },

    #[error("Unexpected end of property access")]
    UnexpectedEndOfPropertyAccess { span: DocumentRange },

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral { span: DocumentRange },

    #[error("Unmatched '{token}'")]
    UnmatchedToken {
        token: DopToken,
        span: DocumentRange,
    },

    #[error(
        "Invalid variable name '{name}'. Variable names must start with a letter and contain only letters, digits, and underscores"
    )]
    InvalidVariableName { name: DocumentRange },

    #[error("Expected token '{expected}' but got '{actual}'")]
    ExpectedTokenButGot {
        expected: DopToken,
        actual: DopToken,
        span: DocumentRange,
    },

    #[error("Unexpected token '{token}'")]
    UnexpectedToken {
        token: DopToken,
        span: DocumentRange,
    },

    #[error("Unexpected character: '{ch}'")]
    UnexpectedCharacter { ch: char, span: DocumentRange },

    #[error("Expected variable name but got {actual}")]
    ExpectedVariableNameButGot {
        actual: DopToken,
        span: DocumentRange,
    },

    #[error("Expected property name but got {actual}")]
    ExpectedPropertyNameButGot {
        actual: DopToken,
        span: DocumentRange,
    },

    #[error("Expected identifier after '.'")]
    ExpectedIdentifierAfterDot { span: DocumentRange },

    #[error("Duplicate argument '{name}'")]
    DuplicateArgument { name: DocumentRange },

    #[error("Expected '==' but got '='")]
    ExpectedDoubleEqButGotSingleEq { span: DocumentRange },

    #[error("Duplicate parameter '{name}'")]
    DuplicateParameter { name: DocumentRange },

    #[error("Duplicate property '{name}'")]
    DuplicateProperty { name: DocumentRange },

    #[error("Expected type name")]
    ExpectedTypeName { span: DocumentRange },

    #[error("Invalid number format")]
    InvalidNumberFormat { span: DocumentRange },
}

impl Ranged for ParseError {
    fn range(&self) -> &DocumentRange {
        match self {
            ParseError::UnexpectedEof { span, .. }
            | ParseError::UnterminatedStringLiteral { span }
            | ParseError::UnmatchedToken { span, .. }
            | ParseError::UnexpectedCharacter { span, .. }
            | ParseError::InvalidNumberFormat { span, .. }
            | ParseError::ExpectedTokenButGot { span, .. }
            | ParseError::ExpectedDoubleEqButGotSingleEq { span, .. }
            | ParseError::UnexpectedToken { span, .. }
            | ParseError::ExpectedVariableNameButGot { span, .. }
            | ParseError::ExpectedPropertyNameButGot { span, .. }
            | ParseError::ExpectedTypeName { span, .. }
            | ParseError::UnexpectedEndOfPropertyAccess { span, .. }
            | ParseError::ExpectedIdentifierAfterDot { span } => span,
            ParseError::InvalidVariableName { name }
            | ParseError::DuplicateArgument { name }
            | ParseError::DuplicateParameter { name }
            | ParseError::DuplicateProperty { name } => name,
        }
    }
}
