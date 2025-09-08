use crate::dop::tokenizer::DopToken;
use crate::span::string_cursor::{Spanned, StringSpan};
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected end of expression")]
    UnexpectedEof { span: StringSpan },

    #[error("Unexpected end of property access")]
    UnexpectedEndOfPropertyAccess { span: StringSpan },

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral { span: StringSpan },

    #[error("Unmatched '{token}'")]
    UnmatchedToken { token: DopToken, span: StringSpan },

    #[error(
        "Invalid variable name '{name}'. Variable names must start with a letter and contain only letters, digits, and underscores"
    )]
    InvalidVariableName { name: StringSpan },

    #[error("Expected {} but got '{actual}'", expected.iter().map(|t| format!("'{}'", t)).collect::<Vec<_>>().join(" or "))]
    ExpectedTokensButGot {
        expected: Vec<DopToken>,
        actual: DopToken,
        span: StringSpan,
    },

    #[error("Expected token '{expected}' but got '{actual}'")]
    ExpectedTokenButGot {
        expected: DopToken,
        actual: DopToken,
        span: StringSpan,
    },

    #[error("Unexpected token '{token}'")]
    UnexpectedToken { token: DopToken, span: StringSpan },

    #[error("Expected variable name but got {actual}")]
    ExpectedVariableNameButGot { actual: DopToken, span: StringSpan },

    #[error("Expected property name but got {actual}")]
    ExpectedPropertyNameButGot { actual: DopToken, span: StringSpan },

    #[error("Expected identifier after '.'")]
    ExpectedIdentifierAfterDot { span: StringSpan },

    #[error("Duplicate argument '{name}'")]
    DuplicateArgument { name: StringSpan },

    #[error("Duplicate parameter '{name}'")]
    DuplicateParameter { name: StringSpan },

    #[error("Duplicate property '{name}'")]
    DuplicateProperty { name: StringSpan },

    #[error("{message}")]
    Spanned { message: String, span: StringSpan },
}

impl ParseError {
    pub fn new(message: String, span: StringSpan) -> Self {
        Self::Spanned { message, span }
    }
}

impl Spanned for ParseError {
    fn span(&self) -> &StringSpan {
        match self {
            ParseError::UnexpectedEof { span, .. }
            | ParseError::UnterminatedStringLiteral { span }
            | ParseError::UnmatchedToken { span, .. }
            | ParseError::ExpectedTokensButGot { span, .. }
            | ParseError::ExpectedTokenButGot { span, .. }
            | ParseError::UnexpectedToken { span, .. }
            | ParseError::ExpectedVariableNameButGot { span, .. }
            | ParseError::ExpectedPropertyNameButGot { span, .. }
            | ParseError::UnexpectedEndOfPropertyAccess { span, .. }
            | ParseError::ExpectedIdentifierAfterDot { span }
            | ParseError::Spanned { span, .. } => span,
            ParseError::InvalidVariableName { name }
            | ParseError::DuplicateArgument { name }
            | ParseError::DuplicateParameter { name }
            | ParseError::DuplicateProperty { name } => name,
        }
    }
}
