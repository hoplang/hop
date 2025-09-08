use crate::dop::tokenizer::DopToken;
use crate::span::string_cursor::StringSpan;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected end of expression")]
    UnexpectedEof,

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

    pub fn span(&self) -> Option<StringSpan> {
        match self {
            ParseError::UnexpectedEof => None,
            ParseError::UnterminatedStringLiteral { span } => Some(span.clone()),
            ParseError::UnmatchedToken { span, .. } => Some(span.clone()),
            ParseError::InvalidVariableName { name } => Some(name.clone()),
            ParseError::ExpectedTokensButGot { span, .. } => Some(span.clone()),
            ParseError::ExpectedTokenButGot { span, .. } => Some(span.clone()),
            ParseError::UnexpectedToken { span, .. } => Some(span.clone()),
            ParseError::ExpectedVariableNameButGot { span, .. } => Some(span.clone()),
            ParseError::ExpectedPropertyNameButGot { span, .. } => Some(span.clone()),
            ParseError::ExpectedIdentifierAfterDot { span } => Some(span.clone()),
            ParseError::DuplicateArgument { name } => Some(name.clone()),
            ParseError::DuplicateParameter { name } => Some(name.clone()),
            ParseError::DuplicateProperty { name } => Some(name.clone()),
            ParseError::Spanned { span, .. } => Some(span.clone()),
        }
    }
}
