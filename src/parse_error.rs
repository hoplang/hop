use crate::document::{CheapString, DocumentRange, Ranged};
use crate::dop::symbols::field_name::InvalidFieldNameError;
use crate::dop::symbols::type_name::InvalidTypeNameError;
use crate::dop::symbols::var_name::InvalidVarNameError;
use crate::dop::syntax::token::Token;
use crate::hop::symbols::module_id::InvalidModuleIdError;
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
        error: InvalidTypeNameError,
        range: DocumentRange,
    },

    #[error("Entrypoint name must start with an uppercase letter")]
    InvalidEntrypointName { range: DocumentRange },

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

    #[error("Unexpected text at top level")]
    UnexpectedTopLevelText { range: DocumentRange },

    #[error("Unexpected end of expression")]
    UnexpectedEof { range: DocumentRange },

    #[error("Unexpected end of field access")]
    UnexpectedEndOfFieldAccess { range: DocumentRange },

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral { range: DocumentRange },

    #[error("Invalid escape sequence '\\{ch}'")]
    InvalidEscapeSequence { ch: char, range: DocumentRange },

    #[error("Invalid escape sequence at end of string")]
    InvalidEscapeSequenceAtEndOfString { range: DocumentRange },

    #[error("Unmatched '{token}'")]
    UnmatchedToken { token: Token, range: DocumentRange },

    #[error("Invalid variable name '{name}': {error}")]
    InvalidVariableName {
        name: CheapString,
        error: InvalidVarNameError,
        range: DocumentRange,
    },

    #[error("Invalid field name '{name}': {error}")]
    InvalidFieldName {
        name: CheapString,
        error: InvalidFieldNameError,
        range: DocumentRange,
    },

    #[error("Expected token '{expected}' but got '{actual}'")]
    ExpectedTokenButGot {
        expected: Token,
        actual: Token,
        range: DocumentRange,
    },

    #[error("Expected token '{expected}' but got end of file")]
    ExpectedTokenButGotEof {
        expected: Token,
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

    #[error("Duplicate parameter '{name}'")]
    DuplicateParameter {
        name: CheapString,
        range: DocumentRange,
    },

    #[error("Duplicate field '{name}'")]
    DuplicateField {
        name: CheapString,
        range: DocumentRange,
    },

    #[error("Duplicate variant '{name}'")]
    DuplicateVariant {
        name: CheapString,
        range: DocumentRange,
    },

    #[error("Expected type name but got {actual}")]
    ExpectedTypeNameButGot { actual: Token, range: DocumentRange },

    #[error("Expected type name but got end of file")]
    ExpectedTypeNameButGotEof { range: DocumentRange },

    #[error("Invalid number format")]
    InvalidNumberFormat { range: DocumentRange },

    #[error("{error}")]
    InvalidTypeName {
        error: InvalidTypeNameError,
        range: DocumentRange,
    },

    #[error("{error}")]
    InvalidModuleName {
        error: InvalidModuleIdError,
        range: DocumentRange,
    },

    #[error("Expected identifier after '::'")]
    ExpectedIdentifierAfterColonColon { range: DocumentRange },

    #[error("Expected module path after 'import'")]
    ExpectedModulePath { range: DocumentRange },

    #[error("Import path must have at least two segments: module::Component")]
    ImportPathTooShort { range: DocumentRange },

    #[error("Expected declaration (import, record, or enum)")]
    ExpectedDeclaration { range: DocumentRange },

    #[error("Unknown macro '{name}'")]
    UnknownMacro {
        name: CheapString,
        range: DocumentRange,
    },
}

impl Ranged for ParseError {
    fn range(&self) -> &DocumentRange {
        match self {
            ParseError::UnmatchedClosingTag { range, .. }
            | ParseError::UnmatchedCharacter { range, .. }
            | ParseError::UnclosedTag { range, .. }
            | ParseError::ClosedVoidTag { range, .. }
            | ParseError::InvalidComponentName { range, .. }
            | ParseError::InvalidEntrypointName { range }
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
            | ParseError::UnterminatedTagStart { range }
            | ParseError::UnexpectedTopLevelText { range }
            | ParseError::UnexpectedEof { range, .. }
            | ParseError::UnterminatedStringLiteral { range }
            | ParseError::InvalidEscapeSequence { range, .. }
            | ParseError::InvalidEscapeSequenceAtEndOfString { range }
            | ParseError::UnmatchedToken { range, .. }
            | ParseError::UnexpectedCharacter { range, .. }
            | ParseError::InvalidNumberFormat { range, .. }
            | ParseError::ExpectedTokenButGot { range, .. }
            | ParseError::ExpectedTokenButGotEof { range, .. }
            | ParseError::UnexpectedToken { range, .. }
            | ParseError::ExpectedVariableNameButGot { range, .. }
            | ParseError::ExpectedFieldNameButGot { range, .. }
            | ParseError::InvalidVariableName { range, .. }
            | ParseError::InvalidFieldName { range, .. }
            | ParseError::ExpectedTypeNameButGot { range, .. }
            | ParseError::ExpectedTypeNameButGotEof { range }
            | ParseError::UnexpectedEndOfFieldAccess { range, .. }
            | ParseError::DuplicateParameter { range, .. }
            | ParseError::DuplicateField { range, .. }
            | ParseError::DuplicateVariant { range, .. }
            | ParseError::ExpectedIdentifierAfterDot { range }
            | ParseError::InvalidTypeName { range, .. }
            | ParseError::InvalidModuleName { range, .. }
            | ParseError::ExpectedIdentifierAfterColonColon { range }
            | ParseError::ExpectedModulePath { range }
            | ParseError::ImportPathTooShort { range }
            | ParseError::ExpectedDeclaration { range }
            | ParseError::UnknownMacro { range, .. } => range,
        }
    }
}
