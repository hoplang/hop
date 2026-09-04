use crate::annotation::Annotation;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::token::LangToken;
use crate::symbols::field_name::InvalidFieldNameError;
use crate::symbols::module_name::InvalidModuleNameError;
use crate::symbols::type_name::InvalidTypeNameError;
use crate::symbols::var_name::InvalidVarNameError;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    range: DocumentRange,
}

impl ParseError {
    pub(crate) fn new(kind: ParseErrorKind, range: DocumentRange) -> Self {
        ParseError { kind, range }
    }

    pub(crate) fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl Annotation for ParseError {
    fn message(&self) -> String {
        self.kind.to_string()
    }
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

#[derive(Debug, Clone, Error)]
pub(crate) enum ParseErrorKind {
    #[error("Unmatched </{tag}>")]
    UnmatchedClosingTag { tag: CheapString },

    #[error("Unclosed <{tag}>")]
    UnclosedTag { tag: CheapString },

    #[error("Unmatched </>")]
    UnmatchedClosingFragment {},

    #[error("Unclosed <>")]
    UnclosedFragment {},

    #[error("Expected a single root: wrap the contents in <>...</>")]
    MultipleRoots,

    #[error("Expected a single root: use <></> for an empty body")]
    EmptyBody,

    #[error("<{tag}> should not be closed using a closing tag")]
    ClosedVoidTag { tag: CheapString },

    #[error("Duplicate attribute '{name}'")]
    DuplicateAttribute { name: CheapString },

    #[error("Unmatched {ch}")]
    UnmatchedCharacter { ch: char },

    #[error("Unrecognized attribute '{attr_name}' on <{tag_name}>")]
    UnrecognizedAttribute {
        tag_name: CheapString,
        attr_name: CheapString,
    },

    #[error("Missing expression in <match> tag")]
    MissingMatchExpression,

    #[error("Missing pattern in <case> tag")]
    MissingCasePattern,

    #[error("Only <case> tags are allowed inside <match>")]
    InvalidMatchChild,

    #[error("<case> is only allowed inside <match>")]
    CaseOutsideMatch,

    #[error("Missing expression in <if> tag")]
    MissingIfExpression,

    #[error("Missing loop generator expression in <for> tag")]
    MissingForExpression,

    #[error("Missing binding in <let> tag")]
    MissingLetBinding,

    #[error("Invalid markup declaration")]
    InvalidMarkupDeclaration,

    #[error("<!doctype> declarations are not allowed: one is inserted automatically")]
    DoctypeNotAllowed,

    #[error("Unterminated comment")]
    UnterminatedComment,

    #[error("Expected quoted attribute value or expression")]
    ExpectedQuotedAttributeValue,

    #[error("Single-quoted attribute values are not supported: use double quotes")]
    SingleQuotedAttributeValue,

    #[error("Unterminated opening tag")]
    UnterminatedOpeningTag,

    #[error("Missing variable name for spread")]
    MissingVariableNameForSpread,

    #[error("Unterminated closing tag")]
    UnterminatedClosingTag,

    #[error("Unterminated tag start")]
    UnterminatedTagStart,

    #[error(
        "Unexpected expression on <{tag_name}>: use attribute syntax instead (e.g. attr={{value}})"
    )]
    UnexpectedComponentExpression { tag_name: CheapString },

    #[error("Unexpected text at top level")]
    UnexpectedTopLevelText,

    #[error("'pub' is not allowed here")]
    UnexpectedPubKeyword,

    #[error("Unexpected end of expression")]
    UnexpectedEof,

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral,

    #[error("Invalid escape sequence '\\{ch}'")]
    InvalidEscapeSequence { ch: char },

    #[error("Invalid escape sequence at end of string")]
    InvalidEscapeSequenceAtEndOfString,

    #[error("Unmatched '{token}'")]
    UnmatchedToken { token: LangToken },

    #[error("Invalid variable name '{name}': {error}")]
    InvalidVariableName {
        name: CheapString,
        error: InvalidVarNameError,
    },

    #[error("Invalid field name '{name}': {error}")]
    InvalidFieldName {
        name: CheapString,
        error: InvalidFieldNameError,
    },

    #[error("Expected token '{expected}' but got '{actual}'")]
    ExpectedTokenButGot {
        expected: LangToken,
        actual: LangToken,
    },

    #[error("Expected token '{expected}' but got end of file")]
    ExpectedTokenButGotEof { expected: LangToken },

    #[error("Unexpected token '{token}'")]
    UnexpectedToken { token: LangToken },

    #[error("Unexpected character: '{ch}'")]
    UnexpectedCharacter { ch: char },

    #[error("Expected variable name but got '{actual}'")]
    ExpectedVariableNameButGot { actual: LangToken },

    #[error("Expected field name but got '{actual}'")]
    ExpectedFieldNameButGot { actual: LangToken },

    #[error("Duplicate field '{name}'")]
    DuplicateField { name: CheapString },

    #[error("Duplicate variant '{name}'")]
    DuplicateVariant { name: CheapString },

    #[error("Expected type name but got '{actual}'")]
    ExpectedTypeNameButGot { actual: LangToken },

    #[error("Expected type name but got end of file")]
    ExpectedTypeNameButGotEof,

    #[error("Invalid number format")]
    InvalidNumberFormat,

    #[error("Integer literal is too large for Int (maximum is 2147483647)")]
    IntLiteralOutOfRange,

    #[error("{error}")]
    InvalidTypeName { error: InvalidTypeNameError },

    #[error("{error}")]
    InvalidModuleName { error: InvalidModuleNameError },

    #[error("Expected identifier after '::'")]
    ExpectedIdentifierAfterColonColon,

    #[error("Expected module path after 'import'")]
    ExpectedModulePath,

    #[error("Import path must have at least two segments: module::Component")]
    ImportPathTooShort,

    #[error("Default values are not allowed on view parameters")]
    DefaultValueNotAllowedOnView,

    #[error("Expected a 'body' block")]
    ExpectedPageBodyBlock,

    #[error("Default values are not allowed on function parameters")]
    DefaultValueNotAllowedOnFunction,

    #[error("Default values must be constant")]
    DefaultValueMustBeConstant,

    #[error("Unknown macro '{name}'")]
    UnknownMacro { name: CheapString },

    #[error("Unknown HTML element <{tag}>")]
    UnknownHtmlElement { tag: CheapString },

    #[error("Rest parameter must be the last parameter")]
    RestParamMustBeLast,

    #[error("At most one rest parameter is allowed")]
    DuplicateRestParam,

    #[error("At most one spread is allowed in a record literal")]
    DuplicateSpreadInRecordLiteral,

    #[error("Spread is not allowed in an enum variant literal")]
    SpreadNotAllowedInEnumLiteral,
}
