use crate::annotation::Annotation;
use crate::document::{CheapString, DocumentRange};
use crate::hop::parsing::token::LangToken;
use crate::symbols::field_name::InvalidFieldNameError;
use crate::symbols::function_name::InvalidFunctionNameError;
use crate::symbols::module_name::InvalidModuleNameError;
use crate::symbols::type_name::InvalidTypeNameError;
use crate::symbols::var_name::InvalidVarNameError;
use thiserror::Error;

/// Proof that a parse error has been recorded.
#[derive(Clone, Copy, Debug)]
#[must_use]
pub struct ErrorEmitted(());

/// A container for parse errors.
///
/// Recording an error in this container is the only way to obtain an
/// [`ErrorEmitted`].
#[derive(Debug, Clone, Default)]
pub struct ParseErrors {
    errors: Vec<ParseError>,
}

impl ParseErrors {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record an error, and return the proof that it was recorded.
    pub(crate) fn emit(&mut self, kind: ParseErrorKind, range: DocumentRange) -> ErrorEmitted {
        self.errors.push(ParseError { kind, range });
        ErrorEmitted(())
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn clear(&mut self) {
        self.errors.clear();
    }

    pub fn iter(&self) -> std::slice::Iter<'_, ParseError> {
        self.errors.iter()
    }
}

impl<'a> IntoIterator for &'a ParseErrors {
    type Item = &'a ParseError;
    type IntoIter = std::slice::Iter<'a, ParseError>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.iter()
    }
}

impl IntoIterator for ParseErrors {
    type Item = ParseError;
    type IntoIter = std::vec::IntoIter<ParseError>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    range: DocumentRange,
}

impl ParseError {
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

    #[error("Function '{name}' has an empty body: a function body must be a single expression")]
    EmptyFunctionBody { name: CheapString },

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

    #[error("Missing expression in <if> tag")]
    MissingIfExpression,

    #[error("<{tag_name}> already has an expression")]
    DuplicateTagExpression { tag_name: CheapString },

    #[error("Missing loop generator expression in <for> tag")]
    MissingForExpression,

    #[error("A block must end with an expression")]
    BlockMissingTailExpression,

    #[error("let is only allowed inside a block: wrap the expression in braces")]
    LetOutsideBlock,

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
    UnexpectedTagExpression { tag_name: CheapString },

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

    #[error("Expected function name but got '{actual}'")]
    ExpectedFunctionNameButGot { actual: LangToken },

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
    InvalidFunctionName { error: InvalidFunctionNameError },

    #[error("{error}")]
    InvalidModuleName { error: InvalidModuleNameError },

    #[error("Expected identifier after '::'")]
    ExpectedIdentifierAfterColonColon,

    #[error("Expected module path after 'import'")]
    ExpectedModulePath,

    #[error("Import path must have at least two segments: module::Name")]
    ImportPathTooShort,

    #[error("Default values are not allowed on page parameters")]
    DefaultValueNotAllowedOnPage,

    #[error("Expected a 'fn body() -> Html' member")]
    ExpectedPageBodyBlock,

    #[error("Expected 'fn head' or 'fn body'")]
    ExpectedPageMember,

    #[error("Unknown page member '{name}': expected 'head' or 'body'")]
    UnknownPageMember { name: CheapString },

    #[error("Duplicate page member '{name}'")]
    DuplicatePageMember { name: CheapString },

    #[error("Page member '{name}' cannot have parameters")]
    PageMemberHasParameters { name: CheapString },

    #[error("Page member '{name}' must return Html")]
    PageMemberMustReturnHtml { name: CheapString },

    #[error("Rest parameters are not allowed on pages")]
    RestParamNotAllowedOnPage,

    #[error("Examples annotations are not allowed on function parameters")]
    ExamplesNotAllowedOnFunction,

    #[error("Arguments must either all be named or all be positional")]
    MixedNamedAndPositionalArguments,

    #[error("Function '{name}' is missing a return type annotation")]
    FunctionMissingReturnTypeAnnotation { name: CheapString },

    #[error("Unknown annotation '{name}'")]
    UnknownAnnotation { name: CheapString },

    #[error("Unknown examples key '{name}'")]
    UnknownExamplesKey { name: CheapString },

    #[error("Expected string literal but got '{actual}'")]
    ExpectedStringLiteralButGot { actual: LangToken },

    #[error("Expected integer literal but got '{actual}'")]
    ExpectedIntLiteralButGot { actual: LangToken },

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
