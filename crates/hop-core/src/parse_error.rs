use crate::diagnostic::Diagnostic;
use crate::diagnostic_severity::DiagnosticSeverity;
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
pub(crate) struct ErrorEmitted(());

pub(crate) trait Emit {
    /// Record a parse error, and return the proof that it was recorded.
    ///
    /// Calling this is the only way to obtain an [`ErrorEmitted`].
    fn emit(&mut self, kind: ParseErrorKind, range: DocumentRange) -> ErrorEmitted;
}

impl Emit for Vec<ParseError> {
    fn emit(&mut self, kind: ParseErrorKind, range: DocumentRange) -> ErrorEmitted {
        self.push(ParseError { kind, range });
        ErrorEmitted(())
    }
}

pub(crate) trait OrEmit<T> {
    fn or_emit(
        self,
        errors: &mut Vec<ParseError>,
        range: &DocumentRange,
    ) -> Result<T, ErrorEmitted>;
}

impl<T, E: Into<ParseErrorKind>> OrEmit<T> for Result<T, E> {
    fn or_emit(
        self,
        errors: &mut Vec<ParseError>,
        range: &DocumentRange,
    ) -> Result<T, ErrorEmitted> {
        self.map_err(|error| errors.emit(error.into(), range.clone()))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ParseError {
    kind: ParseErrorKind,
    range: DocumentRange,
}

impl ParseError {
    pub(crate) fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            message: self.kind.to_string(),
            range: self.range.clone(),
            severity: DiagnosticSeverity::Error,
        }
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

    #[error("A block must end with an expression")]
    BlockMissingTailExpression,

    #[error("let is only allowed inside a block: wrap the expression in braces")]
    LetOutsideBlock,

    #[error("Invalid markup declaration")]
    InvalidMarkupDeclaration,

    #[error("<!doctype> declarations are not allowed: one is inserted automatically")]
    DoctypeNotAllowed,

    #[error(
        "Inline <script> content is not allowed: move the code to a file and reference it with <script src=\"...\">"
    )]
    InlineScriptNotAllowed,

    #[error(
        "<style> elements are not allowed: put the CSS in the project stylesheet, or reference it with <link rel=\"stylesheet\">"
    )]
    StyleElementNotAllowed,

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

    #[error("'pub' is not allowed here")]
    UnexpectedPubKeyword,

    #[error("Unexpected end of expression")]
    UnexpectedEof,

    #[error("Unterminated string literal")]
    UnterminatedStringLiteral,

    #[error("Unmatched '{token}'")]
    UnmatchedToken { token: LangToken },

    #[error("{0}")]
    InvalidVariableName(#[from] InvalidVarNameError),

    #[error("{0}")]
    InvalidFieldName(#[from] InvalidFieldNameError),

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

    #[error("Expected identifier but got '{actual}'")]
    ExpectedIdentifierButGot { actual: LangToken },

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

    #[error("{0}")]
    InvalidTypeName(#[from] InvalidTypeNameError),

    #[error("{0}")]
    InvalidFunctionName(#[from] InvalidFunctionNameError),

    #[error("{0}")]
    InvalidModuleName(#[from] InvalidModuleNameError),

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

    #[error("A record or enum literal is not allowed here: surround it with parentheses")]
    RecordLiteralNotAllowedHere,
}
