use crate::{annotation::Annotation, document::DocumentRange};
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct CssError {
    kind: CssErrorKind,
    range: DocumentRange,
}

impl CssError {
    pub(crate) fn new(kind: CssErrorKind, range: DocumentRange) -> Self {
        CssError { kind, range }
    }
}

impl Annotation for CssError {
    fn message(&self) -> String {
        self.kind.to_string()
    }

    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

#[derive(Debug, Clone, Error)]
pub(crate) enum CssErrorKind {
    /// `--asset(...)` was called with something other than a single string
    /// literal. Examples: `--asset(var(--x))`, `--asset(/path)` (unquoted),
    /// `--asset()`, `--asset("a", "b")`. Carries the raw argument text so
    /// users can grep for it.
    #[error("CSS `--asset()` call has a non-string-literal argument: `{argument}`")]
    NonStringLiteralArgument { argument: String },

    /// `--asset(` appeared but the call was never closed before EOF or
    /// before a hard CSS boundary (newline inside a string literal, etc.).
    #[error("CSS `--asset()` call was not properly closed")]
    UnclosedAssetCall,
}
