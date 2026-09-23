use crate::diagnostic::Diagnostic;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::DocumentRange;
use crate::root_relative_path::RootRelativePathError;
use thiserror::Error;

#[derive(Debug, Clone)]
pub(crate) struct CssError {
    kind: CssErrorKind,
    range: DocumentRange,
}

impl CssError {
    pub(crate) fn new(kind: CssErrorKind, range: DocumentRange) -> Self {
        CssError { kind, range }
    }

    pub(crate) fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic {
            message: self.kind.to_string(),
            range: self.range.clone(),
            severity: DiagnosticSeverity::Error,
        }
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

    /// `--asset("...")` was called with a path that cannot be resolved to an
    /// asset, e.g. an empty path or one containing invalid characters.
    #[error("CSS `--asset()` has an invalid path: {source}")]
    InvalidAssetPath { source: RootRelativePathError },

    /// `--asset(` appeared but the call was never closed before EOF or
    /// before a hard CSS boundary (newline inside a string literal, etc.).
    #[error("CSS `--asset()` call was not properly closed")]
    UnclosedAssetCall,
}
