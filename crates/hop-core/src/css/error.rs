use crate::{annotation::Annotation, document::DocumentRange};

#[derive(Debug, Clone)]
pub enum CssError {
    /// `--asset(...)` was called with something other than a single string
    /// literal. Examples: `--asset(var(--x))`, `--asset(/path)` (unquoted),
    /// `--asset()`, `--asset("a", "b")`. Carries the raw argument text so
    /// users can grep for it.
    NonStringLiteralArgument {
        range: DocumentRange,
        argument: String,
    },

    /// `--asset("...")` references a path that does not exist on disk
    /// relative to the project root.
    MissingAsset { range: DocumentRange, path: String },

    /// `--asset(` appeared but the call was never closed before EOF or
    /// before a hard CSS boundary (newline inside a string literal, etc.).
    UnclosedAssetCall { range: DocumentRange },
}

impl CssError {
    pub fn message(&self) -> String {
        match self {
            CssError::NonStringLiteralArgument { argument, .. } => {
                format!(
                    "CSS `--asset()` call has a non-string-literal argument: `{}`",
                    argument
                )
            }
            CssError::MissingAsset { path, .. } => {
                format!(
                    "CSS `--asset()` references a file that does not exist: `{}`",
                    path
                )
            }
            CssError::UnclosedAssetCall { .. } => {
                "CSS `--asset()` call was not properly closed".to_string()
            }
        }
    }

    pub fn range(&self) -> &DocumentRange {
        match self {
            CssError::NonStringLiteralArgument { range, .. }
            | CssError::MissingAsset { range, .. }
            | CssError::UnclosedAssetCall { range } => range,
        }
    }
}

impl Annotation for CssError {
    fn message(&self) -> String {
        self.message()
    }

    fn range(&self) -> &DocumentRange {
        self.range()
    }
}
