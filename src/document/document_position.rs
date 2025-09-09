use std::fmt;

/// Represents a position in source code with 0-based line and column numbers
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DocumentPosition {
    /// Position using UTF-16 code unit offsets for columns
    Utf16 { line: usize, column: usize },
    /// Position using UTF-32 code point offsets for columns
    Utf32 { line: usize, column: usize },
}

impl DocumentPosition {
    pub fn line(self) -> usize {
        match self {
            DocumentPosition::Utf16 { line, .. } => line,
            DocumentPosition::Utf32 { line, .. } => line,
        }
    }

    pub fn column(self) -> usize {
        match self {
            DocumentPosition::Utf16 { column, .. } => column,
            DocumentPosition::Utf32 { column, .. } => column,
        }
    }
}

impl fmt::Display for DocumentPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line(), self.column())
    }
}
