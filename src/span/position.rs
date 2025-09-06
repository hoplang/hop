use std::fmt;

/// Represents a position in source code with 0-based line and column numbers
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Position {
    /// Position using UTF-16 code unit offsets for columns
    Utf16 { line: usize, column: usize },
    /// Position using UTF-32 code point offsets for columns (character count)
    Utf32 { line: usize, column: usize },
}

impl Position {
    pub fn line(self) -> usize {
        match self {
            Position::Utf16 { line, .. } => line,
            Position::Utf32 { line, .. } => line,
        }
    }

    pub fn column(self) -> usize {
        match self {
            Position::Utf16 { column, .. } => column,
            Position::Utf32 { column, .. } => column,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line(), self.column())
    }
}
