use std::fmt;

/// Represents a position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    /// Line number (1-based)
    pub line: usize,
    /// Byte column within the line (1-based)
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }
    
    /// Internal default position for use within the range module only
    pub(super) fn default_position() -> Self {
        Position { line: 1, column: 1 }
    }

    /// Creates a Range from this position to another position.
    pub fn to(&self, other: Position) -> super::Range {
        super::Range::new(*self, other)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}