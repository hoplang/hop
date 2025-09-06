use std::fmt;

use super::Range;

/// Represents a position in source code
#[derive(Debug, Clone, Hash, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    pub(super) fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }

    pub fn line(self) -> usize {
        self.line
    }

    pub fn column(self) -> usize {
        self.column
    }

    pub fn to(self, end: Position) -> super::Range {
        Range::new(self, end)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line(), self.column())
    }
}

impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(position: tower_lsp::lsp_types::Position) -> Self {
        Self::new(position.line as usize + 1, position.character as usize + 1)
    }
}

impl From<Position> for tower_lsp::lsp_types::Position {
    fn from(position: Position) -> Self {
        tower_lsp::lsp_types::Position {
            line: (position.line() - 1) as u32,
            character: (position.column() - 1) as u32,
        }
    }
}
