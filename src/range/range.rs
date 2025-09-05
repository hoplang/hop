use std::{cmp, fmt};

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
    pub fn to(self, end: Position) -> Range {
        debug_assert!(self < end, "start must be less than end in Position::to");
        Range { start: self, end }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Range {
    start: Position,
    end: Position,
}

impl Range {
    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }

    /// Creates a range that spans the entire string from start to end.
    /// Returns None for empty strings since they don't have a valid range.
    pub fn for_string(input: &str) -> Option<Self> {
        use super::RangedChars;
        let mut ranged_chars = RangedChars::from(input);
        let first = ranged_chars.next()?;
        let last = ranged_chars.last().unwrap_or(first);
        Some(first.1.spanning(last.1))
    }

    /// Creates a range spanning from the earliest start to the latest end of two ranges.
    /// This operation is commutative: a.spanning(b) == b.spanning(a)
    pub fn spanning(&self, range: Range) -> Range {
        Range {
            start: cmp::min(self.start, range.start),
            end: cmp::max(self.end, range.end),
        }
    }

    pub fn contains(&self, position: Position) -> bool {
        position >= self.start && position < self.end
    }

    pub fn intersection(&self, other: &Range) -> Option<Range> {
        let start = cmp::max(self.start, other.start);
        let end = cmp::min(self.end, other.end);

        // If start >= end, there's no intersection
        if start >= end {
            None
        } else {
            Some(Range { start, end })
        }
    }
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl fmt::Debug for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Trait for types that have a Range
pub trait Ranged {
    /// Returns the range of this item
    fn range(&self) -> Range;

    /// Returns true if the position lies within this item's range
    /// (start inclusive, end exclusive)
    fn contains(&self, position: Position) -> bool {
        self.range().contains(position)
    }
}

impl Ranged for Range {
    fn range(&self) -> Range {
        *self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_for_string_empty() {
        let range = Range::for_string("");
        assert_eq!(range, None);
    }

    #[test]
    fn test_for_string_single_line() {
        let range = Range::for_string("hello").unwrap();
        assert_eq!(range.start(), Position::new(1, 1));
        assert_eq!(range.end(), Position::new(1, 6));
    }

    #[test]
    fn test_for_string_multiline() {
        let range = Range::for_string("hello\nworld").unwrap();
        assert_eq!(range.start(), Position::new(1, 1));
        assert_eq!(range.end(), Position::new(2, 6));
    }

    #[test]
    fn test_for_string_with_unicode() {
        let range = Range::for_string("hello 😀 world").unwrap();
        assert_eq!(range.start(), Position::new(1, 1));
        // 😀 emoji is 4 bytes, so: "hello " (6) + "😀" (4) + " world" (6) = 16, +1 for position = 17
        assert_eq!(range.end(), Position::new(1, 17));
    }

    #[test]
    fn test_for_string_multiple_newlines() {
        let range = Range::for_string("line1\nline2\nline3").unwrap();
        assert_eq!(range.start(), Position::new(1, 1));
        assert_eq!(range.end(), Position::new(3, 6));
    }
}
