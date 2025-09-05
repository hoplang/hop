use super::Position;
use std::{cmp, fmt};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Range {
    start: Position,
    end: Position,
}

impl Range {
    pub(super) fn new(start: Position, end: Position) -> Self {
        debug_assert!(start < end, "start must be less than end in Range::new");
        Range { start, end }
    }

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

        // Get the first character to determine start position
        let first = ranged_chars.next()?;
        let start = first.1.start;

        // Find the last position by consuming remaining characters
        let end = ranged_chars
            .last()
            .map(|(_, range)| range.end())
            .unwrap_or(first.1.end());

        Some(Range::new(start, end))
    }

    /// Creates a range spanning from the earliest start to the latest end of two ranges.
    /// This operation is commutative: a.spanning(b) == b.spanning(a)
    pub fn spanning(&self, range: Range) -> Range {
        Range::new(
            cmp::min(self.start, range.start),
            cmp::max(self.end, range.end)
        )
    }

    pub fn contains(&self, position: Position) -> bool {
        position >= self.start && position < self.end
    }

    // returns the intersection of two ranges, or None if they don't overlap
    pub fn intersection(&self, other: &Range) -> Option<Range> {
        let start = cmp::max(self.start, other.start);
        let end = cmp::min(self.end, other.end);

        // If start >= end, there's no intersection
        if start >= end {
            None
        } else {
            Some(Range::new(start, end))
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

