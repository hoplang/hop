use std::{cmp, fmt, str::Chars};

/// Represents a position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    /// Line number (0-based)
    line: usize,
    /// Byte column within the line (1-based)
    column: usize,
}

impl Position {
    #[cfg(test)]
    pub(super) fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }

    pub(super) fn line(self) -> usize {
        self.line
    }

    pub(super) fn column(self) -> usize {
        self.column
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

impl From<tower_lsp::lsp_types::Position> for Position {
    fn from(position: tower_lsp::lsp_types::Position) -> Self {
        Self {
            line: position.line as usize + 1,
            column: position.character as usize + 1,
        }
    }
}

impl From<Position> for tower_lsp::lsp_types::Position {
    fn from(position: Position) -> Self {
        tower_lsp::lsp_types::Position {
            line: (position.line - 1) as u32,
            character: (position.column - 1) as u32,
        }
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

    /// Creates a range spanning from the earliest start to the latest end of two ranges.
    /// This operation is commutative: a.spanning(b) == b.spanning(a)
    pub fn spanning(&self, range: Range) -> Range {
        let result = Range {
            start: cmp::min(self.start, range.start),
            end: cmp::max(self.end, range.end),
        };
        debug_assert!(
            result.start < result.end,
            "start must be less than end in Range::spanning"
        );
        result
    }

    pub fn contains(&self, position: Position) -> bool {
        position >= self.start && position < self.end
    }

    pub fn contains_range(&self, range: Range) -> bool {
        self.start <= range.start && range.end <= self.end
    }

    pub fn intersection(&self, other: &Range) -> Option<Range> {
        let start = cmp::max(self.start, other.start);
        let end = cmp::min(self.end, other.end);

        // If start >= end, there's no intersection
        if start >= end {
            None
        } else {
            let result = Range { start, end };
            debug_assert!(
                result.start < result.end,
                "start must be less than end in Range::spanning"
            );
            Some(result)
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

impl From<Range> for tower_lsp::lsp_types::Range {
    fn from(range: Range) -> Self {
        tower_lsp::lsp_types::Range {
            start: range.start().into(),
            end: range.end().into(),
        }
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

impl<T: Ranged> Ranged for &T {
    fn range(&self) -> Range {
        (*self).range()
    }
}

#[derive(Debug, Clone)]
pub struct RangedChars<'a> {
    chars: Chars<'a>,
    position: Position,
}

impl<'a> RangedChars<'a> {
    pub fn with_position(input: &'a str, position: Position) -> Self {
        Self {
            chars: input.chars(),
            position,
        }
    }

    pub fn split_by<F>(
        mut self,
        predicate: F,
    ) -> impl Iterator<Item = super::ranged_string::RangedString>
    where
        F: Fn(char) -> bool,
    {
        let mut current_segment: Option<super::ranged_string::RangedString> = None;

        std::iter::from_fn(move || {
            loop {
                match self.next() {
                    Some((ch, range)) => {
                        if predicate(ch) {
                            // Hit a delimiter, return current segment if we have one
                            if current_segment.is_some() {
                                return current_segment.take();
                            }
                            // Otherwise continue to skip delimiters
                        } else {
                            // Non-delimiter character, add to current segment
                            match current_segment.as_mut() {
                                Some(segment) => segment.push((ch, range)),
                                None => {
                                    current_segment =
                                        Some(super::ranged_string::RangedString::from((ch, range)))
                                }
                            }
                        }
                    }
                    None => {
                        // End of input, return final segment if we have one
                        return current_segment.take();
                    }
                }
            }
        })
    }

    pub fn lines(self) -> impl Iterator<Item = (usize, super::ranged_string::RangedString)> {
        self.split_by(|ch| ch == '\n')
            .map(|line| (line.range().start().line(), line))
    }
}

impl<'a> From<&'a str> for RangedChars<'a> {
    fn from(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            position: Position { line: 1, column: 1 },
        }
    }
}

impl Iterator for RangedChars<'_> {
    type Item = (char, Range);
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.position;
        self.chars.next().map(|ch| {
            if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += ch.len_utf8();
            }
            (ch, start.to(self.position))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_by_whitespace() {
        let input = "foo   bar  baz";
        let chars = RangedChars::from(input);
        let segments: Vec<_> = chars.split_by(|ch| ch.is_whitespace()).collect();

        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0].value(), "foo");
        assert_eq!(segments[1].value(), "bar");
        assert_eq!(segments[2].value(), "baz");
    }

    #[test]
    fn test_split_by_with_leading_trailing_delimiters() {
        let input = "  foo bar  ";
        let chars = RangedChars::from(input);
        let segments: Vec<_> = chars.split_by(|ch| ch.is_whitespace()).collect();

        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0].value(), "foo");
        assert_eq!(segments[1].value(), "bar");
    }

    #[test]
    fn test_split_by_empty_input() {
        let input = "";
        let chars = RangedChars::from(input);
        let segments: Vec<_> = chars.split_by(|ch| ch.is_whitespace()).collect();

        assert_eq!(segments.len(), 0);
    }

    #[test]
    fn test_split_by_only_delimiters() {
        let input = "   ";
        let chars = RangedChars::from(input);
        let segments: Vec<_> = chars.split_by(|ch| ch.is_whitespace()).collect();

        assert_eq!(segments.len(), 0);
    }

    #[test]
    fn test_split_by_custom_predicate() {
        let input = "foo,bar;baz";
        let chars = RangedChars::from(input);
        let segments: Vec<_> = chars.split_by(|ch| ch == ',' || ch == ';').collect();

        assert_eq!(segments.len(), 3);
        assert_eq!(segments[0].value(), "foo");
        assert_eq!(segments[1].value(), "bar");
        assert_eq!(segments[2].value(), "baz");
    }
}
