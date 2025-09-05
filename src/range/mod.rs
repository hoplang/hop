use std::{cmp, fmt, str::Chars};

pub mod source_annotator;

pub use source_annotator::{Annotated, SourceAnnotator};

#[cfg(test)]
pub use source_annotator::SimpleAnnotation;

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
}

impl Default for Position {
    fn default() -> Self {
        Position { line: 1, column: 1 }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

impl Range {
    pub fn new(start: Position, end: Position) -> Self {
        debug_assert!(start < end, "start must be less than end in Range::new");
        Range { start, end }
    }

    pub fn extend_to(&self, range: Range) -> Range {
        Range::new(self.start, range.end)
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

#[derive(Debug, Clone)]
pub struct RangedChars<'a> {
    chars: Chars<'a>,
    position: Position,
}

impl<'a> RangedChars<'a> {
    pub fn new(input: &'a str, position: Position) -> Self {
        Self {
            chars: input.chars(),
            position,
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
            (ch, Range::new(start, self.position))
        })
    }
}

#[derive(Debug)]
pub struct RangedString(String, Range);

impl RangedString {
    pub fn push(&mut self, (ch, r): (char, Range)) {
        self.0.push(ch);
        self.1 = self.1.extend_to(r);
    }
    pub fn value(&self) -> &str {
        &self.0
    }
}

impl From<RangedString> for (String, Range) {
    fn from(val: RangedString) -> Self {
        (val.0, val.1)
    }
}

impl From<(char, Range)> for RangedString {
    fn from((ch, r): (char, Range)) -> Self {
        RangedString(String::from(ch), r)
    }
}

impl Ranged for RangedString {
    fn range(&self) -> Range {
        self.1
    }
}
