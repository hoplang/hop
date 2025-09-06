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
        Self { start, end }
    }

    pub fn start(&self) -> Position {
        self.start
    }

    pub fn end(&self) -> Position {
        self.end
    }

    /// Creates a range spanning from the earliest start to the latest end of two ranges.
    /// This operation is commutative: a.spanning(b) == b.spanning(a)
    pub fn spanning(&self, range: Range) -> Range {
        Range::new(
            cmp::min(self.start, range.start),
            cmp::max(self.end, range.end),
        )
    }

    pub fn contains(&self, position: Position) -> bool {
        position >= self.start && position < self.end
    }

    pub fn contains_range(&self, range: Range) -> bool {
        self.start <= range.start && range.end <= self.end
    }

    pub fn intersection(&self, other: Range) -> Option<Range> {
        let start = cmp::max(self.start, other.start);
        let end = cmp::min(self.end, other.end);
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

impl From<Range> for tower_lsp::lsp_types::Range {
    fn from(range: Range) -> Self {
        tower_lsp::lsp_types::Range {
            start: range.start().into(),
            end: range.end().into(),
        }
    }
}

pub trait Ranged {
    fn range(&self) -> Range;
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
