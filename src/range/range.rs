use super::Position;
use std::{cmp, fmt};

#[derive(Debug, Clone, Copy)]
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

    pub fn to(&self, other: Range) -> Range {
        Range::new(self.start, other.end)
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
