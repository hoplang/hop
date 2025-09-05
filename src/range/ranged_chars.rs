use super::{Position, Range};
use std::str::Chars;

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