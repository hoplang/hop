use super::range::{Position, Range};
use std::str::Chars;

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
}

impl<'a> From<&'a str> for RangedChars<'a> {
    fn from(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            position: Position::default_position(),
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
