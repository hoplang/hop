use super::range::{Range, Ranged};

#[derive(Debug)]
pub struct RangedString(String, Range);

impl RangedString {
    pub fn push(&mut self, (ch, r): (char, Range)) {
        self.0.push(ch);
        self.1 = self.1.spanning(r);
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

