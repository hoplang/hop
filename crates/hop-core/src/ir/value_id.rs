use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(usize);

impl ValueId {
    pub fn index(self) -> usize {
        self.0
    }
}

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct ValueIdCounter(usize);

impl ValueIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> ValueId {
        let id = ValueId(self.0);
        self.0 += 1;
        id
    }

    pub fn count(&self) -> usize {
        self.0
    }
}
