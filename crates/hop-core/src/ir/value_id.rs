use std::fmt;

/// Identity of a name in the IR: a binding, which names the value its op
/// computes, or a parameter, a loop variable or a binding of a pattern.
///
/// Every name is bound once. Equal ValueIds mean the same name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(usize);

impl ValueId {
    /// The number that distinguishes this id, for building an identifier
    /// from it.
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

    /// How many ids have been handed out, one more than the largest index.
    #[cfg(test)]
    pub fn count(&self) -> usize {
        self.0
    }
}
