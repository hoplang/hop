use std::fmt;

/// Identity of a binding in the Flat IR, and of a let in the Writer: the
/// name of a value an op computes. A binder has a BinderId instead.
///
/// Every binding has its own unique VarId. Equal VarIds mean the same
/// binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

impl VarId {
    /// The number that distinguishes this id, for building an identifier
    /// from it.
    pub fn index(self) -> usize {
        self.0
    }
}

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct VarIdCounter(usize);

impl VarIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> VarId {
        let id = VarId(self.0);
        self.0 += 1;
        id
    }
}
