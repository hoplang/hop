use std::fmt;

/// Identity of a bound variable in the IR.
///
/// Every binder has its own unique VarId. Equal VarIds mean the same binder.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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
