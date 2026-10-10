use std::fmt;

/// Identity of a binder in the IR: a parameter, or a variable that a let, a
/// loop or a match arm binds.
///
/// Every binder has its own unique BinderId. Equal BinderIds mean the same
/// binder.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BinderId(usize);

impl BinderId {
    /// The number that distinguishes this id, for building an identifier
    /// from it.
    pub fn index(self) -> usize {
        self.0
    }
}

impl fmt::Display for BinderId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BinderIdCounter(usize);

impl BinderIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> BinderId {
        let id = BinderId(self.0);
        self.0 += 1;
        id
    }
}
