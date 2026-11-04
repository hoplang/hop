use std::fmt;

/// Identity of an entrypoint in the IR.
///
/// Every declaration has its own unique EntrypointId. Equal EntrypointIds mean the
/// same entrypoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntrypointId(usize);

impl fmt::Display for EntrypointId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct EntrypointIdCounter(usize);

impl EntrypointIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> EntrypointId {
        let id = EntrypointId(self.0);
        self.0 += 1;
        id
    }
}
