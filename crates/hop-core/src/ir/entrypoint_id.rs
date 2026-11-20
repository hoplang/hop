use crate::ir::index_vec::Idx;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntrypointId(usize);

impl Idx for EntrypointId {
    fn new(index: usize) -> Self {
        EntrypointId(index)
    }

    fn index(self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod test_support {
    use super::EntrypointId;
    use std::fmt;

    impl fmt::Display for EntrypointId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}
