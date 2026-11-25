#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssertionId(usize);

impl AssertionId {
    pub fn new(index: usize) -> Self {
        AssertionId(index)
    }

    pub fn index(self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod test_support {
    use super::AssertionId;
    use std::fmt;

    impl fmt::Display for AssertionId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}
