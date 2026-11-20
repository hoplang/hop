#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(usize);

impl FunctionId {
    pub fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct FunctionIdCounter(usize);

impl FunctionIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> FunctionId {
        let id = FunctionId(self.0);
        self.0 += 1;
        id
    }
}

#[cfg(test)]
mod test_support {

    use std::fmt;

    use super::FunctionId;

    impl fmt::Display for FunctionId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }
}
