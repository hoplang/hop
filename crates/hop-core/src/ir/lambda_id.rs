#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LambdaId(usize);

#[derive(Debug, Clone, Copy, Default)]
pub struct LambdaIdCounter(usize);

impl LambdaIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> LambdaId {
        let id = LambdaId(self.0);
        self.0 += 1;
        id
    }
}

#[cfg(test)]
mod test_support {

    use std::fmt;

    use super::LambdaId;

    impl fmt::Display for LambdaId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "l{}", self.0)
        }
    }
}
