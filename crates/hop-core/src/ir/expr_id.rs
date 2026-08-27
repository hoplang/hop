/// Unique identifier for each expression in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(usize);

#[derive(Debug, Clone, Copy, Default)]
pub struct ExprIdCounter(usize);

impl ExprIdCounter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn next(&mut self) -> ExprId {
        let id = ExprId(self.0);
        self.0 += 1;
        id
    }
}
