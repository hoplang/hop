use crate::ir::value_id::ValueId;

#[derive(Debug, Clone)]
pub struct IrBlock<Binding> {
    pub bindings: Vec<Binding>,
    pub result: ValueId,
}

#[cfg(test)]
mod test_support {
    use super::IrBlock;
    use std::fmt;

    impl<Binding: fmt::Display> fmt::Display for IrBlock<Binding> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for binding in &self.bindings {
                writeln!(f, "{binding}")?;
            }
            write!(f, "{}", self.result)
        }
    }
}
