use std::fmt;

use crate::ir::value_id::ValueId;

/// Bindings in order, followed by the name of the block's result.
#[derive(Debug, Clone)]
pub struct IrBlock<Binding> {
    pub bindings: Vec<Binding>,
    pub result: ValueId,
}

impl<Binding: fmt::Display> fmt::Display for IrBlock<Binding> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for binding in &self.bindings {
            writeln!(f, "{binding}")?;
        }
        write!(f, "{}", self.result)
    }
}
