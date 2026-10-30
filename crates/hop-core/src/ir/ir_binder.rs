use std::fmt;

use crate::ir::value_id::ValueId;

#[derive(Debug, Clone)]
pub struct IrBinder<T> {
    pub var: ValueId,
    pub typ: T,
}

impl<T: fmt::Display> fmt::Display for IrBinder<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var, self.typ)
    }
}
