use std::fmt;

use crate::ir::var_id::VarId;

/// A bound variable in the IR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IrVar {
    pub id: VarId,
}

impl IrVar {
    pub fn new(id: VarId) -> Self {
        Self { id }
    }
}

impl fmt::Display for IrVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.id)
    }
}
