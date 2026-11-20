use std::fmt;

use crate::ir::ir_binder::IrBinder;

#[derive(Debug, Clone)]
pub struct IrBinding<Type, Op> {
    pub binder: IrBinder<Type>,
    pub op: Op,
}

impl<Type: fmt::Display, Op: fmt::Display> fmt::Display for IrBinding<Type, Op> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.binder, self.op)
    }
}
