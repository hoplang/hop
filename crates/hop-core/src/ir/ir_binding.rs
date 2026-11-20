use crate::ir::ir_binder::IrBinder;

#[derive(Debug, Clone)]
pub struct IrBinding<Type, Op> {
    pub binder: IrBinder<Type>,
    pub op: Op,
}

#[cfg(test)]
mod test_support {
    use super::IrBinding;
    use std::fmt;

    impl<Type: fmt::Display, Op: fmt::Display> fmt::Display for IrBinding<Type, Op> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "let {} = {}", self.binder, self.op)
        }
    }
}
