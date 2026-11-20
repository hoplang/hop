use crate::ir::value_id::ValueId;

#[derive(Debug, Clone)]
pub struct IrBinder<T> {
    pub var: ValueId,
    pub typ: T,
}

#[cfg(test)]
mod test_support {
    use super::IrBinder;
    use std::fmt;

    impl<T: fmt::Display> fmt::Display for IrBinder<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}: {}", self.var, self.typ)
        }
    }
}
