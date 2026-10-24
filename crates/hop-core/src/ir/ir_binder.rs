use std::fmt;

use crate::ir::value_id::ValueId;

/// A name with the type of the values it takes, as a binding, a parameter,
/// a loop or a pattern binds it. `T` is a Type where the name can be Html,
/// and a DataType where it cannot.
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
