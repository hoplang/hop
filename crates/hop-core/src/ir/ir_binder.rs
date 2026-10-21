use std::fmt;

use crate::hop::typing::Type;
use crate::ir::value_id::ValueId;

/// A name with the type of the values it takes, as a binding, a parameter,
/// a loop or a pattern binds it.
#[derive(Debug, Clone)]
pub struct IrBinder {
    pub var: ValueId,
    pub typ: Type,
}

impl fmt::Display for IrBinder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.var, self.typ)
    }
}
