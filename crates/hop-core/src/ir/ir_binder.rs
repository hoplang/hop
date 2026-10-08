use crate::hop::typing::Type;
use crate::ir::var_id::VarId;

/// A variable a loop or a match arm binds, with the type of the values it
/// takes.
#[derive(Debug, Clone, PartialEq)]
pub struct IrBinder {
    pub var: VarId,
    pub typ: Type,
}
