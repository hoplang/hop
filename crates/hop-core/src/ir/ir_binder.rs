use crate::hop::typing::Type;
use crate::ir::binder_id::BinderId;

/// A variable a let, a loop or a match arm binds, with the type of the values
/// it takes.
#[derive(Debug, Clone, PartialEq)]
pub struct IrBinder {
    pub var: BinderId,
    pub typ: Type,
}
