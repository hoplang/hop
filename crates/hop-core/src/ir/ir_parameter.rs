use crate::hop::typing::Type;
use crate::ir::var_id::VarId;
use crate::symbols::attribute_name::AttributeName;

/// A parameter of a page or a function. The name is what a call names the
/// argument by: the name a declaration gives the parameter, or the name of
/// the attribute a specialization receives through its rest.
#[derive(Debug, Clone, PartialEq)]
pub struct IrParameter {
    pub name: AttributeName,
    pub var: VarId,
    pub typ: Type,
}

impl IrParameter {
    pub fn name(&self) -> &AttributeName {
        &self.name
    }
}
