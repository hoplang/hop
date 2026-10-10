use crate::hop::typing::Type;
use crate::ir::binder_id::BinderId;
use crate::symbols::attribute_name::AttributeName;

/// A parameter of a function or of a Writer page. The name is what a call
/// names the argument by: the name a declaration gives the parameter, or the
/// name of the attribute a specialization receives through its rest.
#[derive(Debug, Clone, PartialEq)]
pub struct IrParameter {
    pub name: AttributeName,
    pub var: BinderId,
    pub typ: Type,
}

impl IrParameter {
    pub fn name(&self) -> &AttributeName {
        &self.name
    }
}

/// A parameter of a page. A page binds no names of its own, so its
/// parameter has a name and a type but no binder. The functions a page
/// points at declare the page's parameters, in order, each with a binder of
/// its own.
#[derive(Debug, Clone, PartialEq)]
pub struct PageParameter {
    pub name: AttributeName,
    pub typ: Type,
}

impl PageParameter {
    pub fn name(&self) -> &AttributeName {
        &self.name
    }
}
