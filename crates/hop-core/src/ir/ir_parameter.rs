use std::fmt;

use crate::ir::ir_binder::IrBinder;
use crate::symbols::attribute_name::AttributeName;

/// A parameter of a function or of a Writer page. The name is what a call
/// names the argument by: the name a declaration gives the parameter, or the
/// name of the attribute a specialization receives through its rest. `T` is
/// the type of its binder.
#[derive(Debug, Clone)]
pub struct IrParameter<T> {
    pub name: AttributeName,
    pub binder: IrBinder<T>,
}

impl<T> IrParameter<T> {
    pub fn name(&self) -> &AttributeName {
        &self.name
    }
}

impl<T: fmt::Display> fmt::Display for IrParameter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.name.as_str(), self.binder)
    }
}
