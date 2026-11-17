use std::fmt;

use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_type::{IrDataType, IrType};
use crate::symbols::attribute_name::AttributeName;

/// A parameter of a function, an entrypoint, or a Writer page. The name is what a call
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

impl IrParameter<IrDataType> {
    /// The data parameter widened to the type of a function or Writer parameter.
    pub fn widen(&self) -> IrParameter<IrType> {
        IrParameter {
            name: self.name.clone(),
            binder: IrBinder {
                var: self.binder.var,
                typ: IrType::Data(self.binder.typ.clone()),
            },
        }
    }
}

impl<T: fmt::Display> fmt::Display for IrParameter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.name.as_str(), self.binder)
    }
}
