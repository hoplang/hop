use std::fmt;

use crate::hop::typing::{DataType, Type};
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

impl IrParameter<Type> {
    /// The parameter as one of a function that returns a value, or None if
    /// it takes Html.
    pub fn into_data(self) -> Option<IrParameter<DataType>> {
        let Type::Data(typ) = self.binder.typ else {
            return None;
        };
        Some(IrParameter {
            name: self.name,
            binder: IrBinder {
                var: self.binder.var,
                typ,
            },
        })
    }
}

impl IrParameter<DataType> {
    /// The parameter as one of a function that may take Html.
    pub fn widen(&self) -> IrParameter<Type> {
        IrParameter {
            name: self.name.clone(),
            binder: IrBinder {
                var: self.binder.var,
                typ: Type::Data(self.binder.typ.clone()),
            },
        }
    }
}

impl<T: fmt::Display> fmt::Display for IrParameter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}", self.name.as_str(), self.binder)
    }
}
