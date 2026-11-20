use crate::ir::flat_type::{FlatDataType, FlatType};
use crate::ir::ir_binder::IrBinder;
use crate::symbols::attribute_name::AttributeName;

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

impl IrParameter<FlatDataType> {
    pub fn widen(&self) -> IrParameter<FlatType> {
        IrParameter {
            name: self.name.clone(),
            binder: IrBinder {
                var: self.binder.var,
                typ: FlatType::Data(self.binder.typ.clone()),
            },
        }
    }
}

#[cfg(test)]
mod test_support {
    use super::IrParameter;
    use std::fmt;

    impl<T: fmt::Display> fmt::Display for IrParameter<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@{}", self.name.as_str(), self.binder)
        }
    }
}
