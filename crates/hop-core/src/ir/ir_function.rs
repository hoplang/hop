use crate::ir::function_id::FunctionId;
use crate::symbols::function_name::FunctionName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrFunction {
    pub id: FunctionId,
    pub name: FunctionName,
}

impl IrFunction {
    pub fn new(id: FunctionId, name: FunctionName) -> Self {
        Self { id, name }
    }
}

#[cfg(test)]
mod test_support {
    use super::IrFunction;
    use std::fmt;

    impl fmt::Display for IrFunction {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}@f{}", self.name, self.id)
        }
    }
}
