use std::fmt;

use crate::document::CheapString;
use crate::ir::function_id::FunctionId;
use crate::symbols::function_name::FunctionName;

/// A function in the IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrFunction {
    pub id: FunctionId,
    pub name: FunctionName,
}

impl IrFunction {
    pub fn new(id: FunctionId, name: FunctionName) -> Self {
        Self { id, name }
    }

    /// The function that renders a page's head, named after the member it
    /// is declared as.
    pub fn page_head(id: FunctionId) -> Self {
        Self::named(id, "head")
    }

    /// The function that renders a page's body, named after the member it
    /// is declared as.
    pub fn page_body(id: FunctionId) -> Self {
        Self::named(id, "body")
    }

    fn named(id: FunctionId, name: &str) -> Self {
        let name = FunctionName::new(CheapString::new(name.to_string()))
            .expect("a page member name is a valid function name");
        Self::new(id, name)
    }
}

impl fmt::Display for IrFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@f{}", self.name, self.id)
    }
}
