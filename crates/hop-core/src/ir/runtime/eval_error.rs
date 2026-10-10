use thiserror::Error;

use crate::ir::ir_function::IrFunction;
use crate::symbols::{attribute_name::AttributeName, type_name::TypeName};

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Page '{page}' not found in module")]
    PageNotFound { page: TypeName },
    #[error("Missing required parameter '{param}' for function '{function}'")]
    MissingParameter {
        function: IrFunction,
        param: AttributeName,
    },
    #[error("Function '{function}' not found in module")]
    FunctionNotFound { function: IrFunction },
    #[error("Function '{function}' takes {expected} arguments but was given {found}")]
    ArgumentCount {
        function: IrFunction,
        expected: usize,
        found: usize,
    },
    #[error("Function '{function}' exceeded the call depth limit of {limit}")]
    RecursionLimit { function: IrFunction, limit: usize },
}
