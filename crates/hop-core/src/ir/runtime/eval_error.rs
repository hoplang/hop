use thiserror::Error;

use crate::ir::ir_function::IrFunction;
use crate::symbols::{attribute_name::AttributeName, type_name::TypeName};

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Page '{page}' not found in module")]
    PageNotFound { page: TypeName },
    #[error("Missing required parameter '{param}' for page '{page}'")]
    MissingParameter {
        page: TypeName,
        param: AttributeName,
    },
    #[error("Function '{function}' not found in module")]
    FunctionNotFound { function: IrFunction },
    #[error("Missing required parameter '{param}' for function '{function}'")]
    MissingFunctionParameter {
        function: IrFunction,
        param: AttributeName,
    },
    #[error("Unknown argument '{name}' for function '{function}'")]
    UnknownArgument {
        function: IrFunction,
        name: AttributeName,
    },
    #[error("Function '{function}' exceeded the call depth limit of {limit}")]
    RecursionLimit { function: IrFunction, limit: usize },
}
