use thiserror::Error;

use crate::symbols::function_name::FunctionName;
use crate::symbols::{attribute_name::AttributeName, type_name::TypeName};

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum EvalError {
    #[error("Page '{page}' not found in program")]
    PageNotFound { page: TypeName },
    #[error("Missing required parameter '{param}' for entrypoint '{entrypoint}'")]
    MissingParameter {
        entrypoint: FunctionName,
        param: AttributeName,
    },
    #[error("Program exceeded the call depth limit of {limit}")]
    CallDepthLimit { limit: usize },
    #[error("An array exceeded the array length limit of {limit}")]
    ArrayLengthLimit { limit: usize },
    #[error("A string exceeded the string length limit of {limit} bytes")]
    StringLengthLimit { limit: usize },
    #[error("An Html value exceeded the Html size limit of {limit}")]
    HtmlSizeLimit { limit: usize },
    #[error("An evaluation exceeded the op count limit of {limit}")]
    OpCountLimit { limit: usize },
}
