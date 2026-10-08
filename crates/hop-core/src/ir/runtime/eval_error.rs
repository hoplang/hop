use thiserror::Error;

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
}
