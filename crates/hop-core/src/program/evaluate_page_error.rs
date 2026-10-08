#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EvaluatePageError {
    #[error("Cannot evaluate page: program has parse errors")]
    ParseErrors,

    #[error("Cannot evaluate page: program has type errors")]
    TypeErrors,

    #[error("Invalid page name '{page}': {reason}")]
    InvalidPageName { page: String, reason: String },

    #[error("Page '{page}' not found. Available pages: {}", available.join(", "))]
    PageNotFound {
        page: String,
        available: Vec<String>,
    },

    #[error("Missing required parameter '{param}' for page '{page}'")]
    MissingParameter { page: String, param: String },

    #[error("Function '{function}' exceeded the call depth limit of {limit}")]
    RecursionLimit { function: String, limit: usize },
}
