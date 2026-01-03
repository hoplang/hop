mod pat_match;
pub mod r#type;
pub mod type_checker;
pub mod type_error;
mod typecheck_match;
pub mod typed;

pub use r#type::Type;
pub use type_checker::{resolve_type, typecheck_expr};
pub use typed::TypedExpr;
