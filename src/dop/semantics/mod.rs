pub mod typed;
pub mod r#type;
pub mod type_checker;
pub mod type_error;

pub use typed::TypedExpr;
pub use r#type::Type;
pub use type_checker::{resolve_type, typecheck_expr};
