pub mod r#type;
pub mod type_checker;
pub mod type_error;
pub mod typed;

pub use r#type::Type;
pub use type_checker::{extract_bindings_from_pattern, resolve_type, typecheck_expr};
pub use typed::TypedExpr;
