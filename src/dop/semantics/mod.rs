pub mod r#type;
pub mod type_checker;
pub mod type_error;
pub mod typecheck_match;
pub mod typed;

pub use r#type::Type;
pub use type_checker::{resolve_type, typecheck_expr};
pub use typecheck_match::{extract_bindings_from_pattern, validate_pattern_type};
pub use typed::TypedExpr;
