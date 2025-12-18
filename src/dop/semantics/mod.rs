pub mod expr;
pub mod r#type;
pub mod type_checker;
pub mod type_error;

pub use expr::Expr;
pub use r#type::Type;
pub use type_checker::{resolve_type, typecheck_expr};
