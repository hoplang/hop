mod optimizer;
pub mod runtime;
pub mod syntax;
pub mod transpile;

pub use optimizer::optimize;
pub use syntax::ast;
pub use syntax::{IrExpr, IrModule, compile_module};
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
