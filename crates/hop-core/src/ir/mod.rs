mod optimizer;
pub mod runtime;
pub mod syntax;
pub mod transpile;

pub use optimizer::optimize;
pub use syntax::ast;
pub use syntax::{Compiler, IrEnumDeclaration, IrExpr, IrModule, IrRecordDeclaration};
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
