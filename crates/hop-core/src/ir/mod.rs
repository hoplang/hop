mod optimizer;
pub mod semantics;
pub mod syntax;
pub mod transpile;

#[cfg(test)]
mod end_to_end_tests;

pub use optimizer::optimize;
pub use syntax::ast;
pub use syntax::{Compiler, IrEnumDeclaration, IrExpr, IrModule, IrRecordDeclaration};
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
