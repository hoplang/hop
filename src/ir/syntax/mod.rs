pub mod ast;
mod compiler;
pub mod transform;

pub use ast::{IrEnumDeclaration, IrExpr, IrModule, IrRecordDeclaration};
pub use compiler::Compiler;
pub use transform::{
    AlphaRenamingPass, ConstantPropagationPass, Pass, UnusedIfEliminationPass,
    UnusedLetEliminationPass, WriteExprSimplificationPass,
};
