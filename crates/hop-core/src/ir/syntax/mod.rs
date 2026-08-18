pub mod ast;
#[cfg(test)]
pub mod builder;
mod compiler;
#[cfg(test)]
pub mod random;
pub mod transform;

pub use ast::{IrExpr, IrModule};
pub use compiler::compile_module;
