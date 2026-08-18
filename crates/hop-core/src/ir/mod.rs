mod compiler;
mod ir_module;
mod optimizer;
mod transform;

#[cfg(test)]
pub mod ir_module_builder;
#[cfg(test)]
pub mod ir_module_generator;

pub mod runtime;
pub mod transpile;

pub use compiler::compile;
pub use ir_module::{IrExpr, IrModule};
pub use optimizer::optimize;
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
