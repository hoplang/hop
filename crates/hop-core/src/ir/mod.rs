mod compiler;
mod expr_id;
mod ir_var;
mod lower_pure;
mod optimizer;
pub mod pure_module;
mod transform;
mod var_id;
mod writer_module;

#[cfg(test)]
pub mod pure_module_builder;
#[cfg(test)]
pub mod pure_module_generator;

pub mod runtime;
pub mod transpile;

pub use compiler::compile;
pub use lower_pure::lower_pure;
pub use optimizer::optimize;
pub use transform::retain_reachable;
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
pub use writer_module::WriterModule;
