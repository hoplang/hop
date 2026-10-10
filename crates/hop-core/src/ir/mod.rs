mod binder_id;
mod document_shell;
pub mod flat_module;
mod flat_optimizer;
mod flat_to_writer;
mod flat_transform;
mod function_id;
mod ir_binary_op;
mod ir_binder;
mod ir_function;
mod ir_match;
pub mod ir_page;
pub mod ir_parameter;
mod ir_unary_op;
pub mod pure_module;
mod pure_to_flat;
mod typed_to_pure;
mod value_id;
pub mod writer_module;

#[cfg(test)]
pub mod pure_module_builder;
#[cfg(test)]
pub mod pure_module_generator;

pub mod runtime;
pub mod transpile;

pub use document_shell::{DocumentShell, TailwindInjection};
pub use flat_optimizer::optimize_flat;
pub use flat_to_writer::flat_to_writer;
pub use pure_to_flat::pure_to_flat;
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
pub use typed_to_pure::typed_to_pure;
