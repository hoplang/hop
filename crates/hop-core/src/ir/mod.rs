mod document_shell;
mod flat_optimizer;
pub mod flat_program;
mod flat_to_writer;
mod flat_transform;
mod function_id;
mod ir_binder;
mod ir_data_op;
mod ir_for;
mod ir_function;
mod ir_html_element;
mod ir_match;
pub mod ir_page;
pub mod ir_parameter;
mod ir_switch;
mod typed_to_flat;
mod value_id;
pub mod writer_program;

#[cfg(test)]
pub mod flat_lint;
#[cfg(test)]
pub mod flat_program_builder;
#[cfg(test)]
pub mod flat_program_generator;

pub mod runtime;
pub mod transpile;

pub use document_shell::{DocumentShell, TailwindInjection};
pub use flat_optimizer::optimize_flat;
pub use flat_to_writer::flat_to_writer;
pub use transpile::{RustTranspiler, Transpiler, TsTranspiler};
pub use typed_to_flat::typed_to_flat;
