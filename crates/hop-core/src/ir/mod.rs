mod assertion_id;
mod document_shell;
mod entrypoint_id;
mod flat_html_element;
mod flat_optimizer;
pub mod flat_program;
mod flat_to_writer;
mod flat_transform;
pub mod flat_type;
pub mod flat_type_declarations;
mod function_id;
mod ir_binder;
pub mod ir_binding;
mod ir_block;
mod ir_data_op;
mod ir_function;
mod ir_lambda;
pub mod ir_page;
pub mod ir_parameter;
pub mod ir_test;
mod lambda_id;
mod typed_to_flat;
mod value_id;
pub mod writer_program;
mod writer_switch;
pub mod writer_type;

#[cfg(test)]
pub mod flat_program_builder;
#[cfg(test)]
pub mod flat_program_generator;
#[cfg(test)]
pub mod flat_type_declarations_builder;
pub mod flat_typecheck;

pub mod runtime;
mod transpile;

pub use document_shell::{DocumentShell, TailwindInjection};
pub use flat_optimizer::optimize_flat;
pub use flat_to_writer::flat_to_writer;
pub use transpile::transpile;
pub use typed_to_flat::typed_to_flat;
