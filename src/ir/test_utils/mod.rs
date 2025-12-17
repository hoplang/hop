pub mod inlined_test_builder;
pub mod ir_test_builder;

pub use inlined_test_builder::build_inlined_auto;
pub use ir_test_builder::{build_ir_auto, build_ir_with_enums, build_ir_with_records};
