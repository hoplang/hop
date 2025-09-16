mod ast;
mod compiler;
pub mod evaluator;
pub mod js_transpiler;
pub mod optimizer;
pub mod passes;
#[cfg(test)]
pub mod test_utils;

pub use ast::IrExpr;
pub use compiler::{CompilationMode, Compiler};
pub use js_transpiler::{JsTranspiler, LanguageMode};
