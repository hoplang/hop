mod ast;
mod compiler;
mod evaluator;
pub mod js_compiler;
pub mod passes;
#[cfg(test)]
pub mod test_utils;

pub use ast::IrExpr;
pub use compiler::Compiler;
pub use js_compiler::{JsCompiler, LanguageMode};
