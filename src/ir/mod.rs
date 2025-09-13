mod ast;
mod compiler;
mod evaluator;
pub mod js_compiler;
pub mod passes;

pub use ast::IrExpr;
pub use compiler::Compiler;
pub use js_compiler::{JsCompiler, LanguageMode};
