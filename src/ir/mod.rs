mod ast;
mod compiler;
mod evaluator;
mod expr;
pub mod js_compiler;
pub mod passes;

pub use compiler::Compiler;
use expr::IrExpr;
pub use js_compiler::{JsCompiler, LanguageMode};
