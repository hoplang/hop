mod ast;
mod compiler;
mod evaluator;
mod expr;
mod js_compiler;
pub mod passes;

pub use compiler::Compiler;
use expr::IrExpr;
pub use js_compiler::JsCompiler;
