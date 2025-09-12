mod compiler;
mod evaluator;
mod js_compiler;

pub use compiler::Compiler;
pub use js_compiler::JsCompiler;

use crate::dop::Expr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum IrNode {
    /// Output a pre-computed string
    Write(String),

    /// Evaluate expression and output as string
    WriteExpr { expr: Expr, escape: bool },

    /// Conditional execution
    If { condition: Expr, body: Vec<IrNode> },

    /// Loop over array
    For {
        var: String,
        array: Expr,
        body: Vec<IrNode>,
    },

    /// Variable binding
    Let {
        var: String,
        value: Expr,
        body: Vec<IrNode>,
    },
}

#[derive(Debug)]
pub struct IrEntrypoint {
    /// Original parameter names (for function signature)
    pub parameters: Vec<String>,
    /// IR nodes for the entrypoint body
    pub body: Vec<IrNode>,
}

#[derive(Debug)]
pub struct IrModule {
    /// Map from component name to its IR representation
    pub entry_points: HashMap<String, IrEntrypoint>,
}

impl IrModule {
    pub fn new() -> Self {
        Self {
            entry_points: HashMap::new(),
        }
    }
}
