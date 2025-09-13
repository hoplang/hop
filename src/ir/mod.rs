mod compiler;
mod evaluator;
mod js_compiler;
pub mod passes;

pub use compiler::Compiler;
pub use js_compiler::JsCompiler;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum IrExpr {
    /// Variable reference
    Var(String),

    /// Property access (e.g., obj.prop)
    PropertyAccess {
        object: Box<IrExpr>,
        property: String,
    },

    /// String literal
    String(String),

    /// Boolean literal
    Boolean(bool),

    /// Number literal
    Number(f64),

    /// Array literal
    Array(Vec<IrExpr>),

    /// Object literal
    Object(Vec<(String, IrExpr)>),

    /// Binary operation
    BinaryOp {
        left: Box<IrExpr>,
        op: BinaryOp,
        right: Box<IrExpr>,
    },

    /// Unary operation
    UnaryOp { op: UnaryOp, operand: Box<IrExpr> },
}

/// Binary operators in IR
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

/// Unary operators in IR
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrNode {
    /// Output a pre-computed string
    Write(String),

    /// Evaluate expression and output as string
    WriteExpr { expr: IrExpr, escape: bool },

    /// Conditional execution
    If {
        condition: IrExpr,
        body: Vec<IrNode>,
    },

    /// Loop over array
    For {
        var: String,
        array: IrExpr,
        body: Vec<IrNode>,
    },

    /// Variable binding
    Let {
        var: String,
        value: IrExpr,
        body: Vec<IrNode>,
    },
}

#[derive(Debug, Default)]
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

impl fmt::Display for IrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrExpr::Var(name) => write!(f, "{}", name),
            IrExpr::PropertyAccess { object, property } => {
                write!(f, "{}.{}", object, property)
            }
            IrExpr::String(s) => write!(f, "{:?}", s),
            IrExpr::Boolean(b) => write!(f, "{}", b),
            IrExpr::Number(n) => write!(f, "{}", n),
            IrExpr::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            IrExpr::Object(props) => {
                write!(f, "{{")?;
                for (i, (key, value)) in props.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            IrExpr::BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            IrExpr::UnaryOp { op, operand } => {
                write!(f, "{}{}", op, operand)
            }
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Equal => write!(f, "=="),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

impl fmt::Display for IrNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_nodes(nodes: &[IrNode], f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            for node in nodes {
                for _ in 0..indent {
                    write!(f, "  ")?;
                }
                match node {
                    IrNode::Write(s) => writeln!(f, "Write({:?})", s)?,
                    IrNode::WriteExpr { expr, escape } => {
                        writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?
                    }
                    IrNode::If { condition, body } => {
                        writeln!(f, "If(condition: {}) {{", condition)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::For { var, array, body } => {
                        writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::Let { var, value, body } => {
                        writeln!(f, "Let(var: {}, value: {}) {{", var, value)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                }
            }
            Ok(())
        }

        fmt_nodes(&[self.clone()], f, 0)?;
        Ok(())
    }
}

impl fmt::Display for IrEntrypoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "IrEntrypoint {{")?;
        writeln!(f, "  parameters: {:?}", self.parameters)?;
        writeln!(f, "  body: {{")?;

        fn fmt_nodes(nodes: &[IrNode], f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
            for node in nodes {
                for _ in 0..indent {
                    write!(f, "  ")?;
                }
                match node {
                    IrNode::Write(s) => writeln!(f, "Write({:?})", s)?,
                    IrNode::WriteExpr { expr, escape } => {
                        writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?
                    }
                    IrNode::If { condition, body } => {
                        writeln!(f, "If(condition: {}) {{", condition)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::For { var, array, body } => {
                        writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::Let { var, value, body } => {
                        writeln!(f, "Let(var: {}, value: {}) {{", var, value)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                }
            }
            Ok(())
        }

        fmt_nodes(&self.body, f, 2)?;
        writeln!(f, "  }}")?;
        writeln!(f, "}}")?;
        Ok(())
    }
}
