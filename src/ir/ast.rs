use std::{collections::HashMap, fmt};

use super::expr::IrExpr;
use crate::dop::r#type::Type;

// This module contains the types and implementations for ASTs in
// the IR.
//
// The AST structure is:
// * IrModule -> IrEntryPoint -> IrNode -> IrExpr

#[derive(Debug, Default)]
pub struct IrModule {
    /// Map from component name to its IR representation
    pub entry_points: HashMap<String, IrEntrypoint>,
}

#[derive(Debug, Default)]
pub struct IrEntrypoint {
    /// Original parameter names with their types (for function signature)
    pub parameters: Vec<(String, Type)>,
    /// IR nodes for the entrypoint body
    pub body: Vec<IrNode>,
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

impl IrModule {
    pub fn new() -> Self {
        Self::default()
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
        write!(f, "  parameters: [")?;
        for (i, (name, _type)) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", name, _type)?;
        }
        writeln!(f, "]")?;
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
