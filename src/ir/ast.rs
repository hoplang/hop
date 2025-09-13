use std::{collections::HashMap, fmt};

use crate::dop::r#type::Type;

// This module contains the types and implementations for ASTs in
// the IR.
//
// The AST structure is:
// * IrModule -> IrEntryPoint -> IrNode -> IrExpr

/// Unique identifier for each expression in the IR
pub type ExprId = u32;

/// Unique identifier for each node in the IR
pub type NodeId = u32;

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
    Write { id: NodeId, content: String },

    /// Evaluate expression and output as string
    WriteExpr { id: NodeId, expr: IrExpr, escape: bool },

    /// Conditional execution
    If {
        id: NodeId,
        condition: IrExpr,
        body: Vec<IrNode>,
    },

    /// Loop over array
    For {
        id: NodeId,
        var: String,
        array: IrExpr,
        body: Vec<IrNode>,
    },

    /// Variable binding
    Let {
        id: NodeId,
        var: String,
        value: IrExpr,
        body: Vec<IrNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrExpr {
    pub id: ExprId,
    pub value: IrExprValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrExprValue {
    Var(String),

    PropertyAccess {
        object: Box<IrExpr>,
        property: String,
    },

    String(String),

    Boolean(bool),

    Number(f64),

    Array(Vec<IrExpr>),

    Object(Vec<(String, IrExpr)>),

    BinaryOp {
        left: Box<IrExpr>,
        op: BinaryOp,
        right: Box<IrExpr>,
    },

    UnaryOp {
        op: UnaryOp,
        operand: Box<IrExpr>,
    },
}

/// Binary operators in IR
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
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
                    IrNode::Write { id: _, content } => writeln!(f, "Write({:?})", content)?,
                    IrNode::WriteExpr { id: _, expr, escape } => {
                        writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?
                    }
                    IrNode::If { id: _, condition, body } => {
                        writeln!(f, "If(condition: {}) {{", condition)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::For { id: _, var, array, body } => {
                        writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::Let { id: _, var, value, body } => {
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
                    IrNode::Write { id: _, content } => writeln!(f, "Write({:?})", content)?,
                    IrNode::WriteExpr { id: _, expr, escape } => {
                        writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?
                    }
                    IrNode::If { id: _, condition, body } => {
                        writeln!(f, "If(condition: {}) {{", condition)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::For { id: _, var, array, body } => {
                        writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                        fmt_nodes(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrNode::Let { id: _, var, value, body } => {
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

impl IrExpr {
    /// Returns an iterator that performs depth-first traversal of the expression tree
    pub fn dfs_iter(&self) -> DfsIter {
        DfsIter {
            stack: vec![self],
        }
    }
}

/// Depth-first iterator over IrExpr nodes
pub struct DfsIter<'a> {
    stack: Vec<&'a IrExpr>,
}

impl<'a> Iterator for DfsIter<'a> {
    type Item = &'a IrExpr;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().map(|expr| {
            // Push children onto stack in reverse order for left-to-right DFS
            match &expr.value {
                IrExprValue::PropertyAccess { object, .. } => {
                    self.stack.push(object);
                }
                IrExprValue::BinaryOp { left, right, .. } => {
                    self.stack.push(right);
                    self.stack.push(left);
                }
                IrExprValue::UnaryOp { operand, .. } => {
                    self.stack.push(operand);
                }
                IrExprValue::Array(elements) => {
                    for elem in elements.iter().rev() {
                        self.stack.push(elem);
                    }
                }
                IrExprValue::Object(properties) => {
                    for (_, value) in properties.iter().rev() {
                        self.stack.push(value);
                    }
                }
                // Explicitly list all leaf nodes (no children to traverse)
                IrExprValue::Var(_) => {}
                IrExprValue::String(_) => {}
                IrExprValue::Boolean(_) => {}
                IrExprValue::Number(_) => {}
            }
            expr
        })
    }
}

impl fmt::Display for IrExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            IrExprValue::Var(name) => write!(f, "{}", name),
            IrExprValue::PropertyAccess { object, property } => {
                write!(f, "{}.{}", object, property)
            }
            IrExprValue::String(s) => write!(f, "{:?}", s),
            IrExprValue::Boolean(b) => write!(f, "{}", b),
            IrExprValue::Number(n) => write!(f, "{}", n),
            IrExprValue::Array(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            IrExprValue::Object(props) => {
                write!(f, "{{")?;
                for (i, (key, value)) in props.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            IrExprValue::BinaryOp { left, op, right } => {
                write!(f, "({} {} {})", left, op, right)
            }
            IrExprValue::UnaryOp { op, operand } => {
                write!(f, "{}{}", op, operand)
            }
        }
    }
}
