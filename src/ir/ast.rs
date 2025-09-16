use std::{collections::HashMap, fmt};

use crate::dop::r#type::Type;

// This module contains the types and implementations for ASTs in
// the IR.
//
// The AST structure is:
// * IrModule -> IrEntryPoint -> IrStatement -> IrExpr

/// Unique identifier for each expression in the IR
pub type ExprId = u32;

/// Unique identifier for each node in the IR
pub type StatementId = u32;

#[derive(Debug, Default)]
pub struct IrModule {
    /// Map from component name (e.g. my-component) to its IR representation
    pub entry_points: HashMap<String, IrEntrypoint>,
}

#[derive(Debug, Default)]
pub struct IrEntrypoint {
    /// Original parameter names with their types (for function signature)
    pub parameters: Vec<(String, Type)>,
    /// IR nodes for the entrypoint body
    pub body: Vec<IrStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrStatement {
    /// Write literal string to the output stream.
    Write { id: StatementId, content: String },

    /// Write an expression to the output stream.
    ///
    /// The typechecker guarantees that the value of the expression
    /// will always be a string.
    WriteExpr {
        id: StatementId,
        expr: IrExpr,
        escape: bool,
    },

    /// Execute the body if a condition holds.
    If {
        id: StatementId,
        condition: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Loop over an array.
    For {
        id: StatementId,
        var: String,
        array: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Bind a variable to the value of an expression.
    Let {
        id: StatementId,
        var: String,
        value: IrExpr,
        body: Vec<IrStatement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrExpr {
    pub id: ExprId,
    pub value: IrExprValue,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrExprValue {
    Var(String),

    PropertyAccess {
        object: Box<IrExpr>,
        property: String,
    },

    StringLiteral(String),

    BooleanLiteral(bool),

    NumberLiteral(f64),

    ArrayLiteral(Vec<IrExpr>),

    ObjectLiteral(Vec<(String, IrExpr)>),

    BinaryOp {
        left: Box<IrExpr>,
        op: BinaryOp,
        right: Box<IrExpr>,
    },

    UnaryOp {
        op: UnaryOp,
        operand: Box<IrExpr>,
    },

    JsonEncode {
        value: Box<IrExpr>,
    },
}

/// Binary operators in IR
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Eq,
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

impl fmt::Display for IrModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "IrModule {{")?;
        writeln!(f, "  entry_points: {{")?;

        // Sort entry points by name for consistent output
        let mut sorted_entries: Vec<_> = self.entry_points.iter().collect();
        sorted_entries.sort_by_key(|(name, _)| name.as_str());

        for (name, entrypoint) in sorted_entries {
            writeln!(f, "    {}: {{", name)?;

            // Format parameters
            write!(f, "      parameters: [")?;
            for (i, (param_name, param_type)) in entrypoint.parameters.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}: {}", param_name, param_type)?;
            }
            writeln!(f, "]")?;

            // Format body
            writeln!(f, "      body: {{")?;
            fn fmt_stmts(
                statements: &[IrStatement],
                f: &mut fmt::Formatter<'_>,
                indent: usize,
            ) -> fmt::Result {
                for statement in statements {
                    for _ in 0..indent {
                        write!(f, "  ")?;
                    }
                    match statement {
                        IrStatement::Write { id: _, content } => {
                            writeln!(f, "Write({:?})", content)?
                        }
                        IrStatement::WriteExpr {
                            id: _,
                            expr,
                            escape,
                        } => writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?,
                        IrStatement::If {
                            id: _,
                            condition,
                            body,
                        } => {
                            writeln!(f, "If(condition: {}) {{", condition)?;
                            fmt_stmts(body, f, indent + 1)?;
                            for _ in 0..indent {
                                write!(f, "  ")?;
                            }
                            writeln!(f, "}}")?;
                        }
                        IrStatement::For {
                            id: _,
                            var,
                            array,
                            body,
                        } => {
                            writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                            fmt_stmts(body, f, indent + 1)?;
                            for _ in 0..indent {
                                write!(f, "  ")?;
                            }
                            writeln!(f, "}}")?;
                        }
                        IrStatement::Let {
                            id: _,
                            var,
                            value,
                            body,
                        } => {
                            writeln!(f, "Let(var: {}, value: {}) {{", var, value)?;
                            fmt_stmts(body, f, indent + 1)?;
                            for _ in 0..indent {
                                write!(f, "  ")?;
                            }
                            writeln!(f, "}}")?;
                        }
                    }
                }
                Ok(())
            }

            fmt_stmts(&entrypoint.body, f, 4)?;
            writeln!(f, "      }}")?;
            writeln!(f, "    }}")?;
        }

        writeln!(f, "  }}")?;
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for IrStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_stmts(
            statements: &[IrStatement],
            f: &mut fmt::Formatter<'_>,
            indent: usize,
        ) -> fmt::Result {
            for statement in statements {
                for _ in 0..indent {
                    write!(f, "  ")?;
                }
                match statement {
                    IrStatement::Write { id: _, content } => writeln!(f, "Write({:?})", content)?,
                    IrStatement::WriteExpr {
                        id: _,
                        expr,
                        escape,
                    } => writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?,
                    IrStatement::If {
                        id: _,
                        condition,
                        body,
                    } => {
                        writeln!(f, "If(condition: {}) {{", condition)?;
                        fmt_stmts(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrStatement::For {
                        id: _,
                        var,
                        array,
                        body,
                    } => {
                        writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                        fmt_stmts(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrStatement::Let {
                        id: _,
                        var,
                        value,
                        body,
                    } => {
                        writeln!(f, "Let(var: {}, value: {}) {{", var, value)?;
                        fmt_stmts(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                }
            }
            Ok(())
        }

        fmt_stmts(&[self.clone()], f, 0)?;
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

        fn fmt_stmts(
            statements: &[IrStatement],
            f: &mut fmt::Formatter<'_>,
            indent: usize,
        ) -> fmt::Result {
            for statement in statements {
                for _ in 0..indent {
                    write!(f, "  ")?;
                }
                match statement {
                    IrStatement::Write { id: _, content } => writeln!(f, "Write({:?})", content)?,
                    IrStatement::WriteExpr {
                        id: _,
                        expr,
                        escape,
                    } => writeln!(f, "WriteExpr(expr: {}, escape: {})", expr, escape)?,
                    IrStatement::If {
                        id: _,
                        condition,
                        body,
                    } => {
                        writeln!(f, "If(condition: {}) {{", condition)?;
                        fmt_stmts(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrStatement::For {
                        id: _,
                        var,
                        array,
                        body,
                    } => {
                        writeln!(f, "For(var: {}, array: {}) {{", var, array)?;
                        fmt_stmts(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                    IrStatement::Let {
                        id: _,
                        var,
                        value,
                        body,
                    } => {
                        writeln!(f, "Let(var: {}, value: {}) {{", var, value)?;
                        fmt_stmts(body, f, indent + 1)?;
                        for _ in 0..indent {
                            write!(f, "  ")?;
                        }
                        writeln!(f, "}}")?;
                    }
                }
            }
            Ok(())
        }

        fmt_stmts(&self.body, f, 2)?;
        writeln!(f, "  }}")?;
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl IrStatement {
    /// Transform all expressions in this node and its children
    pub fn map_expressions<F>(self, f: &F) -> IrStatement
    where
        F: Fn(IrExpr) -> IrExpr,
    {
        match self {
            IrStatement::Write { id, content } => IrStatement::Write { id, content },
            IrStatement::WriteExpr { id, expr, escape } => IrStatement::WriteExpr {
                id,
                expr: expr.map_expr(f),
                escape,
            },
            IrStatement::If {
                id,
                condition,
                body,
            } => IrStatement::If {
                id,
                condition: condition.map_expr(f),
                body: body.into_iter().map(|n| n.map_expressions(f)).collect(),
            },
            IrStatement::For {
                id,
                var,
                array,
                body,
            } => IrStatement::For {
                id,
                var,
                array: array.map_expr(f),
                body: body.into_iter().map(|n| n.map_expressions(f)).collect(),
            },
            IrStatement::Let {
                id,
                var,
                value,
                body,
            } => IrStatement::Let {
                id,
                var,
                value: value.map_expr(f),
                body: body.into_iter().map(|n| n.map_expressions(f)).collect(),
            },
        }
    }
}

/// Event for node traversal
#[derive(Debug, Clone)]
pub enum StatementEvent<'a> {
    Enter(&'a IrStatement),
    Exit(&'a IrStatement),
}

/// Iterator over statements with enter/exit events
pub struct StatementVisitor<'a> {
    stack: Vec<NodeVisitState<'a>>,
}

enum NodeVisitState<'a> {
    Enter(&'a IrStatement),
    Exit(&'a IrStatement),
    VisitChildren(&'a [IrStatement]),
}

impl<'a> Iterator for StatementVisitor<'a> {
    type Item = StatementEvent<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(state) = self.stack.pop() {
            match state {
                NodeVisitState::Enter(node) => {
                    // Push exit event for later
                    self.stack.push(NodeVisitState::Exit(node));

                    // Push children to visit
                    match node {
                        IrStatement::If { body, .. }
                        | IrStatement::For { body, .. }
                        | IrStatement::Let { body, .. } => {
                            self.stack.push(NodeVisitState::VisitChildren(body));
                        }
                        IrStatement::Write { .. } | IrStatement::WriteExpr { .. } => {
                            // Leaf nodes - no children
                        }
                    }

                    return Some(StatementEvent::Enter(node));
                }
                NodeVisitState::Exit(node) => {
                    return Some(StatementEvent::Exit(node));
                }
                NodeVisitState::VisitChildren(statements) => {
                    // Push children in reverse order so they're visited in correct order
                    for statement in statements.iter().rev() {
                        self.stack.push(NodeVisitState::Enter(statement));
                    }
                }
            }
        }
        None
    }
}

impl IrEntrypoint {
    /// Transform all expressions in the entrypoint
    pub fn map_expressions<F>(mut self, f: F) -> IrEntrypoint
    where
        F: Fn(IrExpr) -> IrExpr,
    {
        self.body = self
            .body
            .into_iter()
            .map(|n| n.map_expressions(&f))
            .collect();
        self
    }

    /// Returns an iterator over all statements with enter/exit events
    pub fn visit_statements(&self) -> StatementVisitor {
        let mut stack = Vec::new();
        // Push children in reverse order
        for node in self.body.iter().rev() {
            stack.push(NodeVisitState::Enter(node));
        }
        StatementVisitor { stack }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Eq => write!(f, "=="),
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
        DfsIter { stack: vec![self] }
    }

    /// Transform this expression by applying a function to it and all sub-expressions
    pub fn map_expr<F>(self, f: &F) -> IrExpr
    where
        F: Fn(IrExpr) -> IrExpr,
    {
        // First recursively transform children
        let transformed = match self.value {
            IrExprValue::UnaryOp { op, operand } => IrExpr {
                id: self.id,
                value: IrExprValue::UnaryOp {
                    op,
                    operand: Box::new(operand.map_expr(f)),
                },
                typ: self.typ.clone(),
            },
            IrExprValue::BinaryOp { op, left, right } => IrExpr {
                id: self.id,
                value: IrExprValue::BinaryOp {
                    op,
                    left: Box::new(left.map_expr(f)),
                    right: Box::new(right.map_expr(f)),
                },
                typ: self.typ.clone(),
            },
            IrExprValue::PropertyAccess { object, property } => IrExpr {
                id: self.id,
                value: IrExprValue::PropertyAccess {
                    object: Box::new(object.map_expr(f)),
                    property,
                },
                typ: self.typ.clone(),
            },
            IrExprValue::ArrayLiteral(elements) => IrExpr {
                id: self.id,
                value: IrExprValue::ArrayLiteral(
                    elements.into_iter().map(|e| e.map_expr(f)).collect(),
                ),
                typ: self.typ.clone(),
            },
            IrExprValue::ObjectLiteral(properties) => IrExpr {
                id: self.id,
                value: IrExprValue::ObjectLiteral(
                    properties
                        .into_iter()
                        .map(|(k, v)| (k, v.map_expr(f)))
                        .collect(),
                ),
                typ: self.typ.clone(),
            },
            // Leaf nodes - no children to transform
            IrExprValue::Var(name) => IrExpr {
                id: self.id,
                value: IrExprValue::Var(name),
                typ: self.typ.clone(),
            },
            IrExprValue::StringLiteral(s) => IrExpr {
                id: self.id,
                value: IrExprValue::StringLiteral(s),
                typ: self.typ.clone(),
            },
            IrExprValue::BooleanLiteral(b) => IrExpr {
                id: self.id,
                value: IrExprValue::BooleanLiteral(b),
                typ: self.typ.clone(),
            },
            IrExprValue::NumberLiteral(n) => IrExpr {
                id: self.id,
                value: IrExprValue::NumberLiteral(n),
                typ: self.typ.clone(),
            },
            IrExprValue::JsonEncode { value } => IrExpr {
                id: self.id,
                value: IrExprValue::JsonEncode {
                    value: Box::new(value.map_expr(f)),
                },
                typ: self.typ.clone(),
            },
        };
        // Then apply the function to this node
        f(transformed)
    }
}

/// Depth-first iterator over IrExpr nodes
pub struct DfsIter<'a> {
    stack: Vec<&'a IrExpr>,
}

impl<'a> Iterator for DfsIter<'a> {
    type Item = &'a IrExpr;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().inspect(|expr| {
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
                IrExprValue::ArrayLiteral(elements) => {
                    for elem in elements.iter().rev() {
                        self.stack.push(elem);
                    }
                }
                IrExprValue::ObjectLiteral(properties) => {
                    for (_, value) in properties.iter().rev() {
                        self.stack.push(value);
                    }
                }
                IrExprValue::JsonEncode { value } => {
                    self.stack.push(value);
                }
                // Explicitly list all leaf nodes (no children to traverse)
                IrExprValue::Var(_) => {}
                IrExprValue::StringLiteral(_) => {}
                IrExprValue::BooleanLiteral(_) => {}
                IrExprValue::NumberLiteral(_) => {}
            }
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
            IrExprValue::StringLiteral(s) => write!(f, "{:?}", s),
            IrExprValue::BooleanLiteral(b) => write!(f, "{}", b),
            IrExprValue::NumberLiteral(n) => write!(f, "{}", n),
            IrExprValue::ArrayLiteral(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            IrExprValue::ObjectLiteral(props) => {
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
            IrExprValue::JsonEncode { value } => {
                write!(f, "JsonEncode({})", value)
            }
        }
    }
}
