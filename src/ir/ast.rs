use std::{collections::HashMap, fmt};

use crate::dop::{VarName, expr::Expr, r#type::Type};

pub use crate::dop::expr::{BinaryOp, UnaryOp};

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
    pub parameters: Vec<(VarName, Type)>,
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
        var: VarName,
        array: IrExpr,
        body: Vec<IrStatement>,
    },

    /// Bind a variable to the value of an expression.
    Let {
        id: StatementId,
        var: VarName,
        value: IrExpr,
        body: Vec<IrStatement>,
    },
}

/// Type alias for IR expressions with ExprId and Type annotations
pub type IrExpr = Expr<(ExprId, Type)>;

impl IrStatement {
    /// Visit this statement and all nested statements with a closure
    pub fn visit<F>(&self, visitor: &mut F)
    where
        F: FnMut(&IrStatement),
    {
        visitor(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If { body, .. } => {
                for stmt in body {
                    stmt.visit(visitor);
                }
            }
            IrStatement::For { body, .. } => {
                for stmt in body {
                    stmt.visit(visitor);
                }
            }
            IrStatement::Let { body, .. } => {
                for stmt in body {
                    stmt.visit(visitor);
                }
            }
        }
    }

    /// Visit this statement and all nested statements with a mutable closure
    pub fn visit_mut<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut IrStatement),
    {
        visitor(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If { body, .. } => {
                for stmt in body {
                    stmt.visit_mut(visitor);
                }
            }
            IrStatement::For { body, .. } => {
                for stmt in body {
                    stmt.visit_mut(visitor);
                }
            }
            IrStatement::Let { body, .. } => {
                for stmt in body {
                    stmt.visit_mut(visitor);
                }
            }
        }
    }

    /// Visit all expressions in this statement and nested statements recursively
    pub fn visit_exprs<F>(&self, visitor: &mut F)
    where
        F: FnMut(&IrExpr),
    {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { expr, .. } => {
                expr.visit(visitor);
            }
            IrStatement::If {
                condition, body, ..
            } => {
                condition.visit(visitor);
                for stmt in body {
                    stmt.visit_exprs(visitor);
                }
            }
            IrStatement::For { array, body, .. } => {
                array.visit(visitor);
                for stmt in body {
                    stmt.visit_exprs(visitor);
                }
            }
            IrStatement::Let { value, body, .. } => {
                value.visit(visitor);
                for stmt in body {
                    stmt.visit_exprs(visitor);
                }
            }
        }
    }

    /// Visit all expressions in this statement and nested statements with mutable access recursively
    pub fn visit_exprs_mut<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut IrExpr),
    {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { expr, .. } => {
                expr.visit_mut(visitor);
            }
            IrStatement::If {
                condition, body, ..
            } => {
                condition.visit_mut(visitor);
                for stmt in body {
                    stmt.visit_exprs_mut(visitor);
                }
            }
            IrStatement::For { array, body, .. } => {
                array.visit_mut(visitor);
                for stmt in body {
                    stmt.visit_exprs_mut(visitor);
                }
            }
            IrStatement::Let { value, body, .. } => {
                value.visit_mut(visitor);
                for stmt in body {
                    stmt.visit_exprs_mut(visitor);
                }
            }
        }
    }
}

impl IrExpr {
    /// Get the id of this expression
    pub fn id(&self) -> ExprId {
        self.annotation().0
    }

    /// Get the type of this expression
    pub fn typ(&self) -> &Type {
        &self.annotation().1
    }

    /// Recursively visit this expression and all nested expressions
    pub fn visit<F>(&self, visitor: &mut F)
    where
        F: FnMut(&IrExpr),
    {
        visitor(self);
        match self {
            Expr::PropertyAccess { object, .. } => {
                object.visit(visitor);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.visit(visitor);
                }
            }
            Expr::ObjectLiteral { properties, .. } => {
                for (_, value) in properties {
                    value.visit(visitor);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                left.visit(visitor);
                right.visit(visitor);
            }
            Expr::UnaryOp { operand, .. } => {
                operand.visit(visitor);
            }
            Expr::JsonEncode { value, .. } => {
                value.visit(visitor);
            }
            _ => {}
        }
    }

    /// Recursively visit this expression and all nested expressions with mutable access
    pub fn visit_mut<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut IrExpr),
    {
        visitor(self);
        match self {
            Expr::PropertyAccess { object, .. } => {
                object.visit_mut(visitor);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.visit_mut(visitor);
                }
            }
            Expr::ObjectLiteral { properties, .. } => {
                for (_, value) in properties {
                    value.visit_mut(visitor);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                left.visit_mut(visitor);
                right.visit_mut(visitor);
            }
            Expr::UnaryOp { operand, .. } => {
                operand.visit_mut(visitor);
            }
            Expr::JsonEncode { value, .. } => {
                value.visit_mut(visitor);
            }
            _ => {}
        }
    }
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
