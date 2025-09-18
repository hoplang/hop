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
    /// Traverse this statement and all nested statements with a closure
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrStatement),
    {
        f(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If { body, .. } => {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
            IrStatement::For { body, .. } => {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
            IrStatement::Let { body, .. } => {
                for stmt in body {
                    stmt.traverse(f);
                }
            }
        }
    }

    /// Traverse this statement and all nested statements with a closure that receives
    /// the current scope (variables mapped to the statement that defined them)
    pub fn traverse_with_scope<'a, F>(&'a self, f: &mut F)
    where
        F: FnMut(&'a IrStatement, &HashMap<String, &'a IrStatement>),
    {
        let mut scope = HashMap::new();
        self.traverse_with_scope_impl(&mut scope, f);
    }

    fn traverse_with_scope_impl<'a, F>(
        &'a self,
        scope: &mut HashMap<String, &'a IrStatement>,
        f: &mut F,
    ) where
        F: FnMut(&'a IrStatement, &HashMap<String, &'a IrStatement>),
    {
        f(self, scope);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If { body, .. } => {
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
            }
            IrStatement::For { var, body, .. } => {
                let prev_value = scope.insert(var.to_string(), self);
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
                if let Some(prev) = prev_value {
                    scope.insert(var.to_string(), prev);
                } else {
                    scope.remove(&var.to_string());
                }
            }
            IrStatement::Let { var, body, .. } => {
                let prev_value = scope.insert(var.to_string(), self);
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
                if let Some(prev) = prev_value {
                    scope.insert(var.to_string(), prev);
                } else {
                    scope.remove(&var.to_string());
                }
            }
        }
    }

    /// Traverse this statement and all nested statements with a mutable closure
    pub fn traverse_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut IrStatement),
    {
        f(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If { body, .. } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
            }
            IrStatement::For { body, .. } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
            }
            IrStatement::Let { body, .. } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
            }
        }
    }

    /// Traverse all expressions in this statement and nested statements recursively
    pub fn traverse_exprs<F>(&self, f: &mut F)
    where
        F: FnMut(&IrExpr),
    {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { expr, .. } => {
                expr.traverse(f);
            }
            IrStatement::If {
                condition, body, ..
            } => {
                condition.traverse(f);
                for stmt in body {
                    stmt.traverse_exprs(f);
                }
            }
            IrStatement::For { array, body, .. } => {
                array.traverse(f);
                for stmt in body {
                    stmt.traverse_exprs(f);
                }
            }
            IrStatement::Let { value, body, .. } => {
                value.traverse(f);
                for stmt in body {
                    stmt.traverse_exprs(f);
                }
            }
        }
    }

    /// Traverses all expressions in this statement and nested statements with mutable access recursively
    pub fn traverse_exprs_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut IrExpr),
    {
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { expr, .. } => {
                expr.traverse_mut(f);
            }
            IrStatement::If {
                condition, body, ..
            } => {
                condition.traverse_mut(f);
                for stmt in body {
                    stmt.traverse_exprs_mut(f);
                }
            }
            IrStatement::For { array, body, .. } => {
                array.traverse_mut(f);
                for stmt in body {
                    stmt.traverse_exprs_mut(f);
                }
            }
            IrStatement::Let { value, body, .. } => {
                value.traverse_mut(f);
                for stmt in body {
                    stmt.traverse_exprs_mut(f);
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

    /// Recursively traverses this expression and all nested expressions
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrExpr),
    {
        f(self);
        match self {
            Expr::PropertyAccess { object, .. } => {
                object.traverse(f);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.traverse(f);
                }
            }
            Expr::ObjectLiteral { properties, .. } => {
                for (_, value) in properties {
                    value.traverse(f);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                left.traverse(f);
                right.traverse(f);
            }
            Expr::UnaryOp { operand, .. } => {
                operand.traverse(f);
            }
            Expr::JsonEncode { value, .. } => {
                value.traverse(f);
            }
            _ => {}
        }
    }

    /// Recursively traverses this expression and all nested expressions with mutable access
    pub fn traverse_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut IrExpr),
    {
        f(self);
        match self {
            Expr::PropertyAccess { object, .. } => {
                object.traverse_mut(f);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.traverse_mut(f);
                }
            }
            Expr::ObjectLiteral { properties, .. } => {
                for (_, value) in properties {
                    value.traverse_mut(f);
                }
            }
            Expr::BinaryOp { left, right, .. } => {
                left.traverse_mut(f);
                right.traverse_mut(f);
            }
            Expr::UnaryOp { operand, .. } => {
                operand.traverse_mut(f);
            }
            Expr::JsonEncode { value, .. } => {
                value.traverse_mut(f);
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
