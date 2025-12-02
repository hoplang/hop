use std::{collections::HashMap, fmt};

use crate::dop::{VarName, expr::Expr, r#type::Type};
use crate::hop::component_name::ComponentName;
use pretty::BoxDoc;

// This module contains the types and implementations for ASTs in
// the IR.
//
// The AST structure is:
// * IrEntrypoint -> IrStatement -> IrExpr

/// Unique identifier for each expression in the IR
pub type ExprId = u32;

/// Unique identifier for each statement in the IR
pub type StatementId = u32;

#[derive(Debug, Clone)]
pub struct IrEntrypoint {
    /// Component name (e.g. MyComponent)
    pub name: ComponentName,
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
        else_body: Option<Vec<IrStatement>>,
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

/// Type alias for IR expressions with ExprId annotations
pub type IrExpr = Expr<ExprId>;

impl IrStatement {
    /// Get the primary expression from this statement, if any
    pub fn expr(&self) -> Option<&IrExpr> {
        match self {
            IrStatement::Write { .. } => None,
            IrStatement::WriteExpr { expr, .. } => Some(expr),
            IrStatement::If { condition, .. } => Some(condition),
            IrStatement::For { array, .. } => Some(array),
            IrStatement::Let { value, .. } => Some(value),
        }
    }

    /// Get a mutable reference to the primary expression from this statement, if any
    pub fn expr_mut(&mut self) -> Option<&mut IrExpr> {
        match self {
            IrStatement::Write { .. } => None,
            IrStatement::WriteExpr { expr, .. } => Some(expr),
            IrStatement::If { condition, .. } => Some(condition),
            IrStatement::For { array, .. } => Some(array),
            IrStatement::Let { value, .. } => Some(value),
        }
    }

    /// Traverse this statement and all nested statements with a closure
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrStatement),
    {
        f(self);
        match self {
            IrStatement::Write { .. } => {}
            IrStatement::WriteExpr { .. } => {}
            IrStatement::If {
                body, else_body, ..
            } => {
                for stmt in body {
                    stmt.traverse(f);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        stmt.traverse(f);
                    }
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
            IrStatement::If {
                body, else_body, ..
            } => {
                for stmt in body {
                    stmt.traverse_with_scope_impl(scope, f);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        stmt.traverse_with_scope_impl(scope, f);
                    }
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
            IrStatement::If {
                body, else_body, ..
            } => {
                for stmt in body {
                    stmt.traverse_mut(f);
                }
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        stmt.traverse_mut(f);
                    }
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

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            IrStatement::Write { content, .. } => BoxDoc::text("write")
                .append(BoxDoc::text("("))
                .append(BoxDoc::text(format!("{:?}", content)))
                .append(BoxDoc::text(")")),
            IrStatement::WriteExpr { expr, escape, .. } => {
                let write_fn = if *escape {
                    "write_escaped"
                } else {
                    "write_expr"
                };
                BoxDoc::text(write_fn)
                    .append(BoxDoc::text("("))
                    .append(expr.to_doc())
                    .append(BoxDoc::text(")"))
            }
            IrStatement::If {
                condition,
                body,
                else_body,
                ..
            } => {
                let mut doc = BoxDoc::text("if ")
                    .append(condition.to_doc())
                    .append(BoxDoc::text(" {"))
                    .append(if body.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                body.iter().map(|stmt| stmt.to_doc()),
                                BoxDoc::line(),
                            ))
                            .append(BoxDoc::line())
                            .nest(2)
                    })
                    .append(BoxDoc::text("}"));

                if let Some(else_stmts) = else_body {
                    doc = doc
                        .append(BoxDoc::text(" else {"))
                        .append(if else_stmts.is_empty() {
                            BoxDoc::nil()
                        } else {
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    else_stmts.iter().map(|stmt| stmt.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .append(BoxDoc::line())
                                .nest(2)
                        })
                        .append(BoxDoc::text("}"));
                }

                doc
            }
            IrStatement::For {
                var, array, body, ..
            } => BoxDoc::text("for ")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" in "))
                .append(array.to_doc())
                .append(BoxDoc::text(" {"))
                .append(if body.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            body.iter().map(|stmt| stmt.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("}")),
            IrStatement::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in {"))
                .append(if body.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            body.iter().map(|stmt| stmt.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("}")),
        }
    }
}

impl IrExpr {
    /// Get the id of this expression
    pub fn id(&self) -> ExprId {
        *self.annotation()
    }

    /// Recursively traverses this expression and all nested expressions
    pub fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&IrExpr),
    {
        f(self);
        match self {
            Expr::FieldAccess { record: object, .. } => {
                object.traverse(f);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.traverse(f);
                }
            }
            Expr::RecordInstantiation { fields, .. } => {
                for (_, value) in fields {
                    value.traverse(f);
                }
            }
            Expr::BooleanNegation { operand, .. } => {
                operand.traverse(f);
            }
            Expr::JsonEncode { value, .. } => {
                value.traverse(f);
            }
            Expr::EnvLookup { key, .. } => {
                key.traverse(f);
            }
            Expr::Equals { left, right, .. }
            | Expr::NotEquals { left, right, .. }
            | Expr::LessThan { left, right, .. }
            | Expr::GreaterThan { left, right, .. }
            | Expr::LessThanOrEqual { left, right, .. }
            | Expr::GreaterThanOrEqual { left, right, .. }
            | Expr::StringConcat { left, right, .. }
            | Expr::NumericAdd { left, right, .. }
            | Expr::NumericSubtract { left, right, .. }
            | Expr::NumericMultiply { left, right, .. }
            | Expr::BooleanLogicalAnd { left, right, .. }
            | Expr::BooleanLogicalOr { left, right, .. } => {
                left.traverse(f);
                right.traverse(f);
            }
            Expr::Var { .. }
            | Expr::StringLiteral { .. }
            | Expr::BooleanLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::IntLiteral { .. } => {}
        }
    }

    /// Recursively traverses this expression and all nested expressions with mutable access
    pub fn traverse_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut(&mut IrExpr),
    {
        f(self);
        match self {
            Expr::FieldAccess { record: object, .. } => {
                object.traverse_mut(f);
            }
            Expr::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    elem.traverse_mut(f);
                }
            }
            Expr::RecordInstantiation { fields, .. } => {
                for (_, value) in fields {
                    value.traverse_mut(f);
                }
            }
            Expr::BooleanNegation { operand, .. } => {
                operand.traverse_mut(f);
            }
            Expr::JsonEncode { value, .. } => {
                value.traverse_mut(f);
            }
            Expr::EnvLookup { key, .. } => {
                key.traverse_mut(f);
            }
            Expr::StringConcat { left, right, .. }
            | Expr::NumericAdd { left, right, .. }
            | Expr::NumericSubtract { left, right, .. }
            | Expr::NumericMultiply { left, right, .. }
            | Expr::Equals { left, right, .. }
            | Expr::NotEquals { left, right, .. }
            | Expr::LessThan { left, right, .. }
            | Expr::GreaterThan { left, right, .. }
            | Expr::LessThanOrEqual { left, right, .. }
            | Expr::GreaterThanOrEqual { left, right, .. }
            | Expr::BooleanLogicalAnd { left, right, .. }
            | Expr::BooleanLogicalOr { left, right, .. } => {
                left.traverse_mut(f);
                right.traverse_mut(f);
            }
            _ => {}
        }
    }
}

impl<'a> IrEntrypoint {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        BoxDoc::text(self.name.as_str())
            .append(BoxDoc::text("("))
            .append(
                BoxDoc::nil()
                    // soft line break
                    .append(BoxDoc::line_())
                    .append(BoxDoc::intersperse(
                        self.parameters.iter().map(|(name, typ)| {
                            BoxDoc::text(name.to_string())
                                .append(BoxDoc::text(": "))
                                .append(typ.to_doc())
                        }),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    // trailing comma if laid out on multiple lines
                    .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                    // soft line break
                    .append(BoxDoc::line_())
                    .nest(2)
                    .group(),
            )
            .append(BoxDoc::text(") {"))
            .append(if self.body.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.body.iter().map(|stmt| stmt.to_doc()),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(2)
            })
            .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for IrStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for IrEntrypoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}
