use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::var_name::VarName;
use crate::hop::pretty::Pretty;
use pretty::RcDoc;

use super::Type;

pub type TypedExpr = Expr<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Eq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T = DocumentRange> {
    /// A variable expression, e.g. foo
    Var { value: VarName, annotation: T },

    /// A property access expression, e.g. foo.bar
    PropertyAccess {
        object: Box<Expr<T>>,
        property: String,
        annotation: T,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: String, annotation: T },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, annotation: T },

    /// A number literal expression, e.g. 2.5
    NumberLiteral {
        value: serde_json::Number,
        annotation: T,
    },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<Expr<T>>,
        annotation: T,
    },

    ObjectLiteral {
        properties: Vec<(String, Expr<T>)>,
        annotation: T,
    },

    BinaryOp {
        left: Box<Expr<T>>,
        operator: BinaryOp,
        right: Box<Expr<T>>,
        annotation: T,
    },

    UnaryOp {
        operator: UnaryOp,
        operand: Box<Expr<T>>,
        annotation: T,
    },

    /// JSON encode expression for converting values to JSON strings
    JsonEncode { value: Box<Expr<T>>, annotation: T },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<Expr<T>>,
        right: Box<Expr<T>>,
        annotation: T,
    },
}

impl<T> Expr<T> {
    pub fn annotation(&self) -> &T {
        match self {
            Expr::Var { annotation, .. }
            | Expr::PropertyAccess { annotation, .. }
            | Expr::StringLiteral { annotation, .. }
            | Expr::BooleanLiteral { annotation, .. }
            | Expr::NumberLiteral { annotation, .. }
            | Expr::ArrayLiteral { annotation, .. }
            | Expr::ObjectLiteral { annotation, .. }
            | Expr::BinaryOp { annotation, .. }
            | Expr::UnaryOp { annotation, .. }
            | Expr::JsonEncode { annotation, .. }
            | Expr::StringConcat { annotation, .. } => annotation,
        }
    }

    /// Returns an iterator that performs depth-first traversal of the expression tree
    pub fn dfs_iter(&self) -> DepthFirstIterator<'_, T> {
        DepthFirstIterator::new(self)
    }

    /// Returns all child expressions
    fn children(&self) -> Vec<&Expr<T>> {
        match self {
            Expr::PropertyAccess { object, .. } => vec![object],
            Expr::BinaryOp { left, right, .. } => vec![left, right],
            Expr::UnaryOp { operand, .. } => vec![operand],
            Expr::ArrayLiteral { elements, .. } => elements.iter().collect(),
            Expr::ObjectLiteral { properties, .. } => {
                properties.iter().map(|(_, value)| value).collect()
            }
            Expr::JsonEncode { value, .. } => vec![value],
            Expr::StringConcat { left, right, .. } => vec![left, right],
            // Leaf nodes have no children
            Expr::Var { .. }
            | Expr::StringLiteral { .. }
            | Expr::BooleanLiteral { .. }
            | Expr::NumberLiteral { .. } => vec![],
        }
    }
}

/// Depth-first iterator over Expr nodes
pub struct DepthFirstIterator<'a, T> {
    stack: Vec<&'a Expr<T>>,
}

impl<'a, T> DepthFirstIterator<'a, T> {
    fn new(root: &'a Expr<T>) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a, T> Iterator for DepthFirstIterator<'a, T> {
    type Item = &'a Expr<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.stack.pop()?;

        // Add children in reverse order so they're visited in correct order
        for child in current.children().into_iter().rev() {
            self.stack.push(child);
        }

        Some(current)
    }
}

impl Ranged for Expr<DocumentRange> {
    fn range(&self) -> &DocumentRange {
        self.annotation()
    }
}

impl<T> Pretty for Expr<T> {
    fn to_doc(&self) -> RcDoc<'static> {
        match self {
            Expr::Var { value, .. } => RcDoc::text(value.to_string()),
            Expr::PropertyAccess {
                object, property, ..
            } => object
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::text(property.to_string())),
            Expr::StringLiteral { value, .. } => RcDoc::text(format!("\"{}\"", value)),
            Expr::BooleanLiteral { value, .. } => RcDoc::text(value.to_string()),
            Expr::NumberLiteral { value, .. } => RcDoc::text(value.to_string()),
            Expr::ArrayLiteral { elements, .. } => {
                if elements.is_empty() {
                    RcDoc::text("[]")
                } else {
                    RcDoc::text("[")
                        .append(
                            RcDoc::line_()
                                .append(RcDoc::intersperse(
                                    elements.iter().map(|e| e.to_doc()),
                                    RcDoc::text(",").append(RcDoc::line()),
                                ))
                                .append(RcDoc::text(",").flat_alt(RcDoc::nil()))
                                .append(RcDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(RcDoc::text("]"))
                }
            }
            Expr::ObjectLiteral { properties, .. } => {
                if properties.is_empty() {
                    RcDoc::text("{}")
                } else {
                    RcDoc::nil()
                        .append(RcDoc::text("{"))
                        .append(
                            RcDoc::line_()
                                .append(RcDoc::intersperse(
                                    properties.iter().map(|(key, value)| {
                                        RcDoc::text(key.to_string())
                                            .append(RcDoc::text(": "))
                                            .append(value.to_doc())
                                    }),
                                    RcDoc::text(",").append(RcDoc::line()),
                                ))
                                .append(RcDoc::text(",").flat_alt(RcDoc::nil()))
                                .append(RcDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(RcDoc::text("}"))
                }
            }
            Expr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => RcDoc::nil()
                .append(RcDoc::text("("))
                .append(left.to_doc())
                .append(RcDoc::text(format!(" {} ", operator)))
                .append(right.to_doc())
                .append(RcDoc::text(")")),
            Expr::UnaryOp {
                operator, operand, ..
            } => RcDoc::nil()
                .append(RcDoc::text("("))
                .append(RcDoc::text(operator.to_string()))
                .append(operand.to_doc())
                .append(RcDoc::text(")")),
            Expr::JsonEncode { value, .. } => RcDoc::nil()
                .append(RcDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(RcDoc::text(")")),
            Expr::StringConcat { left, right, .. } => RcDoc::nil()
                .append(RcDoc::text("("))
                .append(left.to_doc())
                .append(RcDoc::text(" + "))
                .append(right.to_doc())
                .append(RcDoc::text(")")),
        }
    }
}

impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Eq => write!(f, "=="),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
}
