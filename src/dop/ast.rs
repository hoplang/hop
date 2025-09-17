use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::parser::VarName;
use crate::hop::pretty::Pretty;
use pretty::RcDoc;

use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Equal,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Equal => write!(f, "=="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone)]
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
        }
    }
}

impl<T> Display for Expr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
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
            | Expr::UnaryOp { annotation, .. } => annotation,
        }
    }
}

/// Type alias for typed expressions
pub type TypedExpr = Expr<Type>;
