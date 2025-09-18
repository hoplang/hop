use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::var_name::VarName;
use pretty::BoxDoc;

use super::Type;

pub type UntypedExpr = Expr<DocumentRange>;
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
pub enum Expr<A> {
    /// A variable expression, e.g. foo
    Var { value: VarName, annotation: A },

    /// A property access expression, e.g. foo.bar
    PropertyAccess {
        object: Box<Self>,
        property: String,
        annotation: A,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: String, annotation: A },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, annotation: A },

    /// A number literal expression, e.g. 2.5
    NumberLiteral {
        value: serde_json::Number,
        annotation: A,
    },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral { elements: Vec<Self>, annotation: A },

    ObjectLiteral {
        properties: Vec<(String, Self)>,
        annotation: A,
    },

    BinaryOp {
        left: Box<Self>,
        operator: BinaryOp,
        right: Box<Self>,
        annotation: A,
    },

    UnaryOp {
        operator: UnaryOp,
        operand: Box<Self>,
        annotation: A,
    },

    /// JSON encode expression for converting values to JSON strings
    JsonEncode { value: Box<Self>, annotation: A },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },
}

impl<A> Expr<A> {
    pub fn annotation(&self) -> &A {
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
}

impl Ranged for Expr<DocumentRange> {
    fn range(&self) -> &DocumentRange {
        self.annotation()
    }
}

impl<'a, T> Expr<T> {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        match self {
            Expr::Var { value, .. } => BoxDoc::text(value.as_str()),
            Expr::PropertyAccess {
                object, property, ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(property.as_str())),
            Expr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            Expr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            Expr::NumberLiteral { value, .. } => BoxDoc::text(value.to_string()),
            Expr::ArrayLiteral { elements, .. } => {
                if elements.is_empty() {
                    BoxDoc::text("[]")
                } else {
                    BoxDoc::text("[")
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    elements.iter().map(|e| e.to_doc()),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("]"))
                }
            }
            Expr::ObjectLiteral { properties, .. } => {
                if properties.is_empty() {
                    BoxDoc::text("{}")
                } else {
                    BoxDoc::nil()
                        .append(BoxDoc::text("{"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    properties.iter().map(|(key, value)| {
                                        BoxDoc::text(key.as_str())
                                            .append(BoxDoc::text(": "))
                                            .append(value.to_doc())
                                    }),
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
            }
            Expr::BinaryOp {
                left,
                operator,
                right,
                ..
            } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(format!(" {} ", operator)))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::UnaryOp {
                operator, operand, ..
            } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text(operator.to_string()))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            Expr::JsonEncode { value, .. } => BoxDoc::nil()
                .append(BoxDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            Expr::StringConcat { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
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
