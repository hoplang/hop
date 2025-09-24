use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::var_name::VarName;
use pretty::BoxDoc;

pub type Expr = AnnotatedExpr<DocumentRange>;

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    LogicalAnd,
    Plus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotatedExpr<A> {
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

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i64, annotation: A },

    /// A float literal expression, e.g. 2.5
    FloatLiteral {
        value: f64,
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

    /// Boolean negation expression for negating boolean values
    Negation { operand: Box<Self>, annotation: A },
}

impl<A> AnnotatedExpr<A> {
    pub fn annotation(&self) -> &A {
        match self {
            AnnotatedExpr::Var { annotation, .. }
            | AnnotatedExpr::PropertyAccess { annotation, .. }
            | AnnotatedExpr::StringLiteral { annotation, .. }
            | AnnotatedExpr::BooleanLiteral { annotation, .. }
            | AnnotatedExpr::IntLiteral { annotation, .. }
            | AnnotatedExpr::FloatLiteral { annotation, .. }
            | AnnotatedExpr::ArrayLiteral { annotation, .. }
            | AnnotatedExpr::ObjectLiteral { annotation, .. }
            | AnnotatedExpr::BinaryOp { annotation, .. }
            | AnnotatedExpr::Negation { annotation, .. } => annotation,
        }
    }
}

impl Ranged for AnnotatedExpr<DocumentRange> {
    fn range(&self) -> &DocumentRange {
        self.annotation()
    }
}

impl<'a, T> AnnotatedExpr<T> {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        match self {
            AnnotatedExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            AnnotatedExpr::PropertyAccess {
                object, property, ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(property.as_str())),
            AnnotatedExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            AnnotatedExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            AnnotatedExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
            AnnotatedExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            AnnotatedExpr::ArrayLiteral { elements, .. } => {
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
            AnnotatedExpr::ObjectLiteral { properties, .. } => {
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
            AnnotatedExpr::BinaryOp {
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
            AnnotatedExpr::Negation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
        }
    }
}

impl<T> Display for AnnotatedExpr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LessThanOrEqual => write!(f, "<="),
            BinaryOp::GreaterThanOrEqual => write!(f, ">="),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::Plus => write!(f, "+"),
        }
    }
}
