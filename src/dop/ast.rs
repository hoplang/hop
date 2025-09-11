use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::parser::VarName;
use crate::hop::pretty::Pretty;
use pretty::RcDoc;

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
pub enum Expr {
    Variable {
        value: VarName,
    },
    PropertyAccess {
        object: Box<Expr>,
        property: DocumentRange,
        range: DocumentRange,
    },
    StringLiteral {
        value: String,
        range: DocumentRange,
    },
    BooleanLiteral {
        value: bool,
        range: DocumentRange,
    },
    NumberLiteral {
        value: serde_json::Number,
        range: DocumentRange,
    },
    /// An array literal, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<Expr>,
        range: DocumentRange,
    },
    ObjectLiteral {
        properties: Vec<(DocumentRange, Expr)>,
        range: DocumentRange,
    },
    BinaryOp {
        left: Box<Expr>,
        operator: BinaryOp,
        right: Box<Expr>,
        range: DocumentRange,
    },
    UnaryOp {
        operator: UnaryOp,
        operand: Box<Expr>,
        range: DocumentRange,
    },
}

impl Ranged for Expr {
    fn range(&self) -> &DocumentRange {
        match self {
            Expr::Variable { value, .. } => value.range(),
            Expr::PropertyAccess { range, .. }
            | Expr::StringLiteral { range, .. }
            | Expr::BooleanLiteral { range, .. }
            | Expr::NumberLiteral { range, .. }
            | Expr::ArrayLiteral { range, .. }
            | Expr::ObjectLiteral { range, .. }
            | Expr::BinaryOp { range, .. }
            | Expr::UnaryOp { range, .. } => range,
        }
    }
}

impl Pretty for Expr {
    fn to_doc(&self) -> RcDoc<'static> {
        match self {
            Expr::Variable { value } => RcDoc::text(value.to_string()),
            Expr::PropertyAccess {
                object,
                property,
                range: _,
            } => object
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::text(property.to_string())),
            Expr::StringLiteral { value, range: _ } => RcDoc::text(format!("\"{}\"", value)),
            Expr::BooleanLiteral { value, range: _ } => RcDoc::text(value.to_string()),
            Expr::NumberLiteral { value, range: _ } => RcDoc::text(value.to_string()),
            Expr::ArrayLiteral { elements, range: _ } => {
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
            Expr::ObjectLiteral {
                properties,
                range: _,
            } => {
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
                range: _,
            } => RcDoc::nil()
                .append(RcDoc::text("("))
                .append(left.to_doc())
                .append(RcDoc::text(format!(" {} ", operator)))
                .append(right.to_doc())
                .append(RcDoc::text(")")),
            Expr::UnaryOp {
                operator,
                operand,
                range: _,
            } => RcDoc::nil()
                .append(RcDoc::text("("))
                .append(RcDoc::text(operator.to_string()))
                .append(operand.to_doc())
                .append(RcDoc::text(")")),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
