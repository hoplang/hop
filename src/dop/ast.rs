use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::parser::DopVarName;
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
pub enum DopExpr {
    Variable {
        value: DopVarName,
    },
    PropertyAccess {
        object: Box<DopExpr>,
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
        elements: Vec<DopExpr>,
        range: DocumentRange,
    },
    ObjectLiteral {
        properties: Vec<(DocumentRange, DopExpr)>,
        range: DocumentRange,
    },
    BinaryOp {
        left: Box<DopExpr>,
        operator: BinaryOp,
        right: Box<DopExpr>,
        range: DocumentRange,
    },
    UnaryOp {
        operator: UnaryOp,
        operand: Box<DopExpr>,
        range: DocumentRange,
    },
}

impl Ranged for DopExpr {
    fn range(&self) -> &DocumentRange {
        match self {
            DopExpr::Variable { value, .. } => value.range(),
            DopExpr::PropertyAccess { range, .. }
            | DopExpr::StringLiteral { range, .. }
            | DopExpr::BooleanLiteral { range, .. }
            | DopExpr::NumberLiteral { range, .. }
            | DopExpr::ArrayLiteral { range, .. }
            | DopExpr::ObjectLiteral { range, .. }
            | DopExpr::BinaryOp { range, .. }
            | DopExpr::UnaryOp { range, .. } => range,
        }
    }
}

impl Pretty for DopExpr {
    fn to_doc(&self) -> RcDoc<'static> {
        match self {
            DopExpr::Variable { value } => RcDoc::text(value.to_string()),
            DopExpr::PropertyAccess {
                object,
                property,
                range: _,
            } => object
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::text(property.to_string())),
            DopExpr::StringLiteral { value, range: _ } => RcDoc::text(format!("\"{}\"", value)),
            DopExpr::BooleanLiteral { value, range: _ } => RcDoc::text(value.to_string()),
            DopExpr::NumberLiteral { value, range: _ } => RcDoc::text(value.to_string()),
            DopExpr::ArrayLiteral { elements, range: _ } => {
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
            DopExpr::ObjectLiteral {
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
            DopExpr::BinaryOp {
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
            DopExpr::UnaryOp {
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

impl Display for DopExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
