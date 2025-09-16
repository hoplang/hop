use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, Ranged};
use crate::dop::ast::{BinaryOp, UnaryOp};
use crate::dop::parser::VarName;
use crate::hop::pretty::Pretty;
use pretty::RcDoc;

use super::Type;

#[derive(Debug, Clone)]
pub enum TypedExpr {
    Variable {
        value: VarName,
        typ: Type,
    },
    PropertyAccess {
        object: Box<TypedExpr>,
        property: DocumentRange,
        range: DocumentRange,
        typ: Type,
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
    ArrayLiteral {
        elements: Vec<TypedExpr>,
        range: DocumentRange,
        element_type: Option<Type>,
    },
    ObjectLiteral {
        properties: Vec<(DocumentRange, TypedExpr)>,
        range: DocumentRange,
        typ: Type,
    },
    BinaryOp {
        left: Box<TypedExpr>,
        operator: BinaryOp,
        right: Box<TypedExpr>,
        range: DocumentRange,
        typ: Type,
    },
    UnaryOp {
        operator: UnaryOp,
        operand: Box<TypedExpr>,
        range: DocumentRange,
        typ: Type,
    },
}

impl TypedExpr {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpr::Variable { typ, .. } => typ.clone(),
            TypedExpr::PropertyAccess { typ, .. } => typ.clone(),
            TypedExpr::StringLiteral { .. } => Type::String,
            TypedExpr::BooleanLiteral { .. } => Type::Bool,
            TypedExpr::NumberLiteral { .. } => Type::Number,
            TypedExpr::ArrayLiteral { element_type, .. } => {
                Type::Array(element_type.as_ref().map(|t| Box::new(t.clone())))
            }
            TypedExpr::ObjectLiteral { typ, .. } => typ.clone(),
            TypedExpr::BinaryOp { typ, .. } => typ.clone(),
            TypedExpr::UnaryOp { typ, .. } => typ.clone(),
        }
    }
}

impl Ranged for TypedExpr {
    fn range(&self) -> &DocumentRange {
        match self {
            TypedExpr::Variable { value, .. } => value.range(),
            TypedExpr::PropertyAccess { range, .. }
            | TypedExpr::StringLiteral { range, .. }
            | TypedExpr::BooleanLiteral { range, .. }
            | TypedExpr::NumberLiteral { range, .. }
            | TypedExpr::ArrayLiteral { range, .. }
            | TypedExpr::ObjectLiteral { range, .. }
            | TypedExpr::BinaryOp { range, .. }
            | TypedExpr::UnaryOp { range, .. } => range,
        }
    }
}

impl Pretty for TypedExpr {
    fn to_doc(&self) -> RcDoc<'static> {
        match self {
            TypedExpr::Variable { value, .. } => RcDoc::text(value.to_string()),
            TypedExpr::PropertyAccess {
                object, property, ..
            } => object
                .to_doc()
                .append(RcDoc::text("."))
                .append(RcDoc::text(property.to_string())),
            TypedExpr::StringLiteral { value, .. } => RcDoc::text(format!("\"{}\"", value)),
            TypedExpr::BooleanLiteral { value, .. } => RcDoc::text(value.to_string()),
            TypedExpr::NumberLiteral { value, .. } => RcDoc::text(value.to_string()),
            TypedExpr::ArrayLiteral { elements, .. } => {
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
            TypedExpr::ObjectLiteral { properties, .. } => {
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
            TypedExpr::BinaryOp {
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
            TypedExpr::UnaryOp {
                operator, operand, ..
            } => RcDoc::nil()
                .append(RcDoc::text("("))
                .append(RcDoc::text(operator.to_string()))
                .append(operand.to_doc())
                .append(RcDoc::text(")")),
        }
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
