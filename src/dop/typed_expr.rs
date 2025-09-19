use std::fmt::{self, Display};

use crate::dop::var_name::VarName;
use pretty::BoxDoc;

use super::{Type, expr::BinaryOp, expr::UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    /// A variable expression, e.g. foo
    Var { value: VarName, kind: Type },

    /// A property access expression, e.g. foo.bar
    PropertyAccess {
        object: Box<Self>,
        property: String,
        kind: Type,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: String, kind: Type },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, kind: Type },

    /// A number literal expression, e.g. 2.5
    NumberLiteral {
        value: serde_json::Number,
        kind: Type,
    },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral { elements: Vec<Self>, kind: Type },

    ObjectLiteral {
        properties: Vec<(String, Self)>,
        kind: Type,
    },

    BinaryOp {
        left: Box<Self>,
        operator: BinaryOp,
        right: Box<Self>,
        kind: Type,
    },

    UnaryOp {
        operator: UnaryOp,
        operand: Box<Self>,
        kind: Type,
    },

    /// JSON encode expression for converting values to JSON strings
    JsonEncode { value: Box<Self>, kind: Type },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<Self>,
        right: Box<Self>,
        kind: Type,
    },
}

impl TypedExpr {
    pub fn kind(&self) -> &Type {
        match self {
            TypedExpr::Var { kind, .. }
            | TypedExpr::PropertyAccess { kind, .. }
            | TypedExpr::StringLiteral { kind, .. }
            | TypedExpr::BooleanLiteral { kind, .. }
            | TypedExpr::NumberLiteral { kind, .. }
            | TypedExpr::ArrayLiteral { kind, .. }
            | TypedExpr::ObjectLiteral { kind, .. }
            | TypedExpr::BinaryOp { kind, .. }
            | TypedExpr::UnaryOp { kind, .. }
            | TypedExpr::JsonEncode { kind, .. }
            | TypedExpr::StringConcat { kind, .. } => kind,
        }
    }

    pub fn to_doc(&self) -> BoxDoc {
        match self {
            TypedExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            TypedExpr::PropertyAccess {
                object, property, ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(property.as_str())),
            TypedExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            TypedExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            TypedExpr::NumberLiteral { value, .. } => BoxDoc::text(value.to_string()),
            TypedExpr::ArrayLiteral { elements, .. } => {
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
            TypedExpr::ObjectLiteral { properties, .. } => {
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
            TypedExpr::BinaryOp {
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
            TypedExpr::UnaryOp {
                operator, operand, ..
            } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text(operator.to_string()))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::JsonEncode { value, .. } => BoxDoc::nil()
                .append(BoxDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::StringConcat { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
        }
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
