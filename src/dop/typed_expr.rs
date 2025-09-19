use std::fmt::{self, Display};

use crate::dop::var_name::VarName;
use pretty::BoxDoc;

use super::{Type, expr::BinaryOp};

/// Type alias for TypedExpr without additional annotation (used in hop/ modules)
pub type TypedExpr = AnnotatedTypedExpr<()>;

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotatedTypedExpr<A> {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        kind: Type,
        annotation: A,
    },

    /// A property access expression, e.g. foo.bar
    PropertyAccess {
        object: Box<Self>,
        property: String,
        kind: Type,
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
    ArrayLiteral {
        elements: Vec<Self>,
        kind: Type,
        annotation: A,
    },

    ObjectLiteral {
        properties: Vec<(String, Self)>,
        kind: Type,
        annotation: A,
    },

    BinaryOp {
        left: Box<Self>,
        operator: BinaryOp,
        right: Box<Self>,
        kind: Type,
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

    /// Boolean negation expression for negating boolean values
    Negation { operand: Box<Self>, annotation: A },
}

impl<A> AnnotatedTypedExpr<A> {
    pub fn kind(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static NUMBER_TYPE: Type = Type::Number;

        match self {
            AnnotatedTypedExpr::Var { kind, .. }
            | AnnotatedTypedExpr::PropertyAccess { kind, .. }
            | AnnotatedTypedExpr::ArrayLiteral { kind, .. }
            | AnnotatedTypedExpr::ObjectLiteral { kind, .. }
            | AnnotatedTypedExpr::BinaryOp { kind, .. } => kind,
            AnnotatedTypedExpr::StringLiteral { .. } => &STRING_TYPE,
            AnnotatedTypedExpr::BooleanLiteral { .. } => &BOOL_TYPE,
            AnnotatedTypedExpr::NumberLiteral { .. } => &NUMBER_TYPE,
            AnnotatedTypedExpr::JsonEncode { .. } => &STRING_TYPE,
            AnnotatedTypedExpr::StringConcat { .. } => &STRING_TYPE,
            AnnotatedTypedExpr::Negation { .. } => &BOOL_TYPE,
        }
    }

    pub fn annotation(&self) -> &A {
        match self {
            AnnotatedTypedExpr::Var { annotation, .. }
            | AnnotatedTypedExpr::PropertyAccess { annotation, .. }
            | AnnotatedTypedExpr::StringLiteral { annotation, .. }
            | AnnotatedTypedExpr::BooleanLiteral { annotation, .. }
            | AnnotatedTypedExpr::NumberLiteral { annotation, .. }
            | AnnotatedTypedExpr::ArrayLiteral { annotation, .. }
            | AnnotatedTypedExpr::ObjectLiteral { annotation, .. }
            | AnnotatedTypedExpr::BinaryOp { annotation, .. }
            | AnnotatedTypedExpr::JsonEncode { annotation, .. }
            | AnnotatedTypedExpr::StringConcat { annotation, .. }
            | AnnotatedTypedExpr::Negation { annotation, .. } => annotation,
        }
    }

    pub fn to_doc(&self) -> BoxDoc {
        match self {
            AnnotatedTypedExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            AnnotatedTypedExpr::PropertyAccess {
                object, property, ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(property.as_str())),
            AnnotatedTypedExpr::StringLiteral { value, .. } => {
                BoxDoc::text(format!("\"{}\"", value))
            }
            AnnotatedTypedExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            AnnotatedTypedExpr::NumberLiteral { value, .. } => BoxDoc::text(value.to_string()),
            AnnotatedTypedExpr::ArrayLiteral { elements, .. } => {
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
            AnnotatedTypedExpr::ObjectLiteral { properties, .. } => {
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
            AnnotatedTypedExpr::BinaryOp {
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
            AnnotatedTypedExpr::JsonEncode { value, .. } => BoxDoc::nil()
                .append(BoxDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            AnnotatedTypedExpr::StringConcat { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            AnnotatedTypedExpr::Negation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
        }
    }
}

impl<A> Display for AnnotatedTypedExpr<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
