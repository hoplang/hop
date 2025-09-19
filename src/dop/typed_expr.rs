use std::fmt::{self, Display};

use crate::dop::var_name::VarName;
use pretty::BoxDoc;

use super::{Type, r#type::ComparableType};

pub type SimpleTypedExpr = TypedExpr<()>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr<A> {
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

    /// JSON encode expression for converting values to JSON strings
    JsonEncode { value: Box<Self>, annotation: A },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },

    /// Boolean negation expression
    Negation { operand: Box<Self>, annotation: A },

    /// Comparison expression
    Comparison {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
        annotation: A,
    },
}

impl<A> TypedExpr<A> {
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static NUMBER_TYPE: Type = Type::Number;

        match self {
            TypedExpr::Var { kind, .. }
            | TypedExpr::PropertyAccess { kind, .. }
            | TypedExpr::ArrayLiteral { kind, .. }
            | TypedExpr::ObjectLiteral { kind, .. } => kind,

            TypedExpr::NumberLiteral { .. } => &NUMBER_TYPE,

            TypedExpr::JsonEncode { .. }
            | TypedExpr::StringConcat { .. }
            | TypedExpr::StringLiteral { .. } => &STRING_TYPE,

            TypedExpr::BooleanLiteral { .. }
            | TypedExpr::Negation { .. }
            | TypedExpr::Comparison { .. } => &BOOL_TYPE,
        }
    }

    pub fn annotation(&self) -> &A {
        match self {
            TypedExpr::Var { annotation, .. }
            | TypedExpr::PropertyAccess { annotation, .. }
            | TypedExpr::StringLiteral { annotation, .. }
            | TypedExpr::BooleanLiteral { annotation, .. }
            | TypedExpr::NumberLiteral { annotation, .. }
            | TypedExpr::ArrayLiteral { annotation, .. }
            | TypedExpr::ObjectLiteral { annotation, .. }
            | TypedExpr::JsonEncode { annotation, .. }
            | TypedExpr::StringConcat { annotation, .. }
            | TypedExpr::Negation { annotation, .. }
            | TypedExpr::Comparison { annotation, .. } => annotation,
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
            TypedExpr::Negation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::Comparison { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" == "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
        }
    }
}

impl<A> Display for TypedExpr<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
