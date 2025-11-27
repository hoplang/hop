use std::fmt::{self, Display};

use crate::dop::field_name::FieldName;
use crate::dop::var_name::VarName;
use pretty::BoxDoc;

use super::{
    Type,
    r#type::{ComparableType, EquatableType, NumericType},
};

pub type SimpleTypedExpr = TypedExpr<()>;

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr<A> {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        kind: Type,
        annotation: A,
    },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        kind: Type,
        annotation: A,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: String, annotation: A },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool, annotation: A },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64, annotation: A },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i64, annotation: A },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<Self>,
        kind: Type,
        annotation: A,
    },

    /// A record instantiation expression, e.g. User(name: "John", age: 30)
    RecordInstantiation {
        record_name: String,
        fields: Vec<(FieldName, Self)>,
        kind: Type,
        annotation: A,
    },

    /// JSON encode expression for converting values to JSON strings
    JsonEncode { value: Box<Self>, annotation: A },

    /// Environment variable lookup expression
    EnvLookup { key: Box<Self>, annotation: A },

    /// String concatenation expression for joining two string expressions
    StringConcat {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },

    /// Numeric addition expression for adding numeric values
    NumericAdd {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: NumericType,
        annotation: A,
    },

    /// Numeric subtraction expression for subtracting numeric values
    NumericSubtract {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: NumericType,
        annotation: A,
    },

    /// Numeric multiplication expression for multiplying numeric values
    NumericMultiply {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: NumericType,
        annotation: A,
    },

    /// Boolean negation expression
    Negation { operand: Box<Self>, annotation: A },

    /// Equals expression
    Equals {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: EquatableType,
        annotation: A,
    },

    /// Not equals expression
    NotEquals {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: EquatableType,
        annotation: A,
    },

    /// Less than expression
    LessThan {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
        annotation: A,
    },

    /// Greater than expression
    GreaterThan {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
        annotation: A,
    },

    /// Less than or equal expression
    LessThanOrEqual {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
        annotation: A,
    },

    /// Greater than or equal expression
    GreaterThanOrEqual {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
        annotation: A,
    },

    /// Logical AND expression
    LogicalAnd {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },

    /// Logical OR expression
    LogicalOr {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },
}

impl<A> TypedExpr<A> {
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;

        match self {
            TypedExpr::Var { kind, .. }
            | TypedExpr::FieldAccess { kind, .. }
            | TypedExpr::ArrayLiteral { kind, .. }
            | TypedExpr::RecordInstantiation { kind, .. } => kind,

            TypedExpr::FloatLiteral { .. } => &FLOAT_TYPE,
            TypedExpr::IntLiteral { .. } => &INT_TYPE,

            TypedExpr::JsonEncode { .. }
            | TypedExpr::EnvLookup { .. }
            | TypedExpr::StringConcat { .. }
            | TypedExpr::StringLiteral { .. } => &STRING_TYPE,

            TypedExpr::NumericAdd { operand_types, .. }
            | TypedExpr::NumericSubtract { operand_types, .. }
            | TypedExpr::NumericMultiply { operand_types, .. } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            TypedExpr::BooleanLiteral { .. }
            | TypedExpr::Negation { .. }
            | TypedExpr::Equals { .. }
            | TypedExpr::NotEquals { .. }
            | TypedExpr::LessThan { .. }
            | TypedExpr::GreaterThan { .. }
            | TypedExpr::LessThanOrEqual { .. }
            | TypedExpr::GreaterThanOrEqual { .. }
            | TypedExpr::LogicalAnd { .. }
            | TypedExpr::LogicalOr { .. } => &BOOL_TYPE,
        }
    }

    pub fn annotation(&self) -> &A {
        match self {
            TypedExpr::Var { annotation, .. }
            | TypedExpr::FieldAccess { annotation, .. }
            | TypedExpr::StringLiteral { annotation, .. }
            | TypedExpr::BooleanLiteral { annotation, .. }
            | TypedExpr::FloatLiteral { annotation, .. }
            | TypedExpr::IntLiteral { annotation, .. }
            | TypedExpr::ArrayLiteral { annotation, .. }
            | TypedExpr::RecordInstantiation { annotation, .. }
            | TypedExpr::JsonEncode { annotation, .. }
            | TypedExpr::EnvLookup { annotation, .. }
            | TypedExpr::StringConcat { annotation, .. }
            | TypedExpr::NumericAdd { annotation, .. }
            | TypedExpr::NumericSubtract { annotation, .. }
            | TypedExpr::NumericMultiply { annotation, .. }
            | TypedExpr::Negation { annotation, .. }
            | TypedExpr::Equals { annotation, .. }
            | TypedExpr::NotEquals { annotation, .. }
            | TypedExpr::LessThan { annotation, .. }
            | TypedExpr::GreaterThan { annotation, .. }
            | TypedExpr::LessThanOrEqual { annotation, .. }
            | TypedExpr::GreaterThanOrEqual { annotation, .. }
            | TypedExpr::LogicalAnd { annotation, .. }
            | TypedExpr::LogicalOr { annotation, .. } => annotation,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedExpr::Var { value, .. } => BoxDoc::text(value.as_str()),
            TypedExpr::FieldAccess {
                record: object,
                field,
                ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            TypedExpr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            TypedExpr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            TypedExpr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            TypedExpr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
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
            TypedExpr::RecordInstantiation {
                record_name,
                fields,
                ..
            } => {
                if fields.is_empty() {
                    BoxDoc::text(record_name.as_str()).append(BoxDoc::text("()"))
                } else {
                    BoxDoc::text(record_name.as_str())
                        .append(BoxDoc::text("("))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    fields.iter().map(|(key, value)| {
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
                        .append(BoxDoc::text(")"))
                }
            }
            TypedExpr::JsonEncode { value, .. } => BoxDoc::nil()
                .append(BoxDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::EnvLookup { key, .. } => BoxDoc::nil()
                .append(BoxDoc::text("EnvLookup("))
                .append(key.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::StringConcat { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::NumericAdd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::NumericSubtract { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" - "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::NumericMultiply { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" * "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::Negation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::Equals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" == "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::NotEquals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" != "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::LessThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" < "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::GreaterThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" > "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::LessThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" <= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::GreaterThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" >= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::LogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::LogicalOr { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" || "))
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
