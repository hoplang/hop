use std::fmt::{self, Display};

use crate::dop::field_name::FieldName;
use crate::dop::var_name::VarName;
use pretty::BoxDoc;

use super::{
    Type,
    r#type::{ComparableType, EquatableType, NumericType},
};

pub type SimpleExpr = Expr<()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<A> {
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

    /// An enum instantiation expression, e.g. Color::Red
    EnumInstantiation {
        enum_name: String,
        variant_name: String,
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
    BooleanNegation { operand: Box<Self>, annotation: A },

    /// Boolean logical AND expression
    BooleanLogicalAnd {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },

    /// Boolean logical OR expression
    BooleanLogicalOr {
        left: Box<Self>,
        right: Box<Self>,
        annotation: A,
    },

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
}

impl<A> Expr<A> {
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;

        match self {
            Expr::Var { kind, .. }
            | Expr::FieldAccess { kind, .. }
            | Expr::ArrayLiteral { kind, .. }
            | Expr::RecordInstantiation { kind, .. }
            | Expr::EnumInstantiation { kind, .. } => kind,

            Expr::FloatLiteral { .. } => &FLOAT_TYPE,
            Expr::IntLiteral { .. } => &INT_TYPE,

            Expr::JsonEncode { .. }
            | Expr::EnvLookup { .. }
            | Expr::StringConcat { .. }
            | Expr::StringLiteral { .. } => &STRING_TYPE,

            Expr::NumericAdd { operand_types, .. }
            | Expr::NumericSubtract { operand_types, .. }
            | Expr::NumericMultiply { operand_types, .. } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            Expr::BooleanLiteral { .. }
            | Expr::BooleanNegation { .. }
            | Expr::Equals { .. }
            | Expr::NotEquals { .. }
            | Expr::LessThan { .. }
            | Expr::GreaterThan { .. }
            | Expr::LessThanOrEqual { .. }
            | Expr::GreaterThanOrEqual { .. }
            | Expr::BooleanLogicalAnd { .. }
            | Expr::BooleanLogicalOr { .. } => &BOOL_TYPE,
        }
    }

    pub fn annotation(&self) -> &A {
        match self {
            Expr::Var { annotation, .. }
            | Expr::FieldAccess { annotation, .. }
            | Expr::StringLiteral { annotation, .. }
            | Expr::BooleanLiteral { annotation, .. }
            | Expr::FloatLiteral { annotation, .. }
            | Expr::IntLiteral { annotation, .. }
            | Expr::ArrayLiteral { annotation, .. }
            | Expr::RecordInstantiation { annotation, .. }
            | Expr::EnumInstantiation { annotation, .. }
            | Expr::JsonEncode { annotation, .. }
            | Expr::EnvLookup { annotation, .. }
            | Expr::StringConcat { annotation, .. }
            | Expr::NumericAdd { annotation, .. }
            | Expr::NumericSubtract { annotation, .. }
            | Expr::NumericMultiply { annotation, .. }
            | Expr::BooleanNegation { annotation, .. }
            | Expr::Equals { annotation, .. }
            | Expr::NotEquals { annotation, .. }
            | Expr::LessThan { annotation, .. }
            | Expr::GreaterThan { annotation, .. }
            | Expr::LessThanOrEqual { annotation, .. }
            | Expr::GreaterThanOrEqual { annotation, .. }
            | Expr::BooleanLogicalAnd { annotation, .. }
            | Expr::BooleanLogicalOr { annotation, .. } => annotation,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Expr::Var { value, .. } => BoxDoc::text(value.as_str()),
            Expr::FieldAccess {
                record: object,
                field,
                ..
            } => object
                .to_doc()
                .append(BoxDoc::text("."))
                .append(BoxDoc::text(field.as_str())),
            Expr::StringLiteral { value, .. } => BoxDoc::text(format!("\"{}\"", value)),
            Expr::BooleanLiteral { value, .. } => BoxDoc::text(value.to_string()),
            Expr::FloatLiteral { value, .. } => BoxDoc::text(value.to_string()),
            Expr::IntLiteral { value, .. } => BoxDoc::text(value.to_string()),
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
            Expr::RecordInstantiation {
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
            Expr::JsonEncode { value, .. } => BoxDoc::nil()
                .append(BoxDoc::text("JsonEncode("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            Expr::EnvLookup { key, .. } => BoxDoc::nil()
                .append(BoxDoc::text("EnvLookup("))
                .append(key.to_doc())
                .append(BoxDoc::text(")")),
            Expr::StringConcat { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::NumericAdd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" + "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::NumericSubtract { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" - "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::NumericMultiply { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" * "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::BooleanNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("!"))
                .append(operand.to_doc())
                .append(BoxDoc::text(")")),
            Expr::Equals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" == "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::NotEquals { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" != "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::LessThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" < "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::GreaterThan { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" > "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::LessThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" <= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::GreaterThanOrEqual { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" >= "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::BooleanLogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::BooleanLogicalOr { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" || "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            Expr::EnumInstantiation {
                enum_name,
                variant_name,
                ..
            } => BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
        }
    }
}

impl<A> Display for Expr<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
