use std::fmt::{self, Display};

use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::var_name::VarName;
use pretty::BoxDoc;

use super::r#type::{ComparableType, EquatableType, NumericType, Type};

/// A pattern that matches an enum variant
#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumPattern {
    pub enum_name: String,
    pub variant_name: String,
}

/// A single arm in a match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct TypedMatchArm {
    /// The enum variant being matched (e.g., `Color::Red`)
    pub pattern: TypedEnumPattern,
    /// The expression to evaluate if this arm matches
    pub body: TypedExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    /// A variable expression, e.g. foo
    Var { value: VarName, kind: Type },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        kind: Type,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: String },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64 },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i64 },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral { elements: Vec<Self>, kind: Type },

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: String,
        fields: Vec<(FieldName, Self)>,
        kind: Type,
    },

    /// An enum literal expression, e.g. Color::Red
    EnumLiteral {
        enum_name: String,
        variant_name: String,
        kind: Type,
    },

    /// A match expression, e.g. match color { Color::Red => "red", Color::Blue => "blue" }
    Match {
        subject: Box<Self>,
        arms: Vec<TypedMatchArm>,
        kind: Type,
    },

    /// String concatenation expression for joining two string expressions
    StringConcat { left: Box<Self>, right: Box<Self> },

    /// Numeric addition expression for adding numeric values
    NumericAdd {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: NumericType,
    },

    /// Numeric subtraction expression for subtracting numeric values
    NumericSubtract {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: NumericType,
    },

    /// Numeric multiplication expression for multiplying numeric values
    NumericMultiply {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: NumericType,
    },

    /// Boolean negation expression
    BooleanNegation { operand: Box<Self> },

    /// Boolean logical AND expression
    BooleanLogicalAnd { left: Box<Self>, right: Box<Self> },

    /// Boolean logical OR expression
    BooleanLogicalOr { left: Box<Self>, right: Box<Self> },

    /// Equals expression
    Equals {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: EquatableType,
    },

    /// Not equals expression
    NotEquals {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: EquatableType,
    },

    /// Less than expression
    LessThan {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
    },

    /// Greater than expression
    GreaterThan {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
    },

    /// Less than or equal expression
    LessThanOrEqual {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
    },

    /// Greater than or equal expression
    GreaterThanOrEqual {
        left: Box<Self>,
        right: Box<Self>,
        operand_types: ComparableType,
    },
}

impl TypedExpr {
    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;

        match self {
            TypedExpr::Var { kind, .. }
            | TypedExpr::FieldAccess { kind, .. }
            | TypedExpr::ArrayLiteral { kind, .. }
            | TypedExpr::RecordLiteral { kind, .. }
            | TypedExpr::EnumLiteral { kind, .. }
            | TypedExpr::Match { kind, .. } => kind,

            TypedExpr::FloatLiteral { .. } => &FLOAT_TYPE,
            TypedExpr::IntLiteral { .. } => &INT_TYPE,

            TypedExpr::StringConcat { .. } | TypedExpr::StringLiteral { .. } => &STRING_TYPE,

            TypedExpr::NumericAdd { operand_types, .. }
            | TypedExpr::NumericSubtract { operand_types, .. }
            | TypedExpr::NumericMultiply { operand_types, .. } => match operand_types {
                NumericType::Int => &INT_TYPE,
                NumericType::Float => &FLOAT_TYPE,
            },

            TypedExpr::BooleanLiteral { .. }
            | TypedExpr::BooleanNegation { .. }
            | TypedExpr::Equals { .. }
            | TypedExpr::NotEquals { .. }
            | TypedExpr::LessThan { .. }
            | TypedExpr::GreaterThan { .. }
            | TypedExpr::LessThanOrEqual { .. }
            | TypedExpr::GreaterThanOrEqual { .. }
            | TypedExpr::BooleanLogicalAnd { .. }
            | TypedExpr::BooleanLogicalOr { .. } => &BOOL_TYPE,
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
            TypedExpr::RecordLiteral {
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
            TypedExpr::BooleanNegation { operand, .. } => BoxDoc::nil()
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
            TypedExpr::BooleanLogicalAnd { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" && "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::BooleanLogicalOr { left, right, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(left.to_doc())
                .append(BoxDoc::text(" || "))
                .append(right.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::EnumLiteral {
                enum_name,
                variant_name,
                ..
            } => BoxDoc::text(enum_name.as_str())
                .append(BoxDoc::text("::"))
                .append(BoxDoc::text(variant_name.as_str())),
            TypedExpr::Match { subject, arms, .. } => {
                if arms.is_empty() {
                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {}"))
                } else {
                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    arms.iter().map(|arm| {
                                        BoxDoc::text(arm.pattern.enum_name.as_str())
                                            .append(BoxDoc::text("::"))
                                            .append(BoxDoc::text(arm.pattern.variant_name.as_str()))
                                            .append(BoxDoc::text(" => "))
                                            .append(arm.body.to_doc())
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
        }
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
