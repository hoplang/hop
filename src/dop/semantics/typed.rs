use std::fmt::{self, Display};

use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::var_name::VarName;
use pretty::BoxDoc;

use super::r#type::{ComparableType, EquatableType, NumericType, Type};

/// A pattern in an enum match arm
#[derive(Debug, Clone, PartialEq)]
pub enum TypedEnumPattern {
    /// An enum variant pattern, e.g. `Color::Red`
    Variant {
        enum_name: String,
        variant_name: String,
    },
}

/// A pattern in a boolean match arm
#[derive(Debug, Clone, PartialEq)]
pub enum TypedBoolPattern {
    /// A boolean literal pattern, e.g. `true` or `false`
    Literal(bool),
}

/// A single arm in an enum match expression, e.g. `Color::Red => "red"`
#[derive(Debug, Clone, PartialEq)]
pub struct TypedEnumMatchArm {
    /// The pattern being matched
    pub pattern: TypedEnumPattern,
    /// The expression to evaluate if this arm matches
    pub body: TypedExpr,
}

/// A single arm in a boolean match expression, e.g. `true => "yes"`
#[derive(Debug, Clone, PartialEq)]
pub struct TypedBoolMatchArm {
    /// The pattern being matched
    pub pattern: TypedBoolPattern,
    /// The expression to evaluate if this arm matches
    pub body: TypedExpr,
}

/// A pattern in an option match arm
#[derive(Debug, Clone, PartialEq)]
pub enum TypedOptionPattern {
    /// A Some pattern with an optional variable binding, e.g. `Some(x)` or `Some(_)`
    Some {
        /// The variable binding for the inner value, if any
        binding: Option<(VarName, Type)>,
    },
    /// A None pattern
    None,
}

/// A single arm in an option match expression, e.g. `Some(_) => "value"`
#[derive(Debug, Clone, PartialEq)]
pub struct TypedOptionMatchArm {
    /// The pattern being matched
    pub pattern: TypedOptionPattern,
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

    /// An option literal expression, e.g. Some(42) or None
    OptionLiteral {
        /// The inner value (Some) or None
        value: Option<Box<Self>>,
        kind: Type,
    },

    /// An enum match expression, e.g. match color { Color::Red => "red", Color::Blue => "blue" }
    EnumMatch {
        subject: Box<Self>,
        arms: Vec<TypedEnumMatchArm>,
        kind: Type,
    },

    /// A boolean match expression, e.g. match flag { true => "yes", false => "no" }
    BoolMatch {
        subject: Box<Self>,
        arms: Vec<TypedBoolMatchArm>,
        kind: Type,
    },

    /// An option match expression, e.g. match opt { Some(_) => "value", None => "empty" }
    OptionMatch {
        subject: Box<Self>,
        arms: Vec<TypedOptionMatchArm>,
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
            | TypedExpr::OptionLiteral { kind, .. }
            | TypedExpr::EnumMatch { kind, .. }
            | TypedExpr::BoolMatch { kind, .. }
            | TypedExpr::OptionMatch { kind, .. } => kind,

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
            TypedExpr::OptionLiteral { value, .. } => match value {
                Some(inner) => BoxDoc::text("Some(")
                    .append(inner.to_doc())
                    .append(BoxDoc::text(")")),
                None => BoxDoc::text("None"),
            },
            TypedExpr::EnumMatch { subject, arms, .. } => {
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
                                        let pattern_doc = match &arm.pattern {
                                            TypedEnumPattern::Variant {
                                                enum_name,
                                                variant_name,
                                            } => BoxDoc::text(enum_name.as_str())
                                                .append(BoxDoc::text("::"))
                                                .append(BoxDoc::text(variant_name.as_str())),
                                        };
                                        pattern_doc
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
            TypedExpr::BoolMatch { subject, arms, .. } => {
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
                                        let pattern_doc = match &arm.pattern {
                                            TypedBoolPattern::Literal(true) => BoxDoc::text("true"),
                                            TypedBoolPattern::Literal(false) => {
                                                BoxDoc::text("false")
                                            }
                                        };
                                        pattern_doc
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
            TypedExpr::OptionMatch { subject, arms, .. } => {
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
                                        let pattern_doc = match &arm.pattern {
                                            TypedOptionPattern::Some { binding } => {
                                                match binding {
                                                    Some((name, _)) => BoxDoc::text("Some(")
                                                        .append(BoxDoc::text(name.as_str()))
                                                        .append(BoxDoc::text(")")),
                                                    None => BoxDoc::text("Some(_)"),
                                                }
                                            }
                                            TypedOptionPattern::None => BoxDoc::text("None"),
                                        };
                                        pattern_doc
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
