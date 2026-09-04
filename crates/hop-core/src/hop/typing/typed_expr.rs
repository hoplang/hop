use std::fmt::{self, Display};
use std::sync::Arc;

use crate::document::CheapString;
use crate::hop::patterns::{EnumPattern, Match};
use crate::html::HtmlElement;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

use super::r#type::{ComparableType, EquatableType, NumericType, Type};

/// The source of iteration in a for loop - either an array or an inclusive range.
#[derive(Debug, Clone)]
pub enum TypedLoopSource {
    /// Iterate over elements of an array
    Array(TypedExpr),
    /// Iterate over an inclusive integer range
    RangeInclusive { start: TypedExpr, end: TypedExpr },
}

#[derive(Debug, Clone)]
pub enum TypedAttributeValue {
    Expression(TypedExpr),
    String(CheapString),
}

impl TypedAttributeValue {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedAttributeValue::Expression(expr) => BoxDoc::text("escape(")
                .append(expr.to_doc())
                .append(BoxDoc::text(")")),
            TypedAttributeValue::String(s) => BoxDoc::text(format!("raw({:?})", s.as_str())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedAttribute {
    pub name: CheapString,
    pub value: Option<TypedAttributeValue>,
}

impl TypedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(self.name.as_str());
        match &self.value {
            Some(value) => name_doc.append(BoxDoc::text(": ")).append(value.to_doc()),
            None => name_doc,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
    /// A variable expression, e.g. foo
    Var {
        value: VarName,
        kind: Arc<Type>,
    },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        kind: Arc<Type>,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral {
        value: CheapString,
    },

    /// A boolean literal expression, e.g. true
    BooleanLiteral {
        value: bool,
    },

    /// A float literal expression, e.g. 2.5
    FloatLiteral {
        value: f64,
    },

    /// An integer literal expression, e.g. 42
    IntLiteral {
        value: i32,
    },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral {
        elements: Vec<Self>,
        kind: Arc<Type>,
    },

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: TypeName,
        fields: Vec<(FieldName, Self)>,
        kind: Arc<Type>,
    },

    /// An enum literal expression, e.g. Color::Red or Result::Ok(value: 42)
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, Self)>,
        kind: Arc<Type>,
    },

    /// An option literal expression, e.g. Some(42) or None
    OptionLiteral {
        /// The inner value (Some) or None
        value: Option<Box<Self>>,
        kind: Arc<Type>,
    },

    /// A match expression (enum, bool, or option)
    Match {
        match_: Match<Self, Self>,
        kind: Arc<Type>,
    },

    /// String concatenation expression for joining a sequence of string
    /// expressions.
    StringConcat {
        parts: Vec<Self>,
    },

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
    BooleanNegation {
        operand: Box<Self>,
    },

    /// Numeric negation expression
    NumericNegation {
        operand: Box<Self>,
        operand_type: NumericType,
    },

    /// Boolean logical AND expression
    BooleanLogicalAnd {
        left: Box<Self>,
        right: Box<Self>,
    },

    /// Boolean logical OR expression
    BooleanLogicalOr {
        left: Box<Self>,
        right: Box<Self>,
    },

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

    /// A let binding expression
    Let {
        var: VarName,
        value: Box<Self>,
        body: Box<Self>,
        kind: Arc<Type>,
    },

    /// FoldMap over a monoid
    For {
        var_name: Option<VarName>,
        source: Box<TypedLoopSource>,
        body: Box<Self>,
        kind: Arc<Type>,
    },

    /// Array length expression, e.g. items.len()
    ArrayLength {
        array: Box<Self>,
    },

    /// Array is empty expression, e.g. items.is_empty()
    ArrayIsEmpty {
        array: Box<Self>,
    },

    /// String is empty expression, e.g. name.is_empty()
    StringIsEmpty {
        string: Box<Self>,
    },

    /// Option is_some expression, e.g. maybe_value.is_some()
    OptionIsSome {
        option: Box<Self>,
    },

    /// Option is_none expression, e.g. maybe_value.is_none()
    OptionIsNone {
        option: Box<Self>,
    },

    /// Int to string conversion, e.g. count.to_string()
    IntToString {
        value: Box<Self>,
    },

    /// Float to int conversion, e.g. price.to_int()
    FloatToInt {
        value: Box<Self>,
    },

    /// Int to float conversion, e.g. count.to_float()
    IntToFloat {
        value: Box<Self>,
    },

    /// Concatenation of fragments
    FragmentConcat {
        nodes: Vec<Self>,
    },

    /// Literal markup text, e.g. `Hello`.
    /// Trusted and emitted without escaping.
    FragmentRaw {
        value: CheapString,
    },

    /// An interpolation in markup, e.g. `{name}`.
    /// HTML-escapes a String-typed expression into a Fragment.
    FragmentEscape {
        expr: Box<Self>,
    },

    /// An HTML element, e.g. `<div class="x">...</div>`
    FragmentHtml {
        element: HtmlElement,
        attrs: Box<Self>,
        children: Box<Self>,
    },

    AttrsConcat {
        parts: Vec<Self>,
    },

    AttrsLiteral {
        attributes: Vec<TypedAttribute>,
    },

    /// An asset path, e.g. asset!("/logo.svg").
    /// Resolved to a concrete string literal at IR compile time based on build mode.
    Asset {
        path: CheapString,
    },

    /// A function call expression, e.g. foo(1, 2)
    FunctionCall {
        function_name: FunctionName,
        args: Vec<(VarName, Self)>,
        kind: Arc<Type>,
    },
}

fn concat_to_doc(nodes: &[TypedExpr]) -> BoxDoc<'_> {
    if nodes.is_empty() {
        BoxDoc::text("concat()")
    } else {
        BoxDoc::text("concat(")
            .append(
                BoxDoc::line_()
                    .append(BoxDoc::intersperse(
                        nodes.iter().map(|node| node.to_doc()),
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

fn bracketed_to_doc(items: Vec<BoxDoc<'_>>) -> BoxDoc<'_> {
    if items.is_empty() {
        BoxDoc::text("[]")
    } else {
        BoxDoc::text("[")
            .append(
                BoxDoc::line_()
                    .append(BoxDoc::intersperse(
                        items,
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

impl TypedExpr {
    pub fn get_type(&self) -> Arc<Type> {
        match self {
            TypedExpr::Var { kind, .. }
            | TypedExpr::FieldAccess { kind, .. }
            | TypedExpr::ArrayLiteral { kind, .. }
            | TypedExpr::RecordLiteral { kind, .. }
            | TypedExpr::EnumLiteral { kind, .. }
            | TypedExpr::OptionLiteral { kind, .. }
            | TypedExpr::Match { kind, .. }
            | TypedExpr::Let { kind, .. }
            | TypedExpr::For { kind, .. }
            | TypedExpr::FunctionCall { kind, .. } => kind.clone(),

            TypedExpr::FloatLiteral { .. } | TypedExpr::IntToFloat { .. } => Arc::new(Type::Float),
            TypedExpr::IntLiteral { .. } => Arc::new(Type::Int),

            TypedExpr::StringConcat { .. }
            | TypedExpr::StringLiteral { .. }
            | TypedExpr::IntToString { .. }
            | TypedExpr::Asset { .. } => Arc::new(Type::String),

            TypedExpr::NumericAdd { operand_types, .. }
            | TypedExpr::NumericSubtract { operand_types, .. }
            | TypedExpr::NumericMultiply { operand_types, .. }
            | TypedExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Arc::new(Type::Int),
                NumericType::Float => Arc::new(Type::Float),
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
            | TypedExpr::BooleanLogicalOr { .. }
            | TypedExpr::ArrayIsEmpty { .. }
            | TypedExpr::StringIsEmpty { .. }
            | TypedExpr::OptionIsSome { .. }
            | TypedExpr::OptionIsNone { .. } => Arc::new(Type::Bool),

            TypedExpr::ArrayLength { .. } | TypedExpr::FloatToInt { .. } => Arc::new(Type::Int),

            TypedExpr::FragmentConcat { .. }
            | TypedExpr::FragmentRaw { .. }
            | TypedExpr::FragmentEscape { .. }
            | TypedExpr::FragmentHtml { .. } => Arc::new(Type::Fragment),

            TypedExpr::AttrsConcat { .. } | TypedExpr::AttrsLiteral { .. } => Arc::new(Type::Attrs),
        }
    }

    pub fn as_type(&self) -> &Type {
        static STRING_TYPE: Type = Type::String;
        static BOOL_TYPE: Type = Type::Bool;
        static FLOAT_TYPE: Type = Type::Float;
        static INT_TYPE: Type = Type::Int;
        static FRAGMENT_TYPE: Type = Type::Fragment;
        static ATTRS_TYPE: Type = Type::Attrs;

        match self {
            TypedExpr::Var { kind, .. }
            | TypedExpr::FieldAccess { kind, .. }
            | TypedExpr::ArrayLiteral { kind, .. }
            | TypedExpr::RecordLiteral { kind, .. }
            | TypedExpr::EnumLiteral { kind, .. }
            | TypedExpr::OptionLiteral { kind, .. }
            | TypedExpr::Match { kind, .. }
            | TypedExpr::Let { kind, .. }
            | TypedExpr::For { kind, .. }
            | TypedExpr::FunctionCall { kind, .. } => kind.as_ref(),

            TypedExpr::FloatLiteral { .. } | TypedExpr::IntToFloat { .. } => &FLOAT_TYPE,
            TypedExpr::IntLiteral { .. } => &INT_TYPE,

            TypedExpr::StringConcat { .. }
            | TypedExpr::StringLiteral { .. }
            | TypedExpr::IntToString { .. }
            | TypedExpr::Asset { .. } => &STRING_TYPE,

            TypedExpr::NumericAdd { operand_types, .. }
            | TypedExpr::NumericSubtract { operand_types, .. }
            | TypedExpr::NumericMultiply { operand_types, .. }
            | TypedExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
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
            | TypedExpr::BooleanLogicalOr { .. }
            | TypedExpr::ArrayIsEmpty { .. }
            | TypedExpr::StringIsEmpty { .. }
            | TypedExpr::OptionIsSome { .. }
            | TypedExpr::OptionIsNone { .. } => &BOOL_TYPE,

            TypedExpr::ArrayLength { .. } | TypedExpr::FloatToInt { .. } => &INT_TYPE,

            TypedExpr::FragmentConcat { .. }
            | TypedExpr::FragmentRaw { .. }
            | TypedExpr::FragmentEscape { .. }
            | TypedExpr::FragmentHtml { .. } => &FRAGMENT_TYPE,

            TypedExpr::AttrsConcat { .. } | TypedExpr::AttrsLiteral { .. } => &ATTRS_TYPE,
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
            TypedExpr::ArrayLiteral { elements, .. } => BoxDoc::text("[")
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
                .append(BoxDoc::text("]")),
            TypedExpr::RecordLiteral {
                record_name,
                fields,
                ..
            } => BoxDoc::text(record_name.as_str())
                .append(BoxDoc::text(" {"))
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
                .append(BoxDoc::text("}")),
            TypedExpr::StringConcat { parts } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::intersperse(
                    parts.iter().map(|part| part.to_doc()),
                    BoxDoc::text(" + "),
                ))
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
            TypedExpr::NumericNegation { operand, .. } => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::text("-"))
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
                fields,
                ..
            } => {
                let base = BoxDoc::text(enum_name.as_str())
                    .append(BoxDoc::text("::"))
                    .append(BoxDoc::text(variant_name.as_str()));
                if fields.is_empty() {
                    base
                } else {
                    base.append(BoxDoc::text(" {"))
                        .append(BoxDoc::intersperse(
                            fields.iter().map(|(name, expr)| {
                                BoxDoc::text(name.as_str())
                                    .append(BoxDoc::text(": "))
                                    .append(expr.to_doc())
                            }),
                            BoxDoc::text(", "),
                        ))
                        .append(BoxDoc::text("}"))
                }
            }
            TypedExpr::OptionLiteral { value, .. } => match value {
                Some(inner) => BoxDoc::text("Some(")
                    .append(inner.to_doc())
                    .append(BoxDoc::text(")")),
                None => BoxDoc::text("None"),
            },
            TypedExpr::Match { match_, .. } => match match_ {
                Match::Enum { subject, arms } => BoxDoc::text("match ")
                    .append(subject.to_doc())
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::line_()
                            .append(BoxDoc::intersperse(
                                arms.iter().map(|arm| {
                                    let pattern_doc = match &arm.pattern {
                                        EnumPattern::Variant {
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
                    .append(BoxDoc::text("}")),
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => {
                    let true_arm_doc = BoxDoc::text("true")
                        .append(BoxDoc::text(" => "))
                        .append(true_body.to_doc());
                    let false_arm_doc = BoxDoc::text("false")
                        .append(BoxDoc::text(" => "))
                        .append(false_body.to_doc());

                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    [true_arm_doc, false_arm_doc],
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => {
                    let some_pattern_doc = match some_arm_binding {
                        Some(name) => BoxDoc::text("Some(")
                            .append(BoxDoc::text(name.as_str()))
                            .append(BoxDoc::text(")")),
                        None => BoxDoc::text("Some(_)"),
                    };
                    let some_arm_doc = some_pattern_doc
                        .append(BoxDoc::text(" => "))
                        .append(some_arm_body.to_doc());
                    let none_arm_doc = BoxDoc::text("None")
                        .append(BoxDoc::text(" => "))
                        .append(none_arm_body.to_doc());

                    BoxDoc::text("match ")
                        .append(subject.to_doc())
                        .append(BoxDoc::text(" {"))
                        .append(
                            BoxDoc::line_()
                                .append(BoxDoc::intersperse(
                                    [some_arm_doc, none_arm_doc],
                                    BoxDoc::text(",").append(BoxDoc::line()),
                                ))
                                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                                .append(BoxDoc::line_())
                                .nest(2)
                                .group(),
                        )
                        .append(BoxDoc::text("}"))
                }
            },
            TypedExpr::Let {
                var, value, body, ..
            } => BoxDoc::text("let ")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text(" in "))
                .append(body.to_doc()),
            TypedExpr::ArrayLength { array } => array.to_doc().append(BoxDoc::text(".len()")),
            TypedExpr::ArrayIsEmpty { array } => array.to_doc().append(BoxDoc::text(".is_empty()")),
            TypedExpr::StringIsEmpty { string } => {
                string.to_doc().append(BoxDoc::text(".is_empty()"))
            }
            TypedExpr::OptionIsSome { option } => {
                option.to_doc().append(BoxDoc::text(".is_some()"))
            }
            TypedExpr::OptionIsNone { option } => {
                option.to_doc().append(BoxDoc::text(".is_none()"))
            }
            TypedExpr::IntToString { value } => value.to_doc().append(BoxDoc::text(".to_string()")),
            TypedExpr::FloatToInt { value } => value.to_doc().append(BoxDoc::text(".to_int()")),
            TypedExpr::IntToFloat { value } => value.to_doc().append(BoxDoc::text(".to_float()")),
            TypedExpr::FragmentConcat { nodes } => concat_to_doc(nodes),
            TypedExpr::AttrsConcat { parts } => concat_to_doc(parts),
            TypedExpr::AttrsLiteral { attributes } => {
                bracketed_to_doc(attributes.iter().map(|attr| attr.to_doc()).collect())
            }
            TypedExpr::FragmentRaw { value } => BoxDoc::text("raw(")
                .append(BoxDoc::text(format!("{:?}", value.as_str())))
                .append(")"),
            TypedExpr::FragmentEscape { expr } => {
                BoxDoc::text("escape(").append(expr.to_doc()).append(")")
            }
            TypedExpr::For {
                var_name,
                source,
                body,
                ..
            } => {
                let source_doc = match &**source {
                    TypedLoopSource::Array(expr) => expr.to_doc(),
                    TypedLoopSource::RangeInclusive { start, end } => start
                        .to_doc()
                        .append(BoxDoc::text("..="))
                        .append(end.to_doc()),
                };
                let var_doc = match var_name {
                    Some(name) => BoxDoc::text(name.as_str()),
                    None => BoxDoc::text("_"),
                };
                BoxDoc::text("for ")
                    .append(var_doc)
                    .append(BoxDoc::text(" in "))
                    .append(source_doc)
                    .append(BoxDoc::text(" {"))
                    .append(BoxDoc::line().append(body.to_doc()).nest(2))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("}"))
            }
            TypedExpr::FragmentHtml {
                element,
                attrs,
                children,
            } => {
                let mut sections = vec![
                    BoxDoc::text(format!("tag: {:?}", element.as_str())),
                    BoxDoc::text("attrs: ").append(attrs.to_doc()),
                ];
                if !element.is_void() {
                    sections.push(BoxDoc::text("children: ").append(children.to_doc()));
                }
                BoxDoc::text("html(")
                    .append(
                        BoxDoc::line_()
                            .append(BoxDoc::intersperse(
                                sections,
                                BoxDoc::text(",").append(BoxDoc::line()),
                            ))
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text(")"))
            }
            TypedExpr::Asset { path } => BoxDoc::text("asset!(\"")
                .append(BoxDoc::text(path.as_str()))
                .append(BoxDoc::text("\")")),
            TypedExpr::FunctionCall {
                function_name,
                args,
                ..
            } => BoxDoc::text(function_name.as_str())
                .append(BoxDoc::text("("))
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            args.iter().map(|(name, e)| {
                                BoxDoc::text(name.as_str())
                                    .append(BoxDoc::text(": "))
                                    .append(e.to_doc())
                            }),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                        .append(BoxDoc::line_())
                        .nest(2)
                        .group(),
                )
                .append(BoxDoc::text(")")),
        }
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
