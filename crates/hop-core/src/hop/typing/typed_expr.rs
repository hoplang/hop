use std::fmt::{self, Display};

use crate::document::CheapString;
use crate::hop::typing::compile_match::Decision;
use crate::hop::typing::typed_match_pattern::TypedMatchPattern;
use crate::html::HtmlElementKind;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::root_relative_file_path::RootRelativeFilePath;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

use super::r#type::{ComparableType, EquatableType, NumericType, Type};

#[derive(Debug, Clone)]
pub enum TypedExpr {
    /// A variable expression, e.g. foo
    Var { value: VarName, typ: Type },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        typ: Type,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: CheapString },

    /// A boolean literal expression, e.g. true
    BooleanLiteral { value: bool },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64 },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i32 },

    /// An array literal expression, e.g. [1, 2, 3]
    ArrayLiteral { elements: Vec<Self>, typ: Type },

    /// A tuple literal expression, e.g. (foo, bar)
    TupleLiteral { elements: Vec<Self>, typ: Type },

    /// A record literal expression, e.g. User(name: "John", age: 30)
    RecordLiteral {
        record_name: TypeName,
        fields: Vec<(FieldName, Self)>,
        typ: Type,
    },

    /// A record literal that reads the fields it does not supply from
    /// `base`, e.g. User { ...user, name: "John" }. The fields are in
    /// declaration order.
    RecordUpdate {
        record_name: TypeName,
        base: Box<Self>,
        fields: Vec<(FieldName, TypedRecordUpdateField)>,
        typ: Type,
    },

    /// An enum literal expression, e.g. Color::Red or Result::Ok(value: 42)
    EnumLiteral {
        enum_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, Self)>,
        typ: Type,
    },

    /// An option literal expression, e.g. Some(42) or None
    OptionLiteral {
        /// The inner value (Some) or None
        value: Option<Box<Self>>,
        typ: Type,
    },

    /// A match expression, with the decision tree its arms compile to.
    Match {
        subject: Box<Self>,
        /// The arms in source order. `Body::value` in the decision indexes
        /// into these.
        arms: Vec<(TypedMatchPattern, Self)>,
        decision: Decision,
        typ: Type,
    },

    /// String concatenation expression for joining a sequence of string
    /// expressions.
    StringConcat { parts: Vec<Self> },

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

    /// Numeric negation expression
    NumericNegation {
        operand: Box<Self>,
        operand_type: NumericType,
    },

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

    /// A let binding expression
    Let {
        var: VarName,
        value: Box<Self>,
        body: Box<Self>,
        typ: Type,
    },

    /// FoldMap over a monoid
    For {
        var_name: Option<VarName>,
        source: Box<TypedLoopSource>,
        body: Box<Self>,
        typ: Type,
    },

    /// Array length expression, e.g. items.len()
    ArrayLength { array: Box<Self> },

    /// Array is empty expression, e.g. items.is_empty()
    ArrayIsEmpty { array: Box<Self> },

    /// String is empty expression, e.g. name.is_empty()
    StringIsEmpty { string: Box<Self> },

    /// Option is_some expression, e.g. maybe_value.is_some()
    OptionIsSome { option: Box<Self> },

    /// Option is_none expression, e.g. maybe_value.is_none()
    OptionIsNone { option: Box<Self> },

    /// Option unwrap_or expression, e.g. maybe_value.unwrap_or("default")
    OptionUnwrapOr {
        option: Box<Self>,
        default: Box<Self>,
        typ: Type,
    },

    /// Int to string conversion, e.g. count.to_string()
    IntToString { value: Box<Self> },

    /// Float to int conversion, e.g. price.to_int()
    FloatToInt { value: Box<Self> },

    /// Int to float conversion, e.g. count.to_float()
    IntToFloat { value: Box<Self> },

    /// Concatenation of Html
    HtmlConcat { nodes: Vec<Self> },

    /// Literal markup text, e.g. `Hello`.
    /// Trusted and emitted without escaping.
    HtmlRaw { value: CheapString },

    /// An interpolation in markup, e.g. `{name}`.
    /// HTML-escapes a String-typed expression into Html.
    HtmlEscape { expr: Box<Self> },

    /// An HTML element, e.g. `<div class="x">...</div>`
    HtmlElement {
        element: HtmlElementKind,
        attrs: TypedAttrs,
        children: Box<Self>,
    },

    /// An asset reference, e.g. asset!("/logo.svg"), resolved to a path
    /// relative to the project root.
    Asset { path: RootRelativeFilePath },

    /// A function call expression, e.g. foo(1, 2)
    FunctionCall {
        function_name: FunctionName,
        /// The module that declares the callee.
        module: RootContainedFilePath,
        args: Vec<(VarName, Self)>,
        /// The callee's rest parameter and the attributes it receives.
        rest: Option<(VarName, TypedAttrs)>,
        typ: Type,
    },
}

#[derive(Debug, Clone)]
pub enum TypedRecordUpdateField {
    /// A field supplied in the literal.
    Explicit(TypedExpr),
    /// A field read from the base record, which has the given type.
    FromBase(Type),
}

#[derive(Debug, Clone)]
pub enum TypedLoopSource {
    Array(TypedExpr),
    RangeInclusive { start: TypedExpr, end: TypedExpr },
}

#[derive(Debug, Clone)]
pub struct TypedAttribute {
    pub name: CheapString,
    pub value: Option<TypedExpr>,
}

/// The attributes an element or a rest parameter receives: those written at
/// the site, followed by those forwarded through a `{...rest}` spread.
#[derive(Debug, Clone)]
pub struct TypedAttrs {
    pub attributes: Vec<TypedAttribute>,
    pub spread: Option<VarName>,
}

impl TypedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(self.name.as_str());
        match &self.value {
            Some(value) => name_doc
                .append(BoxDoc::text(": escape("))
                .append(value.to_doc())
                .append(BoxDoc::text(")")),
            None => name_doc,
        }
    }
}

impl TypedAttrs {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let mut items: Vec<BoxDoc<'_>> = self.attributes.iter().map(|attr| attr.to_doc()).collect();
        if let Some(spread) = &self.spread {
            items.push(BoxDoc::text(format!("...{}", spread.as_str())));
        }
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
}

impl TypedExpr {
    pub fn typ(&self) -> Type {
        match self {
            TypedExpr::Var { typ, .. }
            | TypedExpr::FieldAccess { typ, .. }
            | TypedExpr::ArrayLiteral { typ, .. }
            | TypedExpr::TupleLiteral { typ, .. }
            | TypedExpr::RecordLiteral { typ, .. }
            | TypedExpr::RecordUpdate { typ, .. }
            | TypedExpr::EnumLiteral { typ, .. }
            | TypedExpr::OptionLiteral { typ, .. }
            | TypedExpr::Match { typ, .. }
            | TypedExpr::Let { typ, .. }
            | TypedExpr::For { typ, .. }
            | TypedExpr::OptionUnwrapOr { typ, .. }
            | TypedExpr::FunctionCall { typ, .. } => typ.clone(),

            TypedExpr::FloatLiteral { .. } | TypedExpr::IntToFloat { .. } => Type::Float,
            TypedExpr::IntLiteral { .. } => Type::Int,

            TypedExpr::StringConcat { .. }
            | TypedExpr::StringLiteral { .. }
            | TypedExpr::IntToString { .. }
            | TypedExpr::Asset { .. } => Type::String,

            TypedExpr::NumericAdd { operand_types, .. }
            | TypedExpr::NumericSubtract { operand_types, .. }
            | TypedExpr::NumericMultiply { operand_types, .. }
            | TypedExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Type::Int,
                NumericType::Float => Type::Float,
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
            | TypedExpr::OptionIsNone { .. } => Type::Bool,

            TypedExpr::ArrayLength { .. } | TypedExpr::FloatToInt { .. } => Type::Int,

            TypedExpr::HtmlConcat { .. }
            | TypedExpr::HtmlRaw { .. }
            | TypedExpr::HtmlEscape { .. }
            | TypedExpr::HtmlElement { .. } => Type::Html,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
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
            TypedExpr::TupleLiteral { elements, .. } => BoxDoc::text("(")
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            elements.iter().map(|e| e.to_doc()),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(if elements.len() == 1 {
                            BoxDoc::text(",")
                        } else {
                            BoxDoc::text(",").flat_alt(BoxDoc::nil())
                        })
                        .append(BoxDoc::line_())
                        .nest(2)
                        .group(),
                )
                .append(BoxDoc::text(")")),
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
            TypedExpr::RecordUpdate {
                record_name,
                base,
                fields,
                ..
            } => {
                let entries = std::iter::once(BoxDoc::text("...").append(base.to_doc())).chain(
                    fields.iter().filter_map(|(key, field)| match field {
                        TypedRecordUpdateField::Explicit(value) => Some(
                            BoxDoc::text(key.as_str())
                                .append(BoxDoc::text(": "))
                                .append(value.to_doc()),
                        ),
                        TypedRecordUpdateField::FromBase(_) => None,
                    }),
                );
                BoxDoc::text(record_name.as_str())
                    .append(BoxDoc::text(" {"))
                    .append(
                        BoxDoc::line_()
                            .append(BoxDoc::intersperse(
                                entries,
                                BoxDoc::text(",").append(BoxDoc::line()),
                            ))
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text("}"))
            }
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
            TypedExpr::Match { subject, arms, .. } => BoxDoc::text("match ")
                .append(subject.to_doc())
                .append(BoxDoc::text(" {"))
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            arms.iter().map(|(pattern, body)| {
                                pattern
                                    .to_doc()
                                    .append(BoxDoc::text(" => "))
                                    .append(body.to_doc())
                            }),
                            BoxDoc::text(",").append(BoxDoc::line()),
                        ))
                        .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                        .append(BoxDoc::line_())
                        .nest(2)
                        .group(),
                )
                .append(BoxDoc::text("}")),
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
            TypedExpr::OptionUnwrapOr {
                option, default, ..
            } => option
                .to_doc()
                .append(BoxDoc::text(".unwrap_or("))
                .append(default.to_doc())
                .append(BoxDoc::text(")")),
            TypedExpr::IntToString { value } => value.to_doc().append(BoxDoc::text(".to_string()")),
            TypedExpr::FloatToInt { value } => value.to_doc().append(BoxDoc::text(".to_int()")),
            TypedExpr::IntToFloat { value } => value.to_doc().append(BoxDoc::text(".to_float()")),
            TypedExpr::HtmlConcat { nodes } => concat_to_doc(nodes),
            TypedExpr::HtmlRaw { value } => BoxDoc::text("raw(")
                .append(BoxDoc::text(format!("{:?}", value.as_str())))
                .append(")"),
            TypedExpr::HtmlEscape { expr } => {
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
            TypedExpr::HtmlElement {
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
            TypedExpr::Asset { path } => BoxDoc::text("asset!(\"/")
                .append(BoxDoc::text(path.as_str()))
                .append(BoxDoc::text("\")")),
            TypedExpr::FunctionCall {
                function_name,
                args,
                rest,
                ..
            } => BoxDoc::text(function_name.as_str())
                .append(BoxDoc::text("("))
                .append(
                    BoxDoc::line_()
                        .append(BoxDoc::intersperse(
                            args.iter()
                                .map(|(name, e)| (name, e.to_doc()))
                                .chain(rest.iter().map(|(name, attrs)| (name, attrs.to_doc())))
                                .map(|(name, doc)| {
                                    BoxDoc::text(name.as_str())
                                        .append(BoxDoc::text(": "))
                                        .append(doc)
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
