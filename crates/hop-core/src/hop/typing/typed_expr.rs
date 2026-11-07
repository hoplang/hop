use std::fmt::{self, Display, Write};

use crate::display::{Indent, join, write_lines, write_tuple};
use crate::document::CheapString;
use crate::hop::typing::typed_pattern::TypedPattern;
use crate::html::HtmlElementKind;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::root_relative_file_path::RootRelativeFilePath;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::field_name::FieldName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use indexmap::IndexMap;

use super::r#type::{NumericType, Type};

#[derive(Debug, Clone)]
pub enum TypedExpr {
    /// A variable expression, e.g. foo
    Var { value: VarName, typ: Type },

    /// A parameter that the rest of the enclosing function adds, passed on
    /// to a callee at a `...rest` spread. Unlike a `Var`, it reads the
    /// parameter even where the body binds the same name.
    ForwardedParam { value: VarName, typ: Type },

    /// A field access expression, e.g. foo.bar
    FieldAccess {
        record: Box<Self>,
        field: FieldName,
        typ: Type,
    },

    /// A string literal expression, e.g. "foo bar"
    StringLiteral { value: CheapString },

    /// A boolean literal expression, e.g. true
    BoolLiteral { value: bool },

    /// A float literal expression, e.g. 2.5
    FloatLiteral { value: f64 },

    /// An integer literal expression, e.g. 42
    IntLiteral { value: i32 },

    /// An array expression, e.g. [1, 2, 3]
    Array { elements: Vec<Self>, typ: Type },

    /// A tuple expression, e.g. (foo, bar)
    Tuple { elements: Vec<Self>, typ: Type },

    /// A record expression, e.g. User {name: "John", age: 30}
    Record {
        type_name: TypeName,
        fields: Vec<(FieldName, Self)>,
        typ: Type,
    },

    /// A record expression with a spread, which reads the fields it does not
    /// supply from `base`, e.g. User {...user, name: "John"}. The fields are
    /// in declaration order.
    RecordUpdate {
        type_name: TypeName,
        base: Box<Self>,
        fields: Vec<(FieldName, TypedRecordUpdateField)>,
        typ: Type,
    },

    /// An enum expression, e.g. Status::Active or Status::Away {since: "Monday"}
    Enum {
        type_name: TypeName,
        variant_name: TypeName,
        /// Field values for variants with fields (empty for unit variants)
        fields: Vec<(FieldName, Self)>,
        typ: Type,
    },

    /// An option expression, e.g. Some(42) or None
    Option {
        /// The inner value (Some) or None
        value: Option<Box<Self>>,
        typ: Type,
    },

    /// A match expression, whose patterns cover every value of the
    /// subject's type and are each reached by some value.
    Match {
        subject: Box<Self>,
        /// The arms in source order.
        arms: Vec<(TypedPattern, Self)>,
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

    /// Bool negation expression
    BoolNegation { operand: Box<Self> },

    /// Numeric negation expression
    NumericNegation {
        operand: Box<Self>,
        operand_type: NumericType,
    },

    /// Bool logical AND expression
    BoolLogicalAnd { left: Box<Self>, right: Box<Self> },

    /// Bool logical OR expression
    BoolLogicalOr { left: Box<Self>, right: Box<Self> },

    /// Equals expression
    Equals { left: Box<Self>, right: Box<Self> },

    /// Not equals expression
    NotEquals { left: Box<Self>, right: Box<Self> },

    /// Less than expression
    LessThan { left: Box<Self>, right: Box<Self> },

    /// Greater than expression
    GreaterThan { left: Box<Self>, right: Box<Self> },

    /// Less than or equal expression
    LessThanOrEqual { left: Box<Self>, right: Box<Self> },

    /// Greater than or equal expression
    GreaterThanOrEqual { left: Box<Self>, right: Box<Self> },

    /// A let binding expression.
    Let {
        var: VarName,
        cell: bool,
        value: Box<Self>,
        body: Box<Self>,
        typ: Type,
    },

    /// FoldMap over a monoid
    For {
        var_name: Option<VarName>,
        source: Box<TypedLoopSource>,
        key: Option<Box<Self>>,
        body: Box<Self>,
        typ: Type,
    },

    /// A write of a cell.
    CellWrite { cell: VarName, value: Box<Self> },

    /// The effects in order.
    EffectConcat { parts: Vec<Self> },

    /// Array length expression, e.g. items.len()
    ArrayLength { array: Box<Self> },

    /// Array is empty expression, e.g. items.is_empty()
    ArrayIsEmpty { array: Box<Self> },

    /// Array element lookup, e.g. items.get(0).
    ArrayGet {
        array: Box<Self>,
        index: Box<Self>,
        typ: Type,
    },

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
    HtmlConcat { parts: Vec<Self> },

    /// Literal markup text, e.g. `Hello`.
    /// Contains no `<`, `{` or `}` and is emitted without escaping.
    HtmlText { value: CheapString },

    /// An interpolation in markup, e.g. `{name}`.
    /// HTML-escapes a String-typed expression into Html.
    HtmlEscape { expr: Box<Self> },

    /// An HTML element, e.g. `<div class="x">...</div>`
    Element {
        element: HtmlElementKind,
        attrs: TypedAttrs,
        children: Box<Self>,
        /// The handler of a click on the element, of type `fn() -> Effect`.
        on_click: Option<Box<Self>>,
    },

    /// An asset reference, e.g. asset!("/logo.svg"), resolved to a path
    /// relative to the project root.
    Asset { path: RootRelativeFilePath },

    /// A lambda, e.g. fn(x: Int) { x * 2 }. Its type is a function.
    Lambda {
        params: Vec<(VarName, Type)>,
        body: Box<Self>,
        typ: Type,
    },

    /// An application of a function value bound to a variable, e.g. f(1, 2)
    Apply {
        callee: VarName,
        args: Vec<Self>,
        typ: Type,
    },

    /// A call expression, e.g. foo(1, 2)
    Call {
        function_name: FunctionName,
        /// The module that declares the callee.
        module: RootContainedFilePath,
        args: Vec<(VarName, Self)>,
        /// The callee's rest parameter and the attributes it receives.
        rest: Option<(VarName, Box<TypedAttrs>)>,
        typ: Type,
    },
}

#[derive(Debug, Clone)]
pub enum TypedRecordUpdateField {
    /// A field supplied in the record expression.
    Explicit(TypedExpr),
    /// A field read from the base record, which has the given type.
    FromBase(Type),
}

#[derive(Debug, Clone)]
pub enum TypedLoopSource {
    Array(TypedExpr),
    RangeInclusive { start: TypedExpr, end: TypedExpr },
}

/// The value of an attribute written on an element, or received by a rest
/// parameter.
#[derive(Debug, Clone)]
pub enum TypedAttribute {
    /// An attribute with a value, as in `id={e}`. The value is a `String`,
    /// escaped when rendered.
    Value(TypedExpr),
    /// A boolean attribute, as in `disabled={e}`. The element has the
    /// attribute, without a value, when the condition is true, and does not
    /// have it otherwise. The condition is a `Bool`.
    Presence(TypedExpr),
}

/// The attributes an element or a rest parameter receives: those written at
/// the site, by name in the order they are written, followed by those
/// forwarded through a `...rest` spread.
#[derive(Debug, Clone)]
pub struct TypedAttrs {
    pub attributes: IndexMap<AttributeName, TypedAttribute>,
    pub spread: Option<VarName>,
}

impl TypedExpr {
    pub fn typ(&self) -> Type {
        match self {
            TypedExpr::Var { typ, .. }
            | TypedExpr::ForwardedParam { typ, .. }
            | TypedExpr::FieldAccess { typ, .. }
            | TypedExpr::Array { typ, .. }
            | TypedExpr::Tuple { typ, .. }
            | TypedExpr::Record { typ, .. }
            | TypedExpr::RecordUpdate { typ, .. }
            | TypedExpr::Enum { typ, .. }
            | TypedExpr::Option { typ, .. }
            | TypedExpr::Match { typ, .. }
            | TypedExpr::Let { typ, .. }
            | TypedExpr::For { typ, .. }
            | TypedExpr::OptionUnwrapOr { typ, .. }
            | TypedExpr::ArrayGet { typ, .. }
            | TypedExpr::Lambda { typ, .. }
            | TypedExpr::Apply { typ, .. }
            | TypedExpr::Call { typ, .. } => typ.clone(),

            TypedExpr::FloatLiteral { .. } | TypedExpr::IntToFloat { .. } => Type::FLOAT,
            TypedExpr::IntLiteral { .. } => Type::INT,

            TypedExpr::StringConcat { .. }
            | TypedExpr::StringLiteral { .. }
            | TypedExpr::IntToString { .. }
            | TypedExpr::Asset { .. } => Type::STRING,

            TypedExpr::NumericAdd { operand_types, .. }
            | TypedExpr::NumericSubtract { operand_types, .. }
            | TypedExpr::NumericMultiply { operand_types, .. }
            | TypedExpr::NumericNegation {
                operand_type: operand_types,
                ..
            } => match operand_types {
                NumericType::Int => Type::INT,
                NumericType::Float => Type::FLOAT,
            },

            TypedExpr::BoolLiteral { .. }
            | TypedExpr::BoolNegation { .. }
            | TypedExpr::Equals { .. }
            | TypedExpr::NotEquals { .. }
            | TypedExpr::LessThan { .. }
            | TypedExpr::GreaterThan { .. }
            | TypedExpr::LessThanOrEqual { .. }
            | TypedExpr::GreaterThanOrEqual { .. }
            | TypedExpr::BoolLogicalAnd { .. }
            | TypedExpr::BoolLogicalOr { .. }
            | TypedExpr::ArrayIsEmpty { .. }
            | TypedExpr::StringIsEmpty { .. }
            | TypedExpr::OptionIsSome { .. }
            | TypedExpr::OptionIsNone { .. } => Type::BOOL,

            TypedExpr::ArrayLength { .. } | TypedExpr::FloatToInt { .. } => Type::INT,

            TypedExpr::CellWrite { .. } | TypedExpr::EffectConcat { .. } => Type::Effect,

            TypedExpr::HtmlConcat { .. }
            | TypedExpr::HtmlText { .. }
            | TypedExpr::HtmlEscape { .. }
            | TypedExpr::Element { .. } => Type::HTML,
        }
    }
}

impl Display for TypedAttrs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.attributes.is_empty() && self.spread.is_none() {
            return f.write_str("[]");
        }
        f.write_str("[")?;
        {
            let mut lines = Indent::new(f);
            for (name, attribute) in &self.attributes {
                let name = name.as_str();
                match attribute {
                    TypedAttribute::Value(value) => write!(lines, "\n{name}: escape({value}),")?,
                    TypedAttribute::Presence(present) => write!(lines, "\n{name}: {present},")?,
                }
            }
            if let Some(spread) = &self.spread {
                write!(lines, "\n...{},", spread.as_str())?;
            }
        }
        f.write_str("\n]")
    }
}

impl Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypedExpr::Var { value, .. } | TypedExpr::ForwardedParam { value, .. } => {
                f.write_str(value.as_str())
            }
            TypedExpr::FieldAccess { record, field, .. } => {
                write!(f, "{record}.{}", field.as_str())
            }
            TypedExpr::StringLiteral { value, .. } => write!(f, "{:?}", value.as_str()),
            TypedExpr::BoolLiteral { value, .. } => write!(f, "{value}"),
            TypedExpr::FloatLiteral { value, .. } => write!(f, "{value}"),
            TypedExpr::IntLiteral { value, .. } => write!(f, "{value}"),
            TypedExpr::Array { elements, .. } => write!(f, "[{}]", join(elements, ", ")),
            TypedExpr::Tuple { elements, .. } => write_tuple(f, elements),
            TypedExpr::Record {
                type_name, fields, ..
            } => {
                write!(f, "{} {{", type_name.as_str())?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    let sep = if i > 0 { ", " } else { "" };
                    write!(f, "{sep}{}: {value}", name.as_str())?;
                }
                f.write_str("}")
            }
            TypedExpr::RecordUpdate {
                type_name,
                base,
                fields,
                ..
            } => {
                write!(f, "{} {{...{base}", type_name.as_str())?;
                for (name, field) in fields {
                    if let TypedRecordUpdateField::Explicit(value) = field {
                        write!(f, ", {}: {value}", name.as_str())?;
                    }
                }
                f.write_str("}")
            }
            TypedExpr::StringConcat { parts } => {
                write!(f, "string_concat({})", join(parts, ", "))
            }
            TypedExpr::NumericAdd { left, right, .. } => write!(f, "add({left}, {right})"),
            TypedExpr::NumericSubtract { left, right, .. } => write!(f, "sub({left}, {right})"),
            TypedExpr::NumericMultiply { left, right, .. } => write!(f, "mul({left}, {right})"),
            TypedExpr::BoolNegation { operand, .. } => write!(f, "not({operand})"),
            TypedExpr::NumericNegation { operand, .. } => write!(f, "neg({operand})"),
            TypedExpr::Equals { left, right } => write!(f, "eq({left}, {right})"),
            TypedExpr::NotEquals { left, right } => write!(f, "not_eq({left}, {right})"),
            TypedExpr::LessThan { left, right } => write!(f, "lt({left}, {right})"),
            TypedExpr::GreaterThan { left, right } => write!(f, "gt({left}, {right})"),
            TypedExpr::LessThanOrEqual { left, right } => write!(f, "lt_eq({left}, {right})"),
            TypedExpr::GreaterThanOrEqual { left, right } => write!(f, "gt_eq({left}, {right})"),
            TypedExpr::BoolLogicalAnd { left, right, .. } => write!(f, "and({left}, {right})"),
            TypedExpr::BoolLogicalOr { left, right, .. } => write!(f, "or({left}, {right})"),
            TypedExpr::Enum {
                type_name,
                variant_name,
                fields,
                ..
            } => {
                write!(f, "{}::{}", type_name.as_str(), variant_name.as_str())?;
                if !fields.is_empty() {
                    f.write_str(" {")?;
                    for (i, (name, value)) in fields.iter().enumerate() {
                        let sep = if i > 0 { ", " } else { "" };
                        write!(f, "{sep}{}: {value}", name.as_str())?;
                    }
                    f.write_str("}")?;
                }
                Ok(())
            }
            TypedExpr::Option { value, .. } => match value {
                Some(inner) => write!(f, "Some({inner})"),
                None => f.write_str("None"),
            },
            TypedExpr::Match { subject, arms, .. } => {
                write!(f, "match {subject} {{")?;
                {
                    let mut lines = Indent::new(f);
                    for (pattern, body) in arms {
                        write!(lines, "\n{pattern} => {body},")?;
                    }
                }
                f.write_str(if arms.is_empty() { "}" } else { "\n}" })
            }
            TypedExpr::Let {
                var,
                cell,
                value,
                body,
                ..
            } => {
                let mutable = if *cell { "mut " } else { "" };
                write!(f, "let {mutable}{} = {value} in {body}", var.as_str())
            }
            TypedExpr::CellWrite { cell, value } => write!(f, "write({}, {value})", cell.as_str()),
            TypedExpr::EffectConcat { parts } => write!(f, "effects!({})", join(parts, ", ")),
            TypedExpr::ArrayLength { array } => write!(f, "{array}.len()"),
            TypedExpr::ArrayIsEmpty { array } => write!(f, "{array}.is_empty()"),
            TypedExpr::ArrayGet { array, index, .. } => write!(f, "{array}.get({index})"),
            TypedExpr::StringIsEmpty { string } => write!(f, "{string}.is_empty()"),
            TypedExpr::OptionIsSome { option } => write!(f, "{option}.is_some()"),
            TypedExpr::OptionIsNone { option } => write!(f, "{option}.is_none()"),
            TypedExpr::OptionUnwrapOr {
                option, default, ..
            } => write!(f, "{option}.unwrap_or({default})"),
            TypedExpr::IntToString { value } => write!(f, "{value}.to_string()"),
            TypedExpr::FloatToInt { value } => write!(f, "{value}.to_int()"),
            TypedExpr::IntToFloat { value } => write!(f, "{value}.to_float()"),
            TypedExpr::HtmlConcat { parts } => write_lines(f, "concat(", parts, ")"),
            TypedExpr::HtmlText { value } => write!(f, "text({:?})", value.as_str()),
            TypedExpr::HtmlEscape { expr } => write!(f, "escape({expr})"),
            TypedExpr::For {
                var_name,
                source,
                key,
                body,
                ..
            } => {
                let var = var_name.as_ref().map_or("_", |name| name.as_str());
                match &**source {
                    TypedLoopSource::Array(array) => write!(f, "for {var} in {array}")?,
                    TypedLoopSource::RangeInclusive { start, end } => {
                        write!(f, "for {var} in {start}..={end}")?;
                    }
                }
                if let Some(key) = key {
                    write!(f, " key {key}")?;
                }
                f.write_str(" {")?;
                write!(Indent::new(f), "\n{body}")?;
                f.write_str("\n}")
            }
            TypedExpr::Element {
                element,
                attrs,
                children,
                on_click,
            } => {
                f.write_str("element(")?;
                {
                    let mut args = Indent::new(f);
                    write!(args, "\ntag: {:?},", element.as_str())?;
                    write!(args, "\nattrs: {attrs},")?;
                    if !element.is_void() {
                        write!(args, "\nchildren: {children},")?;
                    }
                    if let Some(on_click) = on_click {
                        write!(args, "\non_click: {on_click},")?;
                    }
                }
                f.write_str("\n)")
            }
            TypedExpr::Asset { path } => write!(f, "asset!(\"/{}\")", path.as_str()),
            TypedExpr::Lambda { params, body, typ } => {
                f.write_str("fn(")?;
                for (i, (name, typ)) in params.iter().enumerate() {
                    let sep = if i > 0 { ", " } else { "" };
                    write!(f, "{sep}{}: {typ}", name.as_str())?;
                }
                f.write_str(")")?;
                if let Type::Function(function) = typ {
                    write!(f, " -> {}", function.result)?;
                }
                f.write_str(" {")?;
                write!(Indent::new(f), "\n{body}")?;
                f.write_str("\n}")
            }
            TypedExpr::Apply { callee, args, .. } => {
                write!(f, "{}({})", callee.as_str(), join(args, ", "))
            }
            TypedExpr::Call {
                function_name,
                args,
                rest,
                ..
            } => {
                write!(f, "{}(", function_name.as_str())?;
                let mut sep = "";
                for (name, value) in args {
                    write!(f, "{sep}{}: {value}", name.as_str())?;
                    sep = ", ";
                }
                if let Some((name, attrs)) = rest {
                    write!(f, "{sep}{}: {attrs}", name.as_str())?;
                }
                f.write_str(")")
            }
        }
    }
}
