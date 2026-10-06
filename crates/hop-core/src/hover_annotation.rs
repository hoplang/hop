use crate::document::DocumentRange;
use crate::hop::typing::Type;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub enum HoverAnnotation {
    TypeName {
        typ: Type,
        type_name: TypeName,
        range: DocumentRange,
    },
    VarName {
        typ: Type,
        var_name: VarName,
        range: DocumentRange,
    },
    ArrayLength {
        range: DocumentRange,
    },
    ArrayIsEmpty {
        range: DocumentRange,
    },
    StringIsEmpty {
        range: DocumentRange,
    },
    IntToString {
        range: DocumentRange,
    },
    IntToFloat {
        range: DocumentRange,
    },
    FloatToInt {
        range: DocumentRange,
    },
    OptionIsSome {
        range: DocumentRange,
    },
    OptionIsNone {
        range: DocumentRange,
    },
    OptionUnwrapOr {
        range: DocumentRange,
    },
    Join {
        range: DocumentRange,
    },
    Format {
        range: DocumentRange,
    },
    Asset {
        range: DocumentRange,
    },
}

impl HoverAnnotation {
    pub fn range(&self) -> &DocumentRange {
        match self {
            HoverAnnotation::TypeName { range, .. } => range,
            HoverAnnotation::VarName { range, .. } => range,
            HoverAnnotation::ArrayLength { range } => range,
            HoverAnnotation::ArrayIsEmpty { range } => range,
            HoverAnnotation::StringIsEmpty { range } => range,
            HoverAnnotation::IntToString { range } => range,
            HoverAnnotation::IntToFloat { range } => range,
            HoverAnnotation::FloatToInt { range } => range,
            HoverAnnotation::OptionIsSome { range } => range,
            HoverAnnotation::OptionIsNone { range } => range,
            HoverAnnotation::OptionUnwrapOr { range } => range,
            HoverAnnotation::Join { range } => range,
            HoverAnnotation::Format { range } => range,
            HoverAnnotation::Asset { range } => range,
        }
    }
}

impl Display for HoverAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Render as:
            // | ```
            // | var_name : type
            // | ```
            HoverAnnotation::VarName { var_name, typ, .. } => {
                write!(f, "```\n{} : {}\n```", var_name, typ)
            }
            // Render as:
            // | ```
            // | type_name : type
            // | ```
            HoverAnnotation::TypeName { type_name, typ, .. } => {
                write!(f, "```\n{} : {}\n```", type_name, typ)
            }
            HoverAnnotation::ArrayLength { .. } => format_builtin(
                f,
                "Array::len() -> Int",
                "Returns the number of elements in the array.",
            ),
            HoverAnnotation::ArrayIsEmpty { .. } => format_builtin(
                f,
                "Array::is_empty() -> Bool",
                "Returns `true` if the array is empty.",
            ),
            HoverAnnotation::StringIsEmpty { .. } => format_builtin(
                f,
                "String::is_empty() -> Bool",
                "Returns `true` if the string is empty.",
            ),
            HoverAnnotation::IntToString { .. } => format_builtin(
                f,
                "Int::to_string() -> String",
                "Returns the decimal representation of the integer.",
            ),
            HoverAnnotation::IntToFloat { .. } => format_builtin(
                f,
                "Int::to_float() -> Float",
                "Returns the same value as a float.",
            ),
            HoverAnnotation::FloatToInt { .. } => format_builtin(
                f,
                "Float::to_int() -> Int",
                "Truncates toward zero and saturates at the `Int` bounds. `NaN` becomes `0`.",
            ),
            HoverAnnotation::OptionIsSome { .. } => format_builtin(
                f,
                "Option::is_some() -> Bool",
                "Returns `true` if the option contains a value.",
            ),
            HoverAnnotation::OptionIsNone { .. } => format_builtin(
                f,
                "Option::is_none() -> Bool",
                "Returns `true` if the option is `None`.",
            ),
            HoverAnnotation::OptionUnwrapOr { .. } => format_builtin(
                f,
                "Option::unwrap_or(default: T) -> T",
                "Returns the contained value, or `default` if the option is `None`.",
            ),
            HoverAnnotation::Join { .. } => format_builtin(
                f,
                "join!(String, ...) -> String",
                "Joins strings with spaces.",
            ),
            HoverAnnotation::Format { .. } => format_builtin(
                f,
                "format!(literal: String, ...) -> String",
                "Replaces each `{}` in the format string with the corresponding argument.",
            ),
            HoverAnnotation::Asset { .. } => format_builtin(
                f,
                "asset!(literal: String) -> String",
                "The path must start with `/`, which denotes the project root. \
                 Resolves to a content-hashed URL prefixed by \
                 `assets.production_prefix` in production builds.",
            ),
        }
    }
}

// Render as:
// | ```
// | signature
// | ```
// |
// | description
fn format_builtin(f: &mut fmt::Formatter<'_>, signature: &str, description: &str) -> fmt::Result {
    write!(f, "```\n{}\n```\n\n{}", signature, description)
}
