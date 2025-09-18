use core::fmt;
use std::collections::BTreeMap;

use pretty::RcDoc;

use crate::document::document_cursor::DocumentRange;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Object(BTreeMap<String, Type>),
    Array(Option<Box<Type>>),
    Bool,
    String,
    Number,
}

impl Type {
    /// Check if `subtype` is a subtype of `supertype`
    pub fn is_subtype(&self, supertype: &Type) -> bool {
        match (self, supertype) {
            // Exact matches
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Number, Type::Number) => true,

            // Arrays are covariant in their element type
            (Type::Array(sub_elem), Type::Array(super_elem)) => {
                match (sub_elem, super_elem) {
                    (Some(sub_type), Some(super_type)) => sub_type.is_subtype(super_type),
                    (None, None) => true,
                    (None, Some(_)) => true, // Empty array can be subtype of any array
                    (Some(_), None) => false, // Typed array cannot be subtype of empty array
                }
            }

            // Objects: subtype must have all properties of supertype with compatible types
            (Type::Object(sub_props), Type::Object(super_props)) => {
                super_props.iter().all(|(key, super_type)| {
                    sub_props
                        .get(key)
                        .is_some_and(|sub_type| sub_type.is_subtype(super_type))
                })
            }

            // Otherwise, not a subtype
            _ => false,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RangedType {
    pub dop_type: Type,
    pub range: DocumentRange,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl Type {
    pub fn to_doc(&self) -> RcDoc<'static> {
        match self {
            Type::String => RcDoc::text("string"),
            Type::Number => RcDoc::text("number"),
            Type::Bool => RcDoc::text("boolean"),
            Type::Array(elem_type) => match elem_type {
                Some(elem) => RcDoc::nil()
                    .append(RcDoc::text("array["))
                    .append(elem.to_doc())
                    .append(RcDoc::text("]")),
                None => RcDoc::text("array"),
            },
            Type::Object(fields) => {
                RcDoc::nil()
                    .append(RcDoc::text("{"))
                    .append(
                        RcDoc::nil()
                            // soft line break
                            .append(RcDoc::line_())
                            .append(RcDoc::intersperse(
                                fields.iter().map(|(key, typ)| {
                                    RcDoc::nil()
                                        // key
                                        .append(RcDoc::text(key.clone()))
                                        // separator
                                        .append(RcDoc::text(": "))
                                        // value
                                        .append(typ.to_doc())
                                }),
                                // intersperse with comma followed by line that acts
                                // as space if laid out on a single line
                                RcDoc::text(",").append(RcDoc::line()),
                            ))
                            // trailing comma if laid out on multiple lines
                            .append(RcDoc::text(",").flat_alt(RcDoc::nil()))
                            // soft line break
                            .append(RcDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(RcDoc::text("}"))
            }
        }
    }
}
