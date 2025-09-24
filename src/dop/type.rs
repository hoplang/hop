use core::fmt;
use std::collections::BTreeMap;

use pretty::BoxDoc;

#[derive(Debug, Clone, PartialEq)]
pub enum ComparableType {
    String,
    Bool,
    Int,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    Bool,
    Float,
    Int,
    Object(BTreeMap<String, Type>),
    Array(Option<Box<Type>>),
}

impl Type {
    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            Type::Bool => Some(ComparableType::Bool),
            Type::String => Some(ComparableType::String),
            Type::Int => Some(ComparableType::Int),
            Type::Object(_) | Type::Array(_) | Type::Float => None,
        }
    }
    /// Check if `subtype` is a subtype of `supertype`
    pub fn is_subtype(&self, supertype: &Type) -> bool {
        match (self, supertype) {
            // Exact matches
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Float, Type::Float) => true,
            (Type::Int, Type::Int) => true,

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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl<'a> Type {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        match self {
            Type::String => BoxDoc::text("string"),
            Type::Float => BoxDoc::text("number"),
            Type::Int => BoxDoc::text("int"),
            Type::Bool => BoxDoc::text("boolean"),
            Type::Array(elem_type) => match elem_type {
                Some(elem) => BoxDoc::nil()
                    .append(BoxDoc::text("array["))
                    .append(elem.to_doc())
                    .append(BoxDoc::text("]")),
                None => BoxDoc::text("array"),
            },
            Type::Object(fields) => {
                BoxDoc::nil()
                    .append(BoxDoc::text("{"))
                    .append(
                        BoxDoc::nil()
                            // soft line break
                            .append(BoxDoc::line_())
                            .append(BoxDoc::intersperse(
                                fields.iter().map(|(key, typ)| {
                                    BoxDoc::nil()
                                        // key
                                        .append(BoxDoc::text(key.clone()))
                                        // separator
                                        .append(BoxDoc::text(": "))
                                        // value
                                        .append(typ.to_doc())
                                }),
                                // intersperse with comma followed by line that acts
                                // as space if laid out on a single line
                                BoxDoc::text(",").append(BoxDoc::line()),
                            ))
                            // trailing comma if laid out on multiple lines
                            .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                            // soft line break
                            .append(BoxDoc::line_())
                            .nest(2)
                            .group(),
                    )
                    .append(BoxDoc::text("}"))
            }
        }
    }
}
