use core::fmt;

use pretty::BoxDoc;

use crate::dop::field_name::FieldName;
use crate::hop::module_name::ModuleName;

/// An EquatableType is a type where its values can be compared
/// using `==` and `!=`.
#[derive(Debug, Clone, PartialEq)]
pub enum EquatableType {
    String,
    Bool,
    Int,
    Float,
}

/// A ComparableType is a type where its values can be ordered
/// using comparison operations like `<`, `>`, `<=`, `>=`.
#[derive(Debug, Clone, PartialEq)]
pub enum ComparableType {
    Int,
    Float,
}

/// A NumericType is a type where its values can be used
/// in arithmetic operations like `+`, `-`, `*`.
#[derive(Debug, Clone, PartialEq)]
pub enum NumericType {
    Int,
    Float,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    Bool,
    Int,
    Float,
    TrustedHTML,
    Array(Box<Type>),
    Record {
        module: ModuleName,
        name: String,
        fields: Vec<(FieldName, Type)>,
    },
}

impl Type {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            Type::Bool => Some(EquatableType::Bool),
            Type::String => Some(EquatableType::String),
            Type::Int => Some(EquatableType::Int),
            Type::Float => Some(EquatableType::Float),
            Type::TrustedHTML | Type::Array(_) | Type::Record { .. } => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            Type::Int => Some(ComparableType::Int),
            Type::Float => Some(ComparableType::Float),
            Type::Bool
            | Type::String
            | Type::TrustedHTML
            | Type::Array(_)
            | Type::Record { .. } => None,
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
            (Type::TrustedHTML, Type::TrustedHTML) => true,

            // Arrays are covariant in their element type
            (Type::Array(sub_elem), Type::Array(super_elem)) => sub_elem.is_subtype(super_elem),

            // Record types must have the same module and name
            (
                Type::Record {
                    module: sub_module,
                    name: sub_name,
                    ..
                },
                Type::Record {
                    module: super_module,
                    name: super_name,
                    ..
                },
            ) => sub_module == super_module && sub_name == super_name,

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
            Type::String => BoxDoc::text("String"),
            Type::Float => BoxDoc::text("Float"),
            Type::Int => BoxDoc::text("Int"),
            Type::Bool => BoxDoc::text("Bool"),
            Type::TrustedHTML => BoxDoc::text("TrustedHTML"),
            Type::Array(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Array["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            Type::Record { module, name, .. } => BoxDoc::text(format!("{}::{}", module, name)),
        }
    }
}
