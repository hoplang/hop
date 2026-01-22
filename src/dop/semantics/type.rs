use core::fmt;
use std::sync::Arc;

use pretty::BoxDoc;

use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::symbols::var_name::VarName;
use crate::hop::symbols::module_name::ModuleName;

/// An EquatableType is a type where its values can be compared
/// using `==` and `!=`.
#[derive(Debug, Clone, PartialEq)]
pub enum EquatableType {
    String,
    Bool,
    Int,
    Float,
    Enum { module: ModuleName, name: TypeName },
    Option(Box<EquatableType>),
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

#[derive(Debug)]
pub enum Type {
    String,
    Bool,
    Int,
    Float,
    TrustedHTML,
    Array(Arc<Type>),
    Option(Arc<Type>),
    Record {
        module: ModuleName,
        name: TypeName,
        fields: Vec<(FieldName, Arc<Type>)>,
    },
    Enum {
        module: ModuleName,
        name: TypeName,
        /// Variants with their optional fields: (variant_name, fields)
        /// Empty fields Vec for unit variants
        variants: Vec<(TypeName, Vec<(FieldName, Arc<Type>)>)>,
    },
    Component {
        module: ModuleName,
        name: TypeName,
        /// Parameters: (name, type, has_default)
        parameters: Vec<(VarName, Arc<Type>, bool)>,
    },
}

impl Type {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            Type::Bool => Some(EquatableType::Bool),
            Type::String => Some(EquatableType::String),
            Type::Int => Some(EquatableType::Int),
            Type::Float => Some(EquatableType::Float),
            Type::Enum { module, name, .. } => Some(EquatableType::Enum {
                module: module.clone(),
                name: name.clone(),
            }),
            Type::Option(inner) => {
                let inner_equatable = inner.as_equatable_type()?;
                Some(EquatableType::Option(Box::new(inner_equatable)))
            }
            Type::TrustedHTML | Type::Array(_) | Type::Record { .. } | Type::Component { .. } => {
                None
            }
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
            | Type::Option(_)
            | Type::Record { .. }
            | Type::Enum { .. }
            | Type::Component { .. } => None,
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::String, Type::String) => true,
            (Type::Float, Type::Float) => true,
            (Type::Int, Type::Int) => true,
            (Type::TrustedHTML, Type::TrustedHTML) => true,
            (Type::Array(left), Type::Array(right)) => left == right,
            (Type::Option(left), Type::Option(right)) => left == right,
            (
                Type::Record {
                    module: left_module,
                    name: left_name,
                    ..
                },
                Type::Record {
                    module: right_module,
                    name: right_name,
                    ..
                },
            ) => left_module == right_module && left_name == right_name,
            (
                Type::Enum {
                    module: left_module,
                    name: left_name,
                    ..
                },
                Type::Enum {
                    module: right_module,
                    name: right_name,
                    ..
                },
            ) => left_module == right_module && left_name == right_name,
            (
                Type::Component {
                    module: left_module,
                    name: left_name,
                    ..
                },
                Type::Component {
                    module: right_module,
                    name: right_name,
                    ..
                },
            ) => left_module == right_module && left_name == right_name,
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
            Type::Option(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Option["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            Type::Record { module, name, .. } => BoxDoc::text(format!("{}::{}", module, name)),
            Type::Enum { module, name, .. } => BoxDoc::text(format!("{}::{}", module, name)),
            Type::Component { module, name, .. } => BoxDoc::text(format!("{}::{}", module, name)),
        }
    }
}
