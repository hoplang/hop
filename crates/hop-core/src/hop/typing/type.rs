use core::fmt;

use pretty::BoxDoc;

use crate::document_id::DocumentId;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    String,
    Bool,
    Int,
    Float,
    Html,
    Attrs,
    Array(Box<Type>),
    Option(Box<Type>),
    Tuple(Vec<Type>),
    Named { module: DocumentId, name: TypeName },
}

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

impl Type {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            Type::Bool => Some(EquatableType::Bool),
            Type::String => Some(EquatableType::String),
            Type::Int => Some(EquatableType::Int),
            Type::Float => Some(EquatableType::Float),
            Type::Option(_)
            | Type::Html
            | Type::Attrs
            | Type::Array(_)
            | Type::Tuple(_)
            | Type::Named { .. } => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            Type::Int => Some(ComparableType::Int),
            Type::Float => Some(ComparableType::Float),
            Type::Bool
            | Type::String
            | Type::Html
            | Type::Attrs
            | Type::Array(_)
            | Type::Option(_)
            | Type::Tuple(_)
            | Type::Named { .. } => None,
        }
    }

    /// Whether values of this type can be destructured by a `match` expression.
    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Bool | Type::Option(_) | Type::Named { .. } => true,
            Type::String
            | Type::Int
            | Type::Float
            | Type::Html
            | Type::Attrs
            | Type::Array(_)
            | Type::Tuple(_) => false,
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
            Type::Html => BoxDoc::text("Html"),
            Type::Attrs => BoxDoc::text("Attrs"),
            Type::Array(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Array["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            Type::Option(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Option["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            Type::Tuple(elements) => BoxDoc::nil()
                .append(BoxDoc::text("("))
                .append(BoxDoc::intersperse(
                    elements.iter().map(|element| element.to_doc()),
                    BoxDoc::text(", "),
                ))
                .append(if elements.len() == 1 {
                    BoxDoc::text(",")
                } else {
                    BoxDoc::nil()
                })
                .append(BoxDoc::text(")")),
            Type::Named { name, .. } => BoxDoc::text(name.as_str()),
        }
    }
}
