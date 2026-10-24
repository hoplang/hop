use core::fmt;

use pretty::BoxDoc;

use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Data(DataType),
    Html(HtmlType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HtmlType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
    String,
    Bool,
    Int,
    Float,
    Array(Box<DataType>),
    Option(Box<DataType>),
    Tuple(Vec<DataType>),
    Named {
        module: RootContainedFilePath,
        name: TypeName,
    },
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
    pub const STRING: Type = Type::Data(DataType::String);
    pub const BOOL: Type = Type::Data(DataType::Bool);
    pub const INT: Type = Type::Data(DataType::Int);
    pub const FLOAT: Type = Type::Data(DataType::Float);
    pub const HTML: Type = Type::Html(HtmlType);

    pub fn array(element: DataType) -> Type {
        Type::Data(DataType::Array(Box::new(element)))
    }

    pub fn option(element: DataType) -> Type {
        Type::Data(DataType::Option(Box::new(element)))
    }

    /// The data type of this type, or None for Html.
    pub fn as_data(&self) -> Option<&DataType> {
        match self {
            Type::Data(data) => Some(data),
            Type::Html(_) => None,
        }
    }

    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            Type::Data(DataType::Bool) => Some(EquatableType::Bool),
            Type::Data(DataType::String) => Some(EquatableType::String),
            Type::Data(DataType::Int) => Some(EquatableType::Int),
            Type::Data(DataType::Float) => Some(EquatableType::Float),
            Type::Data(
                DataType::Option(_)
                | DataType::Array(_)
                | DataType::Tuple(_)
                | DataType::Named { .. },
            )
            | Type::Html(_) => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            Type::Data(DataType::Int) => Some(ComparableType::Int),
            Type::Data(DataType::Float) => Some(ComparableType::Float),
            Type::Data(
                DataType::Bool
                | DataType::String
                | DataType::Array(_)
                | DataType::Option(_)
                | DataType::Tuple(_)
                | DataType::Named { .. },
            )
            | Type::Html(_) => None,
        }
    }

    pub fn as_numeric_type(&self) -> Option<NumericType> {
        match self {
            Type::Data(DataType::Int) => Some(NumericType::Int),
            Type::Data(DataType::Float) => Some(NumericType::Float),
            Type::Data(
                DataType::Bool
                | DataType::String
                | DataType::Array(_)
                | DataType::Option(_)
                | DataType::Tuple(_)
                | DataType::Named { .. },
            )
            | Type::Html(_) => None,
        }
    }

    /// Whether values of this type can be destructured by a `match` expression.
    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Data(
                DataType::Bool | DataType::Option(_) | DataType::Tuple(_) | DataType::Named { .. },
            ) => true,
            Type::Data(DataType::String | DataType::Int | DataType::Float | DataType::Array(_))
            | Type::Html(_) => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl<'a> Type {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        match self {
            Type::Data(data) => data.to_doc(),
            Type::Html(_) => BoxDoc::text("Html"),
        }
    }
}

impl<'a> DataType {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        match self {
            DataType::String => BoxDoc::text("String"),
            DataType::Float => BoxDoc::text("Float"),
            DataType::Int => BoxDoc::text("Int"),
            DataType::Bool => BoxDoc::text("Bool"),
            DataType::Array(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Array["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            DataType::Option(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Option["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            DataType::Tuple(elements) => BoxDoc::nil()
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
            DataType::Named { name, .. } => BoxDoc::text(name.as_str()),
        }
    }
}
