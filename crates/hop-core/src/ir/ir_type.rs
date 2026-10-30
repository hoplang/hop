use std::fmt;

use pretty::BoxDoc;

use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    Data(IrDataType),
    Html(IrHtmlType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrHtmlType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrDataType {
    String,
    Bool,
    Int,
    Float,
    Array(Box<IrDataType>),
    Option(Box<IrDataType>),
    Tuple(Vec<IrDataType>),
    Record(TypeName),
    Enum(TypeName),
}

#[cfg(test)]
impl IrType {
    pub const STRING: IrType = IrType::Data(IrDataType::String);
    pub const BOOL: IrType = IrType::Data(IrDataType::Bool);
    pub const INT: IrType = IrType::Data(IrDataType::Int);
    pub const FLOAT: IrType = IrType::Data(IrDataType::Float);

    pub fn array(element: IrDataType) -> IrType {
        IrType::Data(IrDataType::Array(Box::new(element)))
    }

    pub fn option(element: IrDataType) -> IrType {
        IrType::Data(IrDataType::Option(Box::new(element)))
    }
}

impl IrType {
    pub const HTML: IrType = IrType::Html(IrHtmlType);

    pub fn as_data(&self) -> Option<&IrDataType> {
        match self {
            IrType::Data(data) => Some(data),
            IrType::Html(_) => None,
        }
    }

    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        self.as_data()?.as_equatable_type()
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        self.as_data()?.as_comparable_type()
    }

    pub fn as_numeric_type(&self) -> Option<NumericType> {
        self.as_data()?.as_numeric_type()
    }
}

impl IrDataType {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            IrDataType::Bool => Some(EquatableType::Bool),
            IrDataType::String => Some(EquatableType::String),
            IrDataType::Int => Some(EquatableType::Int),
            IrDataType::Float => Some(EquatableType::Float),
            IrDataType::Option(_)
            | IrDataType::Array(_)
            | IrDataType::Tuple(_)
            | IrDataType::Record(_)
            | IrDataType::Enum(_) => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            IrDataType::Int => Some(ComparableType::Int),
            IrDataType::Float => Some(ComparableType::Float),
            IrDataType::Bool
            | IrDataType::String
            | IrDataType::Array(_)
            | IrDataType::Option(_)
            | IrDataType::Tuple(_)
            | IrDataType::Record(_)
            | IrDataType::Enum(_) => None,
        }
    }

    pub fn as_numeric_type(&self) -> Option<NumericType> {
        match self {
            IrDataType::Int => Some(NumericType::Int),
            IrDataType::Float => Some(NumericType::Float),
            IrDataType::Bool
            | IrDataType::String
            | IrDataType::Array(_)
            | IrDataType::Option(_)
            | IrDataType::Tuple(_)
            | IrDataType::Record(_)
            | IrDataType::Enum(_) => None,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            IrDataType::String => BoxDoc::text("String"),
            IrDataType::Float => BoxDoc::text("Float"),
            IrDataType::Int => BoxDoc::text("Int"),
            IrDataType::Bool => BoxDoc::text("Bool"),
            IrDataType::Array(element) => BoxDoc::text("Array[")
                .append(element.to_doc())
                .append(BoxDoc::text("]")),
            IrDataType::Option(element) => BoxDoc::text("Option[")
                .append(element.to_doc())
                .append(BoxDoc::text("]")),
            IrDataType::Tuple(elements) => BoxDoc::text("(")
                .append(BoxDoc::intersperse(
                    elements.iter().map(IrDataType::to_doc),
                    BoxDoc::text(", "),
                ))
                .append(if elements.len() == 1 {
                    BoxDoc::text(",")
                } else {
                    BoxDoc::nil()
                })
                .append(BoxDoc::text(")")),
            IrDataType::Record(name) | IrDataType::Enum(name) => BoxDoc::text(name.as_str()),
        }
    }
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Data(data) => data.fmt(f),
            IrType::Html(html) => html.fmt(f),
        }
    }
}

impl fmt::Display for IrHtmlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Html")
    }
}

impl fmt::Display for IrDataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
