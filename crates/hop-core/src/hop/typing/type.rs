use core::fmt;

use crate::display::join;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Data(DataType),
    Html(HtmlType),
    Effect(DataType),
    Function(FunctionType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HtmlType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub result: Box<Type>,
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EquatableType {
    String,
    Bool,
    Int,
    Float,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparableType {
    Int,
    Float,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    pub const UNIT_EFFECT: Type = Type::Effect(DataType::Tuple(Vec::new()));

    pub fn array(element: DataType) -> Type {
        Type::Data(DataType::Array(Box::new(element)))
    }

    pub fn option(element: DataType) -> Type {
        Type::Data(DataType::Option(Box::new(element)))
    }

    pub fn as_data(&self) -> Option<&DataType> {
        match self {
            Type::Data(data) => Some(data),
            Type::Html(_) | Type::Effect(_) | Type::Function(_) => None,
        }
    }

    pub fn renders_html(&self) -> bool {
        match self {
            Type::Data(_) | Type::Effect(_) => false,
            Type::Html(_) => true,
            Type::Function(function) => function.result.renders_html(),
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

    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Data(
                DataType::Bool | DataType::Option(_) | DataType::Tuple(_) | DataType::Named { .. },
            ) => true,
            Type::Data(DataType::String | DataType::Int | DataType::Float | DataType::Array(_))
            | Type::Html(_)
            | Type::Effect(_)
            | Type::Function(_) => false,
        }
    }
}

impl DataType {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            DataType::Bool => Some(EquatableType::Bool),
            DataType::String => Some(EquatableType::String),
            DataType::Int => Some(EquatableType::Int),
            DataType::Float => Some(EquatableType::Float),
            DataType::Option(_)
            | DataType::Array(_)
            | DataType::Tuple(_)
            | DataType::Named { .. } => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            DataType::Int => Some(ComparableType::Int),
            DataType::Float => Some(ComparableType::Float),
            DataType::Bool
            | DataType::String
            | DataType::Array(_)
            | DataType::Option(_)
            | DataType::Tuple(_)
            | DataType::Named { .. } => None,
        }
    }

    pub fn as_numeric_type(&self) -> Option<NumericType> {
        match self {
            DataType::Int => Some(NumericType::Int),
            DataType::Float => Some(NumericType::Float),
            DataType::Bool
            | DataType::String
            | DataType::Array(_)
            | DataType::Option(_)
            | DataType::Tuple(_)
            | DataType::Named { .. } => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Data(data) => write!(f, "{data}"),
            Type::Html(html) => write!(f, "{html}"),
            Type::Effect(data) => write!(f, "Effect[{data}]"),
            Type::Function(function) => write!(
                f,
                "fn({}) -> {}",
                join(&function.parameters, ", "),
                function.result
            ),
        }
    }
}

impl fmt::Display for HtmlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Html")
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DataType::String => f.write_str("String"),
            DataType::Float => f.write_str("Float"),
            DataType::Int => f.write_str("Int"),
            DataType::Bool => f.write_str("Bool"),
            DataType::Array(element) => write!(f, "Array[{element}]"),
            DataType::Option(element) => write!(f, "Option[{element}]"),
            DataType::Tuple(elements) => write!(f, "Tuple[{}]", join(elements, ", ")),
            DataType::Named { name, .. } => f.write_str(name.as_str()),
        }
    }
}
