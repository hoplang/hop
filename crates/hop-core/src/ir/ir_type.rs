use std::fmt;

use crate::display::write_tuple;
use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    Data(IrFunctionDataType),
    Html(IrFunctionHtmlType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrFunctionDataType {
    Data(IrDataType),
    Function(IrSignature<IrFunctionDataType, IrDataType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrFunctionHtmlType {
    Html(IrHtmlType),
    Function(IrSignature<IrType, IrHtmlType>),
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

// A signature is that of a function or a closure alike.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrSignature<Param, Result> {
    pub parameters: Vec<Param>,
    pub result: Result,
}

#[cfg(test)]
impl IrType {
    pub const STRING: IrType = IrType::Data(IrFunctionDataType::Data(IrDataType::String));
    pub const BOOL: IrType = IrType::Data(IrFunctionDataType::Data(IrDataType::Bool));
    pub const INT: IrType = IrType::Data(IrFunctionDataType::Data(IrDataType::Int));
    pub const FLOAT: IrType = IrType::Data(IrFunctionDataType::Data(IrDataType::Float));

    pub fn array(element: IrDataType) -> IrType {
        IrType::Data(IrFunctionDataType::Data(IrDataType::Array(Box::new(
            element,
        ))))
    }

    pub fn option(element: IrDataType) -> IrType {
        IrType::Data(IrFunctionDataType::Data(IrDataType::Option(Box::new(
            element,
        ))))
    }

    // Panics unless the parameters are values and the result is data.
    pub fn data_function(parameters: impl IntoIterator<Item = IrType>, result: IrType) -> IrType {
        let parameters = parameters
            .into_iter()
            .map(|parameter| match parameter {
                IrType::Data(data) => data,
                IrType::Html(_) => panic!("a closure that computes a value takes {parameter}"),
            })
            .collect();
        let IrType::Data(IrFunctionDataType::Data(result)) = result else {
            panic!("a closure that computes a value returns {result}");
        };
        IrType::Data(IrFunctionDataType::Function(IrSignature {
            parameters,
            result,
        }))
    }
}

impl IrType {
    pub const HTML: IrType = IrType::Html(IrFunctionHtmlType::Html(IrHtmlType));

    pub fn html_function(parameters: impl IntoIterator<Item = IrType>) -> IrType {
        IrType::Html(IrFunctionHtmlType::Function(IrSignature {
            parameters: parameters.into_iter().collect(),
            result: IrHtmlType,
        }))
    }

    pub fn as_data(&self) -> Option<&IrDataType> {
        match self {
            IrType::Data(IrFunctionDataType::Data(data)) => Some(data),
            IrType::Data(IrFunctionDataType::Function(_)) | IrType::Html(_) => None,
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
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Data(data) => data.fmt(f),
            IrType::Html(html) => html.fmt(f),
        }
    }
}

impl fmt::Display for IrFunctionDataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrFunctionDataType::Data(data) => data.fmt(f),
            IrFunctionDataType::Function(signature) => signature.fmt(f),
        }
    }
}

impl fmt::Display for IrFunctionHtmlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrFunctionHtmlType::Html(html) => html.fmt(f),
            IrFunctionHtmlType::Function(signature) => signature.fmt(f),
        }
    }
}

impl fmt::Display for IrHtmlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Html")
    }
}

impl<Param, Result> fmt::Display for IrSignature<Param, Result>
where
    Param: fmt::Display,
    Result: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{parameter}")?;
        }
        write!(f, ") -> {}", self.result)
    }
}

impl fmt::Display for IrDataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrDataType::String => f.write_str("String"),
            IrDataType::Float => f.write_str("Float"),
            IrDataType::Int => f.write_str("Int"),
            IrDataType::Bool => f.write_str("Bool"),
            IrDataType::Array(element) => write!(f, "Array[{element}]"),
            IrDataType::Option(element) => write!(f, "Option[{element}]"),
            IrDataType::Tuple(elements) => write_tuple(f, elements),
            IrDataType::Record(name) | IrDataType::Enum(name) => f.write_str(name.as_str()),
        }
    }
}
