use std::fmt;

use crate::display::write_tuple;
use crate::hop::typing::{ComparableType, EquatableType, NumericType};
use crate::symbols::type_name::TypeName;

// A value computes no Html, and Html renders markup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    Value(IrValueType),
    Html(IrHtmlType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrValueType {
    Data(IrDataType),
    Function(IrValueSignature),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrHtmlType {
    Html,
    Function(IrHtmlSignature),
}

// A signature is that of a function or a closure alike.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrValueSignature {
    pub parameters: Vec<IrValueType>,
    pub result: IrDataType,
}

// The result is Html.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrHtmlSignature {
    pub parameters: Vec<IrType>,
}

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
    pub const STRING: IrType = IrType::Value(IrValueType::Data(IrDataType::String));
    pub const BOOL: IrType = IrType::Value(IrValueType::Data(IrDataType::Bool));
    pub const INT: IrType = IrType::Value(IrValueType::Data(IrDataType::Int));
    pub const FLOAT: IrType = IrType::Value(IrValueType::Data(IrDataType::Float));

    pub fn array(element: IrDataType) -> IrType {
        IrType::Value(IrValueType::Data(IrDataType::Array(Box::new(element))))
    }

    pub fn option(element: IrDataType) -> IrType {
        IrType::Value(IrValueType::Data(IrDataType::Option(Box::new(element))))
    }

    // Panics unless the parameters are values and the result is data.
    pub fn value_function(parameters: impl IntoIterator<Item = IrType>, result: IrType) -> IrType {
        let parameters = parameters
            .into_iter()
            .map(|parameter| match parameter {
                IrType::Value(value) => value,
                IrType::Html(_) => panic!("a closure that computes a value takes {parameter}"),
            })
            .collect();
        let IrType::Value(IrValueType::Data(result)) = result else {
            panic!("a closure that computes a value returns {result}");
        };
        IrType::Value(IrValueType::Function(IrValueSignature {
            parameters,
            result,
        }))
    }
}

impl IrType {
    pub const HTML: IrType = IrType::Html(IrHtmlType::Html);

    pub fn html_function(parameters: impl IntoIterator<Item = IrType>) -> IrType {
        IrType::Html(IrHtmlType::Function(IrHtmlSignature {
            parameters: parameters.into_iter().collect(),
        }))
    }

    pub fn as_data(&self) -> Option<&IrDataType> {
        match self {
            IrType::Value(IrValueType::Data(data)) => Some(data),
            IrType::Value(IrValueType::Function(_)) | IrType::Html(_) => None,
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
            IrType::Value(value) => value.fmt(f),
            IrType::Html(html) => html.fmt(f),
        }
    }
}

impl fmt::Display for IrValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrValueType::Data(data) => data.fmt(f),
            IrValueType::Function(signature) => signature.fmt(f),
        }
    }
}

impl fmt::Display for IrHtmlType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrHtmlType::Html => write!(f, "Html"),
            IrHtmlType::Function(signature) => signature.fmt(f),
        }
    }
}

fn signature_fmt(
    f: &mut fmt::Formatter<'_>,
    parameters: &[impl fmt::Display],
    result: &impl fmt::Display,
) -> fmt::Result {
    write!(f, "(")?;
    for (i, parameter) in parameters.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{parameter}")?;
    }
    write!(f, ") -> {result}")
}

impl fmt::Display for IrValueSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        signature_fmt(f, &self.parameters, &self.result)
    }
}

impl fmt::Display for IrHtmlSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        signature_fmt(f, &self.parameters, &IrHtmlType::Html)
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
