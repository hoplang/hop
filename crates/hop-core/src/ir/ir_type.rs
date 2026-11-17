use std::fmt;

use crate::display::join;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrType {
    Data(IrDataType),
    Mutation(IrDataType),
    Html,
    Function(IrSignature),
    /// A cell of the payload. Only a cell binding binds one.
    Cell(IrDataType),
    /// Only a mint binds one.
    Id,
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

// A signature is that of a function or a closure alike.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrSignature {
    pub render_modifier: bool,
    pub parameters: Vec<IrType>,
    pub result: Box<IrType>,
}

#[cfg(test)]
impl IrType {
    pub const MUTATION: IrType = IrType::Mutation(IrDataType::UNIT);

    pub fn array(element: IrDataType) -> IrType {
        IrType::Data(IrDataType::Array(Box::new(element)))
    }

    pub fn option(element: IrDataType) -> IrType {
        IrType::Data(IrDataType::Option(Box::new(element)))
    }
}

impl IrType {
    pub const STRING: IrType = IrType::Data(IrDataType::String);
    pub const BOOL: IrType = IrType::Data(IrDataType::Bool);
    pub const INT: IrType = IrType::Data(IrDataType::Int);
    pub const FLOAT: IrType = IrType::Data(IrDataType::Float);
    pub const HTML: IrType = IrType::Html;

    pub fn function(
        render_modifier: bool,
        parameters: impl IntoIterator<Item = IrType>,
        result: IrType,
    ) -> IrType {
        IrType::Function(IrSignature {
            render_modifier,
            parameters: parameters.into_iter().collect(),
            result: Box::new(result),
        })
    }

    pub fn as_data(&self) -> Option<&IrDataType> {
        match self {
            IrType::Data(data) => Some(data),
            IrType::Mutation(_)
            | IrType::Html
            | IrType::Function(_)
            | IrType::Cell(_)
            | IrType::Id => None,
        }
    }

    pub fn is_html_or_html_closure(&self) -> bool {
        match self {
            IrType::Html => true,
            IrType::Function(signature) => *signature.result == IrType::Html,
            IrType::Data(_) | IrType::Mutation(_) | IrType::Cell(_) | IrType::Id => false,
        }
    }

    pub fn is_value_or_value_closure(&self) -> bool {
        match self {
            IrType::Data(_) | IrType::Mutation(_) => true,
            IrType::Function(signature) => {
                matches!(*signature.result, IrType::Data(_) | IrType::Mutation(_))
                    && (signature.parameters.iter()).all(IrType::is_value_or_value_closure)
            }
            IrType::Html | IrType::Cell(_) | IrType::Id => false,
        }
    }
}

impl IrDataType {
    pub const UNIT: IrDataType = IrDataType::Tuple(Vec::new());
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Data(data) => data.fmt(f),
            IrType::Mutation(data) => write!(f, "Mutation[{data}]"),
            IrType::Html => f.write_str("Html"),
            IrType::Function(signature) => signature.fmt(f),
            IrType::Cell(data) => write!(f, "Cell[{data}]"),
            IrType::Id => f.write_str("Id"),
        }
    }
}

impl fmt::Display for IrSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.render_modifier {
            write!(f, "render ")?;
        }
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
            IrDataType::Tuple(elements) => write!(f, "Tuple[{}]", join(elements, ", ")),
            IrDataType::Record(name) | IrDataType::Enum(name) => f.write_str(name.as_str()),
        }
    }
}
