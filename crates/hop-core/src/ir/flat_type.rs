use crate::hop::typing::Modifier;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatType {
    Data(FlatDataType),
    Html,
    Function(FlatSignature),
    Cell(FlatDataType),
    ElementId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatDataType {
    String,
    Bool,
    Int,
    Float,
    Array(Box<FlatDataType>),
    Option(Box<FlatDataType>),
    Tuple(Vec<FlatDataType>),
    Record(TypeName),
    Enum(TypeName),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrModifier {
    Pure,
    Render,
    Mut,
}

impl IrModifier {
    pub fn allows(self, effect: IrModifier) -> bool {
        effect == IrModifier::Pure || self == effect
    }
}

impl From<Modifier> for IrModifier {
    fn from(modifier: Modifier) -> IrModifier {
        match modifier {
            Modifier::Pure => IrModifier::Pure,
            Modifier::Render => IrModifier::Render,
            Modifier::Mut => IrModifier::Mut,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlatSignature {
    pub modifier: IrModifier,
    pub parameters: Vec<FlatType>,
    pub result: Box<FlatType>,
}

impl FlatType {
    pub const STRING: FlatType = FlatType::Data(FlatDataType::String);
    pub const BOOL: FlatType = FlatType::Data(FlatDataType::Bool);
    pub const INT: FlatType = FlatType::Data(FlatDataType::Int);
    pub const FLOAT: FlatType = FlatType::Data(FlatDataType::Float);
    pub const HTML: FlatType = FlatType::Html;

    pub fn function(
        modifier: IrModifier,
        parameters: impl IntoIterator<Item = FlatType>,
        result: FlatType,
    ) -> FlatType {
        FlatType::Function(FlatSignature {
            modifier,
            parameters: parameters.into_iter().collect(),
            result: Box::new(result),
        })
    }

    pub fn as_data(&self) -> Option<&FlatDataType> {
        match self {
            FlatType::Data(data) => Some(data),
            FlatType::Html | FlatType::Function(_) | FlatType::Cell(_) | FlatType::ElementId => {
                None
            }
        }
    }

    pub fn is_html_or_html_closure(&self) -> bool {
        match self {
            FlatType::Html => true,
            FlatType::Function(signature) => *signature.result == FlatType::Html,
            FlatType::Data(_) | FlatType::Cell(_) | FlatType::ElementId => false,
        }
    }

    pub fn is_value_or_value_closure(&self) -> bool {
        match self {
            FlatType::Data(_) => true,
            FlatType::Function(signature) => {
                matches!(*signature.result, FlatType::Data(_))
                    && (signature.parameters.iter()).all(FlatType::is_value_or_value_closure)
            }
            FlatType::Html | FlatType::Cell(_) | FlatType::ElementId => false,
        }
    }
}

impl FlatDataType {
    pub const UNIT: FlatDataType = FlatDataType::Tuple(Vec::new());
}

#[cfg(test)]
mod test_support {

    use crate::display::join;
    use std::fmt;

    use super::FlatDataType;
    use super::FlatSignature;
    use super::FlatType;
    use super::IrModifier;

    impl FlatType {
        pub fn array(element: FlatDataType) -> FlatType {
            FlatType::Data(FlatDataType::Array(Box::new(element)))
        }

        pub fn option(element: FlatDataType) -> FlatType {
            FlatType::Data(FlatDataType::Option(Box::new(element)))
        }
    }

    impl fmt::Display for FlatType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                FlatType::Data(data) => data.fmt(f),
                FlatType::Html => f.write_str("Html"),
                FlatType::Function(signature) => signature.fmt(f),
                FlatType::Cell(data) => write!(f, "Cell[{data}]"),
                FlatType::ElementId => f.write_str("ElementId"),
            }
        }
    }

    impl fmt::Display for IrModifier {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                IrModifier::Pure => Ok(()),
                IrModifier::Render => f.write_str("render "),
                IrModifier::Mut => f.write_str("mut "),
            }
        }
    }

    impl fmt::Display for FlatSignature {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}(", self.modifier)?;
            for (i, parameter) in self.parameters.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{parameter}")?;
            }
            write!(f, ") -> {}", self.result)
        }
    }

    impl fmt::Display for FlatDataType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                FlatDataType::String => f.write_str("String"),
                FlatDataType::Float => f.write_str("Float"),
                FlatDataType::Int => f.write_str("Int"),
                FlatDataType::Bool => f.write_str("Bool"),
                FlatDataType::Array(element) => write!(f, "Array[{element}]"),
                FlatDataType::Option(element) => write!(f, "Option[{element}]"),
                FlatDataType::Tuple(elements) => write!(f, "Tuple[{}]", join(elements, ", ")),
                FlatDataType::Record(name) | FlatDataType::Enum(name) => f.write_str(name.as_str()),
            }
        }
    }
}
