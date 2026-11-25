use crate::hop::typing::Modifier;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FlatType {
    Data(FlatDataType),
    Html,
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
    Function(FlatSignature),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrModifier {
    Pure,
    Render,
    Mut,
    Test,
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
            Modifier::Test => IrModifier::Test,
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

    pub fn as_data(&self) -> Option<&FlatDataType> {
        match self {
            FlatType::Data(data) => Some(data),
            FlatType::Html => None,
        }
    }
}

impl FlatDataType {
    pub const UNIT: FlatDataType = FlatDataType::Tuple(Vec::new());

    /// What a cell of `value` binds: the value a render gives it, and the
    /// setter that updates it to what a closure gives for the value it has.
    /// `Tuple[T, mut fn(fn(T) -> T) -> Tuple[]]`.
    pub fn cell(value: FlatDataType) -> FlatDataType {
        let update = FlatDataType::Function(FlatSignature {
            modifier: IrModifier::Pure,
            parameters: vec![FlatType::Data(value.clone())],
            result: Box::new(FlatType::Data(value.clone())),
        });
        let setter = FlatDataType::Function(FlatSignature {
            modifier: IrModifier::Mut,
            parameters: vec![FlatType::Data(update)],
            result: Box::new(FlatType::Data(FlatDataType::UNIT)),
        });
        FlatDataType::Tuple(vec![value, setter])
    }
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
        pub fn function(
            modifier: IrModifier,
            parameters: impl IntoIterator<Item = FlatType>,
            result: FlatType,
        ) -> FlatType {
            FlatType::Data(FlatDataType::Function(FlatSignature {
                modifier,
                parameters: parameters.into_iter().collect(),
                result: Box::new(result),
            }))
        }

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
            }
        }
    }

    impl fmt::Display for IrModifier {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                IrModifier::Pure => Ok(()),
                IrModifier::Render => f.write_str("render "),
                IrModifier::Mut => f.write_str("mut "),
                IrModifier::Test => f.write_str("test "),
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
                FlatDataType::Function(signature) => signature.fmt(f),
            }
        }
    }
}
