use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WriterType {
    Data(WriterDataType),
    Function(WriterSignature),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WriterDataType {
    String,
    Bool,
    Int,
    Float,
    Array(Box<WriterDataType>),
    Option(Box<WriterDataType>),
    Tuple(Vec<WriterDataType>),
    Record(TypeName),
    Enum(TypeName),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WriterSignature {
    pub parameters: Vec<WriterType>,
    pub result: WriterResult,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WriterResult {
    Writes,
    Returns(WriterDataType),
}

#[derive(Debug, Clone, Default)]
pub struct WriterTypeDeclarations {
    pub records: Vec<WriterRecord>,
    pub enums: Vec<WriterEnum>,
}

#[derive(Debug, Clone)]
pub struct WriterRecord {
    pub name: TypeName,
    pub fields: Vec<WriterField>,
}

#[derive(Debug, Clone)]
pub struct WriterEnum {
    pub name: TypeName,
    pub variants: Vec<WriterVariant>,
}

#[derive(Debug, Clone)]
pub struct WriterVariant {
    pub name: TypeName,
    pub fields: Vec<WriterField>,
}

#[derive(Debug, Clone)]
pub struct WriterField {
    pub name: FieldName,
    pub typ: WriterDataType,
}

impl WriterDataType {
    pub const UNIT: WriterDataType = WriterDataType::Tuple(Vec::new());
}

impl WriterTypeDeclarations {
    pub fn record(&self, name: &TypeName) -> Option<&WriterRecord> {
        self.records.iter().find(|record| record.name == *name)
    }

    pub fn enumeration(&self, name: &TypeName) -> Option<&WriterEnum> {
        self.enums
            .iter()
            .find(|enumeration| enumeration.name == *name)
    }
}

#[cfg(test)]
mod test_support {
    use super::WriterDataType;
    use super::WriterResult;
    use super::WriterSignature;
    use super::WriterType;
    use crate::display::join;
    use std::fmt;

    impl fmt::Display for WriterType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterType::Data(data) => data.fmt(f),
                WriterType::Function(signature) => signature.fmt(f),
            }
        }
    }

    impl fmt::Display for WriterSignature {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "({}) -> {}", join(&self.parameters, ", "), self.result)
        }
    }

    impl fmt::Display for WriterResult {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterResult::Writes => f.write_str("Html"),
                WriterResult::Returns(data) => data.fmt(f),
            }
        }
    }

    impl fmt::Display for WriterDataType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterDataType::String => f.write_str("String"),
                WriterDataType::Float => f.write_str("Float"),
                WriterDataType::Int => f.write_str("Int"),
                WriterDataType::Bool => f.write_str("Bool"),
                WriterDataType::Array(element) => write!(f, "Array[{element}]"),
                WriterDataType::Option(element) => write!(f, "Option[{element}]"),
                WriterDataType::Tuple(elements) => write!(f, "Tuple[{}]", join(elements, ", ")),
                WriterDataType::Record(name) | WriterDataType::Enum(name) => {
                    f.write_str(name.as_str())
                }
            }
        }
    }
}
