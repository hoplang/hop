use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WriterType {
    String,
    Bool,
    Int,
    Float,
    Array(Box<WriterType>),
    Option(Box<WriterType>),
    Tuple(Vec<WriterType>),
    Record(TypeName),
    Enum(TypeName),
    Function(WriterSignature),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WriterSignature {
    pub parameters: Vec<WriterType>,
    pub result: WriterResult,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WriterResult {
    Writes,
    Returns(Box<WriterType>),
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
    pub typ: WriterType,
}

impl WriterType {
    pub const UNIT: WriterType = WriterType::Tuple(Vec::new());
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
    use super::WriterResult;
    use super::WriterSignature;
    use super::WriterType;
    use crate::display::join;
    use std::fmt;

    impl fmt::Display for WriterSignature {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "({}) -> {}", join(&self.parameters, ", "), self.result)
        }
    }

    impl fmt::Display for WriterResult {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterResult::Writes => f.write_str("Html"),
                WriterResult::Returns(typ) => typ.fmt(f),
            }
        }
    }

    impl fmt::Display for WriterType {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                WriterType::String => f.write_str("String"),
                WriterType::Float => f.write_str("Float"),
                WriterType::Int => f.write_str("Int"),
                WriterType::Bool => f.write_str("Bool"),
                WriterType::Array(element) => write!(f, "Array[{element}]"),
                WriterType::Option(element) => write!(f, "Option[{element}]"),
                WriterType::Tuple(elements) => write!(f, "Tuple[{}]", join(elements, ", ")),
                WriterType::Record(name) | WriterType::Enum(name) => f.write_str(name.as_str()),
                WriterType::Function(signature) => signature.fmt(f),
            }
        }
    }
}
