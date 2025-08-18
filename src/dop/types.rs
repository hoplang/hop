use std::collections::BTreeMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum DopType {
    Object(BTreeMap<String, DopType>, u32),
    Array(Box<DopType>),
    Bool,
    String,
    Number,
    Void,
    TypeVar(u32),
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopType::Object(properties, _rest) => {
                write!(f, "{{")?;
                for (idx, (key, value)) in properties.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            DopType::Array(inner_type) => write!(f, "{}[]", inner_type),
            DopType::Bool => write!(f, "boolean"),
            DopType::String => write!(f, "string"),
            DopType::Number => write!(f, "number"),
            DopType::Void => write!(f, "void"),
            DopType::TypeVar(_) => write!(f, "any"),
        }
    }
}
