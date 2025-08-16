use serde_json::{Value, json};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum DopType {
    Object(HashMap<String, DopType>, i32),
    Array(Box<DopType>),
    Bool,
    String,
    Void,
    TypeVar(i32),
}

impl fmt::Display for DopType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DopType::Object(properties, _rest) => {
                write!(f, "{{")?;
                let mut first = true;

                // Collect and sort properties by key
                let mut sorted_props: Vec<_> = properties.iter().collect();
                sorted_props.sort_by_key(|(key, _)| *key);

                for (key, value) in sorted_props {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                    first = false;
                }
                write!(f, "}}")
            }
            DopType::Array(inner_type) => write!(f, "{}[]", inner_type),
            DopType::Bool => write!(f, "boolean"),
            DopType::String => write!(f, "string"),
            DopType::Void => write!(f, "void"),
            DopType::TypeVar(id) => write!(f, "?t{}", id),
        }
    }
}

impl DopType {
    /// Convert a Type to a JSON Schema representation
    pub fn to_json_schema(&self) -> Value {
        match self {
            DopType::Object(properties, _rest) => {
                let mut schema_properties = serde_json::Map::new();
                let mut required = Vec::new();

                for (key, value) in properties {
                    schema_properties.insert(key.clone(), value.to_json_schema());
                    required.push(key.clone());
                }

                json!({
                    "type": "object",
                    "properties": schema_properties,
                    "required": required,
                    "additionalProperties": true
                })
            }
            DopType::Array(inner_type) => {
                json!({
                    "type": "array",
                    "items": inner_type.to_json_schema()
                })
            }
            DopType::Bool => {
                json!({
                    "type": "boolean"
                })
            }
            DopType::String => {
                json!({
                    "type": "string"
                })
            }
            DopType::Void => {
                json!({
                    "type": "null"
                })
            }
            DopType::TypeVar(_id) => {
                // Type variables are placeholders during type inference
                // In JSON Schema, we represent them as allowing any type
                json!({})
            }
        }
    }
}