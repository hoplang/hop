use std::collections::HashMap;

use crate::symbols::{field_name::FieldName, type_name::TypeName};

/// Runtime value for the evaluator.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Bool(bool),
    Int(i64),
    Float(f64),
    Array(Vec<Value>),
    Record(HashMap<FieldName, Value>),
    /// Option::Some with inner value
    Some(Box<Value>),
    /// Option::None
    None,
    /// Enum variant with name and optional fields
    Enum {
        variant_name: TypeName,
        fields: HashMap<FieldName, Value>,
    },
}

impl Value {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            // Allow Int to be read as f64 for convenience
            Value::Int(i) => Some(*i as f64),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(arr) => Some(arr),
            _ => None,
        }
    }

    pub fn as_record(&self) -> Option<&HashMap<FieldName, Value>> {
        match self {
            Value::Record(rec) => Some(rec),
            _ => None,
        }
    }

    /// Convert Value to a display string for output
    pub fn to_output_string(&self) -> String {
        match self {
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Array(_) => "[array]".to_string(),
            Value::Record(_) => "[record]".to_string(),
            Value::Some(inner) => inner.to_output_string(),
            Value::None => "".to_string(),
            Value::Enum { variant_name, .. } => variant_name.to_string(),
        }
    }
}
