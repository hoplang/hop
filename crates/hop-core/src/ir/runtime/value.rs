use std::collections::HashMap;

use crate::symbols::{field_name::FieldName, type_name::TypeName};

/// Runtime value for the evaluator.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Bool(bool),
    Int(i32),
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

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            Value::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
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
}
