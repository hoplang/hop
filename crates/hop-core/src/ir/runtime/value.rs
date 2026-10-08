use std::collections::HashMap;

use crate::ir::runtime::html_node::HtmlNode;
use crate::symbols::{field_name::FieldName, type_name::TypeName};

/// Runtime value for the evaluator.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Html(Vec<HtmlNode>),
    Bool(bool),
    Int(i32),
    Float(f64),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Record(HashMap<FieldName, Value>),
    Option(Option<Box<Value>>),
    /// Enum variant with name and optional fields
    Enum {
        variant_name: TypeName,
        fields: HashMap<FieldName, Value>,
    },
}

impl Value {
    /// Panics if the value is not a String.
    pub fn unwrap_string(self) -> String {
        match self {
            Value::String(s) => s,
            _ => panic!("Expected a String value, found {self:?}"),
        }
    }

    /// Panics if the value is not Html.
    pub fn unwrap_html(self) -> Vec<HtmlNode> {
        match self {
            Value::Html(nodes) => nodes,
            _ => panic!("Expected Html value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Bool.
    pub fn unwrap_bool(self) -> bool {
        match self {
            Value::Bool(b) => b,
            _ => panic!("Expected a Bool value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Int.
    pub fn unwrap_int(self) -> i32 {
        match self {
            Value::Int(i) => i,
            _ => panic!("Expected an Int value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Float.
    pub fn unwrap_float(self) -> f64 {
        match self {
            Value::Float(f) => f,
            _ => panic!("Expected a Float value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Array.
    pub fn unwrap_array(self) -> Vec<Value> {
        match self {
            Value::Array(arr) => arr,
            _ => panic!("Expected an Array value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Tuple.
    pub fn unwrap_tuple(self) -> Vec<Value> {
        match self {
            Value::Tuple(elements) => elements,
            _ => panic!("Expected a Tuple value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Record.
    pub fn unwrap_record(self) -> HashMap<FieldName, Value> {
        match self {
            Value::Record(rec) => rec,
            _ => panic!("Expected a Record value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Option.
    pub fn unwrap_option(self) -> Option<Box<Value>> {
        match self {
            Value::Option(opt) => opt,
            _ => panic!("Expected an Option value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Enum.
    pub fn unwrap_enum(self) -> (TypeName, HashMap<FieldName, Value>) {
        match self {
            Value::Enum {
                variant_name,
                fields,
            } => (variant_name, fields),
            _ => panic!("Expected an Enum value, found {self:?}"),
        }
    }
}
