use std::rc::Rc;

use crate::hop::typing::{Constructor, Pattern};
use crate::ir::ir_match::IrPattern;
use crate::ir::runtime::bounded_vec::BoundedVec;
use crate::ir::runtime::html_node::Html;
use crate::ir::value_id::ValueId;
use crate::symbols::{field_name::FieldName, type_name::TypeName};

pub const MAX_ARRAY_LENGTH: usize = 10_000_000;

/// The most bytes a String may hold.
pub const MAX_STRING_LENGTH: usize = 10_000_000;

/// The largest size an Html value may have.
pub const MAX_HTML_SIZE: usize = 10_000_000;

/// Runtime value for the evaluator.
#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Html(Html),
    Bool(bool),
    Int(i32),
    Float(f64),
    Array(Rc<BoundedVec<Value, MAX_ARRAY_LENGTH>>),
    Tuple(Rc<[Value]>),
    Record(Rc<[(FieldName, Value)]>),
    Option(Option<Rc<Value>>),
    Enum(TypeName, Rc<[(FieldName, Value)]>),
}

impl Value {
    /// Panics if the value is not a String.
    pub fn unwrap_string(self) -> String {
        match self {
            Value::String(s) => s,
            _ => panic!("Expected a String value, found {self:?}"),
        }
    }

    /// Panics if the value is not a String.
    pub fn as_str(&self) -> &str {
        match self {
            Value::String(s) => s,
            _ => panic!("Expected a String value, found {self:?}"),
        }
    }

    /// Panics if the value is not Html.
    pub fn unwrap_html(self) -> Html {
        match self {
            Value::Html(html) => html,
            _ => panic!("Expected Html value, found {self:?}"),
        }
    }

    /// The markup an Html value renders as. Panics if the value is not Html.
    #[cfg(test)]
    pub fn into_markup(self) -> String {
        let mut markup = String::new();
        self.unwrap_html().write(&mut markup);
        markup
    }

    /// Panics if the value is not a Bool.
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Expected a Bool value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Int.
    pub fn as_int(&self) -> i32 {
        match self {
            Value::Int(i) => *i,
            _ => panic!("Expected an Int value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Float.
    pub fn as_float(&self) -> f64 {
        match self {
            Value::Float(f) => *f,
            _ => panic!("Expected a Float value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Array.
    pub fn as_array(&self) -> &[Value] {
        match self {
            Value::Array(arr) => arr,
            _ => panic!("Expected an Array value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Tuple.
    pub fn as_tuple(&self) -> &[Value] {
        match self {
            Value::Tuple(elements) => elements,
            _ => panic!("Expected a Tuple value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Record.
    pub fn as_record(&self) -> &[(FieldName, Value)] {
        match self {
            Value::Record(rec) => rec,
            _ => panic!("Expected a Record value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Option.
    pub fn as_option(&self) -> Option<&Value> {
        match self {
            Value::Option(opt) => opt.as_deref(),
            _ => panic!("Expected an Option value, found {self:?}"),
        }
    }

    /// Whether the value matches the pattern. Each binding of a pattern
    /// that matches is pushed to `bound` with the part of the value it
    /// binds. Panics if the value does not have the pattern's type.
    pub fn matches(&self, pattern: &IrPattern, bound: &mut Vec<(ValueId, Value)>) -> bool {
        let (constructor, args, fields) = match pattern {
            Pattern::Wildcard => return true,
            Pattern::Binding(binder) => {
                bound.push((binder.var, self.clone()));
                return true;
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => (constructor, args, fields),
        };
        let field_matches = |values: &[(FieldName, Value)], bound: &mut Vec<(ValueId, Value)>| {
            fields.iter().all(|field| {
                let (_, value) = values
                    .iter()
                    .find(|(name, _)| *name == field.name)
                    .unwrap_or_else(|| panic!("{self:?} has no field {}", field.name.as_str()));
                value.matches(&field.pattern, bound)
            })
        };
        match (constructor, self) {
            (Constructor::BoolTrue, Value::Bool(value)) => *value,
            (Constructor::BoolFalse, Value::Bool(value)) => !*value,
            (Constructor::OptionSome, Value::Option(Some(inner))) => inner.matches(&args[0], bound),
            (Constructor::OptionSome, Value::Option(None)) => false,
            (Constructor::OptionNone, Value::Option(inner)) => inner.is_none(),
            (Constructor::EnumVariant { variant_name, .. }, Value::Enum(value_variant, values)) => {
                variant_name == value_variant && field_matches(values, bound)
            }
            (Constructor::Record { .. }, Value::Record(values)) => field_matches(values, bound),
            (Constructor::Tuple, Value::Tuple(elements)) => args
                .iter()
                .zip(elements.iter())
                .all(|(arg, element)| element.matches(arg, bound)),
            _ => panic!("{self:?} cannot match the pattern {pattern:?}"),
        }
    }
}
