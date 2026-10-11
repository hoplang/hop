use crate::hop::typing::{Constructor, Pattern};
use crate::ir::binder_id::BinderId;
use crate::ir::ir_match::IrPattern;
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
    Record(Vec<(FieldName, Value)>),
    Option(Option<Box<Value>>),
    /// Enum variant with name and optional fields
    Enum {
        variant_name: TypeName,
        fields: Vec<(FieldName, Value)>,
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

    /// The markup an Html value renders as. Panics if the value is not Html.
    #[cfg(test)]
    pub fn into_markup(self) -> String {
        let mut markup = String::new();
        crate::ir::runtime::html_node::write_html(&self.unwrap_html(), &mut markup);
        markup
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
    pub fn unwrap_record(self) -> Vec<(FieldName, Value)> {
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

    /// Whether the value matches the pattern. Each binding of a pattern
    /// that matches is pushed to `bound` with the part of the value it
    /// binds. Panics if the value does not have the pattern's type.
    pub fn matches(&self, pattern: &IrPattern, bound: &mut Vec<(BinderId, Value)>) -> bool {
        let (constructor, args, fields) = match pattern {
            Pattern::Wildcard => return true,
            Pattern::Binding { name, .. } => {
                bound.push((*name, self.clone()));
                return true;
            }
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => (constructor, args, fields),
        };
        let field_matches = |values: &[(FieldName, Value)], bound: &mut Vec<(BinderId, Value)>| {
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
            (
                Constructor::EnumVariant { variant_name, .. },
                Value::Enum {
                    variant_name: value_variant,
                    fields: values,
                },
            ) => variant_name == value_variant && field_matches(values, bound),
            (Constructor::Record { .. }, Value::Record(values)) => field_matches(values, bound),
            (Constructor::Tuple, Value::Tuple(elements)) => args
                .iter()
                .zip(elements)
                .all(|(arg, element)| element.matches(arg, bound)),
            _ => panic!("{self:?} cannot match the pattern {pattern:?}"),
        }
    }
}
