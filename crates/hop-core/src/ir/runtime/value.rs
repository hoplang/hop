use std::ops::Index;
use std::rc::Rc;

use crate::hop::typing::{Constructor, Pattern};
use crate::html::{HtmlElementKind, write_escaped_html};
use crate::ir::flat_program::{FlatHtmlLambda, FlatPureLambda};
use crate::ir::ir_match::IrPattern;
use crate::ir::runtime::bounded_vec::BoundedVec;
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::state::{ElementIdentity, Place};
use crate::ir::value_id::ValueId;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::{field_name::FieldName, type_name::TypeName};

pub const MAX_ARRAY_LENGTH: usize = 10_000_000;

/// The most bytes a String may hold.
pub const MAX_STRING_LENGTH: usize = 10_000_000;

/// Runtime value for the evaluator.
#[derive(Debug, Clone)]
pub enum Value<'a> {
    String(Rc<str>),
    Html(Html<'a>),
    Bool(bool),
    Int(i32),
    Float(f64),
    Array(Rc<BoundedVec<Value<'a>, MAX_ARRAY_LENGTH>>),
    Tuple(Rc<[Value<'a>]>),
    Record(Rc<[(FieldName, Value<'a>)]>),
    Option(Option<Rc<Value<'a>>>),
    Enum(TypeName, Rc<[(FieldName, Value<'a>)]>),
    Closure(Rc<Closure<'a>>),
    Effect(Rc<Effect<'a>>),
    Cell(Place),
}

#[derive(Debug)]
#[cfg_attr(not(test), allow(dead_code))]
pub enum Effect<'a> {
    Pure(Value<'a>),
    Write(Place, Value<'a>),
    Read(Place),
    Bind(Rc<Effect<'a>>, Rc<Closure<'a>>),
    Then(Rc<Effect<'a>>, Rc<Effect<'a>>),
}

#[derive(Debug)]
pub struct Closure<'a> {
    pub lambda: Lambda<'a>,
    pub env: Option<Rc<Env<'a>>>,
}

#[derive(Debug, Default)]
pub struct Env<'a> {
    pub parent: Option<Rc<Env<'a>>>,
    pub bindings: Vec<(ValueId, Value<'a>)>,
}

impl<'a> Env<'a> {
    pub fn freeze(&mut self) -> Option<Rc<Env<'a>>> {
        if !self.bindings.is_empty() {
            self.parent = Some(Rc::new(Env {
                parent: self.parent.take(),
                bindings: std::mem::take(&mut self.bindings),
            }));
        }
        self.parent.clone()
    }
}

impl<'a> Index<&ValueId> for Env<'a> {
    type Output = Value<'a>;

    fn index(&self, name: &ValueId) -> &Value<'a> {
        let mut env = self;
        loop {
            if let Some((_, value)) = env.bindings.iter().rev().find(|(bound, _)| bound == name) {
                return value;
            }
            env = env
                .parent
                .as_deref()
                .unwrap_or_else(|| panic!("{name} is not in scope"));
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Lambda<'a> {
    Pure(&'a FlatPureLambda),
    Html(&'a FlatHtmlLambda),
}

impl<'a> Value<'a> {
    /// Structural equality of data, comparing floats as `f64::total_cmp`
    /// does, by their bits: NaN is equal to itself, and 0.0 is not -0.0. It
    /// is an equivalence relation, so values that share an allocation are
    /// equal without looking inside, as a value that nests a shared part in
    /// itself can be exponentially larger as a tree than in memory. Panics
    /// on Html, a closure, an Effect or a cell.
    #[cfg(test)]
    pub fn total_eq(&self, other: &Value<'_>) -> bool {
        fn all(a: &[Value<'_>], b: &[Value<'_>]) -> bool {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.total_eq(b))
        }
        fn fields(a: &[(FieldName, Value<'_>)], b: &[(FieldName, Value<'_>)]) -> bool {
            a.len() == b.len()
                && (a.iter().zip(b))
                    .all(|((a_name, a), (b_name, b))| a_name == b_name && a.total_eq(b))
        }
        match (self, other) {
            (Value::Html(_) | Value::Closure(_) | Value::Effect(_) | Value::Cell(_), _)
            | (_, Value::Html(_) | Value::Closure(_) | Value::Effect(_) | Value::Cell(_)) => {
                panic!("total_eq compares data, not {self:?} and {other:?}")
            }
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a.to_bits() == b.to_bits(),
            (Value::Array(a), Value::Array(b)) => Rc::ptr_eq(a, b) || all(a, b),
            (Value::Tuple(a), Value::Tuple(b)) => Rc::ptr_eq(a, b) || all(a, b),
            (Value::Record(a), Value::Record(b)) => Rc::ptr_eq(a, b) || fields(a, b),
            (Value::Option(Some(a)), Value::Option(Some(b))) => Rc::ptr_eq(a, b) || a.total_eq(b),
            (Value::Option(None), Value::Option(None)) => true,
            (Value::Enum(a_name, a), Value::Enum(b_name, b)) => {
                a_name == b_name && (Rc::ptr_eq(a, b) || fields(a, b))
            }
            _ => false,
        }
    }

    /// Panics if the value is not Html.
    pub fn unwrap_html(self) -> Html<'a> {
        match self {
            Value::Html(html) => html,
            _ => panic!("Expected Html value, found {self:?}"),
        }
    }

    /// Panics if the value is not a String.
    pub fn as_string(&self) -> &Rc<str> {
        match self {
            Value::String(s) => s,
            _ => panic!("Expected a String value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Bool.
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("expected a Bool value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Int.
    pub fn as_int(&self) -> i32 {
        match self {
            Value::Int(i) => *i,
            _ => panic!("expected an Int value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Float.
    pub fn as_float(&self) -> f64 {
        match self {
            Value::Float(f) => *f,
            _ => panic!("expected a Float value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Array.
    pub fn as_array(&self) -> &[Value<'a>] {
        match self {
            Value::Array(arr) => arr,
            _ => panic!("expected an Array value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Tuple.
    pub fn as_tuple(&self) -> &[Value<'a>] {
        match self {
            Value::Tuple(elements) => elements,
            _ => panic!("expected a Tuple value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Record.
    pub fn as_record(&self) -> &[(FieldName, Value<'a>)] {
        match self {
            Value::Record(rec) => rec,
            _ => panic!("expected a Record value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Option.
    pub fn as_option(&self) -> Option<&Value<'a>> {
        match self {
            Value::Option(opt) => opt.as_deref(),
            _ => panic!("expected an Option value, found {self:?}"),
        }
    }

    /// Panics if the value is not a Closure.
    pub fn as_closure(&self) -> &Rc<Closure<'a>> {
        match self {
            Value::Closure(closure) => closure,
            _ => panic!("expected a Closure value, found {self:?}"),
        }
    }

    /// Panics if the value is not a cell.
    pub fn as_place(&self) -> &Place {
        match self {
            Value::Cell(place) => place,
            _ => panic!("expected a Cell value, found {self:?}"),
        }
    }

    /// Panics if the value is not an Effect.
    pub fn as_effect(&self) -> &Rc<Effect<'a>> {
        match self {
            Value::Effect(effect) => effect,
            _ => panic!("expected an Effect value, found {self:?}"),
        }
    }

    /// Whether the value matches the pattern. Each binding of a pattern
    /// that matches is pushed to `bound` with the part of the value it
    /// binds. Panics if the value does not have the pattern's type.
    pub fn matches(&self, pattern: &IrPattern, bound: &mut Vec<(ValueId, Value<'a>)>) -> bool {
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
        let field_matches = |values: &[(FieldName, Value<'a>)],
                             bound: &mut Vec<(ValueId, Value<'a>)>| {
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

    /// The markup an Html value renders as. Panics if the value is not Html.
    #[cfg(test)]
    pub fn into_markup(self) -> String {
        let mut markup = String::new();
        self.unwrap_html().write(&mut markup);
        markup
    }
}

/// The largest size an Html value may have.
const MAX_HTML_SIZE: usize = 10_000_000;

#[derive(Debug, Clone)]
pub struct Html<'a> {
    node: Rc<HtmlNode<'a>>,
    size: usize,
}

#[derive(Debug)]
pub enum HtmlNode<'a> {
    Text(&'a str),
    Escape(Rc<str>),
    Element {
        element: HtmlElementKind,
        attributes: Vec<HtmlAttribute>,
        children: Html<'a>,
        #[cfg_attr(not(test), allow(dead_code))]
        on_click: Option<Rc<Closure<'a>>>,
        #[cfg_attr(not(test), allow(dead_code))]
        identity: ElementIdentity,
    },
    Concat(Vec<Html<'a>>),
}

/// An attribute that renders. The value is unescaped and escapes when
/// written. A boolean attribute has no value.
#[derive(Debug)]
pub struct HtmlAttribute {
    pub name: AttributeName,
    pub value: Option<Rc<str>>,
}

impl<'a> Html<'a> {
    /// The node as Html, unless it exceeds the Html size limit.
    pub fn new(node: HtmlNode<'a>) -> Result<Self, EvalError> {
        let size = match &node {
            HtmlNode::Text(text) => text.len().max(1),
            HtmlNode::Escape(text) => text.len().max(1),
            HtmlNode::Element {
                attributes,
                children,
                ..
            } => {
                let values: usize = attributes
                    .iter()
                    .filter_map(|attribute| attribute.value.as_ref())
                    .map(|value| value.len())
                    .sum();
                1 + values + children.size
            }
            HtmlNode::Concat(parts) => 1 + parts.iter().map(|part| part.size).sum::<usize>(),
        };
        if size > MAX_HTML_SIZE {
            return Err(EvalError::HtmlSizeLimit {
                limit: MAX_HTML_SIZE,
            });
        }
        Ok(Html {
            node: Rc::new(node),
            size,
        })
    }

    #[cfg(test)]
    pub fn node(&self) -> &HtmlNode<'a> {
        &self.node
    }

    pub fn write(&self, out: &mut String) {
        match &*self.node {
            HtmlNode::Text(content) => out.push_str(content),
            HtmlNode::Escape(text) => write_escaped_html(text, out),
            HtmlNode::Element {
                element,
                attributes,
                children,
                ..
            } => {
                out.push('<');
                out.push_str(element.as_str());
                for attribute in attributes {
                    out.push(' ');
                    out.push_str(attribute.name.as_str());
                    if let Some(value) = &attribute.value {
                        out.push_str("=\"");
                        write_escaped_html(value, out);
                        out.push('"');
                    }
                }
                out.push('>');
                if !element.is_void() {
                    children.write(out);
                    out.push_str("</");
                    out.push_str(element.as_str());
                    out.push('>');
                }
            }
            HtmlNode::Concat(parts) => {
                for part in parts {
                    part.write(out);
                }
            }
        }
    }
}
