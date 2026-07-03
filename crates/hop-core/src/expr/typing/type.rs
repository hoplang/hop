use core::fmt;
use std::sync::Arc;

use pretty::BoxDoc;

use super::typed_expr::TypedExpr;
use crate::document::CheapString;
use crate::document_id::DocumentId;
use crate::html::HtmlElement;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    String,
    Bool,
    Int,
    Float,
    Fragment,
    Array(Arc<Type>),
    Option(Arc<Type>),
    Named { module: DocumentId, name: TypeName },
}

#[derive(Debug, Clone)]
pub struct ComponentSignature {
    pub module: DocumentId,
    pub params: Vec<ParamEntry>,
    pub tail: Tail,
    pub is_recursive: bool,
}

#[derive(Debug, Clone)]
pub struct ParamEntry {
    pub name: VarName,
    pub typ: Arc<Type>,
    pub default: Option<TypedExpr>,
}

#[derive(Debug, Clone)]
pub enum Tail {
    Closed,
    Html {
        element: HtmlElement,
        reserved: Vec<CheapString>,
    },
}

/// Metadata from `#[examples(...)]` annotations on fields/parameters.
#[derive(Debug, Clone, Default)]
pub struct ExamplesAnnotation {
    /// Regex pattern for String fields.
    pub pattern: Option<String>,
    /// Minimum value for Int fields.
    pub min: Option<i64>,
    /// Maximum value for Int fields.
    pub max: Option<i64>,
    /// Minimum length for Array fields.
    pub min_len: Option<i64>,
    /// Maximum length for Array fields.
    pub max_len: Option<i64>,
}

/// An EquatableType is a type where its values can be compared
/// using `==` and `!=`.
#[derive(Debug, Clone, PartialEq)]
pub enum EquatableType {
    String,
    Bool,
    Int,
    Float,
    Option(Box<EquatableType>),
}

/// A ComparableType is a type where its values can be ordered
/// using comparison operations like `<`, `>`, `<=`, `>=`.
#[derive(Debug, Clone, PartialEq)]
pub enum ComparableType {
    Int,
    Float,
}

/// A NumericType is a type where its values can be used
/// in arithmetic operations like `+`, `-`, `*`.
#[derive(Debug, Clone, PartialEq)]
pub enum NumericType {
    Int,
    Float,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: TypeName,
    pub fields: Vec<(FieldName, Arc<Type>, Option<ExamplesAnnotation>)>,
}

impl ExamplesAnnotation {
    /// Formats the annotation as `#[examples(pattern = "...", min = N, max = N, min_len = N, max_len = N)]`.
    pub fn to_annotation_string(&self) -> String {
        let mut parts = Vec::new();
        if let Some(pattern) = &self.pattern {
            parts.push(format!("pattern = \"{}\"", pattern));
        }
        if let Some(min) = self.min {
            parts.push(format!("min = {}", min));
        }
        if let Some(max) = self.max {
            parts.push(format!("max = {}", max));
        }
        if let Some(min_len) = self.min_len {
            parts.push(format!("min_len = {}", min_len));
        }
        if let Some(max_len) = self.max_len {
            parts.push(format!("max_len = {}", max_len));
        }
        format!("#[examples({})]", parts.join(", "))
    }
}

impl Type {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            Type::Bool => Some(EquatableType::Bool),
            Type::String => Some(EquatableType::String),
            Type::Int => Some(EquatableType::Int),
            Type::Float => Some(EquatableType::Float),
            Type::Option(inner) => {
                let inner_equatable = inner.as_equatable_type()?;
                Some(EquatableType::Option(Box::new(inner_equatable)))
            }
            Type::Fragment | Type::Array(_) | Type::Named { .. } => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            Type::Int => Some(ComparableType::Int),
            Type::Float => Some(ComparableType::Float),
            Type::Bool
            | Type::String
            | Type::Fragment
            | Type::Array(_)
            | Type::Option(_)
            | Type::Named { .. } => None,
        }
    }

    /// Whether values of this type can be destructured by a `match` expression.
    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Bool | Type::Option(_) | Type::Named { .. } => true,
            Type::String | Type::Int | Type::Float | Type::Fragment | Type::Array(_) => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl<'a> Type {
    pub fn to_doc(&'a self) -> BoxDoc<'a> {
        match self {
            Type::String => BoxDoc::text("String"),
            Type::Float => BoxDoc::text("Float"),
            Type::Int => BoxDoc::text("Int"),
            Type::Bool => BoxDoc::text("Bool"),
            Type::Fragment => BoxDoc::text("Fragment"),
            Type::Array(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Array["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            Type::Option(elem_type) => BoxDoc::nil()
                .append(BoxDoc::text("Option["))
                .append(elem_type.to_doc())
                .append(BoxDoc::text("]")),
            Type::Named { module, name, .. } => {
                BoxDoc::text(format!("{}::{}", module.to_module_id(), name))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn should_be_equal_when_primitives_are_the_same() {
        assert_eq!(Type::Bool, Type::Bool);
        assert_eq!(Type::String, Type::String);
        assert_eq!(Type::Int, Type::Int);
        assert_eq!(Type::Float, Type::Float);
        assert_eq!(Type::Fragment, Type::Fragment);
    }

    #[test]
    fn should_not_be_equal_when_primitives_differ() {
        assert_ne!(Type::Bool, Type::Int);
        assert_ne!(Type::String, Type::Float);
        assert_ne!(Type::Int, Type::Fragment);
    }

    #[test]
    fn should_be_equal_when_arrays_have_the_same_element_type() {
        let a = Type::Array(Arc::new(Type::Int));
        let b = Type::Array(Arc::new(Type::Int));
        assert_eq!(a, b);
    }

    #[test]
    fn should_not_be_equal_when_arrays_have_different_element_types() {
        let a = Type::Array(Arc::new(Type::Int));
        let b = Type::Array(Arc::new(Type::String));
        assert_ne!(a, b);
    }

    #[test]
    fn should_be_equal_when_options_have_the_same_inner_type() {
        let a = Type::Option(Arc::new(Type::Bool));
        let b = Type::Option(Arc::new(Type::Bool));
        assert_eq!(a, b);
    }

    #[test]
    fn should_not_be_equal_when_options_have_different_inner_types() {
        let a = Type::Option(Arc::new(Type::Bool));
        let b = Type::Option(Arc::new(Type::String));
        assert_ne!(a, b);
    }
}
