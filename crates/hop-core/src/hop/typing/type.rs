use core::fmt;
use std::sync::Arc;

use pretty::BoxDoc;

use super::typed_expr::TypedExpr;
use crate::document::CheapString;
use crate::document_id::DocumentId;
use crate::examples_annotation::ExamplesAnnotation;
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
    Attrs,
    Array(Arc<Type>),
    Option(Arc<Type>),
    Named { module: DocumentId, name: TypeName },
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<ParamEntry>,
    pub return_type: Arc<Type>,
    pub tail: Tail,
    pub rest_param: Option<VarName>,
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

/// An EquatableType is a type where its values can be compared
/// using `==` and `!=`.
#[derive(Debug, Clone, PartialEq)]
pub enum EquatableType {
    String,
    Bool,
    Int,
    Float,
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

impl Type {
    pub fn as_equatable_type(&self) -> Option<EquatableType> {
        match self {
            Type::Bool => Some(EquatableType::Bool),
            Type::String => Some(EquatableType::String),
            Type::Int => Some(EquatableType::Int),
            Type::Float => Some(EquatableType::Float),
            Type::Option(_)
            | Type::Fragment
            | Type::Attrs
            | Type::Array(_)
            | Type::Named { .. } => None,
        }
    }

    pub fn as_comparable_type(&self) -> Option<ComparableType> {
        match self {
            Type::Int => Some(ComparableType::Int),
            Type::Float => Some(ComparableType::Float),
            Type::Bool
            | Type::String
            | Type::Fragment
            | Type::Attrs
            | Type::Array(_)
            | Type::Option(_)
            | Type::Named { .. } => None,
        }
    }

    /// Whether values of this type can be destructured by a `match` expression.
    pub fn is_matchable(&self) -> bool {
        match self {
            Type::Bool | Type::Option(_) | Type::Named { .. } => true,
            Type::String
            | Type::Int
            | Type::Float
            | Type::Fragment
            | Type::Attrs
            | Type::Array(_) => false,
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
            Type::Attrs => BoxDoc::text("Attrs"),
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
