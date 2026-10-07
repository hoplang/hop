use std::collections::HashMap;

use super::r#type::Type;
use super::typed_expr::TypedExpr;
use crate::document::{CheapString, DocumentRange};
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::var_name::VarName;

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// How many fields of the row the function declares. They come first,
    /// and they are the only fields a caller can pass by position.
    pub declared: usize,
    /// The parameters the function declares and those its rest adds.
    pub row: Row,
    pub return_type: Type,
    pub rest_param: Option<VarName>,
}

/// The parameters of a callee, which a caller passes by name.
#[derive(Debug, Clone)]
pub struct Row {
    /// The parameters the callee declares, followed by those its rest adds
    /// from the function it is spread into.
    pub fields: Vec<ParamEntry>,
    pub tail: Tail,
}

#[derive(Debug, Clone)]
pub struct ParamEntry {
    pub name: VarName,
    pub typ: Type,
    pub fallback: Option<TypedExpr>,
}

/// The parameters of a row besides its fields.
#[derive(Debug, Clone)]
pub enum Tail {
    /// No other parameters.
    Closed,
    /// An optional parameter for each attribute `element` accepts, with the
    /// type of the attribute, except the names in `lacks`. A row lacks the
    /// names of its fields, so a name never reaches both a field and the
    /// tail.
    Element {
        element: HtmlElementKind,
        lacks: Vec<AttributeName>,
    },
}

/// What a declared or imported name refers to.
#[derive(Debug, Clone)]
pub enum NameKind {
    /// A local record or enum, or an imported type. Always a `Type::Named`.
    Type(Type),
    Function,
    Page,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub kind: NameKind,
    pub definition_range: DocumentRange,
    /// The import statement, for imported names.
    pub import_range: Option<DocumentRange>,
}

/// The names a module's bodies are checked against.
#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    /// Every name declared or imported in the module. A module has one
    /// namespace: a type, a page and a function cannot share a name.
    pub names: HashMap<CheapString, Name>,
    /// Settled signatures for the names of kind `Function`, imported and
    /// local.
    pub functions: HashMap<CheapString, FunctionSignature>,
}
