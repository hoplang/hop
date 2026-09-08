use std::collections::HashMap;

use super::r#type::Type;
use super::typed_expr::TypedExpr;
use crate::document::{CheapString, DocumentRange};
use crate::html::HtmlElementKind;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<ParamEntry>,
    pub return_type: Type,
    pub tail: Tail,
    pub rest_param: Option<VarName>,
}

#[derive(Debug, Clone)]
pub struct ParamEntry {
    pub name: VarName,
    pub typ: Type,
    pub default: Option<TypedExpr>,
}

#[derive(Debug, Clone)]
pub enum Tail {
    Closed,
    Html {
        element: HtmlElementKind,
        reserved: Vec<CheapString>,
    },
}

/// What a name in the type namespace refers to.
#[derive(Debug, Clone)]
pub enum NameKind {
    /// A local record or enum, or an imported type. Always a `Type::Named`.
    Type(Type),
    Component,
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
    pub names: HashMap<TypeName, Name>,
    /// Settled component signatures, imported and local.
    pub components: HashMap<TypeName, FunctionSignature>,
    pub functions: HashMap<VarName, (FunctionSignature, DocumentRange)>,
}
