use std::collections::HashMap;

use super::r#type::Type;
use super::typed_expr::TypedExpr;
use crate::document::{CheapString, DocumentRange};
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::var_name::VarName;

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// The parameters the function declares.
    pub params: Vec<ParamEntry>,
    /// The parameters of a callee that the function's rest carries, which a
    /// caller passes by name only.
    pub forwarded: Vec<ParamEntry>,
    pub return_type: Type,
    pub tail: Tail,
    pub rest_param: Option<VarName>,
}

#[derive(Debug, Clone)]
pub struct ParamEntry {
    pub name: VarName,
    pub typ: Type,
    pub fallback: Option<TypedExpr>,
}

#[derive(Debug, Clone)]
pub enum Tail {
    Closed,
    Html {
        element: HtmlElementKind,
        reserved: Vec<AttributeName>,
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
