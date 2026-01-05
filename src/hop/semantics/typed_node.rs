use std::collections::BTreeMap;

use crate::document::document_cursor::StringSpan;
use crate::dop::patterns::Match;
use crate::dop::{TypedExpr, VarName};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

#[derive(Debug, Clone)]
pub enum TypedAttributeValue {
    Expressions(Vec<TypedExpr>),
    String(StringSpan),
}

#[derive(Debug, Clone)]
pub struct TypedAttribute {
    pub name: String,
    pub value: Option<TypedAttributeValue>,
}

#[derive(Debug, Clone)]
pub enum TypedNode {
    Text {
        value: StringSpan,
    },

    TextExpression {
        expression: TypedExpr,
    },

    ComponentReference {
        component_name: ComponentName,
        declaring_module: Option<ModuleName>,
        args: Vec<(VarName, TypedExpr)>,
        children: Vec<TypedNode>,
    },

    If {
        condition: TypedExpr,
        children: Vec<TypedNode>,
    },

    For {
        var_name: VarName,
        array_expr: TypedExpr,
        children: Vec<TypedNode>,
    },

    Match {
        match_: Match<TypedExpr, Vec<TypedNode>>,
    },

    Let {
        var: VarName,
        value: TypedExpr,
        children: Vec<TypedNode>,
    },

    Doctype {
        value: StringSpan,
    },

    Html {
        tag_name: StringSpan,
        attributes: BTreeMap<StringSpan, TypedAttribute>,
        children: Vec<TypedNode>,
    },

    Placeholder,
}
