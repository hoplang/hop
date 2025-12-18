use std::collections::BTreeMap;

use crate::document::document_cursor::StringSpan;
use crate::dop::{Expr, VarName};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;

#[derive(Debug, Clone)]
pub struct TypedArgument {
    pub var_name: VarName,
    pub var_expr: Expr,
}

#[derive(Debug, Clone)]
pub enum TypedAttributeValue {
    Expressions(Vec<Expr>),
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
        expression: Expr,
    },

    ComponentReference {
        component_name: ComponentName,
        definition_module: Option<ModuleName>,
        args: Option<Vec<TypedArgument>>,
        children: Vec<TypedNode>,
    },

    If {
        condition: Expr,
        children: Vec<TypedNode>,
    },

    For {
        var_name: VarName,
        array_expr: Expr,
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
