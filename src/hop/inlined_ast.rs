use std::collections::BTreeMap;

use crate::{
    document::document_cursor::StringSpan,
    dop::{Parameter, VarName, expr::TypedExpr},
};

use super::ast::Attribute;

#[derive(Debug, Clone)]
pub struct InlinedEntryPoint {
    pub tag_name: StringSpan,
    pub params: Vec<Parameter>,
    pub children: Vec<InlinedNode>,
}

#[derive(Debug, Clone)]
pub enum InlinedNode {
    Text {
        value: StringSpan,
    },
    TextExpression {
        expression: TypedExpr,
    },
    If {
        condition: TypedExpr,
        children: Vec<Self>,
    },
    For {
        var_name: VarName,
        array_expr: TypedExpr,
        children: Vec<Self>,
    },
    Doctype {
        value: StringSpan,
    },
    Html {
        tag_name: StringSpan,
        attributes: BTreeMap<StringSpan, Attribute<TypedExpr>>,
        children: Vec<Self>,
    },
    Let {
        var: VarName,
        value: TypedExpr,
        children: Vec<Self>,
    },
}
