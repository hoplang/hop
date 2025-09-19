use std::collections::BTreeMap;

use crate::{
    document::document_cursor::StringSpan,
    dop::{VarName, expr::TypedExpr},
};


#[derive(Debug, Clone)]
pub struct InlinedParameter {
    pub var_name: VarName,
    pub var_type: crate::dop::Type,
}

#[derive(Debug, Clone)]
pub enum InlinedAttributeValue {
    Expression(TypedExpr),
    String(String),
}

#[derive(Debug, Clone)]
pub struct InlinedAttribute {
    pub name: String,
    pub value: Option<InlinedAttributeValue>,
}

#[derive(Debug, Clone)]
pub struct InlinedEntryPoint {
    pub tag_name: StringSpan,
    pub params: Vec<InlinedParameter>,
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
        attributes: BTreeMap<String, InlinedAttribute>,
        children: Vec<Self>,
    },
    Let {
        var: VarName,
        value: TypedExpr,
        children: Vec<Self>,
    },
}
