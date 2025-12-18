use std::{collections::BTreeMap, fmt};

use crate::document::document_cursor::StringSpan;
use crate::dop::Type;
use crate::dop::TypedExpr;
use crate::dop::VarName;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use pretty::BoxDoc;

#[derive(Debug, Clone)]
pub struct InlinedParameter {
    pub var_name: VarName,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub enum InlinedAttributeValue {
    Expressions(Vec<TypedExpr>),
    String(String),
}

#[derive(Debug, Clone)]
pub struct InlinedAttribute {
    pub name: String,
    pub value: Option<InlinedAttributeValue>,
}

#[derive(Debug, Clone)]
pub struct InlinedEntrypoint {
    pub module_name: ModuleName,
    pub component_name: ComponentName,
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

impl InlinedParameter {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.var_name.as_str())
            .append(BoxDoc::text(": "))
            .append(BoxDoc::text(self.var_type.to_string()))
    }
}

impl InlinedAttributeValue {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            InlinedAttributeValue::Expressions(exprs) => {
                let mut doc = BoxDoc::text("{");
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        doc = doc.append(BoxDoc::text(", "));
                    }
                    doc = doc.append(expr.to_doc());
                }
                doc.append(BoxDoc::text("}"))
            }
            InlinedAttributeValue::String(s) => BoxDoc::text(format!("\"{}\"", s)),
        }
    }
}

impl InlinedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(&self.name);
        match &self.value {
            Some(value) => name_doc.append(BoxDoc::text("=")).append(value.to_doc()),
            None => name_doc,
        }
    }
}

impl InlinedEntrypoint {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("<")
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(if self.params.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::text(" {")
                    .append(BoxDoc::intersperse(
                        self.params.iter().map(|param| param.to_doc()),
                        BoxDoc::text(", "),
                    ))
                    .append(BoxDoc::text("}"))
            })
            .append(BoxDoc::text(">"))
            .append(if self.children.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.children.iter().map(|child| child.to_doc()),
                        BoxDoc::line(),
                    ))
                    .append(BoxDoc::line())
                    .nest(2)
            })
            .append(BoxDoc::text("</"))
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(BoxDoc::text(">"))
    }
}

impl InlinedNode {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            InlinedNode::Text { value } => BoxDoc::text(format!("{:?}", value.as_str())),
            InlinedNode::TextExpression { expression } => BoxDoc::text("{")
                .append(expression.to_doc())
                .append(BoxDoc::text("}")),
            InlinedNode::If {
                condition,
                children,
            } => BoxDoc::text("<if {")
                .append(condition.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if children.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            children.iter().map(|child| child.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("</if>")),
            InlinedNode::For {
                var_name,
                array_expr,
                children,
            } => BoxDoc::text("<for {")
                .append(BoxDoc::text(var_name.as_str()))
                .append(BoxDoc::text(" in "))
                .append(array_expr.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if children.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            children.iter().map(|child| child.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("</for>")),
            InlinedNode::Doctype { value } => BoxDoc::text(value.as_str()),
            InlinedNode::Html {
                tag_name,
                attributes,
                children,
            } => {
                let mut tag_doc = BoxDoc::text("<").append(BoxDoc::text(tag_name.as_str()));

                if !attributes.is_empty() {
                    tag_doc = tag_doc
                        .append(BoxDoc::text(" "))
                        .append(BoxDoc::intersperse(
                            attributes.values().map(|attr| attr.to_doc()),
                            BoxDoc::text(" "),
                        ));
                }

                if children.is_empty() {
                    tag_doc.append(BoxDoc::text(" />"))
                } else {
                    tag_doc
                        .append(BoxDoc::text(">"))
                        .append(BoxDoc::line())
                        .append(BoxDoc::intersperse(
                            children.iter().map(|child| child.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                        .append(BoxDoc::text("</"))
                        .append(BoxDoc::text(tag_name.as_str()))
                        .append(BoxDoc::text(">"))
                }
            }
            InlinedNode::Let {
                var,
                value,
                children,
            } => BoxDoc::text("<let {")
                .append(BoxDoc::text(var.as_str()))
                .append(BoxDoc::text(" = "))
                .append(value.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if children.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            children.iter().map(|child| child.to_doc()),
                            BoxDoc::line(),
                        ))
                        .append(BoxDoc::line())
                        .nest(2)
                })
                .append(BoxDoc::text("</let>")),
        }
    }
}

impl fmt::Display for InlinedParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for InlinedAttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for InlinedAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for InlinedEntrypoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for InlinedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
