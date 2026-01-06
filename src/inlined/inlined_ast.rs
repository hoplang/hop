use std::{collections::BTreeMap, fmt};

use crate::document::document_cursor::StringSpan;
use crate::dop::patterns::{EnumPattern, Match};
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
    pub default_value: Option<TypedExpr>,
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
pub struct InlinedComponentDeclaration {
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
    Match {
        match_: Match<Vec<InlinedNode>>,
    },
}

impl InlinedParameter {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let base = BoxDoc::text(self.var_name.as_str())
            .append(BoxDoc::text(": "))
            .append(BoxDoc::text(self.var_type.to_string()));
        match &self.default_value {
            Some(default) => base.append(BoxDoc::text(" = ")).append(default.to_doc()),
            None => base,
        }
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

impl InlinedComponentDeclaration {
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
            InlinedNode::Match { match_ } => {
                fn children_to_doc<'a>(children: &'a [InlinedNode]) -> BoxDoc<'a> {
                    if children.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|child| child.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2)
                            .append(BoxDoc::line())
                    }
                }

                fn case_doc<'a>(pattern: &str, children: &'a [InlinedNode]) -> BoxDoc<'a> {
                    BoxDoc::text("<case {")
                        .append(BoxDoc::text(pattern.to_string()))
                        .append(BoxDoc::text("}>"))
                        .append(children_to_doc(children))
                        .append(BoxDoc::text("</case>"))
                }

                match match_ {
                    Match::Bool {
                        subject,
                        true_body,
                        false_body,
                    } => BoxDoc::text("<match {")
                        .append(BoxDoc::text(subject.0.as_str()))
                        .append(BoxDoc::text("}>"))
                        .append(
                            BoxDoc::line()
                                .append(case_doc("true", true_body))
                                .append(BoxDoc::line())
                                .append(case_doc("false", false_body))
                                .nest(2)
                                .append(BoxDoc::line()),
                        )
                        .append(BoxDoc::text("</match>")),
                    Match::Option {
                        subject,
                        some_arm_binding,
                        some_arm_body,
                        none_arm_body,
                    } => {
                        let some_pattern = match some_arm_binding {
                            Some((name, _)) => format!("Some({})", name.as_str()),
                            None => "Some(_)".to_string(),
                        };
                        BoxDoc::text("<match {")
                            .append(BoxDoc::text(subject.0.as_str()))
                            .append(BoxDoc::text("}>"))
                            .append(
                                BoxDoc::line()
                                    .append(case_doc(&some_pattern, some_arm_body))
                                    .append(BoxDoc::line())
                                    .append(case_doc("None", none_arm_body))
                                    .nest(2)
                                    .append(BoxDoc::line()),
                            )
                            .append(BoxDoc::text("</match>"))
                    }
                    Match::Enum { subject, arms } => BoxDoc::text("<match {")
                        .append(BoxDoc::text(subject.0.as_str()))
                        .append(BoxDoc::text("}>"))
                        .append(if arms.is_empty() {
                            BoxDoc::nil()
                        } else {
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    arms.iter().map(|arm| {
                                        let pattern = match &arm.pattern {
                                            EnumPattern::Variant {
                                                enum_name,
                                                variant_name,
                                            } => format!("{}::{}", enum_name, variant_name),
                                        };
                                        case_doc(&pattern, &arm.body)
                                    }),
                                    BoxDoc::line(),
                                ))
                                .nest(2)
                                .append(BoxDoc::line())
                        })
                        .append(BoxDoc::text("</match>")),
                }
            }
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

impl fmt::Display for InlinedComponentDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for InlinedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
