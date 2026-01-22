use std::fmt;

use crate::document::CheapString;
use crate::dop::Type;
use crate::dop::TypedExpr;
use crate::dop::VarName;
use crate::dop::patterns::{EnumPattern, Match};
use crate::hop::semantics::typed_node::TypedLoopSource;
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
    Expression(TypedExpr),
    String(CheapString),
}

#[derive(Debug, Clone)]
pub struct InlinedAttribute {
    pub name: CheapString,
    pub value: Option<InlinedAttributeValue>,
}

#[derive(Debug, Clone)]
pub struct InlinedEntrypointDeclaration {
    pub module_name: ModuleName,
    pub component_name: ComponentName,
    pub params: Vec<InlinedParameter>,
    pub children: Vec<InlinedNode>,
}

#[derive(Debug, Clone)]
pub enum InlinedNode {
    Text {
        value: CheapString,
    },
    TextExpression {
        expression: TypedExpr,
    },
    If {
        condition: TypedExpr,
        children: Vec<Self>,
    },
    For {
        var_name: Option<VarName>,
        source: TypedLoopSource,
        children: Vec<Self>,
    },
    Doctype {
        value: CheapString,
    },
    Html {
        tag_name: CheapString,
        attributes: Vec<InlinedAttribute>,
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
            InlinedAttributeValue::Expression(expr) => BoxDoc::text("{")
                .append(expr.to_doc())
                .append(BoxDoc::text("}")),
            InlinedAttributeValue::String(s) => BoxDoc::text(format!("\"{}\"", s)),
        }
    }
}

impl InlinedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(self.name.as_str());
        match &self.value {
            Some(value) => name_doc.append(BoxDoc::text("=")).append(value.to_doc()),
            None => name_doc,
        }
    }
}

impl InlinedEntrypointDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = if self.params.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(
                self.params.iter().map(|param| param.to_doc()),
                BoxDoc::text(", "),
            )
        };

        BoxDoc::text("entrypoint")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.component_name.as_str()))
            .append(BoxDoc::text("("))
            .append(params_doc)
            .append(BoxDoc::text(")"))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if self.children.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.children.iter().map(|child| child.to_doc()),
                        BoxDoc::line(),
                    ))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

impl InlinedNode {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            InlinedNode::Text { value } => BoxDoc::text(value.as_str()),
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
                source,
                children,
            } => {
                let source_doc = match source {
                    TypedLoopSource::Array(expr) => expr.to_doc(),
                    TypedLoopSource::RangeInclusive { start, end } => start
                        .to_doc()
                        .append(BoxDoc::text("..="))
                        .append(end.to_doc()),
                };
                let var_doc = match var_name {
                    Some(name) => BoxDoc::text(name.as_str()),
                    None => BoxDoc::text("_"),
                };
                BoxDoc::text("<for {")
                    .append(var_doc)
                    .append(BoxDoc::text(" in "))
                    .append(source_doc)
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
                    .append(BoxDoc::text("</for>"))
            }
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
                            attributes.iter().map(|attr| attr.to_doc()),
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
                            Some(name) => format!("Some({})", name.as_str()),
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
                                            } => {
                                                if arm.bindings.is_empty() {
                                                    format!("{}::{}", enum_name, variant_name)
                                                } else {
                                                    let bindings_str = arm
                                                        .bindings
                                                        .iter()
                                                        .map(|(field, var)| {
                                                            format!(
                                                                "{}: {}",
                                                                field.as_str(),
                                                                var.as_str()
                                                            )
                                                        })
                                                        .collect::<Vec<_>>()
                                                        .join(", ");
                                                    format!(
                                                        "{}::{}({})",
                                                        enum_name, variant_name, bindings_str
                                                    )
                                                }
                                            }
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

impl fmt::Display for InlinedEntrypointDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

impl fmt::Display for InlinedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}
