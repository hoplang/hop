use std::fmt;

use crate::document::CheapString;
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::TypedExpr;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;

pub use crate::hop::typing::typed_node::{TypedArgument, TypedAttribute, TypedLoopSource};

#[derive(Debug, Clone)]
pub enum InlinedNode {
    Text {
        value: CheapString,
    },

    TextExpression {
        expression: TypedExpr,
    },

    ComponentInvocation {
        component_name: TypeName,
        args: Vec<TypedArgument>,
    },

    If {
        condition: TypedExpr,
        children: Vec<InlinedNode>,
    },

    For {
        var_name: Option<VarName>,
        source: TypedLoopSource,
        children: Vec<InlinedNode>,
    },

    Match {
        match_: Match<TypedExpr, Vec<InlinedNode>>,
    },

    Let {
        var: VarName,
        value: TypedExpr,
        children: Vec<InlinedNode>,
    },

    LetRecordDestructure {
        subject: TypedExpr,
        bindings: Vec<(FieldName, VarName)>,
        children: Vec<InlinedNode>,
    },

    Doctype {
        value: CheapString,
    },

    Html {
        tag_name: CheapString,
        attributes: Vec<TypedAttribute>,
        children: Vec<InlinedNode>,
    },
}

impl InlinedNode {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            InlinedNode::Text { value } => BoxDoc::text(value.as_str()),
            InlinedNode::TextExpression { expression } => BoxDoc::text("{")
                .append(expression.to_doc())
                .append(BoxDoc::text("}")),
            InlinedNode::Doctype { value } => BoxDoc::text(value.as_str()),
            InlinedNode::ComponentInvocation {
                component_name,
                args,
            } => {
                let tag = BoxDoc::text("<").append(BoxDoc::text(component_name.as_str()));
                let tag_with_args = if args.is_empty() {
                    tag
                } else {
                    tag.append(BoxDoc::space()).append(BoxDoc::intersperse(
                        args.iter().map(|arg| {
                            BoxDoc::text(arg.name.as_str())
                                .append(BoxDoc::text("={"))
                                .append(arg.expr.to_doc())
                                .append(BoxDoc::text("}"))
                        }),
                        BoxDoc::space(),
                    ))
                };
                tag_with_args.append(BoxDoc::text("/>"))
            }
            InlinedNode::If {
                condition,
                children,
            } => {
                let tag = BoxDoc::text("<if {")
                    .append(condition.to_doc())
                    .append(BoxDoc::text("}>"));
                if children.is_empty() {
                    tag.append(BoxDoc::text("</if>"))
                } else {
                    tag.append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</if>"))
                }
            }
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
                let tag = BoxDoc::text("<for {")
                    .append(var_doc)
                    .append(BoxDoc::text(" in "))
                    .append(source_doc)
                    .append(BoxDoc::text("}>"));
                if children.is_empty() {
                    tag.append(BoxDoc::text("</for>"))
                } else {
                    tag.append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</for>"))
                }
            }
            InlinedNode::Let {
                var,
                value,
                children,
            } => {
                let tag = BoxDoc::text("<let {")
                    .append(BoxDoc::text(var.as_str()))
                    .append(BoxDoc::text(" = "))
                    .append(value.to_doc())
                    .append(BoxDoc::text("}>"));
                if children.is_empty() {
                    tag.append(BoxDoc::text("</let>"))
                } else {
                    tag.append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</let>"))
                }
            }
            InlinedNode::LetRecordDestructure {
                subject,
                bindings,
                children,
            } => {
                let bindings_doc = BoxDoc::intersperse(
                    bindings.iter().map(|(field, var)| {
                        BoxDoc::text(field.as_str())
                            .append(BoxDoc::text(": "))
                            .append(BoxDoc::text(var.as_str()))
                    }),
                    BoxDoc::text(", "),
                );
                let tag = BoxDoc::text("<let {")
                    .append(bindings_doc)
                    .append(BoxDoc::text("} = "))
                    .append(subject.to_doc())
                    .append(BoxDoc::text("}>"));
                if children.is_empty() {
                    tag.append(BoxDoc::text("</let>"))
                } else {
                    tag.append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("</let>"))
                }
            }
            InlinedNode::Match { match_ } => match match_ {
                Match::Enum { subject, arms } => {
                    let header = BoxDoc::text("<match {")
                        .append(subject.to_doc())
                        .append(BoxDoc::text("}>"));
                    let cases = arms.iter().map(|arm| {
                        let pattern = match &arm.pattern {
                            EnumPattern::Variant {
                                enum_name,
                                variant_name,
                            } => {
                                let base = BoxDoc::text(enum_name.as_str())
                                    .append(BoxDoc::text("::"))
                                    .append(BoxDoc::text(variant_name.as_str()));
                                if arm.bindings.is_empty() {
                                    base
                                } else {
                                    base.append(BoxDoc::text("{"))
                                        .append(BoxDoc::intersperse(
                                            arm.bindings.iter().map(|(field, var)| {
                                                BoxDoc::text(field.as_str())
                                                    .append(BoxDoc::text(": "))
                                                    .append(BoxDoc::text(var.as_str()))
                                            }),
                                            BoxDoc::text(", "),
                                        ))
                                        .append(BoxDoc::text("}"))
                                }
                            }
                        };
                        let case_header = BoxDoc::text("<case {")
                            .append(pattern)
                            .append(BoxDoc::text("}>"));
                        if arm.body.is_empty() {
                            case_header.append(BoxDoc::text("</case>"))
                        } else {
                            case_header
                                .append(
                                    BoxDoc::line()
                                        .append(BoxDoc::intersperse(
                                            arm.body.iter().map(|c| c.to_doc()),
                                            BoxDoc::line(),
                                        ))
                                        .nest(2),
                                )
                                .append(BoxDoc::line())
                                .append(BoxDoc::text("</case>"))
                        }
                    });
                    header
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(cases, BoxDoc::line()))
                                .nest(2),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("</match>"))
                }
                Match::Bool {
                    subject,
                    true_body,
                    false_body,
                } => {
                    let header = BoxDoc::text("<match {")
                        .append(subject.to_doc())
                        .append(BoxDoc::text("}>"));
                    let true_case = {
                        let case_header = BoxDoc::text("<case {true}>");
                        if true_body.is_empty() {
                            case_header.append(BoxDoc::text("</case>"))
                        } else {
                            case_header
                                .append(
                                    BoxDoc::line()
                                        .append(BoxDoc::intersperse(
                                            true_body.iter().map(|c| c.to_doc()),
                                            BoxDoc::line(),
                                        ))
                                        .nest(2),
                                )
                                .append(BoxDoc::line())
                                .append(BoxDoc::text("</case>"))
                        }
                    };
                    let false_case = {
                        let case_header = BoxDoc::text("<case {false}>");
                        if false_body.is_empty() {
                            case_header.append(BoxDoc::text("</case>"))
                        } else {
                            case_header
                                .append(
                                    BoxDoc::line()
                                        .append(BoxDoc::intersperse(
                                            false_body.iter().map(|c| c.to_doc()),
                                            BoxDoc::line(),
                                        ))
                                        .nest(2),
                                )
                                .append(BoxDoc::line())
                                .append(BoxDoc::text("</case>"))
                        }
                    };
                    header
                        .append(
                            BoxDoc::line()
                                .append(true_case)
                                .append(BoxDoc::line())
                                .append(false_case)
                                .nest(2),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("</match>"))
                }
                Match::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                } => {
                    let header = BoxDoc::text("<match {")
                        .append(subject.to_doc())
                        .append(BoxDoc::text("}>"));
                    let some_pattern = match some_arm_binding {
                        Some(var) => BoxDoc::text("Some(")
                            .append(BoxDoc::text(var.as_str()))
                            .append(BoxDoc::text(")")),
                        None => BoxDoc::text("Some(_)"),
                    };
                    let some_case = {
                        let case_header = BoxDoc::text("<case {")
                            .append(some_pattern)
                            .append(BoxDoc::text("}>"));
                        if some_arm_body.is_empty() {
                            case_header.append(BoxDoc::text("</case>"))
                        } else {
                            case_header
                                .append(
                                    BoxDoc::line()
                                        .append(BoxDoc::intersperse(
                                            some_arm_body.iter().map(|c| c.to_doc()),
                                            BoxDoc::line(),
                                        ))
                                        .nest(2),
                                )
                                .append(BoxDoc::line())
                                .append(BoxDoc::text("</case>"))
                        }
                    };
                    let none_case = {
                        let case_header = BoxDoc::text("<case {None}>");
                        if none_arm_body.is_empty() {
                            case_header.append(BoxDoc::text("</case>"))
                        } else {
                            case_header
                                .append(
                                    BoxDoc::line()
                                        .append(BoxDoc::intersperse(
                                            none_arm_body.iter().map(|c| c.to_doc()),
                                            BoxDoc::line(),
                                        ))
                                        .nest(2),
                                )
                                .append(BoxDoc::line())
                                .append(BoxDoc::text("</case>"))
                        }
                    };
                    header
                        .append(
                            BoxDoc::line()
                                .append(some_case)
                                .append(BoxDoc::line())
                                .append(none_case)
                                .nest(2),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("</match>"))
                }
            },
            InlinedNode::Html {
                tag_name,
                attributes,
                children,
            } => {
                let tag = BoxDoc::text("<").append(BoxDoc::text(tag_name.as_str()));
                let tag_with_attrs = if attributes.is_empty() {
                    tag
                } else if attributes.len() <= 3 {
                    tag.append(BoxDoc::space()).append(BoxDoc::intersperse(
                        attributes.iter().map(|a| a.to_doc()),
                        BoxDoc::space(),
                    ))
                } else {
                    tag.append(
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                attributes.iter().map(|a| a.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2),
                    )
                    .append(BoxDoc::line())
                };
                if children.is_empty() {
                    tag_with_attrs
                        .append(BoxDoc::text(">"))
                        .append(BoxDoc::text("</"))
                        .append(BoxDoc::text(tag_name.as_str()))
                        .append(BoxDoc::text(">"))
                } else {
                    tag_with_attrs
                        .append(BoxDoc::text(">"))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    children.iter().map(|c| c.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("</"))
                        .append(BoxDoc::text(tag_name.as_str()))
                        .append(BoxDoc::text(">"))
                }
            }
        }
    }
}

impl fmt::Display for InlinedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(60))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use std::sync::Arc;

    #[test]
    fn component_invocation_renders_self_closing_with_args() {
        let node = InlinedNode::ComponentInvocation {
            component_name: TypeName::new("NodeView").unwrap(),
            args: vec![TypedArgument {
                name: VarName::new("node").unwrap(),
                expr: TypedExpr::Var {
                    value: VarName::new("next").unwrap(),
                    kind: Arc::new(Type::String),
                },
            }],
        };
        assert_eq!(node.to_string(), "<NodeView node={next}/>");
    }
}
