use crate::document::CheapString;
use crate::dop::patterns::{EnumPattern, Match};
use crate::dop::{TypedExpr, VarName};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_name::ModuleName;
use pretty::BoxDoc;

/// The source of iteration in a for loop - either an array or an inclusive range.
#[derive(Debug, Clone)]
pub enum TypedLoopSource {
    /// Iterate over elements of an array
    Array(TypedExpr),
    /// Iterate over an inclusive integer range
    RangeInclusive { start: TypedExpr, end: TypedExpr },
}

#[derive(Debug, Clone)]
pub enum TypedAttributeValue {
    Expression(TypedExpr),
    String(CheapString),
}

impl TypedAttributeValue {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedAttributeValue::Expression(expr) => BoxDoc::text("{")
                .append(expr.to_doc())
                .append(BoxDoc::text("}")),
            TypedAttributeValue::String(s) => BoxDoc::text(format!("\"{}\"", s.as_str())),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedAttribute {
    pub name: String,
    pub value: Option<TypedAttributeValue>,
}

impl TypedAttribute {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let name_doc = BoxDoc::text(&self.name);
        match &self.value {
            Some(value) => name_doc.append(BoxDoc::text("=")).append(value.to_doc()),
            None => name_doc,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedNode {
    Text {
        value: CheapString,
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
        source: TypedLoopSource,
        children: Vec<TypedNode>,
    },

    Match {
        match_: Match<Vec<TypedNode>>,
    },

    Let {
        var: VarName,
        value: TypedExpr,
        children: Vec<TypedNode>,
    },

    Doctype {
        value: CheapString,
    },

    Html {
        tag_name: CheapString,
        attributes: Vec<TypedAttribute>,
        children: Vec<TypedNode>,
    },

    Placeholder,
}

impl TypedNode {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            TypedNode::Text { value } => BoxDoc::text(value.as_str()),
            TypedNode::TextExpression { expression } => BoxDoc::text("{")
                .append(expression.to_doc())
                .append(BoxDoc::text("}")),
            TypedNode::Doctype { value } => BoxDoc::text("<!DOCTYPE ")
                .append(BoxDoc::text(value.as_str()))
                .append(BoxDoc::text(">")),
            TypedNode::ComponentReference {
                component_name,
                args,
                children,
                ..
            } => {
                let tag = BoxDoc::text("<").append(BoxDoc::text(component_name.as_str()));
                let tag_with_args = if args.is_empty() {
                    tag
                } else {
                    tag.append(BoxDoc::space()).append(BoxDoc::intersperse(
                        args.iter().map(|(name, expr)| {
                            BoxDoc::text(name.as_str())
                                .append(BoxDoc::text("={"))
                                .append(expr.to_doc())
                                .append(BoxDoc::text("}"))
                        }),
                        BoxDoc::space(),
                    ))
                };
                if children.is_empty() {
                    tag_with_args.append(BoxDoc::text("/>"))
                } else {
                    tag_with_args
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
                        .append(BoxDoc::text(component_name.as_str()))
                        .append(BoxDoc::text(">"))
                }
            }
            TypedNode::If {
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
            TypedNode::For {
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
                let tag = BoxDoc::text("<for {")
                    .append(BoxDoc::text(var_name.as_str()))
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
            TypedNode::Let {
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
            TypedNode::Match { match_ } => match match_ {
                Match::Enum { subject, arms } => {
                    let header = BoxDoc::text("<match {")
                        .append(BoxDoc::text(subject.0.as_str()))
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
                                    base.append(BoxDoc::text("("))
                                        .append(BoxDoc::intersperse(
                                            arm.bindings.iter().map(|(field, var)| {
                                                BoxDoc::text(field.as_str())
                                                    .append(BoxDoc::text(": "))
                                                    .append(BoxDoc::text(var.as_str()))
                                            }),
                                            BoxDoc::text(", "),
                                        ))
                                        .append(BoxDoc::text(")"))
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
                        .append(BoxDoc::text(subject.0.as_str()))
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
                        .append(BoxDoc::text(subject.0.as_str()))
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
            TypedNode::Html {
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
            TypedNode::Placeholder => BoxDoc::text("<placeholder/>"),
        }
    }
}
