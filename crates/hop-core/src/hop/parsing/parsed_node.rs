use crate::document::DocumentRange;
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::ParsedType;
use crate::hop::parsing::parsed_expr::ParsedMatchPattern;
use crate::html::HtmlElement;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub enum ParsedNode {
    /// A Text node represents plain text. E.g.
    /// ```text
    /// <div>hello</div>
    ///      ^^^^^
    /// ```
    Text { range: DocumentRange },

    /// A Newline node represents a newline character in text position.
    /// This is separate from Text to allow precise whitespace handling.
    Newline { range: DocumentRange },

    /// An interpolation of an expression. E.g.
    /// ```text
    /// <div>hello {world}</div>
    ///            ^^^^^^^
    /// ```
    Interpolation {
        expression: ParsedExpr,
        range: DocumentRange,
    },

    /// An HTML node represents a plain HTML node. E.g.
    /// ```text
    /// <div class="hidden">...</div>
    /// ```
    Html {
        element: HtmlElement,
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A ComponentInvocation represents an invocation of a component. E.g.
    /// ```text
    /// <Foo x={10} y={20}>...</Foo>
    /// ```
    ComponentInvocation {
        component_name: TypeName,
        component_name_opening_range: DocumentRange,
        component_name_closing_range: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
        children: Option<Vec<ParsedNode>>,
        range: DocumentRange,
    },

    /// An If node contains content that is only evaluated when its condition
    /// expression evaluates to true. E.g.
    /// ```text
    /// <if {x == 20}>...</if>
    /// ```
    If {
        condition: ParsedExpr,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A For node contains content that is evaluated once for each item of
    /// an array or each value in a range. E.g.
    /// ```text
    /// <for {user in users}>...</for>
    /// ```
    For {
        /// The bound variable name, `None` when the variable is discarded
        /// using `_`.
        var_name: Option<VarName>,
        var_name_range: Option<DocumentRange>,
        source: Box<ParsedLoopSource>,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A Match node contains pattern matching over an expression. E.g.
    /// ```text
    /// <match {x}>
    ///   <case {Some(y)}>found {y}!</case>
    ///   <case {None}>not found</case>
    /// </match>
    /// ```
    Match {
        subject: ParsedExpr,
        cases: Vec<ParsedMatchCase>,
        range: DocumentRange,
    },

    /// A Let node introduces one or more local variable bindings. E.g.
    /// ```text
    /// <let {name: String = "World", count: Int = 0}>...</let>
    /// ```
    Let {
        bindings: Vec<ParsedLetBinding>,
        bindings_range: DocumentRange,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A Comment node represents an HTML comment. E.g.
    /// ```text
    /// <!-- This is a comment -->
    /// ```
    Comment { range: DocumentRange },

    /// A Fragment node groups children under no tag of its own, so that
    /// markup with several roots can still be written where one is required. E.g.
    /// ```text
    /// <>hello {name}</>
    /// ```
    Fragment {
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },
}

/// A ParsedAttribute is a single entry an attribute list.
#[derive(Debug, Clone)]
pub enum ParsedAttribute {
    /// An attribute containing only a key. E.g.
    /// ```text
    /// <input required>
    ///        ^^^^^^^^
    /// ```
    KeyOnly { name: DocumentRange },
    /// An attribute containing an expression. E.g.
    /// ```text
    /// <Square side={5 + 3}>
    ///         ^^^^^^^^^^^^
    /// ```
    Expression {
        name: DocumentRange,
        value: ParsedExpr,
    },
    /// An attribute containing a static string. E.g.
    /// ```text
    /// <div class="hidden">
    ///      ^^^^^^^^^^^^^^
    /// ```
    String {
        name: DocumentRange,
        /// The inner content range, excluding quotes. None for empty strings like `attr=""`.
        content: Option<DocumentRange>,
        /// Range of the whole value including the surrounding quotes, e.g. `"bar"`.
        quoted_range: DocumentRange,
    },
    /// A spread. E.g.
    /// ```text
    /// <div ...rest>
    ///      ^^^^^^^
    /// ```
    Spread { name: VarName, range: DocumentRange },
}

impl ParsedAttribute {
    /// The range of the attribute name, or `None` for a spread.
    pub fn name_range(&self) -> Option<&DocumentRange> {
        match self {
            ParsedAttribute::KeyOnly { name }
            | ParsedAttribute::Expression { name, .. }
            | ParsedAttribute::String { name, .. } => Some(name),
            ParsedAttribute::Spread { .. } => None,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedAttribute::KeyOnly { name } => BoxDoc::text(name.as_str()),
            ParsedAttribute::Expression { name, value } => BoxDoc::text(name.as_str())
                .append(BoxDoc::text("={"))
                .append(BoxDoc::line_().append(value.to_doc()).nest(2))
                .append(BoxDoc::line_())
                .append(BoxDoc::text("}"))
                .group(),
            ParsedAttribute::String { name, content, .. } => {
                let content = content.as_ref().map(|r| r.as_str()).unwrap_or("");
                BoxDoc::text(name.as_str()).append(BoxDoc::text(format!("=\"{}\"", content)))
            }
            ParsedAttribute::Spread { name, .. } => {
                BoxDoc::text("...").append(BoxDoc::text(name.as_str()))
            }
        }
    }
}

/// The source of iteration in a for loop - either an array or an inclusive range.
#[derive(Debug, Clone)]
pub enum ParsedLoopSource {
    /// Iterate over elements of an array. E.g.
    /// ```text
    /// <for {item in [1, 2, 3]}>
    ///               ^^^^^^^^^
    /// ```
    Array(ParsedExpr),
    /// Iterate over an inclusive integer range, e.g. `i in 0..=5`
    /// ```text
    /// <for {i in 0..=5}>
    ///            ^^^^^
    /// ```
    RangeInclusive { start: ParsedExpr, end: ParsedExpr },
}

/// A case in a match node.
///
/// E.g.
/// ```text
/// <match {x}>
///   <case {Some(y)}>...</case>
///   ^^^^^^^^^^^^^^^^^^^^^^^^^^
/// </match>
/// ```
#[derive(Debug, Clone)]
pub struct ParsedMatchCase {
    pub pattern: ParsedMatchPattern,
    pub children: Vec<ParsedNode>,
}

impl ParsedMatchCase {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text("<case {")
            .append(BoxDoc::text(self.pattern.to_string()))
            .append(BoxDoc::text("}>"))
            .append(if self.children.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        self.children.iter().map(|c| c.to_doc()),
                        BoxDoc::line(),
                    ))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("</case>"))
    }
}

/// A single binding in a let node.
///
/// E.g.
/// ```text
/// <let {
///   name: String = "World",
///   ^^^^^^^^^^^^^^^^^^^^^^
///   count: Int = 0,
/// }>
/// ```
#[derive(Debug, Clone)]
pub struct ParsedLetBinding {
    pub var_name: VarName,
    pub var_name_range: DocumentRange,
    pub var_type: Option<ParsedType>,
    pub value_expr: ParsedExpr,
}

impl ParsedNode {
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedNode::Text { range, .. }
            | ParsedNode::Newline { range }
            | ParsedNode::Interpolation { range, .. }
            | ParsedNode::ComponentInvocation { range, .. }
            | ParsedNode::If { range, .. }
            | ParsedNode::For { range, .. }
            | ParsedNode::Let { range, .. }
            | ParsedNode::Match { range, .. }
            | ParsedNode::Comment { range }
            | ParsedNode::Fragment { range, .. }
            | ParsedNode::Html { range, .. } => range,
        }
    }

    /// The nodes written inside this node's tags, in source order.
    pub fn children(&self) -> Vec<&Self> {
        match self {
            ParsedNode::ComponentInvocation { children, .. } => children.iter().flatten().collect(),
            ParsedNode::If { children, .. }
            | ParsedNode::For { children, .. }
            | ParsedNode::Let { children, .. }
            | ParsedNode::Html { children, .. }
            | ParsedNode::Fragment { children, .. } => children.iter().collect(),
            ParsedNode::Match { cases, .. } => {
                cases.iter().flat_map(|case| &case.children).collect()
            }
            ParsedNode::Comment { .. }
            | ParsedNode::Text { .. }
            | ParsedNode::Newline { .. }
            | ParsedNode::Interpolation { .. } => Vec::new(),
        }
    }
    /// The expressions this node contain.
    pub fn expressions(&self) -> Vec<&ParsedExpr> {
        match self {
            ParsedNode::Interpolation { expression, .. } => vec![expression],
            ParsedNode::If { condition, .. } => vec![condition],
            ParsedNode::Match { subject, .. } => vec![subject],
            ParsedNode::ComponentInvocation { attributes, .. }
            | ParsedNode::Html { attributes, .. } => attributes
                .iter()
                .filter_map(|attribute| match attribute {
                    ParsedAttribute::Expression { value, .. } => Some(value),
                    ParsedAttribute::KeyOnly { .. }
                    | ParsedAttribute::String { .. }
                    | ParsedAttribute::Spread { .. } => None,
                })
                .collect(),
            ParsedNode::For { source, .. } => match source.as_ref() {
                ParsedLoopSource::Array(expr) => vec![expr],
                ParsedLoopSource::RangeInclusive { start, end } => vec![start, end],
            },
            ParsedNode::Let { bindings, .. } => {
                bindings.iter().map(|binding| &binding.value_expr).collect()
            }
            ParsedNode::Text { .. }
            | ParsedNode::Newline { .. }
            | ParsedNode::Comment { .. }
            | ParsedNode::Fragment { .. } => Vec::new(),
        }
    }

    /// Get the range for the opening tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedNode::ComponentInvocation {
                component_name_opening_range: tag_name,
                ..
            } => Some(tag_name),
            ParsedNode::Html { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    /// Get the range for the closing tag of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///                   ^^^
    pub fn closing_tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedNode::ComponentInvocation {
                component_name_closing_range: closing_tag_name,
                ..
            } => closing_tag_name.as_ref(),
            ParsedNode::Html {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            _ => None,
        }
    }

    /// Get the name ranges for the tags of a node.
    ///
    /// Example:
    /// <div>hello world</div>
    ///  ^^^              ^^^
    pub fn tag_names(&self) -> impl Iterator<Item = &DocumentRange> {
        self.tag_name().into_iter().chain(self.closing_tag_name())
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedNode::Text { range } => BoxDoc::text(range.as_str()),
            // Newlines don't produce output - they signal where breaks can occur
            ParsedNode::Newline { .. } => BoxDoc::nil(),
            ParsedNode::Interpolation { expression, .. } => BoxDoc::text("{")
                .append(expression.to_doc())
                .append(BoxDoc::text("}")),
            ParsedNode::ComponentInvocation {
                component_name,
                attributes,
                children,
                ..
            } => {
                let component_name_str = component_name.as_str();

                // Build opening tag with attributes (same format as HTML)
                let opening_tag_doc = if attributes.is_empty() {
                    BoxDoc::text("<").append(BoxDoc::text(component_name_str))
                } else {
                    BoxDoc::text("<")
                        .append(BoxDoc::text(component_name_str))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    attributes.iter().map(|a| a.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
                        .append(BoxDoc::line_())
                        .group()
                };

                match children {
                    // Self-closing invocation, `<Foo/>`.
                    None => opening_tag_doc.append(BoxDoc::text("/>")),
                    // Explicit closing tag, `<Foo></Foo>`, possibly with a body.
                    Some(children) => opening_tag_doc
                        .append(BoxDoc::text(">"))
                        .append(if children.is_empty() {
                            BoxDoc::nil()
                        } else {
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    children.iter().map(|c| c.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2)
                                .append(BoxDoc::line())
                        })
                        .append(BoxDoc::text("</"))
                        .append(BoxDoc::text(component_name_str))
                        .append(BoxDoc::text(">")),
                }
            }
            ParsedNode::Fragment { children, .. } => BoxDoc::text("<>")
                .append(if children.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2)
                        .append(BoxDoc::line())
                })
                .append(BoxDoc::text("</>")),
            ParsedNode::If {
                condition,
                children,
                ..
            } => BoxDoc::text("<if {")
                .append(condition.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if children.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2)
                        .append(BoxDoc::line())
                })
                .append(BoxDoc::text("</if>")),
            ParsedNode::For {
                var_name,
                source,
                children,
                ..
            } => {
                let source_doc = match &**source {
                    ParsedLoopSource::Array(expr) => expr.to_doc(),
                    ParsedLoopSource::RangeInclusive { start, end } => start
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
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2)
                            .append(BoxDoc::line())
                    })
                    .append(BoxDoc::text("</for>"))
            }
            ParsedNode::Let {
                bindings, children, ..
            } => {
                let bindings_doc = BoxDoc::line_()
                    .append(BoxDoc::intersperse(
                        bindings.iter().map(|b| {
                            let mut doc = BoxDoc::text(b.var_name.as_str());
                            if let Some(var_type) = &b.var_type {
                                doc = doc
                                    .append(BoxDoc::text(": "))
                                    .append(BoxDoc::text(var_type.to_string()));
                            }
                            doc.append(BoxDoc::text(" = "))
                                .append(b.value_expr.to_doc())
                        }),
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                    .nest(2)
                    .append(BoxDoc::line_())
                    .group();
                BoxDoc::text("<let {")
                    .append(bindings_doc)
                    .append(BoxDoc::text("}>"))
                    .append(if children.is_empty() {
                        BoxDoc::nil()
                    } else {
                        BoxDoc::line()
                            .append(BoxDoc::intersperse(
                                children.iter().map(|c| c.to_doc()),
                                BoxDoc::line(),
                            ))
                            .nest(2)
                            .append(BoxDoc::line())
                    })
                    .append(BoxDoc::text("</let>"))
            }
            ParsedNode::Comment { range } => BoxDoc::text(range.as_str()),
            ParsedNode::Match { subject, cases, .. } => BoxDoc::text("<match {")
                .append(subject.to_doc())
                .append(BoxDoc::text("}>"))
                .append(if cases.is_empty() {
                    BoxDoc::nil()
                } else {
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            cases.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2)
                        .append(BoxDoc::line())
                })
                .append(BoxDoc::text("</match>")),
            ParsedNode::Html {
                element,
                tag_name,
                attributes,
                children,
                ..
            } => {
                let tag_name_str = tag_name.as_str();

                // Build the opening tag with attributes
                // When attributes break to multiple lines, each attribute goes on its own line
                // and the closing > goes on its own line
                let opening_tag_doc = if attributes.is_empty() {
                    BoxDoc::text("<")
                        .append(BoxDoc::text(tag_name_str))
                        .append(BoxDoc::text(">"))
                } else {
                    BoxDoc::text("<")
                        .append(BoxDoc::text(tag_name_str))
                        .append(
                            BoxDoc::line()
                                .append(BoxDoc::intersperse(
                                    attributes.iter().map(|item| item.to_doc()),
                                    BoxDoc::line(),
                                ))
                                .nest(2),
                        )
                        .append(BoxDoc::line_())
                        .append(BoxDoc::text(">"))
                        .group()
                };

                if element.is_void() {
                    opening_tag_doc
                } else if children.is_empty() {
                    opening_tag_doc
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("</"))
                        .append(BoxDoc::text(tag_name_str))
                        .append(BoxDoc::text(">"))
                } else {
                    opening_tag_doc
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
                        .append(BoxDoc::text(tag_name_str))
                        .append(BoxDoc::text(">"))
                }
            }
        }
    }
}

impl Display for ParsedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}
