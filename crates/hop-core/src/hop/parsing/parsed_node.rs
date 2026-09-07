use crate::document::DocumentRange;
use crate::hop::parsing::ParsedExpr;
use crate::hop::parsing::ParsedType;
use crate::hop::parsing::parsed_expr::ParsedMatchPattern;
use crate::html::HtmlElementKind;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;
use std::borrow::Cow;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub enum ParsedNode {
    /// A plain text node.
    ///
    /// ```text
    /// <div>hello {user.name}</div>
    ///      ^^^^^^
    /// ```
    Text { range: DocumentRange },

    /// A newline.
    ///
    /// This is separate from Text to allow precise whitespace handling.
    Newline { range: DocumentRange },

    /// An interpolation.
    ///
    /// ```text
    /// <div>hello {world}</div>
    ///            ^^^^^^^
    /// ```
    Interpolation {
        expression: ParsedExpr,
        range: DocumentRange,
    },

    /// An HTML element.
    ///
    /// ```text
    /// <div class="hidden">
    ///   ...
    /// </div>
    /// ```
    HtmlElement {
        kind: HtmlElementKind,
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A component invocation node.
    ///
    /// ```text
    /// <Foo x={10} y={20}>
    ///   ...
    /// </Foo>
    /// ```
    ComponentInvocation {
        component_name: TypeName,
        component_name_opening_range: DocumentRange,
        component_name_closing_range: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
        children: Option<Vec<ParsedNode>>,
        range: DocumentRange,
    },

    /// An if node.
    ///
    /// ```text
    /// <if {x == 20}>
    ///   ...
    /// </if>
    /// ```
    If {
        condition: ParsedExpr,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// A for node.
    ///
    /// ```text
    /// <for {user in users}>
    ///   ...
    /// </for>
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

    /// A match node.
    ///
    /// ```text
    /// <match {user.email}>
    ///   <case {Some(y)}>
    ///     ...
    ///   </case>
    ///   <case {None}>
    ///     ...
    ///   </case>
    /// </match>
    /// ```
    Match {
        subject: ParsedExpr,
        cases: Vec<ParsedMatchCase>,
        range: DocumentRange,
    },

    /// A let node.
    ///
    /// ```text
    /// <let {name: String = "World", count: Int = 0}>
    ///   ...
    /// </let>
    /// ```
    Let {
        bindings: Vec<ParsedLetBinding>,
        bindings_range: DocumentRange,
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },

    /// An HTML comment.
    ///
    /// ```text
    /// <!-- This is a comment -->
    /// ```
    Comment { range: DocumentRange },

    /// A fragment.
    ///
    /// ```text
    /// <>hello {name}</>
    /// ```
    Fragment {
        children: Vec<ParsedNode>,
        range: DocumentRange,
    },
}

/// A single entry an attribute list.
#[derive(Debug, Clone)]
pub enum ParsedAttribute {
    /// An attribute containing only a key.
    ///
    /// ```text
    /// <input required>
    ///        ^^^^^^^^
    /// ```
    KeyOnly { name: DocumentRange },
    /// An attribute containing an expression.
    ///
    /// ```text
    /// <Square side={5 + 3}>
    ///         ^^^^^^^^^^^^
    /// ```
    Expression {
        name: DocumentRange,
        value: ParsedExpr,
    },
    /// An attribute containing a static string.
    ///
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
    /// A spread.
    ///
    /// ```text
    /// <div ...rest>
    ///      ^^^^^^^
    /// ```
    Spread { name: VarName, range: DocumentRange },
}

/// The source of iteration in a for node.
#[derive(Debug, Clone)]
pub enum ParsedLoopSource {
    /// An array expression.
    ///
    /// ```text
    /// <for {item in [1, 2, 3]}>
    ///               ^^^^^^^^^
    /// ```
    Array(ParsedExpr),
    /// An inclusive integer range.
    ///
    /// ```text
    /// <for {i in 0..=5}>
    ///            ^^^^^
    /// ```
    RangeInclusive { start: ParsedExpr, end: ParsedExpr },
}

/// A case in a match node.
///
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

/// A single binding in a let node.
///
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

fn call_doc<'a>(name: impl Into<Cow<'a, str>>, args: Vec<BoxDoc<'a>>) -> BoxDoc<'a> {
    let name = BoxDoc::text(name);
    if args.is_empty() {
        return name.append("()");
    }
    name.append("(")
        .append(
            BoxDoc::line_()
                .append(BoxDoc::intersperse(
                    args,
                    BoxDoc::text(",").append(BoxDoc::line()),
                ))
                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                .append(BoxDoc::line_())
                .nest(2)
                .group(),
        )
        .append(")")
}

fn bracketed_doc(items: Vec<BoxDoc<'_>>) -> BoxDoc<'_> {
    if items.is_empty() {
        return BoxDoc::text("[]");
    }
    BoxDoc::text("[")
        .append(
            BoxDoc::line_()
                .append(BoxDoc::intersperse(
                    items,
                    BoxDoc::text(",").append(BoxDoc::line()),
                ))
                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                .append(BoxDoc::line_())
                .nest(2)
                .group(),
        )
        .append("]")
}

fn braced_doc(items: Vec<BoxDoc<'_>>) -> BoxDoc<'_> {
    if items.is_empty() {
        return BoxDoc::text("{}");
    }
    BoxDoc::text("{")
        .append(
            BoxDoc::line()
                .append(BoxDoc::intersperse(
                    items,
                    BoxDoc::text(",").append(BoxDoc::line()),
                ))
                .append(BoxDoc::text(",").flat_alt(BoxDoc::nil()))
                .nest(2)
                .append(BoxDoc::line())
                .group(),
        )
        .append("}")
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
                .append(": ")
                .append(value.to_doc()),
            ParsedAttribute::String { name, content, .. } => {
                let content = content.as_ref().map(|r| r.as_str()).unwrap_or("");
                BoxDoc::text(name.as_str())
                    .append(": ")
                    .append(format!("{content:?}"))
            }
            ParsedAttribute::Spread { name, .. } => BoxDoc::text("...").append(name.as_str()),
        }
    }
}

impl ParsedMatchCase {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        self.pattern.to_doc().append(" => ").append(braced_doc(
            self.children.iter().map(|c| c.to_doc()).collect(),
        ))
    }
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
            | ParsedNode::HtmlElement { range, .. } => range,
        }
    }

    /// The nodes written inside this node's tags, in source order.
    pub fn children(&self) -> Vec<&Self> {
        match self {
            ParsedNode::ComponentInvocation { children, .. } => children.iter().flatten().collect(),
            ParsedNode::If { children, .. }
            | ParsedNode::For { children, .. }
            | ParsedNode::Let { children, .. }
            | ParsedNode::HtmlElement { children, .. }
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
            | ParsedNode::HtmlElement { attributes, .. } => attributes
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
    /// ```text
    /// <div>hello world</div>
    ///  ^^^
    /// ```
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedNode::ComponentInvocation {
                component_name_opening_range: tag_name,
                ..
            } => Some(tag_name),
            ParsedNode::HtmlElement { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    /// Get the range for the closing tag of a node.
    ///
    /// ```text
    /// <div>hello world</div>
    ///                   ^^^
    /// ```
    pub fn closing_tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedNode::ComponentInvocation {
                component_name_closing_range: closing_tag_name,
                ..
            } => closing_tag_name.as_ref(),
            ParsedNode::HtmlElement {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            _ => None,
        }
    }

    /// Get the name ranges for the tags of a node.
    ///
    /// ```text
    /// <div>hello world</div>
    ///  ^^^              ^^^
    /// ```
    pub fn tag_names(&self) -> impl Iterator<Item = &DocumentRange> {
        self.tag_name().into_iter().chain(self.closing_tag_name())
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedNode::Text { range } => {
                call_doc("text", vec![BoxDoc::text(format!("{:?}", range.as_str()))])
            }
            ParsedNode::Newline { .. } => call_doc("newline", vec![]),
            ParsedNode::Comment { range } => call_doc(
                "comment",
                vec![BoxDoc::text(format!("{:?}", range.as_str()))],
            ),
            ParsedNode::Interpolation { expression, .. } => {
                call_doc("interpolate", vec![expression.to_doc()])
            }
            ParsedNode::Fragment { children, .. } => {
                call_doc("fragment", children.iter().map(|c| c.to_doc()).collect())
            }
            ParsedNode::HtmlElement {
                kind,
                tag_name,
                attributes,
                children,
                ..
            } => {
                let mut args = vec![
                    BoxDoc::text(format!("tag: {:?}", tag_name.as_str())),
                    BoxDoc::text("attrs: ").append(bracketed_doc(
                        attributes.iter().map(|a| a.to_doc()).collect(),
                    )),
                ];
                if !kind.is_void() || !children.is_empty() {
                    args.push(
                        BoxDoc::text("children: ")
                            .append(bracketed_doc(children.iter().map(|c| c.to_doc()).collect())),
                    );
                }
                call_doc("html", args)
            }
            ParsedNode::ComponentInvocation {
                component_name,
                attributes,
                children,
                ..
            } => {
                let mut args = vec![BoxDoc::text("attrs: ").append(bracketed_doc(
                    attributes.iter().map(|a| a.to_doc()).collect(),
                ))];
                // `None` is a self-closing invocation, `<Foo/>`; `Some` has an
                // explicit closing tag, `<Foo></Foo>`, possibly with a body.
                if let Some(children) = children {
                    args.push(
                        BoxDoc::text("children: ")
                            .append(bracketed_doc(children.iter().map(|c| c.to_doc()).collect())),
                    );
                }
                call_doc(component_name.as_str(), args)
            }
            ParsedNode::If {
                condition,
                children,
                ..
            } => BoxDoc::text("if ")
                .append(condition.to_doc())
                .append(" ")
                .append(braced_doc(children.iter().map(|c| c.to_doc()).collect())),
            ParsedNode::For {
                var_name,
                source,
                children,
                ..
            } => {
                let source_doc = match &**source {
                    ParsedLoopSource::Array(expr) => expr.to_doc(),
                    ParsedLoopSource::RangeInclusive { start, end } => {
                        start.to_doc().append("..=").append(end.to_doc())
                    }
                };
                let var_doc = match var_name {
                    Some(name) => BoxDoc::text(name.as_str()),
                    None => BoxDoc::text("_"),
                };
                BoxDoc::text("for ")
                    .append(var_doc)
                    .append(" in ")
                    .append(source_doc)
                    .append(" ")
                    .append(braced_doc(children.iter().map(|c| c.to_doc()).collect()))
            }
            ParsedNode::Let {
                bindings, children, ..
            } => {
                let bindings_doc = BoxDoc::intersperse(
                    bindings.iter().map(|b| {
                        let mut doc = BoxDoc::text(b.var_name.as_str());
                        if let Some(var_type) = &b.var_type {
                            doc = doc.append(": ").append(var_type.to_doc());
                        }
                        doc.append(" = ").append(b.value_expr.to_doc())
                    }),
                    BoxDoc::text(", "),
                );
                BoxDoc::text("let ")
                    .append(bindings_doc)
                    .append(" in ")
                    .append(braced_doc(children.iter().map(|c| c.to_doc()).collect()))
            }
            ParsedNode::Match { subject, cases, .. } => BoxDoc::text("match ")
                .append(subject.to_doc())
                .append(" ")
                .append(braced_doc(cases.iter().map(|c| c.to_doc()).collect())),
        }
    }
}

impl Display for ParsedNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(40))
    }
}
