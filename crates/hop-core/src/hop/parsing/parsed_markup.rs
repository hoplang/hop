use crate::document::DocumentRange;
use crate::hop::parsing::ParsedExpr;
use crate::hop::uncooked_string::UncookedString;
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;
use pretty::BoxDoc;
use std::borrow::Cow;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub enum ParsedMarkup {
    /// Plain text.
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

    /// An element.
    ///
    /// ```text
    /// <div class="hidden">
    ///   ...
    /// </div>
    /// ```
    Element {
        kind: HtmlElementKind,
        tag_name: DocumentRange,
        closing_tag_name: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
        children: Vec<ParsedMarkup>,
        range: DocumentRange,
    },

    /// A markup call.
    ///
    /// ```text
    /// <Foo x={10} y={20}>
    ///   ...
    /// </Foo>
    /// ```
    Call {
        function_name: FunctionName,
        function_name_opening_range: DocumentRange,
        function_name_closing_range: Option<DocumentRange>,
        attributes: Vec<ParsedAttribute>,
        children: Option<Vec<ParsedMarkup>>,
        range: DocumentRange,
    },

    /// A fragment.
    ///
    /// ```text
    /// <>hello {name}</>
    /// ```
    Fragment {
        children: Vec<ParsedMarkup>,
        range: DocumentRange,
    },
}

/// A single entry an attribute list.
#[derive(Debug, Clone)]
pub enum ParsedAttribute {
    /// An attribute containing only a key.
    ///
    /// ```text
    /// <input required/>
    ///        ^^^^^^^^
    /// ```
    KeyOnly {
        name: AttributeName,
        name_range: DocumentRange,
    },
    /// An attribute containing an expression.
    ///
    /// ```text
    /// <Square side={5 + 3}>
    ///         ^^^^^^^^^^^^
    /// ```
    Expression {
        name: AttributeName,
        name_range: DocumentRange,
        value: ParsedExpr,
    },
    /// An attribute containing a string literal.
    ///
    /// ```text
    /// <div class="hidden">
    ///      ^^^^^^^^^^^^^^
    /// ```
    String {
        name: AttributeName,
        name_range: DocumentRange,
        value: UncookedString,
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

pub(super) fn braced_doc(items: Vec<BoxDoc<'_>>) -> BoxDoc<'_> {
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
    /// The attribute name, or `None` for a spread.
    pub fn name(&self) -> Option<&AttributeName> {
        match self {
            ParsedAttribute::KeyOnly { name, .. }
            | ParsedAttribute::Expression { name, .. }
            | ParsedAttribute::String { name, .. } => Some(name),
            ParsedAttribute::Spread { .. } => None,
        }
    }

    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            ParsedAttribute::KeyOnly { name, .. } => BoxDoc::text(name.as_str()),
            ParsedAttribute::Expression { name, value, .. } => BoxDoc::text(name.as_str())
                .append(": ")
                .append(value.to_doc()),
            ParsedAttribute::String { name, value, .. } => BoxDoc::text(name.as_str())
                .append(": ")
                .append(format!("{value:?}")),
            ParsedAttribute::Spread { name, .. } => BoxDoc::text("...").append(name.as_str()),
        }
    }
}

impl ParsedMarkup {
    pub fn range(&self) -> &DocumentRange {
        match self {
            ParsedMarkup::Text { range, .. }
            | ParsedMarkup::Newline { range }
            | ParsedMarkup::Interpolation { range, .. }
            | ParsedMarkup::Call { range, .. }
            | ParsedMarkup::Fragment { range, .. }
            | ParsedMarkup::Element { range, .. } => range,
        }
    }

    /// The markup written inside the tags of this markup, in source order.
    pub fn children(&self) -> Vec<&Self> {
        match self {
            ParsedMarkup::Call { children, .. } => children.iter().flatten().collect(),
            ParsedMarkup::Element { children, .. } | ParsedMarkup::Fragment { children, .. } => {
                children.iter().collect()
            }
            ParsedMarkup::Text { .. }
            | ParsedMarkup::Newline { .. }
            | ParsedMarkup::Interpolation { .. } => Vec::new(),
        }
    }
    /// The expressions this markup contains.
    pub fn expressions(&self) -> Vec<&ParsedExpr> {
        match self {
            ParsedMarkup::Interpolation { expression, .. } => vec![expression],
            ParsedMarkup::Call { attributes, .. } | ParsedMarkup::Element { attributes, .. } => {
                attributes
                    .iter()
                    .filter_map(|attribute| match attribute {
                        ParsedAttribute::Expression { value, .. } => Some(value),
                        ParsedAttribute::KeyOnly { .. }
                        | ParsedAttribute::String { .. }
                        | ParsedAttribute::Spread { .. } => None,
                    })
                    .collect()
            }
            ParsedMarkup::Text { .. }
            | ParsedMarkup::Newline { .. }
            | ParsedMarkup::Fragment { .. } => Vec::new(),
        }
    }

    /// Get the range for the opening tag of this markup.
    ///
    /// ```text
    /// <div>hello world</div>
    ///  ^^^
    /// ```
    pub fn tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedMarkup::Call {
                function_name_opening_range: tag_name,
                ..
            } => Some(tag_name),
            ParsedMarkup::Element { tag_name, .. } => Some(tag_name),
            _ => None,
        }
    }

    /// Get the range for the closing tag of this markup.
    ///
    /// ```text
    /// <div>hello world</div>
    ///                   ^^^
    /// ```
    pub fn closing_tag_name(&self) -> Option<&DocumentRange> {
        match self {
            ParsedMarkup::Call {
                function_name_closing_range: closing_tag_name,
                ..
            } => closing_tag_name.as_ref(),
            ParsedMarkup::Element {
                closing_tag_name, ..
            } => closing_tag_name.as_ref(),
            _ => None,
        }
    }

    /// Get the name ranges for the tags of this markup.
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
            ParsedMarkup::Text { range } => {
                call_doc("text", vec![BoxDoc::text(format!("{:?}", range.as_str()))])
            }
            ParsedMarkup::Newline { .. } => call_doc("newline", vec![]),
            ParsedMarkup::Interpolation { expression, .. } => {
                call_doc("interpolate", vec![expression.to_doc()])
            }
            ParsedMarkup::Fragment { children, .. } => {
                call_doc("fragment", children.iter().map(|c| c.to_doc()).collect())
            }
            ParsedMarkup::Element {
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
            ParsedMarkup::Call {
                function_name,
                attributes,
                children,
                ..
            } => {
                let mut args = vec![BoxDoc::text("attrs: ").append(bracketed_doc(
                    attributes.iter().map(|a| a.to_doc()).collect(),
                ))];
                // `None` is a self-closing markup call, `<Foo/>`, and `Some` has
                // an explicit closing tag, `<Foo></Foo>`, possibly with a body.
                if let Some(children) = children {
                    args.push(
                        BoxDoc::text("children: ")
                            .append(bracketed_doc(children.iter().map(|c| c.to_doc()).collect())),
                    );
                }
                call_doc(function_name.as_str(), args)
            }
        }
    }
}

impl Display for ParsedMarkup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(40))
    }
}
