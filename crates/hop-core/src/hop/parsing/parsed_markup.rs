use crate::display::{Indent, write_lines};
use crate::document::DocumentRange;
use crate::hop::parsing::ParsedExpr;
use crate::hop::uncooked_string::UncookedString;
use crate::html::HtmlElementKind;
use crate::symbols::attribute_name::AttributeName;
use crate::symbols::function_name::FunctionName;
use crate::symbols::var_name::VarName;
use std::fmt::{self, Display, Write};

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
}

impl Display for ParsedAttribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsedAttribute::KeyOnly { name, .. } => f.write_str(name.as_str()),
            ParsedAttribute::Expression { name, value, .. } => {
                write!(f, "{}: {value}", name.as_str())
            }
            ParsedAttribute::String { name, value, .. } => {
                write!(f, "{}: {value:?}", name.as_str())
            }
            ParsedAttribute::Spread { name, .. } => write!(f, "...{}", name.as_str()),
        }
    }
}

/// Text, newlines and interpolations are one line, and every other markup
/// has its arguments on lines of their own.
impl Display for ParsedMarkup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        /// The items in brackets, each on a line of its own.
        fn bracketed<T: Display>(items: &[T]) -> impl Display {
            fmt::from_fn(move |f| write_lines(f, "[", items, "]"))
        }

        match self {
            ParsedMarkup::Text { range } => write!(f, "text({:?})", range.as_str()),
            ParsedMarkup::Newline { .. } => f.write_str("newline()"),
            ParsedMarkup::Interpolation { expression, .. } => {
                write!(f, "interpolate({expression})")
            }
            ParsedMarkup::Fragment {
                children: fragment_children,
                ..
            } => write_lines(f, "fragment(", fragment_children, ")"),
            ParsedMarkup::Element {
                kind,
                tag_name,
                attributes,
                children: element_children,
                ..
            } => {
                f.write_str("element(")?;
                {
                    let mut args = Indent::new(f);
                    write!(args, "\ntag: {:?},", tag_name.as_str())?;
                    write!(args, "\nattrs: {},", bracketed(attributes))?;
                    if !kind.is_void() || !element_children.is_empty() {
                        write!(args, "\nchildren: {},", bracketed(element_children))?;
                    }
                }
                f.write_str("\n)")
            }
            ParsedMarkup::Call {
                function_name,
                attributes,
                children: call_children,
                ..
            } => {
                write!(f, "{}(", function_name.as_str())?;
                {
                    let mut args = Indent::new(f);
                    write!(args, "\nattrs: {},", bracketed(attributes))?;
                    // `None` is a self-closing markup call, `<Foo/>`, and `Some`
                    // has an explicit closing tag, `<Foo></Foo>`, possibly with
                    // a body.
                    if let Some(call_children) = call_children {
                        write!(args, "\nchildren: {},", bracketed(call_children))?;
                    }
                }
                f.write_str("\n)")
            }
        }
    }
}
