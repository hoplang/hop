use std::rc::Rc;

use crate::html::{HtmlElementKind, write_escaped_html};
use crate::symbols::attribute_name::AttributeName;

#[derive(Debug, Clone)]
pub struct Html {
    node: Rc<HtmlNode>,
    size: usize,
}

/// A node of an evaluated Html value.
#[derive(Debug)]
pub enum HtmlNode {
    /// Text that renders as written. What a HtmlText evaluates to.
    Text(String),

    /// A string that renders escaped. What a HtmlEscape evaluates to.
    Escape(String),

    /// An element with the attributes that render, in the order they
    /// render.
    Element {
        element: HtmlElementKind,
        attributes: Vec<HtmlAttribute>,
        children: Html,
    },

    Concat(Vec<Html>),
}

/// An attribute that renders. The value is unescaped and escapes when
/// written. A boolean attribute has no value.
#[derive(Debug, Clone, PartialEq)]
pub struct HtmlAttribute {
    pub name: AttributeName,
    pub value: Option<String>,
}

impl Html {
    pub fn new(node: HtmlNode) -> Self {
        let size = match &node {
            HtmlNode::Text(text) | HtmlNode::Escape(text) => text.len().max(1),
            HtmlNode::Element {
                attributes,
                children,
                ..
            } => {
                let values: usize = attributes
                    .iter()
                    .filter_map(|attribute| attribute.value.as_ref())
                    .map(String::len)
                    .sum();
                1 + values + children.size
            }
            HtmlNode::Concat(parts) => 1 + parts.iter().map(|part| part.size).sum::<usize>(),
        };
        Html {
            node: Rc::new(node),
            size,
        }
    }

    pub fn empty() -> Self {
        Html::new(HtmlNode::Concat(Vec::new()))
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn write(&self, out: &mut String) {
        match &*self.node {
            HtmlNode::Text(content) => out.push_str(content),
            HtmlNode::Escape(text) => write_escaped_html(text, out),
            HtmlNode::Element {
                element,
                attributes,
                children,
            } => {
                out.push('<');
                out.push_str(element.as_str());
                for attribute in attributes {
                    out.push(' ');
                    out.push_str(attribute.name.as_str());
                    if let Some(value) = &attribute.value {
                        out.push_str("=\"");
                        write_escaped_html(value, out);
                        out.push('"');
                    }
                }
                out.push('>');
                if !element.is_void() {
                    children.write(out);
                    out.push_str("</");
                    out.push_str(element.as_str());
                    out.push('>');
                }
            }
            HtmlNode::Concat(parts) => {
                for part in parts {
                    part.write(out);
                }
            }
        }
    }
}
