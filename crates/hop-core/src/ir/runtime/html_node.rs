use std::rc::Rc;

use crate::html::{HtmlElementKind, write_escaped_html};
use crate::ir::runtime::eval_error::EvalError;
use crate::ir::runtime::state::NodeId;
use crate::ir::runtime::value::Closure;
use crate::symbols::attribute_name::AttributeName;

/// The largest size an Html value may have.
pub const MAX_HTML_SIZE: usize = 10_000_000;

#[derive(Debug, Clone)]
pub struct Html<'a> {
    node: Rc<HtmlNode<'a>>,
    size: usize,
}

#[derive(Debug)]
pub enum HtmlNode<'a> {
    Text(String),
    Escape(Rc<str>),
    Element {
        element: HtmlElementKind,
        attributes: Vec<HtmlAttribute>,
        children: Html<'a>,
        #[cfg_attr(not(test), allow(dead_code))]
        on_click: Option<Rc<Closure<'a>>>,
        #[cfg_attr(not(test), allow(dead_code))]
        node: NodeId,
    },
    Concat(Vec<Html<'a>>),
}

/// An attribute that renders. The value is unescaped and escapes when
/// written. A boolean attribute has no value.
#[derive(Debug, Clone, PartialEq)]
pub struct HtmlAttribute {
    pub name: AttributeName,
    pub value: Option<Rc<str>>,
}

impl<'a> Html<'a> {
    /// The node as Html, unless it exceeds the Html size limit.
    pub fn new(node: HtmlNode<'a>) -> Result<Self, EvalError> {
        let size = match &node {
            HtmlNode::Text(text) => text.len().max(1),
            HtmlNode::Escape(text) => text.len().max(1),
            HtmlNode::Element {
                attributes,
                children,
                ..
            } => {
                let values: usize = attributes
                    .iter()
                    .filter_map(|attribute| attribute.value.as_ref())
                    .map(|value| value.len())
                    .sum();
                1 + values + children.size
            }
            HtmlNode::Concat(parts) => 1 + parts.iter().map(|part| part.size).sum::<usize>(),
        };
        if size > MAX_HTML_SIZE {
            return Err(EvalError::HtmlSizeLimit {
                limit: MAX_HTML_SIZE,
            });
        }
        Ok(Html {
            node: Rc::new(node),
            size,
        })
    }

    #[cfg(test)]
    pub fn node(&self) -> &HtmlNode<'a> {
        &self.node
    }

    pub fn empty() -> Self {
        Html {
            node: Rc::new(HtmlNode::Concat(Vec::new())),
            size: 1,
        }
    }

    pub fn write(&self, out: &mut String) {
        match &*self.node {
            HtmlNode::Text(content) => out.push_str(content),
            HtmlNode::Escape(text) => write_escaped_html(text, out),
            HtmlNode::Element {
                element,
                attributes,
                children,
                ..
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
