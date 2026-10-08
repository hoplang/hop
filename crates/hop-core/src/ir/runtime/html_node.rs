use crate::html::{HtmlElementKind, write_escaped_html};
use crate::symbols::attribute_name::AttributeName;

/// A node of an evaluated Html value.
#[derive(Debug, Clone, PartialEq)]
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
        children: Vec<HtmlNode>,
    },
}

/// An attribute that renders. The value is unescaped and escapes when
/// written. A boolean attribute has no value.
#[derive(Debug, Clone, PartialEq)]
pub struct HtmlAttribute {
    pub name: AttributeName,
    pub value: Option<String>,
}

/// Write nodes as markup, byte for byte what the writer IR produces.
pub fn write_html(nodes: &[HtmlNode], out: &mut String) {
    for node in nodes {
        match node {
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
                    write_html(children, out);
                    out.push_str("</");
                    out.push_str(element.as_str());
                    out.push('>');
                }
            }
        }
    }
}
