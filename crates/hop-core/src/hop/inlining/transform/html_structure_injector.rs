use crate::{
    hop::inlining::{InlinedNode, InlinedViewDeclaration},
    html::HtmlElement,
};

/// Transform that injects proper HTML structure (<html>, <head>, <body>)
/// if they are missing from views
pub struct HtmlStructureInjector;

impl HtmlStructureInjector {
    /// Check if nodes contain an HTML element
    fn has_html_element(nodes: &[InlinedNode]) -> bool {
        nodes.iter().any(
            |node| matches!(node, InlinedNode::Html { element, .. } if *element == HtmlElement::Html),
        )
    }

    /// Check if nodes contain a head element (recursive search)
    fn has_head_element(nodes: &[InlinedNode]) -> bool {
        for node in nodes {
            match node {
                InlinedNode::Html {
                    element, children, ..
                } => {
                    if *element == HtmlElement::Head {
                        return true;
                    }
                    // Recursively search children
                    if Self::has_head_element(children) {
                        return true;
                    }
                }
                InlinedNode::If { children, .. }
                | InlinedNode::For { children, .. }
                | InlinedNode::Let { children, .. }
                    if Self::has_head_element(children) =>
                {
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    /// Check if nodes contain a body element (recursive search)
    fn has_body_element(nodes: &[InlinedNode]) -> bool {
        for node in nodes {
            match node {
                InlinedNode::Html {
                    element, children, ..
                } => {
                    if *element == HtmlElement::Body {
                        return true;
                    }
                    // Recursively search children
                    if Self::has_body_element(children) {
                        return true;
                    }
                }
                InlinedNode::If { children, .. }
                | InlinedNode::For { children, .. }
                | InlinedNode::Let { children, .. }
                    if Self::has_body_element(children) =>
                {
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    /// Create an empty HTML element
    fn create_html_element(tag_name: &str, children: Vec<InlinedNode>) -> InlinedNode {
        InlinedNode::Html {
            element: HtmlElement::parse(tag_name)
                .expect("create_html_element called with an unrecognized tag name"),
            attributes: Vec::new(),
            children,
        }
    }

    /// Find position after DOCTYPE and leading whitespace
    fn find_insert_position(nodes: &[InlinedNode]) -> usize {
        let mut pos = 0;
        for (i, node) in nodes.iter().enumerate() {
            match node {
                InlinedNode::Doctype { .. } => {
                    pos = i + 1;
                }
                InlinedNode::Text { value, .. } if value.as_str().trim().is_empty() => {
                    // Skip whitespace if we haven't found non-whitespace content yet
                    if pos == i {
                        pos = i + 1;
                    }
                }
                _ => {
                    // Found non-whitespace, non-doctype content
                    break;
                }
            }
        }
        pos
    }

    pub fn run(view: &mut InlinedViewDeclaration) {
        // If there's no <html> element, wrap everything in proper structure
        if !Self::has_html_element(&view.children) {
            // Find where to insert (after DOCTYPE and leading whitespace)
            let insert_pos = Self::find_insert_position(&view.children);

            let mut prefix = std::mem::take(&mut view.children);

            // Split nodes: keep DOCTYPE and leading whitespace, wrap the rest
            let content = prefix.split_off(insert_pos);

            // Create head if needed
            let head = if Self::has_head_element(&content) {
                // Head exists somewhere in content, don't add another
                vec![]
            } else {
                vec![Self::create_html_element("head", vec![])]
            };

            // Create body with existing content
            let body = if Self::has_body_element(&content) {
                // Body exists, use content as-is
                content
            } else {
                // Wrap content in body
                vec![Self::create_html_element("body", content)]
            };

            // Combine head and body
            let mut html_children = head;
            html_children.extend(body);

            // Create html element
            let html_element = Self::create_html_element("html", html_children);

            // Reconstruct view: prefix + html + any trailing content
            let mut new_children = prefix;
            new_children.push(html_element);

            view.children = new_children;
        } else {
            // HTML exists, but check if head/body need to be added inside it
            view.children = Self::ensure_head_and_body_in_html(std::mem::take(&mut view.children));
        }
    }

    /// Recursively ensure head and body exist within HTML elements
    fn ensure_head_and_body_in_html(nodes: Vec<InlinedNode>) -> Vec<InlinedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                InlinedNode::Html {
                    element,
                    attributes,
                    children,
                } => {
                    if element == HtmlElement::Html {
                        let mut new_children = Vec::new();

                        // Add head if missing
                        if !Self::has_head_element(&children) {
                            new_children.push(Self::create_html_element("head", vec![]));
                        }

                        // Add existing children or wrap in body if no body exists
                        if Self::has_body_element(&children) {
                            new_children.extend(children);
                        } else {
                            new_children.push(Self::create_html_element("body", children));
                        }

                        InlinedNode::Html {
                            element,
                            attributes,
                            children: new_children,
                        }
                    } else {
                        // Recursively process other HTML elements
                        InlinedNode::Html {
                            element,
                            attributes,
                            children: Self::ensure_head_and_body_in_html(children),
                        }
                    }
                }
                InlinedNode::If {
                    condition,
                    children,
                } => InlinedNode::If {
                    condition,
                    children: Self::ensure_head_and_body_in_html(children),
                },
                InlinedNode::For {
                    var_name,
                    source,
                    children,
                } => InlinedNode::For {
                    var_name,
                    source,
                    children: Self::ensure_head_and_body_in_html(children),
                },
                InlinedNode::Let {
                    var,
                    value,
                    children,
                } => InlinedNode::Let {
                    var,
                    value,
                    children: Self::ensure_head_and_body_in_html(children),
                },
                other => other,
            })
            .collect()
    }
}
