use crate::{
    document::CheapString,
    hop::typing::{typed_ast::TypedViewDeclaration, typed_node::TypedNode},
};

/// Transform that injects proper HTML structure (<html>, <head>, <body>)
/// if they are missing from views
pub struct HtmlStructureInjector;

impl HtmlStructureInjector {
    /// Check if nodes contain an HTML element
    fn has_html_element(nodes: &[TypedNode]) -> bool {
        nodes.iter().any(
            |node| matches!(node, TypedNode::Html { tag_name, .. } if tag_name.as_str() == "html"),
        )
    }

    /// Check if nodes contain a head element (recursive search)
    fn has_head_element(nodes: &[TypedNode]) -> bool {
        for node in nodes {
            match node {
                TypedNode::Html {
                    tag_name, children, ..
                } => {
                    if tag_name.as_str() == "head" {
                        return true;
                    }
                    // Recursively search children
                    if Self::has_head_element(children) {
                        return true;
                    }
                }
                TypedNode::If { children, .. }
                | TypedNode::For { children, .. }
                | TypedNode::Let { children, .. }
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
    fn has_body_element(nodes: &[TypedNode]) -> bool {
        for node in nodes {
            match node {
                TypedNode::Html {
                    tag_name, children, ..
                } => {
                    if tag_name.as_str() == "body" {
                        return true;
                    }
                    // Recursively search children
                    if Self::has_body_element(children) {
                        return true;
                    }
                }
                TypedNode::If { children, .. }
                | TypedNode::For { children, .. }
                | TypedNode::Let { children, .. }
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
    fn create_html_element(tag_name: &str, children: Vec<TypedNode>) -> TypedNode {
        TypedNode::Html {
            tag_name: CheapString::new(tag_name.to_string()),
            attributes: Vec::new(),
            children,
        }
    }

    /// Find position after DOCTYPE and leading whitespace
    fn find_insert_position(nodes: &[TypedNode]) -> usize {
        let mut pos = 0;
        for (i, node) in nodes.iter().enumerate() {
            match node {
                TypedNode::Doctype { .. } => {
                    pos = i + 1;
                }
                TypedNode::Text { value, .. } if value.as_str().trim().is_empty() => {
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

    pub fn run(view: &mut TypedViewDeclaration) {
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
    fn ensure_head_and_body_in_html(nodes: Vec<TypedNode>) -> Vec<TypedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                TypedNode::Html {
                    tag_name,
                    attributes,
                    children,
                } => {
                    if tag_name.as_str() == "html" {
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

                        TypedNode::Html {
                            tag_name,
                            attributes,
                            children: new_children,
                        }
                    } else {
                        // Recursively process other HTML elements
                        TypedNode::Html {
                            tag_name,
                            attributes,
                            children: Self::ensure_head_and_body_in_html(children),
                        }
                    }
                }
                TypedNode::If {
                    condition,
                    children,
                } => TypedNode::If {
                    condition,
                    children: Self::ensure_head_and_body_in_html(children),
                },
                TypedNode::For {
                    var_name,
                    source,
                    children,
                } => TypedNode::For {
                    var_name,
                    source,
                    children: Self::ensure_head_and_body_in_html(children),
                },
                TypedNode::Let {
                    var,
                    value,
                    children,
                } => TypedNode::Let {
                    var,
                    value,
                    children: Self::ensure_head_and_body_in_html(children),
                },
                other => other,
            })
            .collect()
    }
}
