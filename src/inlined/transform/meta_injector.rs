use std::sync::Arc;

use crate::document::CheapString;
use crate::inlined::{
    InlinedAttribute, InlinedAttributeValue, InlinedEntrypointDeclaration, InlinedNode,
};

/// Transform that injects meta tags into the <head> element
/// Assumes HtmlStructureInjector has already run, so <head> exists
pub struct MetaInjector;

impl MetaInjector {
    /// Create an attribute with a string value
    fn create_attribute(name: &str, value: &str) -> InlinedAttribute {
        InlinedAttribute {
            name: CheapString::new(name.to_string()),
            value: Some(InlinedAttributeValue::String(CheapString::new(
                value.to_string(),
            ))),
        }
    }

    /// Create the standard meta elements
    fn create_meta_elements() -> Vec<Arc<InlinedNode>> {
        vec![
            // <meta charset="utf-8">
            Arc::new(InlinedNode::Html {
                tag_name: CheapString::new("meta".to_string()),
                attributes: vec![Self::create_attribute("charset", "utf-8")],
                children: vec![],
            }),
            // <meta name="viewport" content="width=device-width,initial-scale=1">
            Arc::new(InlinedNode::Html {
                tag_name: CheapString::new("meta".to_string()),
                attributes: vec![
                    Self::create_attribute("content", "width=device-width, initial-scale=1"),
                    Self::create_attribute("name", "viewport"),
                ],
                children: vec![],
            }),
        ]
    }

    /// Recursively find and inject meta tags into <head> elements
    fn inject_meta_into_head(nodes: Vec<Arc<InlinedNode>>) -> Vec<Arc<InlinedNode>> {
        nodes
            .into_iter()
            .map(|node| match node.as_ref() {
                InlinedNode::Html {
                    tag_name,
                    attributes,
                    children,
                } => {
                    if tag_name.as_str() == "head" {
                        // Found <head> - inject meta tags at the beginning
                        let mut new_children = Self::create_meta_elements();
                        new_children.extend(children.clone());

                        Arc::new(InlinedNode::Html {
                            tag_name: tag_name.clone(),
                            attributes: attributes.clone(),
                            children: new_children,
                        })
                    } else {
                        // Recursively search other HTML elements
                        Arc::new(InlinedNode::Html {
                            tag_name: tag_name.clone(),
                            attributes: attributes.clone(),
                            children: Self::inject_meta_into_head(children.clone()),
                        })
                    }
                }
                InlinedNode::If {
                    condition,
                    children,
                } => Arc::new(InlinedNode::If {
                    condition: condition.clone(),
                    children: Self::inject_meta_into_head(children.clone()),
                }),
                InlinedNode::For {
                    var_name,
                    source,
                    children,
                } => Arc::new(InlinedNode::For {
                    var_name: var_name.clone(),
                    source: source.clone(),
                    children: Self::inject_meta_into_head(children.clone()),
                }),
                InlinedNode::Let { bindings, children } => Arc::new(InlinedNode::Let {
                    bindings: bindings.clone(),
                    children: Self::inject_meta_into_head(children.clone()),
                }),
                _ => node,
            })
            .collect()
    }

    pub fn run(mut entrypoint: InlinedEntrypointDeclaration) -> InlinedEntrypointDeclaration {
        entrypoint.children = Self::inject_meta_into_head(entrypoint.children);
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::inlined::builder::build_inlined_no_params;
    use expect_test::{Expect, expect};

    /// Helper to pretty-print entrypoint children for testing
    fn format_entrypoint_children(entrypoint: &InlinedEntrypointDeclaration) -> String {
        entrypoint
            .children
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Helper to check meta injection
    fn check_meta_injection(entrypoint: InlinedEntrypointDeclaration, expected: Expect) {
        // Format before
        let before = format_entrypoint_children(&entrypoint);

        // Apply transform
        let transformed_entrypoint = MetaInjector::run(entrypoint);

        // Format after
        let after = format_entrypoint_children(&transformed_entrypoint);

        // Create output with before/after format
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);

        expected.assert_eq(&output);
    }

    #[test]
    fn should_inject_meta_tags_into_head() {
        let entrypoint = build_inlined_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        check_meta_injection(
            entrypoint,
            expect![[r#"
                -- before --
                <html>
                  <head />
                  <body>
                    Hello World
                  </body>
                </html>
                -- after --
                <html>
                  <head>
                    <meta charset="utf-8" />
                    <meta content="width=device-width, initial-scale=1" name="viewport" />
                  </head>
                  <body>
                    Hello World
                  </body>
                </html>
            "#]],
        );
    }

    #[test]
    fn should_prepend_meta_tags_before_existing_head_content() {
        let entrypoint = build_inlined_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |t| {
                    t.html("title", vec![], |t| {
                        t.text("My Page");
                    });
                });
                t.html("body", vec![], |_| {});
            });
        });

        check_meta_injection(
            entrypoint,
            expect![[r#"
                -- before --
                <html>
                  <head>
                    <title>
                      My Page
                    </title>
                  </head>
                  <body />
                </html>
                -- after --
                <html>
                  <head>
                    <meta charset="utf-8" />
                    <meta content="width=device-width, initial-scale=1" name="viewport" />
                    <title>
                      My Page
                    </title>
                  </head>
                  <body />
                </html>
            "#]],
        );
    }
}
