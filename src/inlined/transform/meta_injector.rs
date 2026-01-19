use crate::{
    document::CheapString,
    inlined::{InlinedAttribute, InlinedAttributeValue, InlinedEntrypointDeclaration, InlinedNode},
};
use std::collections::BTreeMap;

/// Transform that injects meta tags into the <head> element
/// Assumes HtmlStructureInjector has already run, so <head> exists
pub struct MetaInjector;

impl MetaInjector {
    /// Create an attribute with a string value
    fn create_attribute(name: &str, value: &str) -> (String, InlinedAttribute) {
        (
            name.to_string(),
            InlinedAttribute {
                name: name.to_string(),
                value: Some(InlinedAttributeValue::String(value.to_string())),
            },
        )
    }

    /// Create the standard meta elements
    fn create_meta_elements() -> Vec<InlinedNode> {
        vec![
            // <meta charset="utf-8">
            InlinedNode::Html {
                tag_name: CheapString::new("meta".to_string()),
                attributes: BTreeMap::from([Self::create_attribute("charset", "utf-8")]),
                children: vec![],
            },
            // <meta name="viewport" content="width=device-width,initial-scale=1">
            InlinedNode::Html {
                tag_name: CheapString::new("meta".to_string()),
                attributes: BTreeMap::from([
                    Self::create_attribute("name", "viewport"),
                    Self::create_attribute("content", "width=device-width, initial-scale=1"),
                ]),
                children: vec![],
            },
        ]
    }

    /// Recursively find and inject meta tags into <head> elements
    fn inject_meta_into_head(nodes: Vec<InlinedNode>) -> Vec<InlinedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                InlinedNode::Html {
                    tag_name,
                    attributes,
                    children,
                } => {
                    if tag_name.as_str() == "head" {
                        // Found <head> - inject meta tags at the beginning
                        let mut new_children = Self::create_meta_elements();
                        new_children.extend(children);

                        InlinedNode::Html {
                            tag_name,
                            attributes,
                            children: new_children,
                        }
                    } else {
                        // Recursively search other HTML elements
                        InlinedNode::Html {
                            tag_name,
                            attributes,
                            children: Self::inject_meta_into_head(children),
                        }
                    }
                }
                InlinedNode::If {
                    condition,
                    children,
                } => InlinedNode::If {
                    condition,
                    children: Self::inject_meta_into_head(children),
                },
                InlinedNode::For {
                    var_name,
                    source,
                    children,
                } => InlinedNode::For {
                    var_name,
                    source,
                    children: Self::inject_meta_into_head(children),
                },
                InlinedNode::Let {
                    var,
                    value,
                    children,
                } => InlinedNode::Let {
                    var,
                    value,
                    children: Self::inject_meta_into_head(children),
                },
                other => other,
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
    use crate::inlined::builder::build_inlined;
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
        let entrypoint = build_inlined("MainComp", [], |t| {
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
        let entrypoint = build_inlined("MainComp", [], |t| {
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
