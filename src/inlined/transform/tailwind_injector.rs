use crate::{
    document::CheapString,
    inlined::{InlinedEntrypointDeclaration, InlinedNode},
};

/// Transform that injects <style> tags with Tailwind CSS into the <head> element
/// Assumes HtmlStructureInjector has already run, so <head> exists
pub struct TailwindInjector;

impl TailwindInjector {
    /// Create a <style> element with the given CSS content
    fn create_style_element(css_content: &str) -> InlinedNode {
        let css_text = InlinedNode::Text {
            value: CheapString::new(css_content.to_string()),
        };

        InlinedNode::Html {
            tag_name: CheapString::new("style".to_string()),
            attributes: Vec::new(),
            children: vec![css_text],
        }
    }

    /// Recursively find and inject CSS into <head> elements
    fn inject_css_into_head(nodes: Vec<InlinedNode>, css_content: &str) -> Vec<InlinedNode> {
        nodes
            .into_iter()
            .map(|node| {
                match node {
                    InlinedNode::Html {
                        tag_name,
                        attributes,
                        children,
                    } => {
                        if tag_name.as_str() == "head" {
                            // Found <head> - inject <style> as last child
                            let mut new_children = children;
                            let style_element = Self::create_style_element(css_content);
                            new_children.push(style_element);

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
                                children: Self::inject_css_into_head(children, css_content),
                            }
                        }
                    }
                    InlinedNode::If {
                        condition,
                        children,
                    } => InlinedNode::If {
                        condition,
                        children: Self::inject_css_into_head(children, css_content),
                    },
                    InlinedNode::For {
                        var_name,
                        source,
                        children,
                    } => InlinedNode::For {
                        var_name,
                        source,
                        children: Self::inject_css_into_head(children, css_content),
                    },
                    InlinedNode::Let {
                        var,
                        value,
                        children,
                    } => InlinedNode::Let {
                        var,
                        value,
                        children: Self::inject_css_into_head(children, css_content),
                    },
                    other => other,
                }
            })
            .collect()
    }

    pub fn run(
        mut entrypoint: InlinedEntrypointDeclaration,
        css_content: Option<&str>,
    ) -> InlinedEntrypointDeclaration {
        // Only inject if CSS content is provided
        if let Some(css) = css_content {
            entrypoint.children = Self::inject_css_into_head(entrypoint.children, css);
        }
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

    /// Helper to check Tailwind injection
    fn check_tailwind_injection(
        entrypoint: InlinedEntrypointDeclaration,
        css: Option<&str>,
        expected: Expect,
    ) {
        // Format before
        let before = format_entrypoint_children(&entrypoint);

        // Apply transform
        let transformed_entrypoint = TailwindInjector::run(entrypoint, css);

        // Format after
        let after = format_entrypoint_children(&transformed_entrypoint);

        // Create output with before/after format
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);

        expected.assert_eq(&output);
    }

    #[test]
    fn should_inject_css_into_head() {
        let entrypoint = build_inlined("MainComp", [], |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        let css = ".text-red { color: red; }";

        check_tailwind_injection(
            entrypoint,
            Some(css),
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
                    <style>
                      .text-red { color: red; }
                    </style>
                  </head>
                  <body>
                    Hello World
                  </body>
                </html>
            "#]],
        );
    }
}
