use crate::{
    document::CheapString,
    hop::inlining::{InlinedNode, InlinedViewDeclaration},
    hop::typing::typed_node::{TypedAttribute, TypedAttributeValue},
};

/// Transform that injects a `<script type="module" src={src}></script>` element
/// referencing the bundled JS into the `<head>` element.
/// Assumes HtmlStructureInjector has already run, so `<head>` exists.
///
/// The JS is written to disk under the assets output directory with a
/// content-hashed filename during `hop build`, mirroring how the Tailwind CSS
/// `<link>` is injected by [`super::TailwindInjector`].
pub struct ScriptInjector;

impl ScriptInjector {
    fn create_script_element(src: &str) -> InlinedNode {
        let attr = |name: &str, value: &str| TypedAttribute {
            name: CheapString::new(name.to_string()),
            value: Some(TypedAttributeValue::String(CheapString::new(
                value.to_string(),
            ))),
        };

        InlinedNode::Html {
            tag_name: CheapString::new("script".to_string()),
            attributes: vec![attr("type", "module"), attr("src", src)],
            children: vec![],
        }
    }

    /// Recursively find and inject the script reference into `<head>` elements.
    fn inject_into_head(nodes: Vec<InlinedNode>, src: &str) -> Vec<InlinedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                InlinedNode::Html {
                    tag_name,
                    attributes,
                    children,
                } => {
                    if tag_name.as_str() == "head" {
                        let mut new_children = children;
                        new_children.push(Self::create_script_element(src));

                        InlinedNode::Html {
                            tag_name,
                            attributes,
                            children: new_children,
                        }
                    } else {
                        InlinedNode::Html {
                            tag_name,
                            attributes,
                            children: Self::inject_into_head(children, src),
                        }
                    }
                }
                InlinedNode::If {
                    condition,
                    children,
                } => InlinedNode::If {
                    condition,
                    children: Self::inject_into_head(children, src),
                },
                InlinedNode::For {
                    var_name,
                    source,
                    children,
                } => InlinedNode::For {
                    var_name,
                    source,
                    children: Self::inject_into_head(children, src),
                },
                InlinedNode::Let {
                    var,
                    value,
                    children,
                } => InlinedNode::Let {
                    var,
                    value,
                    children: Self::inject_into_head(children, src),
                },
                other => other,
            })
            .collect()
    }

    pub fn run(view: &mut InlinedViewDeclaration, src: Option<&str>) {
        if let Some(src) = src {
            view.children = Self::inject_into_head(std::mem::take(&mut view.children), src);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::inlining::builder::build_inlined_view_no_params;
    use expect_test::{Expect, expect};

    /// Helper to pretty-print view children for testing
    fn format_view_children(view: &InlinedViewDeclaration) -> String {
        view.children
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn check_script_injection(
        mut view: InlinedViewDeclaration,
        src: Option<&str>,
        expected: Expect,
    ) {
        let before = format_view_children(&view);
        ScriptInjector::run(&mut view, src);
        let after = format_view_children(&view);
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_inject_script_into_head() {
        let view = build_inlined_view_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        check_script_injection(
            view,
            Some("/scripts-deadbeef.js"),
            expect![[r#"
                -- before --
                <html>
                  <head></head>
                  <body>
                    Hello World
                  </body>
                </html>
                -- after --
                <html>
                  <head>
                    <script type="module" src="/scripts-deadbeef.js"></script>
                  </head>
                  <body>
                    Hello World
                  </body>
                </html>
            "#]],
        );
    }

    #[test]
    fn should_not_inject_when_src_is_none() {
        let view = build_inlined_view_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        check_script_injection(
            view,
            None,
            expect![[r#"
                -- before --
                <html>
                  <head></head>
                  <body>
                    Hello World
                  </body>
                </html>
                -- after --
                <html>
                  <head></head>
                  <body>
                    Hello World
                  </body>
                </html>
            "#]],
        );
    }
}
