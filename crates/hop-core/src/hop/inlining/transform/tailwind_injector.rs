use crate::{
    document::CheapString,
    hop::inlining::{InlinedNode, InlinedViewDeclaration},
    hop::typing::typed_node::{TypedAttribute, TypedAttributeValue},
    html::HtmlElement,
};

/// How the generated Tailwind CSS should be referenced from the rendered <head>.
#[derive(Debug, Clone, Copy)]
pub enum TailwindInjection<'a> {
    /// Inline the CSS as a `<style>{css}</style>` element. Used by `hop dev`
    /// so hot-reload can ship CSS through the same render pipeline.
    Inline(&'a str),
    /// Reference an external stylesheet via `<link rel="stylesheet" href={href} />`.
    /// Used by `hop build`, where the CSS is written to disk under the assets
    /// output directory with a content-hashed filename.
    Link { href: &'a str },
}

/// Transform that injects either a `<style>` or a `<link>` referencing the
/// generated Tailwind CSS into the `<head>` element.
/// Assumes HtmlStructureInjector has already run, so `<head>` exists.
pub struct TailwindInjector;

impl TailwindInjector {
    fn create_style_element(css_content: &str) -> InlinedNode {
        let css_text = InlinedNode::Text {
            value: CheapString::new(css_content.to_string()),
        };

        InlinedNode::Html {
            element: HtmlElement::Style,
            attributes: Vec::new(),
            children: vec![css_text],
        }
    }

    fn create_link_element(href: &str) -> InlinedNode {
        let attr = |name: &str, value: &str| TypedAttribute {
            name: CheapString::new(name.to_string()),
            value: Some(TypedAttributeValue::String(CheapString::new(
                value.to_string(),
            ))),
        };

        InlinedNode::Html {
            element: HtmlElement::Link,
            attributes: vec![attr("rel", "stylesheet"), attr("href", href)],
            children: vec![],
        }
    }

    fn create_injection_element(injection: TailwindInjection<'_>) -> InlinedNode {
        match injection {
            TailwindInjection::Inline(css) => Self::create_style_element(css),
            TailwindInjection::Link { href } => Self::create_link_element(href),
        }
    }

    /// Recursively find and inject the CSS reference into `<head>` elements.
    fn inject_into_head(
        nodes: Vec<InlinedNode>,
        injection: TailwindInjection<'_>,
    ) -> Vec<InlinedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                InlinedNode::Html {
                    element,
                    attributes,
                    children,
                } => {
                    if element == HtmlElement::Head {
                        let mut new_children = children;
                        new_children.push(Self::create_injection_element(injection));

                        InlinedNode::Html {
                            element,
                            attributes,
                            children: new_children,
                        }
                    } else {
                        InlinedNode::Html {
                            element,
                            attributes,
                            children: Self::inject_into_head(children, injection),
                        }
                    }
                }
                InlinedNode::If {
                    condition,
                    children,
                } => InlinedNode::If {
                    condition,
                    children: Self::inject_into_head(children, injection),
                },
                InlinedNode::For {
                    var_name,
                    source,
                    children,
                } => InlinedNode::For {
                    var_name,
                    source,
                    children: Self::inject_into_head(children, injection),
                },
                InlinedNode::Let {
                    var,
                    value,
                    children,
                } => InlinedNode::Let {
                    var,
                    value,
                    children: Self::inject_into_head(children, injection),
                },
                other => other,
            })
            .collect()
    }

    pub fn run(view: &mut InlinedViewDeclaration, injection: Option<TailwindInjection<'_>>) {
        if let Some(injection) = injection {
            view.children = Self::inject_into_head(std::mem::take(&mut view.children), injection);
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

    fn check_tailwind_injection(
        mut view: InlinedViewDeclaration,
        injection: Option<TailwindInjection<'_>>,
        expected: Expect,
    ) {
        let before = format_view_children(&view);
        TailwindInjector::run(&mut view, injection);
        let after = format_view_children(&view);
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_inject_style_into_head() {
        let view = build_inlined_view_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        let css = ".text-red { color: red; }";

        check_tailwind_injection(
            view,
            Some(TailwindInjection::Inline(css)),
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

    #[test]
    fn should_inject_link_into_head() {
        let view = build_inlined_view_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        check_tailwind_injection(
            view,
            Some(TailwindInjection::Link {
                href: "/styles-deadbeef.css",
            }),
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
                    <link rel="stylesheet" href="/styles-deadbeef.css"></link>
                  </head>
                  <body>
                    Hello World
                  </body>
                </html>
            "#]],
        );
    }
}
