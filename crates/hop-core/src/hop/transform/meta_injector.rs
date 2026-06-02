use crate::document::CheapString;
use crate::hop::typing::{
    typed_ast::TypedViewDeclaration,
    typed_node::{TypedAttribute, TypedAttributeValue, TypedNode},
};

/// Transform that injects meta tags into the <head> element
/// Assumes HtmlStructureInjector has already run, so <head> exists
pub struct MetaInjector;

impl MetaInjector {
    /// Create an attribute with a string value
    fn create_attribute(name: &str, value: &str) -> TypedAttribute {
        TypedAttribute {
            name: CheapString::new(name.to_string()),
            value: Some(TypedAttributeValue::String(CheapString::new(
                value.to_string(),
            ))),
        }
    }

    /// Create the standard meta elements
    fn create_meta_elements() -> Vec<TypedNode> {
        vec![
            // <meta charset="utf-8">
            TypedNode::Html {
                tag_name: CheapString::new("meta".to_string()),
                attributes: vec![Self::create_attribute("charset", "utf-8")],
                children: vec![],
            },
            // <meta name="viewport" content="width=device-width,initial-scale=1">
            TypedNode::Html {
                tag_name: CheapString::new("meta".to_string()),
                attributes: vec![
                    Self::create_attribute("content", "width=device-width, initial-scale=1"),
                    Self::create_attribute("name", "viewport"),
                ],
                children: vec![],
            },
        ]
    }

    /// Recursively find and inject meta tags into <head> elements
    fn inject_meta_into_head(nodes: Vec<TypedNode>) -> Vec<TypedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                TypedNode::Html {
                    tag_name,
                    attributes,
                    children,
                } => {
                    if tag_name.as_str() == "head" {
                        // Found <head> - inject meta tags at the beginning
                        let mut new_children = Self::create_meta_elements();
                        new_children.extend(children);

                        TypedNode::Html {
                            tag_name,
                            attributes,
                            children: new_children,
                        }
                    } else {
                        // Recursively search other HTML elements
                        TypedNode::Html {
                            tag_name,
                            attributes,
                            children: Self::inject_meta_into_head(children),
                        }
                    }
                }
                TypedNode::If {
                    condition,
                    children,
                } => TypedNode::If {
                    condition,
                    children: Self::inject_meta_into_head(children),
                },
                TypedNode::For {
                    var_name,
                    source,
                    children,
                } => TypedNode::For {
                    var_name,
                    source,
                    children: Self::inject_meta_into_head(children),
                },
                TypedNode::Let {
                    var,
                    value,
                    children,
                } => TypedNode::Let {
                    var,
                    value,
                    children: Self::inject_meta_into_head(children),
                },
                other => other,
            })
            .collect()
    }

    pub fn run(view: &mut TypedViewDeclaration) {
        view.children = Self::inject_meta_into_head(std::mem::take(&mut view.children));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::transform::builder::build_typed_view_no_params;
    use expect_test::{Expect, expect};

    /// Helper to pretty-print view children for testing
    fn format_view_children(view: &TypedViewDeclaration) -> String {
        view.children
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Helper to check meta injection
    fn check_meta_injection(mut view: TypedViewDeclaration, expected: Expect) {
        // Format before
        let before = format_view_children(&view);

        // Apply transform
        MetaInjector::run(&mut view);

        // Format after
        let after = format_view_children(&view);

        // Create output with before/after format
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);

        expected.assert_eq(&output);
    }

    #[test]
    fn should_inject_meta_tags_into_head() {
        let view = build_typed_view_no_params("MainComp", |t| {
            t.html("html", vec![], |t| {
                t.html("head", vec![], |_| {});
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
            });
        });

        check_meta_injection(
            view,
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
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
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
        let view = build_typed_view_no_params("MainComp", |t| {
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
            view,
            expect![[r#"
                -- before --
                <html>
                  <head>
                    <title>
                      My Page
                    </title>
                  </head>
                  <body></body>
                </html>
                -- after --
                <html>
                  <head>
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                    <title>
                      My Page
                    </title>
                  </head>
                  <body></body>
                </html>
            "#]],
        );
    }
}
