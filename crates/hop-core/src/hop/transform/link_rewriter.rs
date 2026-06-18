use crate::{
    document::CheapString,
    dop::patterns::Match,
    hop::typing::{
        typed_ast::{TypedComponentDeclaration, TypedViewDeclaration},
        typed_node::{TypedAttribute, TypedAttributeValue, TypedNode},
    },
};

/// Transform that replaces all `href` attributes on `<a>` elements with `"#"`.
/// Used in dev mode to prevent navigation away from the preview.
pub struct LinkRewriter;

impl LinkRewriter {
    fn rewrite_links(nodes: Vec<TypedNode>) -> Vec<TypedNode> {
        nodes
            .into_iter()
            .map(|node| match node {
                TypedNode::Html {
                    tag_name,
                    attributes,
                    children,
                } => {
                    let new_attributes = if tag_name.as_str() == "a" {
                        attributes
                            .into_iter()
                            .map(|attr| {
                                if attr.name.as_str() == "href" {
                                    TypedAttribute {
                                        name: attr.name,
                                        value: Some(TypedAttributeValue::String(CheapString::new(
                                            "#".to_string(),
                                        ))),
                                    }
                                } else {
                                    attr
                                }
                            })
                            .collect()
                    } else {
                        attributes
                    };

                    TypedNode::Html {
                        tag_name,
                        attributes: new_attributes,
                        children: Self::rewrite_links(children),
                    }
                }
                TypedNode::If {
                    condition,
                    children,
                } => TypedNode::If {
                    condition,
                    children: Self::rewrite_links(children),
                },
                TypedNode::For {
                    var_name,
                    source,
                    children,
                } => TypedNode::For {
                    var_name,
                    source,
                    children: Self::rewrite_links(children),
                },
                TypedNode::Let {
                    var,
                    value,
                    children,
                } => TypedNode::Let {
                    var,
                    value,
                    children: Self::rewrite_links(children),
                },
                TypedNode::LetRecordDestructure {
                    subject,
                    bindings,
                    children,
                } => TypedNode::LetRecordDestructure {
                    subject,
                    bindings,
                    children: Self::rewrite_links(children),
                },
                TypedNode::ComponentInvocation {
                    component_name,
                    component_type,
                    args,
                    children,
                } => TypedNode::ComponentInvocation {
                    component_name,
                    component_type,
                    args,
                    children: Self::rewrite_links(children),
                },
                TypedNode::Match { match_ } => TypedNode::Match {
                    match_: match match_ {
                        Match::Enum { subject, arms } => Match::Enum {
                            subject,
                            arms: arms
                                .into_iter()
                                .map(|arm| crate::dop::patterns::EnumMatchArm {
                                    pattern: arm.pattern,
                                    bindings: arm.bindings,
                                    body: Self::rewrite_links(arm.body),
                                })
                                .collect(),
                        },
                        Match::Bool {
                            subject,
                            true_body,
                            false_body,
                        } => Match::Bool {
                            subject,
                            true_body: Box::new(Self::rewrite_links(*true_body)),
                            false_body: Box::new(Self::rewrite_links(*false_body)),
                        },
                        Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body,
                            none_arm_body,
                        } => Match::Option {
                            subject,
                            some_arm_binding,
                            some_arm_body: Box::new(Self::rewrite_links(*some_arm_body)),
                            none_arm_body: Box::new(Self::rewrite_links(*none_arm_body)),
                        },
                    },
                },
                node @ (TypedNode::Text { .. }
                | TypedNode::TextExpression { .. }
                | TypedNode::Doctype { .. }) => node,
            })
            .collect()
    }

    pub fn run(view: &mut TypedViewDeclaration) {
        view.children = Self::rewrite_links(std::mem::take(&mut view.children));
    }

    pub fn run_component(component: &mut TypedComponentDeclaration) {
        component.children = Self::rewrite_links(std::mem::take(&mut component.children));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hop::transform::builder::build_typed_view_no_params;
    use expect_test::{Expect, expect};

    fn format_view_children(view: &TypedViewDeclaration) -> String {
        view.children
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn check_link_rewrite(mut view: TypedViewDeclaration, expected: Expect) {
        let before = format_view_children(&view);
        LinkRewriter::run(&mut view);
        let after = format_view_children(&view);
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_rewrite_href_to_hash() {
        let view = build_typed_view_no_params("Nav", |t| {
            t.html("a", vec![("href", t.attr_str("/about"))], |t| {
                t.text("About");
            });
        });

        check_link_rewrite(
            view,
            expect![[r##"
                -- before --
                <a href="/about">
                  About
                </a>
                -- after --
                <a href="#">
                  About
                </a>
            "##]],
        );
    }

    #[test]
    fn should_rewrite_nested_links() {
        let view = build_typed_view_no_params("Nav", |t| {
            t.html("nav", vec![], |t| {
                t.html("a", vec![("href", t.attr_str("/home"))], |t| {
                    t.text("Home");
                });
                t.html("a", vec![("href", t.attr_str("/about"))], |t| {
                    t.text("About");
                });
            });
        });

        check_link_rewrite(
            view,
            expect![[r##"
                -- before --
                <nav>
                  <a href="/home">
                    Home
                  </a>
                  <a href="/about">
                    About
                  </a>
                </nav>
                -- after --
                <nav>
                  <a href="#">
                    Home
                  </a>
                  <a href="#">
                    About
                  </a>
                </nav>
            "##]],
        );
    }

    #[test]
    fn should_preserve_other_attributes() {
        let view = build_typed_view_no_params("Nav", |t| {
            t.html(
                "a",
                vec![
                    ("href", t.attr_str("/about")),
                    ("class", t.attr_str("nav-link")),
                ],
                |t| {
                    t.text("About");
                },
            );
        });

        check_link_rewrite(
            view,
            expect![[r##"
                -- before --
                <a href="/about" class="nav-link">
                  About
                </a>
                -- after --
                <a href="#" class="nav-link">
                  About
                </a>
            "##]],
        );
    }

    #[test]
    fn should_rewrite_links_inside_component_reference() {
        use crate::dop::Type;
        use crate::symbols::type_name::TypeName;
        use std::sync::Arc;

        let mut view = build_typed_view_no_params("Page", |t| {
            t.html("div", vec![], |_t| {});
        });
        // Replace the div with a ComponentInvocation containing a link
        view.children = vec![TypedNode::ComponentInvocation {
            component_name: TypeName::new("Nav").unwrap(),
            component_type: Arc::new(Type::String),
            args: vec![],
            children: vec![TypedNode::Html {
                tag_name: CheapString::new("a".to_string()),
                attributes: vec![TypedAttribute {
                    name: CheapString::new("href".to_string()),
                    value: Some(TypedAttributeValue::String(CheapString::new(
                        "/about".to_string(),
                    ))),
                }],
                children: vec![TypedNode::Text {
                    value: CheapString::new("About".to_string()),
                }],
            }],
        }];

        check_link_rewrite(
            view,
            expect![[r##"
                -- before --
                <Nav>
                  <a href="/about">
                    About
                  </a>
                </Nav>
                -- after --
                <Nav>
                  <a href="#">
                    About
                  </a>
                </Nav>
            "##]],
        );
    }

    #[test]
    fn should_rewrite_links_inside_bool_match() {
        use crate::dop::Type;

        let view =
            crate::hop::transform::builder::build_typed_ast("Page", [("flag", Type::Bool)], |t| {
                t.bool_match_node(
                    t.var_expr("flag"),
                    |t| {
                        t.html("a", vec![("href", t.attr_str("/yes"))], |t| {
                            t.text("Yes");
                        });
                    },
                    |t| {
                        t.html("a", vec![("href", t.attr_str("/no"))], |t| {
                            t.text("No");
                        });
                    },
                );
            });

        check_link_rewrite(
            view,
            expect![[r##"
                -- before --
                <match {flag}>
                  <case {true}>
                    <a href="/yes">
                      Yes
                    </a>
                  </case>
                  <case {false}>
                    <a href="/no">
                      No
                    </a>
                  </case>
                </match>
                -- after --
                <match {flag}>
                  <case {true}>
                    <a href="#">
                      Yes
                    </a>
                  </case>
                  <case {false}>
                    <a href="#">
                      No
                    </a>
                  </case>
                </match>
            "##]],
        );
    }

    #[test]
    fn should_not_modify_non_link_elements() {
        let view = build_typed_view_no_params("Page", |t| {
            t.html("div", vec![("class", t.attr_str("container"))], |t| {
                t.html("p", vec![], |t| {
                    t.text("Hello");
                });
            });
        });

        check_link_rewrite(
            view,
            expect![[r#"
                -- before --
                <div class="container">
                  <p>
                    Hello
                  </p>
                </div>
                -- after --
                <div class="container">
                  <p>
                    Hello
                  </p>
                </div>
            "#]],
        );
    }
}
