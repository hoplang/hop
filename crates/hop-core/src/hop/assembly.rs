use crate::document::CheapString;
use crate::hop::typing::TypedExpr;
use crate::hop::typing::typed_ast::{TypedPageDeclaration, TypedParameter};
use crate::hop::typing::{TypedAttribute, TypedAttributeValue};
use crate::html::HtmlElement;
use crate::symbols::type_name::TypeName;
use pretty::BoxDoc;
use std::fmt;

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

/// A page whose `head`/`body` have been merged into a single document tree.
#[derive(Debug, Clone)]
pub struct AssembledPageDeclaration {
    pub name: TypeName,
    pub params: Vec<TypedParameter>,
    pub body: TypedExpr,
}

impl AssembledPageDeclaration {
    /// Takes the page's body as-is, without wrapping it in a document
    /// structure. The head is dropped by design: callers on this path
    /// (`skip_html_structure`, used to keep head/body assembly noise out of
    /// tests) never emit head content.
    pub fn from_body_only(page: TypedPageDeclaration) -> Self {
        Self {
            name: page.name,
            params: page.params,
            body: page.body,
        }
    }
}

impl AssembledPageDeclaration {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let params_doc = BoxDoc::intersperse(
            self.params.iter().map(|param| {
                BoxDoc::text(param.var_name.as_str())
                    .append(BoxDoc::text(": "))
                    .append(param.var_type.to_doc())
            }),
            BoxDoc::text(", "),
        );

        BoxDoc::text("page")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(params_doc)
            .append(BoxDoc::text(") {"))
            .append(
                BoxDoc::line()
                    .append(self.body.to_doc())
                    .nest(2)
                    .append(BoxDoc::line()),
            )
            .append(BoxDoc::text("}"))
    }
}

impl fmt::Display for AssembledPageDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_doc().pretty(60))
    }
}

/// Injects the standard meta tags, and optionally a Tailwind CSS reference
/// and a bundled-script reference, into the page's head, then assembles the
/// head and body into the final document structure.
pub fn assemble_page(
    page: TypedPageDeclaration,
    tailwind_injection: Option<TailwindInjection<'_>>,
    script_src: Option<&str>,
) -> AssembledPageDeclaration {
    let TypedPageDeclaration {
        name,
        head,
        body,
        params,
    } = page;

    let mut head = fragment_nodes(head);
    head.splice(0..0, create_meta_elements());
    if let Some(injection) = tailwind_injection {
        head.push(create_tailwind_element(injection));
    }
    if let Some(src) = script_src {
        head.push(create_script_element(src));
    }

    let doctype = TypedExpr::FragmentRaw {
        value: CheapString::new("<!doctype html>".to_string()),
    };
    let html = create_html_element(
        HtmlElement::Html,
        vec![
            create_html_element(HtmlElement::Head, head),
            create_html_element(HtmlElement::Body, fragment_nodes(body)),
        ],
    );

    AssembledPageDeclaration {
        name,
        params,
        body: TypedExpr::FragmentConcat {
            nodes: vec![doctype, html],
        },
    }
}

fn fragment_nodes(expr: TypedExpr) -> Vec<TypedExpr> {
    match expr {
        TypedExpr::FragmentConcat { nodes } => nodes,
        other => vec![other],
    }
}

fn create_html_element(element: HtmlElement, children: Vec<TypedExpr>) -> TypedExpr {
    TypedExpr::FragmentHtml {
        element,
        attrs: Box::new(TypedExpr::AttrsLiteral {
            attributes: Vec::new(),
        }),
        children: Box::new(TypedExpr::FragmentConcat { nodes: children }),
    }
}

fn create_attribute(name: &str, value: &str) -> TypedAttribute {
    TypedAttribute {
        name: CheapString::new(name.to_string()),
        value: Some(TypedAttributeValue::String(CheapString::new(
            value.to_string(),
        ))),
    }
}

fn create_meta_elements() -> Vec<TypedExpr> {
    vec![
        TypedExpr::FragmentHtml {
            element: HtmlElement::Meta,
            attrs: Box::new(TypedExpr::AttrsLiteral {
                attributes: vec![create_attribute("charset", "utf-8")],
            }),
            children: Box::new(TypedExpr::FragmentConcat { nodes: vec![] }),
        },
        TypedExpr::FragmentHtml {
            element: HtmlElement::Meta,
            attrs: Box::new(TypedExpr::AttrsLiteral {
                attributes: vec![
                    create_attribute("content", "width=device-width, initial-scale=1"),
                    create_attribute("name", "viewport"),
                ],
            }),
            children: Box::new(TypedExpr::FragmentConcat { nodes: vec![] }),
        },
    ]
}

fn create_style_element(css_content: &str) -> TypedExpr {
    let css_text = TypedExpr::FragmentRaw {
        value: CheapString::new(css_content.to_string()),
    };

    TypedExpr::FragmentHtml {
        element: HtmlElement::Style,
        attrs: Box::new(TypedExpr::AttrsLiteral {
            attributes: Vec::new(),
        }),
        children: Box::new(TypedExpr::FragmentConcat {
            nodes: vec![css_text],
        }),
    }
}

fn create_link_element(href: &str) -> TypedExpr {
    TypedExpr::FragmentHtml {
        element: HtmlElement::Link,
        attrs: Box::new(TypedExpr::AttrsLiteral {
            attributes: vec![
                create_attribute("rel", "stylesheet"),
                create_attribute("href", href),
            ],
        }),
        children: Box::new(TypedExpr::FragmentConcat { nodes: vec![] }),
    }
}

fn create_tailwind_element(injection: TailwindInjection<'_>) -> TypedExpr {
    match injection {
        TailwindInjection::Inline(css) => create_style_element(css),
        TailwindInjection::Link { href } => create_link_element(href),
    }
}

fn create_script_element(src: &str) -> TypedExpr {
    TypedExpr::FragmentHtml {
        element: HtmlElement::Script,
        attrs: Box::new(TypedExpr::AttrsLiteral {
            attributes: vec![
                create_attribute("type", "module"),
                create_attribute("src", src),
            ],
        }),
        children: Box::new(TypedExpr::FragmentConcat { nodes: vec![] }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn text(value: &str) -> TypedExpr {
        TypedExpr::FragmentRaw {
            value: CheapString::new(value.to_string()),
        }
    }

    fn element(tag_name: &str, children: Vec<TypedExpr>) -> TypedExpr {
        TypedExpr::FragmentHtml {
            element: HtmlElement::parse(tag_name).expect("unrecognized tag name"),
            attrs: Box::new(TypedExpr::AttrsLiteral {
                attributes: Vec::new(),
            }),
            children: Box::new(TypedExpr::FragmentConcat { nodes: children }),
        }
    }

    fn page(page_name: &str, head: Vec<TypedExpr>, body: Vec<TypedExpr>) -> TypedPageDeclaration {
        TypedPageDeclaration {
            name: TypeName::new(page_name).unwrap(),
            head: TypedExpr::FragmentConcat { nodes: head },
            body: TypedExpr::FragmentConcat { nodes: body },
            params: Vec::new(),
        }
    }

    fn format_children(page: &AssembledPageDeclaration) -> String {
        let TypedExpr::FragmentConcat { nodes } = &page.body else {
            panic!("an assembled page body is a Fragment literal");
        };
        nodes
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn check(
        page: TypedPageDeclaration,
        tailwind_injection: Option<TailwindInjection<'_>>,
        script_src: Option<&str>,
        expected: Expect,
    ) {
        let assembled = assemble_page(page, tailwind_injection, script_src);
        expected.assert_eq(&format!("{}\n", format_children(&assembled)));
    }

    #[test]
    fn assembles_empty_page() {
        check(
            page("EmptyComp", vec![], vec![]),
            None,
            None,
            expect![[r#"
                raw("<!doctype html>")
                html(
                  tag: "html",
                  attrs: [],
                  children: concat(
                    html(
                      tag: "head",
                      attrs: [],
                      children: concat(
                        html(tag: "meta", attrs: [charset: raw("utf-8")]),
                        html(
                          tag: "meta",
                          attrs: [
                            content: raw("width=device-width, initial-scale=1"),
                            name: raw("viewport"),
                          ],
                        ),
                      ),
                    ),
                    html(tag: "body", attrs: [], children: concat()),
                  ),
                )
            "#]],
        );
    }

    #[test]
    fn assembles_page_with_head_and_body_content() {
        check(
            page(
                "MainComp",
                vec![element("title", vec![text("My Page")])],
                vec![text("Hello World")],
            ),
            None,
            None,
            expect![[r#"
                raw("<!doctype html>")
                html(
                  tag: "html",
                  attrs: [],
                  children: concat(
                    html(
                      tag: "head",
                      attrs: [],
                      children: concat(
                        html(tag: "meta", attrs: [charset: raw("utf-8")]),
                        html(
                          tag: "meta",
                          attrs: [
                            content: raw("width=device-width, initial-scale=1"),
                            name: raw("viewport"),
                          ],
                        ),
                        html(
                          tag: "title",
                          attrs: [],
                          children: concat(raw("My Page")),
                        ),
                      ),
                    ),
                    html(
                      tag: "body",
                      attrs: [],
                      children: concat(raw("Hello World")),
                    ),
                  ),
                )
            "#]],
        );
    }

    #[test]
    fn injects_style_into_head() {
        let css = ".text-red { color: red; }";

        check(
            page("MainComp", vec![], vec![]),
            Some(TailwindInjection::Inline(css)),
            None,
            expect![[r#"
                raw("<!doctype html>")
                html(
                  tag: "html",
                  attrs: [],
                  children: concat(
                    html(
                      tag: "head",
                      attrs: [],
                      children: concat(
                        html(tag: "meta", attrs: [charset: raw("utf-8")]),
                        html(
                          tag: "meta",
                          attrs: [
                            content: raw("width=device-width, initial-scale=1"),
                            name: raw("viewport"),
                          ],
                        ),
                        html(
                          tag: "style",
                          attrs: [],
                          children: concat(
                            raw(".text-red { color: red; }"),
                          ),
                        ),
                      ),
                    ),
                    html(tag: "body", attrs: [], children: concat()),
                  ),
                )
            "#]],
        );
    }

    #[test]
    fn injects_link_into_head() {
        check(
            page("MainComp", vec![], vec![]),
            Some(TailwindInjection::Link {
                href: "/styles-deadbeef.css",
            }),
            None,
            expect![[r#"
                raw("<!doctype html>")
                html(
                  tag: "html",
                  attrs: [],
                  children: concat(
                    html(
                      tag: "head",
                      attrs: [],
                      children: concat(
                        html(tag: "meta", attrs: [charset: raw("utf-8")]),
                        html(
                          tag: "meta",
                          attrs: [
                            content: raw("width=device-width, initial-scale=1"),
                            name: raw("viewport"),
                          ],
                        ),
                        html(
                          tag: "link",
                          attrs: [
                            rel: raw("stylesheet"),
                            href: raw("/styles-deadbeef.css"),
                          ],
                        ),
                      ),
                    ),
                    html(tag: "body", attrs: [], children: concat()),
                  ),
                )
            "#]],
        );
    }

    #[test]
    fn injects_script_into_head() {
        check(
            page("MainComp", vec![], vec![]),
            None,
            Some("/scripts-deadbeef.js"),
            expect![[r#"
                raw("<!doctype html>")
                html(
                  tag: "html",
                  attrs: [],
                  children: concat(
                    html(
                      tag: "head",
                      attrs: [],
                      children: concat(
                        html(tag: "meta", attrs: [charset: raw("utf-8")]),
                        html(
                          tag: "meta",
                          attrs: [
                            content: raw("width=device-width, initial-scale=1"),
                            name: raw("viewport"),
                          ],
                        ),
                        html(
                          tag: "script",
                          attrs: [
                            type: raw("module"),
                            src: raw("/scripts-deadbeef.js"),
                          ],
                          children: concat(),
                        ),
                      ),
                    ),
                    html(tag: "body", attrs: [], children: concat()),
                  ),
                )
            "#]],
        );
    }

    #[test]
    fn does_not_inject_tailwind_or_script_when_none() {
        check(
            page("MainComp", vec![], vec![text("Hello World")]),
            None,
            None,
            expect![[r#"
                raw("<!doctype html>")
                html(
                  tag: "html",
                  attrs: [],
                  children: concat(
                    html(
                      tag: "head",
                      attrs: [],
                      children: concat(
                        html(tag: "meta", attrs: [charset: raw("utf-8")]),
                        html(
                          tag: "meta",
                          attrs: [
                            content: raw("width=device-width, initial-scale=1"),
                            name: raw("viewport"),
                          ],
                        ),
                      ),
                    ),
                    html(
                      tag: "body",
                      attrs: [],
                      children: concat(raw("Hello World")),
                    ),
                  ),
                )
            "#]],
        );
    }
}
