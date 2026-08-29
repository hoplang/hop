use crate::document::CheapString;
use crate::hop::typing::typed_ast::{TypedPageDeclaration, TypedParameter};
use crate::hop::typing::typed_node::{TypedAttribute, TypedAttributeValue, TypedNode};
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
    pub children: Vec<TypedNode>,
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
            children: page.body,
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

        let header = BoxDoc::text("page")
            .append(BoxDoc::space())
            .append(BoxDoc::text(self.name.as_str()))
            .append(BoxDoc::text("("))
            .append(params_doc)
            .append(BoxDoc::text(")"))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"));

        if self.children.is_empty() {
            header.append(BoxDoc::text("}"))
        } else {
            header
                .append(
                    BoxDoc::line()
                        .append(BoxDoc::intersperse(
                            self.children.iter().map(|c| c.to_doc()),
                            BoxDoc::line(),
                        ))
                        .nest(2),
                )
                .append(BoxDoc::line())
                .append(BoxDoc::text("}"))
        }
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
        mut head,
        body,
        params,
    } = page;

    head.splice(0..0, create_meta_elements());
    if let Some(injection) = tailwind_injection {
        head.push(create_tailwind_element(injection));
    }
    if let Some(src) = script_src {
        head.push(create_script_element(src));
    }

    let doctype = TypedNode::Text {
        value: CheapString::new("<!doctype html>".to_string()),
    };
    let html = create_html_element(
        HtmlElement::Html,
        vec![
            create_html_element(HtmlElement::Head, head),
            create_html_element(HtmlElement::Body, body),
        ],
    );

    AssembledPageDeclaration {
        name,
        params,
        children: vec![doctype, html],
    }
}

fn create_html_element(element: HtmlElement, children: Vec<TypedNode>) -> TypedNode {
    TypedNode::Html {
        element,
        attributes: Vec::new(),
        rest_spread: None,
        children,
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

fn create_meta_elements() -> Vec<TypedNode> {
    vec![
        TypedNode::Html {
            element: HtmlElement::Meta,
            attributes: vec![create_attribute("charset", "utf-8")],
            rest_spread: None,
            children: vec![],
        },
        TypedNode::Html {
            element: HtmlElement::Meta,
            attributes: vec![
                create_attribute("content", "width=device-width, initial-scale=1"),
                create_attribute("name", "viewport"),
            ],
            rest_spread: None,
            children: vec![],
        },
    ]
}

fn create_style_element(css_content: &str) -> TypedNode {
    let css_text = TypedNode::Text {
        value: CheapString::new(css_content.to_string()),
    };

    TypedNode::Html {
        element: HtmlElement::Style,
        attributes: Vec::new(),
        rest_spread: None,
        children: vec![css_text],
    }
}

fn create_link_element(href: &str) -> TypedNode {
    TypedNode::Html {
        element: HtmlElement::Link,
        attributes: vec![
            create_attribute("rel", "stylesheet"),
            create_attribute("href", href),
        ],
        rest_spread: None,
        children: vec![],
    }
}

fn create_tailwind_element(injection: TailwindInjection<'_>) -> TypedNode {
    match injection {
        TailwindInjection::Inline(css) => create_style_element(css),
        TailwindInjection::Link { href } => create_link_element(href),
    }
}

fn create_script_element(src: &str) -> TypedNode {
    TypedNode::Html {
        element: HtmlElement::Script,
        attributes: vec![
            create_attribute("type", "module"),
            create_attribute("src", src),
        ],
        rest_spread: None,
        children: vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn text(value: &str) -> TypedNode {
        TypedNode::Text {
            value: CheapString::new(value.to_string()),
        }
    }

    fn element(tag_name: &str, children: Vec<TypedNode>) -> TypedNode {
        TypedNode::Html {
            element: HtmlElement::parse(tag_name).expect("unrecognized tag name"),
            attributes: Vec::new(),
            rest_spread: None,
            children,
        }
    }

    fn page(page_name: &str, head: Vec<TypedNode>, body: Vec<TypedNode>) -> TypedPageDeclaration {
        TypedPageDeclaration {
            name: TypeName::new(page_name).unwrap(),
            head,
            body,
            params: Vec::new(),
        }
    }

    fn format_children(page: &AssembledPageDeclaration) -> String {
        page.children
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
                <!doctype html>
                <html>
                  <head>
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                  </head>
                  <body></body>
                </html>
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
                <!doctype html>
                <html>
                  <head>
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                    <title>
                      My Page
                    </title>
                  </head>
                  <body>
                    Hello World
                  </body>
                </html>
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
                <!doctype html>
                <html>
                  <head>
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                    <style>
                      .text-red { color: red; }
                    </style>
                  </head>
                  <body></body>
                </html>
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
                <!doctype html>
                <html>
                  <head>
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                    <link rel="stylesheet" href="/styles-deadbeef.css"></link>
                  </head>
                  <body></body>
                </html>
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
                <!doctype html>
                <html>
                  <head>
                    <meta charset="utf-8"></meta>
                    <meta content="width=device-width, initial-scale=1" name="viewport"></meta>
                    <script type="module" src="/scripts-deadbeef.js"></script>
                  </head>
                  <body></body>
                </html>
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
                <!doctype html>
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
}
