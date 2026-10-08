use crate::html::write_escaped_html;

/// How the generated Tailwind CSS is referenced from the `<head>`.
#[derive(Debug, Clone, Copy)]
pub enum TailwindInjection<'a> {
    /// Inline the CSS as a `<style>{css}</style>` element. Used by `hop dev`
    /// so hot reload can ship CSS through the same render pipeline.
    Inline(&'a str),
    /// Reference an external stylesheet via `<link rel="stylesheet" href={href}>`.
    /// Used by `hop build`, where the CSS is written to disk under the assets
    /// output directory with a content hashed filename.
    Link { href: &'a str },
}

/// The document around the head and body of a page.
///
/// The fixed markup is what the language reference lists under rendering.
/// What the host adds to the end of the head, a stylesheet or a script, is
/// not part of the language, so neither is part of the IR. The shell is
/// written around a page where the page becomes output, in flat_to_writer and
/// in the evaluator.
#[derive(Debug, Clone, PartialEq)]
pub struct DocumentShell {
    /// The doctype, `<html><head>` and the two meta elements.
    pub before_head: &'static str,
    /// The host additions, then `</head><body>`.
    pub after_head: String,
    /// `</body></html>`
    pub after_body: &'static str,
}

impl DocumentShell {
    pub fn new(
        tailwind_injection: Option<TailwindInjection<'_>>,
        script_src: Option<&str>,
    ) -> Self {
        let mut after_head = String::new();
        match tailwind_injection {
            Some(TailwindInjection::Inline(css)) => {
                after_head.push_str("<style>");
                after_head.push_str(css);
                after_head.push_str("</style>");
            }
            Some(TailwindInjection::Link { href }) => {
                after_head.push_str("<link rel=\"stylesheet\" href=\"");
                write_escaped_html(href, &mut after_head);
                after_head.push_str("\">");
            }
            None => {}
        }
        if let Some(src) = script_src {
            after_head.push_str("<script type=\"module\" src=\"");
            write_escaped_html(src, &mut after_head);
            after_head.push_str("\"></script>");
        }
        after_head.push_str("</head><body>");
        DocumentShell {
            before_head: concat!(
                "<!doctype html>",
                "<html><head>",
                "<meta charset=\"utf-8\">",
                "<meta content=\"width=device-width, initial-scale=1\" name=\"viewport\">",
            ),
            after_head,
            after_body: "</body></html>",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};

    fn check(shell: DocumentShell, expected: Expect) {
        expected.assert_eq(&format!(
            "{}\n{}\n{}\n",
            shell.before_head, shell.after_head, shell.after_body
        ));
    }

    #[test]
    fn has_the_fixed_markup_without_host_additions() {
        check(
            DocumentShell::new(None, None),
            expect![[r#"
                <!doctype html><html><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport">
                </head><body>
                </body></html>
            "#]],
        );
    }

    #[test]
    fn adds_an_inline_stylesheet_to_the_end_of_the_head() {
        check(
            DocumentShell::new(
                Some(TailwindInjection::Inline(".text-red { color: red; }")),
                None,
            ),
            expect![[r#"
                <!doctype html><html><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport">
                <style>.text-red { color: red; }</style></head><body>
                </body></html>
            "#]],
        );
    }

    #[test]
    fn adds_a_stylesheet_link_to_the_end_of_the_head() {
        check(
            DocumentShell::new(
                Some(TailwindInjection::Link {
                    href: "/styles-deadbeef.css",
                }),
                None,
            ),
            expect![[r#"
                <!doctype html><html><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport">
                <link rel="stylesheet" href="/styles-deadbeef.css"></head><body>
                </body></html>
            "#]],
        );
    }

    #[test]
    fn adds_a_script_after_the_stylesheet() {
        check(
            DocumentShell::new(
                Some(TailwindInjection::Link {
                    href: "/styles-deadbeef.css",
                }),
                Some("/scripts-deadbeef.js"),
            ),
            expect![[r#"
                <!doctype html><html><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport">
                <link rel="stylesheet" href="/styles-deadbeef.css"><script type="module" src="/scripts-deadbeef.js"></script></head><body>
                </body></html>
            "#]],
        );
    }

    #[test]
    fn escapes_the_href_and_the_src() {
        check(
            DocumentShell::new(
                Some(TailwindInjection::Link {
                    href: "/a\"b&c.css",
                }),
                Some("/a\"b&c.js"),
            ),
            expect![[r#"
                <!doctype html><html><head><meta charset="utf-8"><meta content="width=device-width, initial-scale=1" name="viewport">
                <link rel="stylesheet" href="/a&quot;b&amp;c.css"><script type="module" src="/a&quot;b&amp;c.js"></script></head><body>
                </body></html>
            "#]],
        );
    }
}
