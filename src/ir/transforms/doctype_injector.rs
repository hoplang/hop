use crate::{
    document::document_cursor::StringSpan,
    hop::inlined_ast::{InlinedEntrypoint, InlinedNode},
};

/// Transform that injects <!DOCTYPE html> at the beginning of entrypoints
/// that don't already have a doctype declaration
pub struct DoctypeInjector;

impl DoctypeInjector {
    /// Check if a list of nodes starts with a DOCTYPE declaration (ignoring leading whitespace)
    fn has_doctype(nodes: &[InlinedNode]) -> bool {
        for node in nodes {
            match node {
                InlinedNode::Doctype { .. } => return true,
                InlinedNode::Text { value, .. } if value.as_str().trim().is_empty() => {
                    // Skip whitespace-only text nodes
                    continue;
                }
                _ => {
                    // Found a non-DOCTYPE, non-whitespace node
                    return false;
                }
            }
        }
        false
    }

    /// Find the position to insert DOCTYPE (after leading whitespace)
    fn find_doctype_insert_position(nodes: &[InlinedNode]) -> usize {
        for (i, node) in nodes.iter().enumerate() {
            if let InlinedNode::Text { value, .. } = node {
                if value.as_str().trim().is_empty() {
                    // Skip whitespace-only text nodes
                    continue;
                }
            }
            // Found first non-whitespace node
            return i;
        }
        // All nodes are whitespace or list is empty
        nodes.len()
    }

    pub fn run(mut entrypoint: InlinedEntrypoint) -> InlinedEntrypoint {
        // Only inject DOCTYPE for entrypoints
        if !Self::has_doctype(&entrypoint.children) {
            // Create a synthetic DOCTYPE node
            let doctype_node = InlinedNode::Doctype {
                value: StringSpan::new("<!DOCTYPE html>".to_string()),
            };

            // Find the right position to insert (after any leading whitespace)
            let insert_pos = Self::find_doctype_insert_position(&entrypoint.children);
            entrypoint.children.insert(insert_pos, doctype_node);
        }
        entrypoint
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::test_utils::build_inlined_auto;
    use expect_test::{Expect, expect};

    /// Helper to pretty-print entrypoint children for testing
    fn format_entrypoint_children(entrypoint: &InlinedEntrypoint) -> String {
        entrypoint
            .children
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Helper to check DOCTYPE injection for entrypoint
    fn check_doctype_injection(entrypoint: InlinedEntrypoint, expected: Expect) {
        // Format before
        let before = format_entrypoint_children(&entrypoint);

        // Apply transform
        let transformed_entrypoint = DoctypeInjector::run(entrypoint);

        // Format after
        let after = format_entrypoint_children(&transformed_entrypoint);

        // Create output with before/after format
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);

        expected.assert_eq(&output);
    }

    #[test]
    fn test_inject_doctype_when_missing() {
        let entrypoint = build_inlined_auto("main-comp", vec![], |t| {
            t.text("                    "); // Leading whitespace
            t.html("html", vec![], |t| {
                t.text("\n                        ");
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
                t.text("\n                    ");
            });
            t.text("\n                "); // Trailing whitespace
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
            -- before --
            "                    "
            <html>
              "\n                        "
              <body>
                "Hello World"
              </body>
              "\n                    "
            </html>
            "\n                "
            -- after --
            "                    "
            <!DOCTYPE html>
            <html>
              "\n                        "
              <body>
                "Hello World"
              </body>
              "\n                    "
            </html>
            "\n                "
        "#]],
        );
    }

    #[test]
    fn test_no_injection_when_doctype_present() {
        let entrypoint = build_inlined_auto("main-comp", vec![], |t| {
            t.text("                    "); // Leading whitespace
            t.doctype("<!DOCTYPE html>");
            t.text("                    "); // Whitespace after doctype
            t.html("html", vec![], |t| {
                t.text("\n                        ");
                t.html("body", vec![], |t| {
                    t.text("Hello World");
                });
                t.text("\n                    ");
            });
            t.text("\n                "); // Trailing whitespace
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
            -- before --
            "                    "
            <!DOCTYPE html>
            "                    "
            <html>
              "\n                        "
              <body>
                "Hello World"
              </body>
              "\n                    "
            </html>
            "\n                "
            -- after --
            "                    "
            <!DOCTYPE html>
            "                    "
            <html>
              "\n                        "
              <body>
                "Hello World"
              </body>
              "\n                    "
            </html>
            "\n                "
        "#]],
        );
    }

    #[test]
    fn test_empty_entrypoint() {
        let entrypoint = build_inlined_auto("empty-comp", vec![], |t| {
            t.text("\n                "); // Only whitespace
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
            -- before --
            "\n                "
            -- after --
            "\n                "
            <!DOCTYPE html>
        "#]],
        );
    }

    #[test]
    fn test_entrypoint_with_text_only() {
        let entrypoint = build_inlined_auto("text-comp", vec![], |t| {
            t.text("\n                    Just some text content\n                ");
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
            -- before --
            "\n                    Just some text content\n                "
            -- after --
            <!DOCTYPE html>
            "\n                    Just some text content\n                "
        "#]],
        );
    }

    #[test]
    fn test_doctype_with_leading_whitespace() {
        let entrypoint = build_inlined_auto("main-comp", vec![], |t| {
            t.text("\n\n                    "); // Extra leading whitespace
            t.doctype("<!DOCTYPE html>");
            t.text("\n                    "); // Whitespace after doctype
            t.html("html", vec![], |t| {
                t.text("Content");
            });
            t.text("\n                "); // Trailing whitespace
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
            -- before --
            "\n\n                    "
            <!DOCTYPE html>
            "\n                    "
            <html>
              "Content"
            </html>
            "\n                "
            -- after --
            "\n\n                    "
            <!DOCTYPE html>
            "\n                    "
            <html>
              "Content"
            </html>
            "\n                "
        "#]],
        );
    }

    #[test]
    fn test_inject_preserves_leading_whitespace() {
        let entrypoint = build_inlined_auto("main-comp", vec![], |t| {
            t.text("\n\n                    "); // Extra leading whitespace
            t.html("html", vec![], |t| {
                t.text("Content");
            });
            t.text("\n                "); // Trailing whitespace
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
            -- before --
            "\n\n                    "
            <html>
              "Content"
            </html>
            "\n                "
            -- after --
            "\n\n                    "
            <!DOCTYPE html>
            <html>
              "Content"
            </html>
            "\n                "
        "#]],
        );
    }
}
