use crate::{
    document::document_cursor::CheapString,
    inlined::{InlinedComponentDeclaration, InlinedNode},
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

    pub fn run(mut entrypoint: InlinedComponentDeclaration) -> InlinedComponentDeclaration {
        // Only inject DOCTYPE for entrypoints
        if !Self::has_doctype(&entrypoint.children) {
            // Create a synthetic DOCTYPE node
            let doctype_node = InlinedNode::Doctype {
                value: CheapString::new("<!DOCTYPE html>".to_string()),
            };

            // Always insert DOCTYPE at the beginning (position 0)
            entrypoint.children.insert(0, doctype_node);
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
    fn format_entrypoint_children(entrypoint: &InlinedComponentDeclaration) -> String {
        entrypoint
            .children
            .iter()
            .map(|child| child.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Helper to check DOCTYPE injection for entrypoint
    fn check_doctype_injection(entrypoint: InlinedComponentDeclaration, expected: Expect) {
        let before = format_entrypoint_children(&entrypoint);
        let transformed_entrypoint = DoctypeInjector::run(entrypoint);
        let after = format_entrypoint_children(&transformed_entrypoint);
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_not_inject_when_doctype_is_already_present() {
        let entrypoint = build_inlined("MainComp", [], |t| {
            t.doctype("<!DOCTYPE html>");
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
                -- before --
                <!DOCTYPE html>
                -- after --
                <!DOCTYPE html>
            "#]],
        );
    }

    #[test]
    fn should_not_inject_when_doctype_is_already_present_after_leading_whitespace() {
        let entrypoint = build_inlined("MainComp", [], |t| {
            t.text("\n");
            t.doctype("<!DOCTYPE html>");
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
                -- before --


                <!DOCTYPE html>
                -- after --


                <!DOCTYPE html>
            "#]],
        );
    }

    #[test]
    fn should_inject_doctype_for_whitespace_only_entrypoint() {
        let entrypoint = build_inlined("EmptyComp", [], |t| {
            t.text("\n");
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
                -- before --


                -- after --
                <!DOCTYPE html>


            "#]],
        );
    }

    #[test]
    fn should_inject_doctype_for_text_only_entrypoint() {
        let entrypoint = build_inlined("TextComp", [], |t| {
            t.text("Just some text content");
        });

        check_doctype_injection(
            entrypoint,
            expect![[r#"
                -- before --
                Just some text content
                -- after --
                <!DOCTYPE html>
                Just some text content
            "#]],
        );
    }
}
