use crate::{
    document::CheapString,
    hop::typing::{typed_ast::TypedViewDeclaration, typed_node::TypedNode},
};

/// Transform that injects <!DOCTYPE html> at the beginning of views
/// that don't already have a doctype declaration
pub struct DoctypeInjector;

impl DoctypeInjector {
    /// Check if a list of nodes starts with a DOCTYPE declaration (ignoring leading whitespace)
    fn has_doctype(nodes: &[TypedNode]) -> bool {
        for node in nodes {
            match node {
                TypedNode::Doctype { .. } => return true,
                TypedNode::Text { value, .. } if value.as_str().trim().is_empty() => {
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

    pub fn run(view: &mut TypedViewDeclaration) {
        // Only inject DOCTYPE for views
        if !Self::has_doctype(&view.children) {
            // Create a synthetic DOCTYPE node
            let doctype_node = TypedNode::Doctype {
                value: CheapString::new("<!DOCTYPE html>".to_string()),
            };

            // Always insert DOCTYPE at the beginning (position 0)
            view.children.insert(0, doctype_node);
        }
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

    /// Helper to check DOCTYPE injection for view
    fn check_doctype_injection(mut view: TypedViewDeclaration, expected: Expect) {
        let before = format_view_children(&view);
        DoctypeInjector::run(&mut view);
        let after = format_view_children(&view);
        let output = format!("-- before --\n{}\n-- after --\n{}\n", before, after);
        expected.assert_eq(&output);
    }

    #[test]
    fn should_not_inject_when_doctype_is_already_present() {
        let view = build_typed_view_no_params("MainComp", |t| {
            t.doctype("<!DOCTYPE html>");
        });

        check_doctype_injection(
            view,
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
        let view = build_typed_view_no_params("MainComp", |t| {
            t.text("\n");
            t.doctype("<!DOCTYPE html>");
        });

        check_doctype_injection(
            view,
            expect![[r#"
                -- before --


                <!DOCTYPE html>
                -- after --


                <!DOCTYPE html>
            "#]],
        );
    }

    #[test]
    fn should_inject_doctype_for_whitespace_only_view() {
        let view = build_typed_view_no_params("EmptyComp", |t| {
            t.text("\n");
        });

        check_doctype_injection(
            view,
            expect![[r#"
                -- before --


                -- after --
                <!DOCTYPE html>


            "#]],
        );
    }

    #[test]
    fn should_inject_doctype_for_text_only_view() {
        let view = build_typed_view_no_params("TextComp", |t| {
            t.text("Just some text content");
        });

        check_doctype_injection(
            view,
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
