use crate::{
    document::document_cursor::StringSpan,
    hop::inlined_ast::{InlinedEntryPoint, InlinedNode},
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

    pub fn run(mut entrypoint: InlinedEntryPoint) -> InlinedEntryPoint {
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
    use crate::hop::inlined_test_utils::InlinedTestBuilder;
    use expect_test::{Expect, expect};

    /// Helper to pretty-print entrypoint children for testing
    fn format_entrypoint_children(entrypoint: &InlinedEntryPoint) -> String {
        let mut output = String::new();
        for (i, node) in entrypoint.children.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            match node {
                InlinedNode::Doctype { value, .. } => {
                    output.push_str(&format!("DOCTYPE: {}", value.as_str()));
                }
                InlinedNode::Text { value, .. } => {
                    let text = value.as_str();
                    if text.trim().is_empty() {
                        output.push_str(&format!("Text: <whitespace:{} chars>", text.len()));
                    } else {
                        output.push_str(&format!("Text: {:?}", text));
                    }
                }
                InlinedNode::Html {
                    tag_name, children, ..
                } => {
                    output.push_str(&format!(
                        "Html: <{}> ({} children)",
                        tag_name.as_str(),
                        children.len()
                    ));
                }
                InlinedNode::If { children, .. } => {
                    output.push_str(&format!("If: ({} children)", children.len()));
                }
                InlinedNode::For { children, .. } => {
                    output.push_str(&format!("For: ({} children)", children.len()));
                }
                InlinedNode::TextExpression { .. } => {
                    output.push_str("TextExpression");
                }
                InlinedNode::Let { children, .. } => {
                    output.push_str(&format!("Let: ({} children)", children.len()));
                }
            }
        }
        output
    }

    /// Helper to check DOCTYPE injection for entrypoint
    fn check_doctype_injection(entrypoint: InlinedEntryPoint, expected: Expect) {
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
        let t = InlinedTestBuilder::new(vec![]);
        let entrypoint = t.build("main-comp", vec![
            t.text("                    "), // Leading whitespace
            t.html("html", vec![], vec![
                t.text("\n                        "),
                t.html("body", vec![], vec![t.text("Hello World")]),
                t.text("\n                    ")
            ]),
            t.text("\n                ") // Trailing whitespace
        ]);

        check_doctype_injection(entrypoint, expect![[r#"
            -- before --
            Text: <whitespace:20 chars>
            Html: <html> (3 children)
            Text: <whitespace:17 chars>
            -- after --
            Text: <whitespace:20 chars>
            DOCTYPE: <!DOCTYPE html>
            Html: <html> (3 children)
            Text: <whitespace:17 chars>
        "#]]);
    }

    #[test]
    fn test_no_injection_when_doctype_present() {
        let t = InlinedTestBuilder::new(vec![]);
        let entrypoint = t.build("main-comp", vec![
            t.text("                    "), // Leading whitespace
            t.doctype("<!DOCTYPE html>"),
            t.text("                    "), // Whitespace after doctype
            t.html("html", vec![], vec![
                t.text("\n                        "),
                t.html("body", vec![], vec![t.text("Hello World")]),
                t.text("\n                    ")
            ]),
            t.text("\n                ") // Trailing whitespace
        ]);

        check_doctype_injection(entrypoint, expect![[r#"
            -- before --
            Text: <whitespace:20 chars>
            DOCTYPE: <!DOCTYPE html>
            Text: <whitespace:20 chars>
            Html: <html> (3 children)
            Text: <whitespace:17 chars>
            -- after --
            Text: <whitespace:20 chars>
            DOCTYPE: <!DOCTYPE html>
            Text: <whitespace:20 chars>
            Html: <html> (3 children)
            Text: <whitespace:17 chars>
        "#]]);
    }

    #[test]
    fn test_empty_entrypoint() {
        let t = InlinedTestBuilder::new(vec![]);
        let entrypoint = t.build("empty-comp", vec![
            t.text("\n                ") // Only whitespace
        ]);

        check_doctype_injection(entrypoint, expect![[r#"
            -- before --
            Text: <whitespace:17 chars>
            -- after --
            Text: <whitespace:17 chars>
            DOCTYPE: <!DOCTYPE html>
        "#]]);
    }

    #[test]
    fn test_entrypoint_with_text_only() {
        let t = InlinedTestBuilder::new(vec![]);
        let entrypoint = t.build("text-comp", vec![
            t.text("\n                    Just some text content\n                ")
        ]);

        check_doctype_injection(entrypoint, expect![[r#"
            -- before --
            Text: "\n                    Just some text content\n                "
            -- after --
            DOCTYPE: <!DOCTYPE html>
            Text: "\n                    Just some text content\n                "
        "#]]);
    }

    #[test]
    fn test_doctype_with_leading_whitespace() {
        let t = InlinedTestBuilder::new(vec![]);
        let entrypoint = t.build("main-comp", vec![
            t.text("\n\n                    "), // Extra leading whitespace
            t.doctype("<!DOCTYPE html>"),
            t.text("\n                    "), // Whitespace after doctype
            t.html("html", vec![], vec![t.text("Content")]),
            t.text("\n                ") // Trailing whitespace
        ]);

        check_doctype_injection(entrypoint, expect![[r#"
            -- before --
            Text: <whitespace:22 chars>
            DOCTYPE: <!DOCTYPE html>
            Text: <whitespace:21 chars>
            Html: <html> (1 children)
            Text: <whitespace:17 chars>
            -- after --
            Text: <whitespace:22 chars>
            DOCTYPE: <!DOCTYPE html>
            Text: <whitespace:21 chars>
            Html: <html> (1 children)
            Text: <whitespace:17 chars>
        "#]]);
    }

    #[test]
    fn test_inject_preserves_leading_whitespace() {
        let t = InlinedTestBuilder::new(vec![]);
        let entrypoint = t.build("main-comp", vec![
            t.text("\n\n                    "), // Extra leading whitespace
            t.html("html", vec![], vec![t.text("Content")]),
            t.text("\n                ") // Trailing whitespace
        ]);

        check_doctype_injection(entrypoint, expect![[r#"
            -- before --
            Text: <whitespace:22 chars>
            Html: <html> (1 children)
            Text: <whitespace:17 chars>
            -- after --
            Text: <whitespace:22 chars>
            DOCTYPE: <!DOCTYPE html>
            Html: <html> (1 children)
            Text: <whitespace:17 chars>
        "#]]);
    }
}
