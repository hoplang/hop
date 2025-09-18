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
    use crate::error_collector::ErrorCollector;
    use crate::hop::module_name::ModuleName;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::type_checker::TypeChecker;
    use crate::ir::inliner::Inliner;
    use expect_test::expect;

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

    /// Helper to create a typed AST from Hop source code
    fn create_typed_ast(source: &str) -> InlinedEntryPoint {
        let mut errors = ErrorCollector::new();
        let module_name = ModuleName::new("test".to_string()).unwrap();
        let tokenizer = Tokenizer::new(source.to_string());
        let ast = parse(module_name.clone(), tokenizer, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Type check
        let mut typechecker = TypeChecker::default();
        typechecker.typecheck(&[&ast]);
        assert!(
            typechecker
                .type_errors
                .get(&module_name)
                .unwrap()
                .is_empty(),
            "Type errors: {:?}",
            typechecker.type_errors
        );

        // Get the inlined entrypoints
        let entrypoints = Inliner::inline_entrypoints(typechecker.typed_asts);
        assert!(
            !entrypoints.is_empty(),
            "No entrypoint found in source code"
        );
        assert!(
            entrypoints.len() == 1,
            "Expected exactly one entrypoint, found {}",
            entrypoints.len()
        );

        entrypoints.into_iter().next().unwrap()
    }

    /// Helper to check DOCTYPE injection for entrypoint
    fn check(input: &str, expected: expect_test::Expect) {
        let entrypoint = create_typed_ast(input);

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
        check(
            r#"
                <main-comp entrypoint>
                    <html>
                        <body>Hello World</body>
                    </html>
                </main-comp>
            "#,
            expect![[r#"
                -- before --
                Text: <whitespace:21 chars>
                Html: <html> (3 children)
                Text: <whitespace:17 chars>
                -- after --
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Html: <html> (3 children)
                Text: <whitespace:17 chars>
            "#]],
        );
    }

    #[test]
    fn test_no_injection_when_doctype_present() {
        check(
            r#"
                <main-comp entrypoint>
                    <!DOCTYPE html>
                    <html>
                        <body>Hello World</body>
                    </html>
                </main-comp>
            "#,
            expect![[r#"
                -- before --
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Text: <whitespace:21 chars>
                Html: <html> (3 children)
                Text: <whitespace:17 chars>
                -- after --
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Text: <whitespace:21 chars>
                Html: <html> (3 children)
                Text: <whitespace:17 chars>
            "#]],
        );
    }

    #[test]
    fn test_empty_entrypoint() {
        check(
            r#"
                <empty-comp entrypoint>
                </empty-comp>
            "#,
            expect![[r#"
                -- before --
                Text: <whitespace:17 chars>
                -- after --
                Text: <whitespace:17 chars>
                DOCTYPE: <!DOCTYPE html>
            "#]],
        );
    }

    #[test]
    fn test_entrypoint_with_text_only() {
        check(
            r#"
                <text-comp entrypoint>
                    Just some text content
                </text-comp>
            "#,
            expect![[r#"
                -- before --
                Text: "\n                    Just some text content\n                "
                -- after --
                DOCTYPE: <!DOCTYPE html>
                Text: "\n                    Just some text content\n                "
            "#]],
        );
    }

    #[test]
    fn test_doctype_with_leading_whitespace() {
        check(
            r#"
                <main-comp entrypoint>

                    <!DOCTYPE html>
                    <html>Content</html>
                </main-comp>
            "#,
            expect![[r#"
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
            "#]],
        );
    }

    #[test]
    fn test_inject_preserves_leading_whitespace() {
        check(
            r#"
                <main-comp entrypoint>

                    <html>Content</html>
                </main-comp>
            "#,
            expect![[r#"
                -- before --
                Text: <whitespace:22 chars>
                Html: <html> (1 children)
                Text: <whitespace:17 chars>
                -- after --
                Text: <whitespace:22 chars>
                DOCTYPE: <!DOCTYPE html>
                Html: <html> (1 children)
                Text: <whitespace:17 chars>
            "#]],
        );
    }
}
