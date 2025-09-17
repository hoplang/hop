use crate::document::document_cursor::DocumentRange;
use crate::dop::Type;
use crate::hop::ast::{Ast, Node};

use super::AstTransform;

/// Transform that injects <!DOCTYPE html> at the beginning of entrypoint components
/// that don't already have a doctype declaration
pub struct DoctypeInjector;

impl DoctypeInjector {
    pub fn new() -> Self {
        Self
    }

    /// Check if a list of nodes starts with a DOCTYPE declaration (ignoring leading whitespace)
    fn has_doctype(nodes: &[Node<Type>]) -> bool {
        for node in nodes {
            match node {
                Node::Doctype { .. } => return true,
                Node::Text { range } if range.as_str().trim().is_empty() => {
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
    fn find_doctype_insert_position(nodes: &[Node<Type>]) -> usize {
        for (i, node) in nodes.iter().enumerate() {
            if let Node::Text { range } = node {
                if range.as_str().trim().is_empty() {
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
}

impl AstTransform for DoctypeInjector {
    fn transform(&mut self, ast: &mut Ast<Type>) {
        for component in ast.get_component_definitions_mut() {
            // Only inject DOCTYPE for entrypoint components
            if component.is_entrypoint && !Self::has_doctype(&component.children) {
                // Create a synthetic DOCTYPE node
                let doctype_node = Node::Doctype {
                    range: DocumentRange::new("<!DOCTYPE html>".to_string()),
                };

                // Find the right position to insert (after any leading whitespace)
                let insert_pos = Self::find_doctype_insert_position(&component.children);
                component.children.insert(insert_pos, doctype_node);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dop::Type;
    use crate::error_collector::ErrorCollector;
    use crate::hop::ast::{ComponentDefinition, Node};
    use crate::hop::module_name::ModuleName;
    use crate::hop::parser::parse;
    use crate::hop::tokenizer::Tokenizer;
    use crate::hop::type_checker::TypeChecker;
    use expect_test::expect;

    /// Helper to pretty-print component children for testing
    fn format_component_children(component: &ComponentDefinition<Type>) -> String {
        let mut output = String::new();
        for (i, node) in component.children.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            match node {
                Node::Doctype { range } => {
                    output.push_str(&format!("DOCTYPE: {}", range.as_str()));
                }
                Node::Text { range } => {
                    let text = range.as_str();
                    if text.trim().is_empty() {
                        output.push_str(&format!("Text: <whitespace:{} chars>", text.len()));
                    } else {
                        output.push_str(&format!("Text: {:?}", text));
                    }
                }
                Node::Html { tag_name, children, .. } => {
                    output.push_str(&format!("Html: <{}> ({} children)", tag_name.as_str(), children.len()));
                }
                Node::ComponentReference { tag_name, children, .. } => {
                    output.push_str(&format!("Component: <{}> ({} children)", tag_name.as_str(), children.len()));
                }
                Node::If { children, .. } => {
                    output.push_str(&format!("If: ({} children)", children.len()));
                }
                Node::For { children, .. } => {
                    output.push_str(&format!("For: ({} children)", children.len()));
                }
                Node::SlotDefinition { .. } => {
                    output.push_str("Slot");
                }
                Node::TextExpression { .. } => {
                    output.push_str("TextExpression");
                }
                Node::Placeholder { .. } => {
                    output.push_str("Placeholder");
                }
            }
        }
        output
    }

    /// Helper to create a typed AST from Hop source code
    fn create_typed_ast(source: &str) -> Ast<Type> {
        let mut errors = ErrorCollector::new();
        let module_name = ModuleName::new("test".to_string()).unwrap();
        let tokenizer = Tokenizer::new(source.to_string());
        let ast = parse(module_name.clone(), tokenizer, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        // Type check
        let mut typechecker = TypeChecker::default();
        typechecker.typecheck(&[&ast]);
        assert!(
            typechecker.type_errors.get(&module_name).unwrap().is_empty(),
            "Type errors: {:?}",
            typechecker.type_errors
        );

        // Return the typed AST
        typechecker.typed_asts.get(&module_name).unwrap().clone()
    }

    /// Helper to check DOCTYPE injection for entrypoint components
    fn check_entrypoint(input: &str, expected: expect_test::Expect) {
        let mut ast = create_typed_ast(input);

        // Apply transform
        let mut injector = DoctypeInjector::new();
        injector.transform(&mut ast);

        // Format output for all entrypoint components
        let mut output = String::new();
        for (i, component) in ast.get_component_definitions().iter().enumerate() {
            if component.is_entrypoint {
                if !output.is_empty() {
                    output.push_str("\n\n");
                }
                output.push_str(&format!("=== {} ===\n", component.tag_name.as_str()));
                output.push_str(&format_component_children(component));
            }
        }

        expected.assert_eq(&output);
    }

    /// Helper to check that non-entrypoints are not modified
    fn check_non_entrypoint(input: &str, expected: expect_test::Expect) {
        let mut ast = create_typed_ast(input);

        // Apply transform
        let mut injector = DoctypeInjector::new();
        injector.transform(&mut ast);

        // Format output for all non-entrypoint components
        let mut output = String::new();
        for (i, component) in ast.get_component_definitions().iter().enumerate() {
            if !component.is_entrypoint {
                if !output.is_empty() {
                    output.push_str("\n\n");
                }
                output.push_str(&format!("=== {} ===\n", component.tag_name.as_str()));
                output.push_str(&format_component_children(component));
            }
        }

        expected.assert_eq(&output);
    }

    #[test]
    fn test_inject_doctype_when_missing() {
        check_entrypoint(
            r#"
                <main-comp entrypoint>
                    <html>
                        <body>Hello World</body>
                    </html>
                </main-comp>
            "#,
            expect![[r#"
                === main-comp ===
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Html: <html> (3 children)
                Text: <whitespace:17 chars>"#]],
        );
    }

    #[test]
    fn test_no_injection_when_doctype_present() {
        check_entrypoint(
            r#"
                <main-comp entrypoint>
                    <!DOCTYPE html>
                    <html>
                        <body>Hello World</body>
                    </html>
                </main-comp>
            "#,
            expect![[r#"
                === main-comp ===
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Text: <whitespace:21 chars>
                Html: <html> (3 children)
                Text: <whitespace:17 chars>"#]],
        );
    }

    #[test]
    fn test_no_injection_for_non_entrypoint() {
        check_non_entrypoint(
            r#"
                <regular-comp>
                    <div>I'm not an entrypoint</div>
                </regular-comp>
            "#,
            expect![[r#"
                === regular-comp ===
                Text: <whitespace:21 chars>
                Html: <div> (1 children)
                Text: <whitespace:17 chars>"#]],
        );
    }

    #[test]
    fn test_multiple_entrypoints() {
        check_entrypoint(
            r#"
                <first-comp entrypoint>
                    <div>First</div>
                </first-comp>

                <second-comp entrypoint>
                    <!DOCTYPE html>
                    <div>Second</div>
                </second-comp>

                <third-comp entrypoint>
                    <div>Third</div>
                </third-comp>
            "#,
            expect![[r#"
                === first-comp ===
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Html: <div> (1 children)
                Text: <whitespace:17 chars>

                === second-comp ===
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Text: <whitespace:21 chars>
                Html: <div> (1 children)
                Text: <whitespace:17 chars>

                === third-comp ===
                Text: <whitespace:21 chars>
                DOCTYPE: <!DOCTYPE html>
                Html: <div> (1 children)
                Text: <whitespace:17 chars>"#]],
        );
    }

    #[test]
    fn test_empty_entrypoint() {
        check_entrypoint(
            r#"
                <empty-comp entrypoint>
                </empty-comp>
            "#,
            expect![[r#"
                === empty-comp ===
                Text: <whitespace:17 chars>
                DOCTYPE: <!DOCTYPE html>"#]],
        );
    }

    #[test]
    fn test_entrypoint_with_text_only() {
        check_entrypoint(
            r#"
                <text-comp entrypoint>
                    Just some text content
                </text-comp>
            "#,
            expect![[r#"
                === text-comp ===
                DOCTYPE: <!DOCTYPE html>
                Text: "\n                    Just some text content\n                ""#]],
        );
    }

    #[test]
    fn test_doctype_with_leading_whitespace() {
        check_entrypoint(
            r#"
                <main-comp entrypoint>

                    <!DOCTYPE html>
                    <html>Content</html>
                </main-comp>
            "#,
            expect![[r#"
                === main-comp ===
                Text: <whitespace:22 chars>
                DOCTYPE: <!DOCTYPE html>
                Text: <whitespace:21 chars>
                Html: <html> (1 children)
                Text: <whitespace:17 chars>"#]],
        );
    }

    #[test]
    fn test_inject_preserves_leading_whitespace() {
        check_entrypoint(
            r#"
                <main-comp entrypoint>

                    <html>Content</html>
                </main-comp>
            "#,
            expect![[r#"
                === main-comp ===
                Text: <whitespace:22 chars>
                DOCTYPE: <!DOCTYPE html>
                Html: <html> (1 children)
                Text: <whitespace:17 chars>"#]],
        );
    }

    #[test]
    fn test_idempotent_transform() {
        // Test that running the transform twice produces the same result
        let source = r#"
            <main-comp entrypoint>
                <div>Test</div>
            </main-comp>
        "#;

        let mut ast1 = create_typed_ast(source);
        let mut ast2 = create_typed_ast(source);
        let mut injector = DoctypeInjector::new();

        // Apply once
        injector.transform(&mut ast1);
        let once = format_component_children(&ast1.get_component_definitions()[0]);

        // Apply twice
        injector.transform(&mut ast2);
        injector.transform(&mut ast2);
        let twice = format_component_children(&ast2.get_component_definitions()[0]);

        // Should be identical
        assert_eq!(once, twice, "Transform should be idempotent");

        // Verify the expected output
        expect![[r#"
            Text: <whitespace:17 chars>
            DOCTYPE: <!DOCTYPE html>
            Html: <div> (1 children)
            Text: <whitespace:13 chars>"#]]
        .assert_eq(&once);
    }
}