use crate::document::DocumentPosition;
use crate::document::document_cursor::Ranged;

use super::parsed_ast::ParsedAst;
use super::parsed_node::ParsedNode;

/// Finds the deepest AST node that contains the given position.
///
/// Upper bound on time complexity is max(depth of tree, number of top level nodes).
///
/// # Example
///
/// <div>
///     <span>text</span>
///                  ^
/// </div>
///
/// returns
///
/// <div>
///     <span>text</span>
///     ^^^^^^^^^^^^^^^^^
/// </div>
///
pub fn find_node_at_position(ast: &ParsedAst, position: DocumentPosition) -> Option<&ParsedNode> {
    for n in ast.get_component_declarations() {
        if n.range.contains_position(position) {
            for child in &n.children {
                if let Some(node) = find_node_at_position_in_node(child, position) {
                    return Some(node);
                }
            }
            return None;
        }
    }

    None
}

fn find_node_at_position_in_node(
    node: &ParsedNode,
    position: DocumentPosition,
) -> Option<&ParsedNode> {
    if !node.range().contains_position(position) {
        return None;
    }
    for child in node.children() {
        if let Some(found) = find_node_at_position_in_node(child, position) {
            return Some(found);
        }
    }
    Some(node)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::DocumentAnnotator;
    use crate::document::SimpleAnnotation;
    use crate::document::document_cursor::Ranged;
    use crate::document::extract_position::extract_position;
    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parser::parse;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_find_node_at_position(input: &str, expected: Expect) {
        let (source, position) = extract_position(input).expect("Position marker not found");
        let mut errors = ErrorCollector::new();
        let ast = parse(ModuleName::new("test").unwrap(), source, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        let found_node = find_node_at_position(&ast, position);

        let output = if let Some(node) = found_node {
            let annotator = DocumentAnnotator::new().without_location();
            annotator.annotate(
                None,
                [SimpleAnnotation {
                    range: node.range().clone(),
                    message: "range".to_string(),
                }],
            )
        } else {
            "No node found at position".to_string()
        };

        expected.assert_eq(&output);
    }

    #[test]
    fn should_find_text_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>Hello World</div>
                             ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div>Hello World</div>
                  |          ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_html_element_when_on_tag_name() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>Content</div>
                     ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div>Content</div>
                  |     ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_component_reference() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <FooBar>Content</FooBar>
                        ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <FooBar>Content</FooBar>
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_if_node() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <if {true}>
                        ^
                        <div/>
                    </if>
                </Main>
            "},
            expect![[r#"
                range
                2 |     <if {true}>
                  |     ^^^^^^^^^^^
                3 |         <div/>
                  | ^^^^^^^^^^^^^^
                4 |     </if>
                  | ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_nested_text_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>
                        <span>Nested text</span>
                                    ^
                    </div>
                </Main>
            "},
            expect![[r#"
                range
                3 |         <span>Nested text</span>
                  |               ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_return_none_when_position_is_outside_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>Content</div>
                </Main>
                ^
            "},
            expect!["No node found at position"],
        );
    }

    #[test]
    fn should_find_doctype() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <!DOCTYPE html>
                     ^
                    <div>Content</div>
                </Main>
            "},
            expect![[r#"
                range
                2 |     <!DOCTYPE html>
                  |     ^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_expression_in_deeply_nested_structure() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>
                        <if {condition}>
                            <for {item in items}>
                                <span>{item}</span>
                                        ^
                            </for>
                        </if>
                    </div>
                </Main>
            "},
            expect![[r#"
                range
                5 |                 <span>{item}</span>
                  |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_void_element() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <p>Some text <br> more text</p>
                                  ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <p>Some text <br> more text</p>
                  |                  ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_first_element_on_line_with_multiple_nodes() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><span>Hello</span> <strong>World</strong></div>
                           ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |          ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_second_element_on_line_with_multiple_nodes() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><span>Hello</span> <strong>World</strong></div>
                                               ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |                             ^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_text_between_elements_on_same_line() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><span>Hello</span> and <strong>World</strong></div>
                                            ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> and <strong>World</strong></div>
                  |                            ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_expression_in_very_deep_nesting() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>
                        <section>
                            <article>
                                <if {condition}>
                                    <for {item in items}>
                                        <header>
                                            <h1>
                                                <span>
                                                    <em>Deep {item.name} text</em>
                                                             ^
                                                </span>
                                            </h1>
                                        </header>
                                    </for>
                                </if>
                            </article>
                        </section>
                    </div>
                </Main>
            "},
            expect![[r#"
                range
                10 |                                     <em>Deep {item.name} text</em>
                   |                                              ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_parent_element_in_deep_nesting() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>
                        <section>
                            <h1>
                                <em>Deep text</em>
                                <span>
                                     ^
                                    <div></div>
                                    <em>Deep text</em>
                                </span>
                            </h1>
                        </section>
                    </div>
                </Main>
            "},
            expect![[r#"
                range
                 6 |                 <span>
                   |                 ^^^^^^
                 7 |                     <div></div>
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 8 |                     <em>Deep text</em>
                   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                 9 |                 </span>
                   | ^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_inline_element_with_expression() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                                                                  ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                  |                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_text_inside_component_with_inline_content() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div><UserCard {data: user}><span>Content</span></UserCard> more text</div>
                                                        ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div><UserCard {data: user}><span>Content</span></UserCard> more text</div>
                  |                                       ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_element_in_nested_control_structures() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <if {users}>
                        <for {user in users}>
                            <if {user.active}>
                                <for {role in user.roles}>
                                    <span>{role}</span>
                                       ^
                                </for>
                            </if>
                        </for>
                    </if>
                </Main>
            "},
            expect![[r#"
                range
                 6 |                     <span>{role}</span>
                   |                     ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_self_closing_element_with_attributes() {
        check_find_node_at_position(
            indoc! {r#"
                <Main>
                    <div>
                        <input type="text" placeholder="Enter name" required />
                               ^
                        <br/>
                    </div>
                </Main>
            "#},
            expect![[r#"
                range
                3 |         <input type="text" placeholder="Enter name" required />
                  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_element_between_closing_and_opening_tags() {
        check_find_node_at_position(
            indoc! {"
                <Main>
                    <div>First</div> <div>Second</div>
                                     ^
                </Main>
            "},
            expect![[r#"
                range
                2 |     <div>First</div> <div>Second</div>
                  |                      ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
