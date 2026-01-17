use crate::hop::syntax::parsed_ast::{ParsedAst, ParsedComponentDeclaration, ParsedDeclaration};
use crate::hop::syntax::parsed_node::{ParsedMatchCase, ParsedNode};

/// Removes whitespace from a ParsedAst to normalize it for formatting.
///
/// This works by:
/// 1. Trimming leading whitespace from Text nodes that follow a Newline (or are first)
/// 2. Trimming trailing whitespace from Text nodes that precede a Newline (or are last)
/// 3. Preserving interior whitespace between inline elements (Text, TextExpression)
/// 4. Removing Text nodes that become empty after trimming
/// 5. Keeping Newline nodes as hints for the formatter
pub fn remove_whitespace(ast: ParsedAst) -> ParsedAst {
    let declarations = ast
        .get_declarations()
        .iter()
        .cloned()
        .map(transform_declaration)
        .collect();
    ParsedAst::new(ast.name.clone(), declarations, ast.comments().clone())
}

fn transform_declaration(decl: ParsedDeclaration) -> ParsedDeclaration {
    match decl {
        ParsedDeclaration::Component(comp) => {
            ParsedDeclaration::Component(transform_component(comp))
        }
        other => other,
    }
}

fn transform_component(comp: ParsedComponentDeclaration) -> ParsedComponentDeclaration {
    ParsedComponentDeclaration {
        children: transform_nodes(comp.children),
        ..comp
    }
}

/// Classifies a node for whitespace trimming decisions.
#[derive(Clone, Copy, PartialEq)]
enum NodeKind {
    Newline,
    Text,
    Other, // TextExpression, Html, etc.
}

fn classify_node(node: &ParsedNode) -> NodeKind {
    match node {
        ParsedNode::Newline { .. } => NodeKind::Newline,
        ParsedNode::Text { .. } => NodeKind::Text,
        _ => NodeKind::Other,
    }
}

fn transform_nodes(nodes: Vec<ParsedNode>) -> Vec<ParsedNode> {
    if nodes.is_empty() {
        return nodes;
    }

    // Pre-classify all nodes to determine trimming context
    let kinds: Vec<NodeKind> = nodes.iter().map(classify_node).collect();
    let len = nodes.len();

    nodes
        .into_iter()
        .enumerate()
        .filter_map(|(i, node)| {
            let prev_kind = if i > 0 { Some(kinds[i - 1]) } else { None };
            let next_kind = if i + 1 < len {
                Some(kinds[i + 1])
            } else {
                None
            };
            transform_node_with_context(node, prev_kind, next_kind)
        })
        .collect()
}

fn transform_node_with_context(
    node: ParsedNode,
    prev_kind: Option<NodeKind>,
    next_kind: Option<NodeKind>,
) -> Option<ParsedNode> {
    match node {
        ParsedNode::Text { range } => {
            // Trim start if preceded by Newline or at start of children
            let trim_start = prev_kind.is_none() || prev_kind == Some(NodeKind::Newline);
            // Trim end if followed by Newline or at end of children
            let trim_end = next_kind.is_none() || next_kind == Some(NodeKind::Newline);

            let trimmed = match (trim_start, trim_end) {
                (true, true) => range.trim(),
                (true, false) => range.trim_start(),
                (false, true) => range.trim_end(),
                (false, false) => range,
            };

            if trimmed.as_str().is_empty() {
                None
            } else {
                Some(ParsedNode::Text { range: trimmed })
            }
        }
        // Keep Newline nodes - they serve as hints for where to insert line breaks
        ParsedNode::Newline { .. } => Some(node),
        ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children,
            range,
        } => Some(ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children: transform_nodes(children),
            range,
        }),
        ParsedNode::If {
            condition,
            children,
            range,
        } => Some(ParsedNode::If {
            condition,
            children: transform_nodes(children),
            range,
        }),
        ParsedNode::For {
            var_name,
            var_name_range,
            source,
            children,
            range,
        } => Some(ParsedNode::For {
            var_name,
            var_name_range,
            source,
            children: transform_nodes(children),
            range,
        }),
        ParsedNode::Let {
            bindings,
            bindings_range,
            children,
            range,
        } => Some(ParsedNode::Let {
            bindings,
            bindings_range,
            children: transform_nodes(children),
            range,
        }),
        ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children,
            range,
        } => Some(ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children: transform_nodes(children),
            range,
        }),
        ParsedNode::Placeholder { children, range } => Some(ParsedNode::Placeholder {
            children: transform_nodes(children),
            range,
        }),
        ParsedNode::Match {
            subject,
            cases,
            range,
        } => Some(ParsedNode::Match {
            subject,
            cases: cases
                .into_iter()
                .map(|case| ParsedMatchCase {
                    pattern: case.pattern,
                    pattern_range: case.pattern_range,
                    children: transform_nodes(case.children),
                    range: case.range,
                })
                .collect(),
            range,
        }),
        other => Some(other),
    }
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::document::Document;
    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::formatter;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;

    use super::remove_whitespace;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            Document::new(source.to_string()),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        let ast = remove_whitespace(ast);
        expected.assert_eq(&formatter::format(ast));
    }

    #[test]
    fn removes_leading_whitespace_from_text() {
        check(
            indoc! {"
                <Main>
                    hello
                </Main>
            "},
            expect![[r#"
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn removes_trailing_whitespace_from_text() {
        check(
            "<Main>hello   </Main>",
            expect![[r#"
                <Main>
                  hello
                </Main>
            "#]],
        );
    }

    #[test]
    fn splits_multiline_text_into_separate_nodes() {
        check(
            indoc! {"
                <Main>
                  first
                  second
                  third
                </Main>
            "},
            expect![[r#"
                <Main>
                  first
                  second
                  third
                </Main>
            "#]],
        );
    }

    #[test]
    fn removes_empty_lines() {
        check(
            indoc! {"
                <Main>

                  content

                </Main>
            "},
            expect![[r#"
                <Main>
                  content
                </Main>
            "#]],
        );
    }

    #[test]
    fn removes_whitespace_only_lines() {
        check(
            "<Main>\n   \n  text\n   \n</Main>",
            expect![[r#"
                <Main>
                  text
                </Main>
            "#]],
        );
    }

    #[test]
    fn handles_nested_html_elements() {
        check(
            indoc! {"
                <Main>
                  <div>
                    <span>
                      nested
                    </span>
                  </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    <span>
                      nested
                    </span>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn handles_for_loop_children() {
        check(
            indoc! {"
                <Main {items: Array[String]}>
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
                </Main>
            "},
            expect![[r#"
                <Main {items: Array[String]}>
                  <for {item in items}>
                    <div>
                      {item}
                    </div>
                  </for>
                </Main>
            "#]],
        );
    }

    #[test]
    fn handles_if_condition_children() {
        check(
            indoc! {"
                <Main {show: Bool}>
                  <if {show}>
                    <div>
                      visible
                    </div>
                  </if>
                </Main>
            "},
            expect![[r#"
                <Main {show: Bool}>
                  <if {show}>
                    <div>
                      visible
                    </div>
                  </if>
                </Main>
            "#]],
        );
    }

    #[test]
    fn preserves_record_declarations() {
        check(
            indoc! {"
                record User { name: String, age: Int }
                <Main {user: User}>
                  <div>
                    {user.name}
                  </div>
                </Main>
            "},
            expect![[r#"
                record User {
                  name: String,
                  age: Int,
                }

                <Main {user: User}>
                  <div>
                    {user.name}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn preserves_enum_declarations() {
        check(
            indoc! {"
                enum Status { Active, Inactive }
                <Main></Main>
            "},
            expect![[r#"
                enum Status {
                  Active,
                  Inactive,
                }

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn preserves_import_declarations() {
        check(
            indoc! {"
                import components::Button
                <Main></Main>
            "},
            expect![[r#"
                import components::Button

                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn handles_empty_component() {
        check(
            "<Main></Main>",
            expect![[r#"
                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn handles_component_with_only_whitespace() {
        check(
            "<Main>   \n   \n   </Main>",
            expect![[r#"
                <Main></Main>
            "#]],
        );
    }

    #[test]
    fn handles_expression_nodes() {
        check(
            indoc! {"
                <Main {name: String}>
                  {name}
                </Main>
            "},
            expect![[r#"
                <Main {name: String}>
                  {name}
                </Main>
            "#]],
        );
    }

    #[test]
    fn handles_mixed_text_and_expressions() {
        check(
            indoc! {"
                <Main {name: String}>
                  Hello
                  {name}
                  World
                </Main>
            "},
            expect![[r#"
                <Main {name: String}>
                  Hello
                  {name}
                  World
                </Main>
            "#]],
        );
    }
}
