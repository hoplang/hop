use crate::document::{CheapString, DocumentRange, Ranged};

use crate::hop::syntax::parsed_ast::{ParsedAst, ParsedComponentDeclaration, ParsedDeclaration};
use crate::hop::syntax::parsed_node::{ParsedMatchCase, ParsedNode};

/// Internal representation for grouping children into lines during whitespace processing.
enum LineContent {
    /// Inline content (Text and TextExpression nodes) that stays on the same line
    Inline(Vec<ParsedNode>),
    /// Block-level node (elements, control flow) that gets its own line
    Block(ParsedNode),
    /// Explicit line break from newline in source text
    Break(DocumentRange),
}

/// Removes leading and trailing whitespace from text nodes in a ParsedAst.
///
/// This works by:
/// 1. Grouping children into inline content (text + expressions) and block content (elements)
/// 2. Splitting text on newlines to create explicit LineBreak markers
/// 3. Trimming whitespace at line boundaries (start/end of inline groups)
/// 4. Inserting LineBreak nodes between groups
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

fn transform_nodes(nodes: Vec<ParsedNode>) -> Vec<ParsedNode> {
    if nodes.is_empty() {
        return nodes;
    }

    // Step 1: Group into LineContent
    let content = group_into_lines(nodes);

    // Step 2: Trim inline groups at their boundaries
    let content = trim_inline_groups(content);

    // Step 3: Flatten back to nodes with LineBreaks
    flatten_to_nodes(content)
}

/// Groups children into LineContent items.
///
/// - Text and TextExpression nodes are accumulated into inline groups
/// - Newlines in text cause the inline group to be flushed and a Break to be added
/// - Block nodes (elements, control flow) cause the inline group to be flushed
fn group_into_lines(nodes: Vec<ParsedNode>) -> Vec<LineContent> {
    let mut result = Vec::new();
    let mut current_inline: Vec<ParsedNode> = Vec::new();

    for node in nodes {
        match node {
            ParsedNode::Text { ref value, ref range } => {
                let text = value.as_str();
                let segments: Vec<&str> = text.split('\n').collect();

                for (i, segment) in segments.iter().enumerate() {
                    if i > 0 {
                        // Encountered a newline - flush current inline group
                        if !current_inline.is_empty() {
                            result.push(LineContent::Inline(std::mem::take(&mut current_inline)));
                        }
                        result.push(LineContent::Break(range.clone()));
                    }

                    // Add non-empty segment to current inline group
                    // We don't trim here - trimming happens in trim_inline_groups
                    if !segment.is_empty() {
                        current_inline.push(ParsedNode::Text {
                            value: CheapString::new(segment.to_string()),
                            range: range.clone(),
                        });
                    }
                }
            }
            ParsedNode::TextExpression { .. } => {
                // Expressions are inline content
                current_inline.push(node);
            }
            // All other nodes are block-level
            block_node => {
                // Flush current inline group
                if !current_inline.is_empty() {
                    result.push(LineContent::Inline(std::mem::take(&mut current_inline)));
                }

                // Recursively transform the block node's children
                let transformed = transform_block_node(block_node);
                result.push(LineContent::Block(transformed));
            }
        }
    }

    // Flush remaining inline group
    if !current_inline.is_empty() {
        result.push(LineContent::Inline(current_inline));
    }

    result
}

/// Recursively transforms a block node's children.
fn transform_block_node(node: ParsedNode) -> ParsedNode {
    match node {
        ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children,
            range,
        } => ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children: transform_nodes(children),
            range,
        },
        ParsedNode::If {
            condition,
            children,
            range,
        } => ParsedNode::If {
            condition,
            children: transform_nodes(children),
            range,
        },
        ParsedNode::For {
            var_name,
            var_name_range,
            source,
            children,
            range,
        } => ParsedNode::For {
            var_name,
            var_name_range,
            source,
            children: transform_nodes(children),
            range,
        },
        ParsedNode::Let {
            bindings,
            bindings_range,
            children,
            range,
        } => ParsedNode::Let {
            bindings,
            bindings_range,
            children: transform_nodes(children),
            range,
        },
        ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children,
            range,
        } => ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children: transform_nodes(children),
            range,
        },
        ParsedNode::Placeholder { children, range } => ParsedNode::Placeholder {
            children: transform_nodes(children),
            range,
        },
        ParsedNode::Match {
            subject,
            cases,
            range,
        } => ParsedNode::Match {
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
        },
        // Text, TextExpression, Doctype, LineBreak pass through unchanged
        other => other,
    }
}

/// Trims whitespace at the boundaries of inline groups.
fn trim_inline_groups(content: Vec<LineContent>) -> Vec<LineContent> {
    content
        .into_iter()
        .map(|item| match item {
            LineContent::Inline(nodes) => LineContent::Inline(trim_inline_group(nodes)),
            other => other,
        })
        .collect()
}

/// Trims an inline group:
/// - Trim leading whitespace from the first Text node
/// - Trim trailing whitespace from the last Text node
/// - Filter out empty Text nodes
fn trim_inline_group(mut nodes: Vec<ParsedNode>) -> Vec<ParsedNode> {
    if nodes.is_empty() {
        return nodes;
    }

    // Handle single-node case (need to trim both ends)
    if nodes.len() == 1 {
        if let ParsedNode::Text { ref mut value, .. } = nodes[0] {
            let trimmed = value.as_str().trim();
            *value = CheapString::new(trimmed.to_string());
        }
    } else {
        // Trim leading whitespace from first Text node
        if let Some(ParsedNode::Text { value, .. }) = nodes.first_mut() {
            let trimmed = value.as_str().trim_start();
            *value = CheapString::new(trimmed.to_string());
        }

        // Trim trailing whitespace from last Text node
        if let Some(ParsedNode::Text { value, .. }) = nodes.last_mut() {
            let trimmed = value.as_str().trim_end();
            *value = CheapString::new(trimmed.to_string());
        }
    }

    // Filter out empty Text nodes
    nodes
        .into_iter()
        .filter(|node| match node {
            ParsedNode::Text { value, .. } => !value.as_str().is_empty(),
            _ => true,
        })
        .collect()
}

/// Flattens LineContent back to Vec<ParsedNode>, inserting LineBreak nodes.
/// LineBreak nodes are only inserted between inline elements, not around blocks.
/// This is because LineBreak becomes a space in the typed AST, and we don't want
/// extra spaces around block elements.
fn flatten_to_nodes(content: Vec<LineContent>) -> Vec<ParsedNode> {
    let mut result: Vec<ParsedNode> = Vec::new();
    let mut last_was_inline = false;

    for item in content {
        match item {
            LineContent::Inline(nodes) => {
                if nodes.is_empty() {
                    // Empty inline group (was whitespace-only) - skip
                    continue;
                }

                // Add LineBreak before this inline content only if previous was also inline
                // (and there's a pending line break from a Break item)
                if last_was_inline
                    && !result.is_empty()
                    && matches!(result.last(), Some(ParsedNode::LineBreak { .. }))
                {
                    // LineBreak already added by Break, keep it
                } else if last_was_inline && !result.is_empty() {
                    // Need to add LineBreak between consecutive inline groups
                    let range = nodes[0].range().clone();
                    result.push(ParsedNode::LineBreak { range });
                }

                result.extend(nodes);
                last_was_inline = true;
            }
            LineContent::Block(node) => {
                // Remove any pending LineBreak before a block (we don't want spaces around blocks)
                if matches!(result.last(), Some(ParsedNode::LineBreak { .. })) {
                    result.pop();
                }

                result.push(node);
                last_was_inline = false;
            }
            LineContent::Break(range) => {
                // Only add LineBreak if last was inline (for potential inline->inline transition)
                if last_was_inline
                    && !result.is_empty()
                    && !matches!(result.last(), Some(ParsedNode::LineBreak { .. }))
                {
                    result.push(ParsedNode::LineBreak { range });
                }
            }
        }
    }

    // Remove trailing LineBreak if present
    if matches!(result.last(), Some(ParsedNode::LineBreak { .. })) {
        result.pop();
    }

    result
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

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            Document::new(source.to_string()),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        // Use format() which calls remove_whitespace internally
        let formatted = formatter::format(ast);
        expected.assert_eq(&formatted);
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

    // New tests for inline content preservation

    #[test]
    fn preserves_inline_expressions_on_same_line() {
        check(
            "<Main {x: String, y: String}><div>{x} {y}</div></Main>",
            expect![[r#"
                <Main {x: String, y: String}>
                  <div>
                    {x} {y}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn preserves_text_between_expressions() {
        check(
            "<Main {x: String, y: String}><div>{x} hello {y}</div></Main>",
            expect![[r#"
                <Main {x: String, y: String}>
                  <div>
                    {x} hello {y}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn breaks_line_around_elements() {
        check(
            "<Main><div>hello<p>world</p></div></Main>",
            expect![[r#"
                <Main>
                  <div>
                    hello
                    <p>
                      world
                    </p>
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn breaks_line_around_elements_with_trailing_text() {
        check(
            "<Main><div>hello <b>world</b>!</div></Main>",
            expect![[r#"
                <Main>
                  <div>
                    hello
                    <b>
                      world
                    </b>
                    !
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn preserves_string_literal_expressions() {
        check(
            r#"<Main><div>{x}{"  "}{y}</div></Main>"#,
            expect![[r#"
                <Main>
                  <div>
                    {x}{"  "}{y}
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn trims_leading_indentation_per_line() {
        check(
            indoc! {"
                <Main>
                <div>
                   foo
                  bar
                </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    foo
                    bar
                  </div>
                </Main>
            "#]],
        );
    }

    #[test]
    fn removes_multiple_empty_lines() {
        check(
            indoc! {"
                <Main>
                <div>

                foo

                bar

                </div>
                </Main>
            "},
            expect![[r#"
                <Main>
                  <div>
                    foo
                    bar
                  </div>
                </Main>
            "#]],
        );
    }
}
