use crate::document::document_cursor::{DocumentRange, StringSpan};

use crate::hop::syntax::parsed_ast::{ParsedAst, ParsedComponentDeclaration, ParsedDeclaration};
use crate::hop::syntax::parsed_node::{ParsedMatchCase, ParsedNode};

/// Removes leading and trailing whitespace from text nodes in a ParsedAst.
///
/// This works by:
/// 1. Splitting text on newlines
/// 2. Trimming leading and trailing whitespace from each piece
/// 3. Filtering out empty pieces
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
    nodes.into_iter().flat_map(transform_node).collect()
}

fn transform_node(node: ParsedNode) -> Vec<ParsedNode> {
    match node {
        ParsedNode::Text { value, range } => split_text_node(&value, &range),
        ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children,
            range,
        } => vec![ParsedNode::ComponentReference {
            component_name,
            component_name_opening_range,
            component_name_closing_range,
            declaring_module,
            args,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::If {
            condition,
            children,
            range,
        } => vec![ParsedNode::If {
            condition,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::For {
            var_name,
            var_name_range,
            array_expr,
            children,
            range,
        } => vec![ParsedNode::For {
            var_name,
            var_name_range,
            array_expr,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::Let {
            bindings,
            children,
            range,
        } => vec![ParsedNode::Let {
            bindings,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children,
            range,
        } => vec![ParsedNode::Html {
            tag_name,
            closing_tag_name,
            attributes,
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::Placeholder { children, range } => vec![ParsedNode::Placeholder {
            children: transform_nodes(children),
            range,
        }],
        ParsedNode::Match {
            subject,
            cases,
            range,
        } => vec![ParsedNode::Match {
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
        }],
        other => vec![other],
    }
}

fn split_text_node(value: &StringSpan, range: &DocumentRange) -> Vec<ParsedNode> {
    value
        .as_str()
        .split('\n')
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| ParsedNode::Text {
            value: StringSpan::new(line.to_string()),
            range: range.clone(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use indoc::indoc;

    use crate::error_collector::ErrorCollector;
    use crate::hop::symbols::module_name::ModuleName;
    use crate::hop::syntax::parse_error::ParseError;
    use crate::hop::syntax::parser;

    use super::remove_whitespace;

    fn check(source: &str, expected: Expect) {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(
            ModuleName::new("test").unwrap(),
            source.to_string(),
            &mut errors,
        );
        assert!(errors.is_empty(), "Parse errors: {:?}", errors);
        let ast = remove_whitespace(ast);
        expected.assert_eq(&ast.to_doc().pretty(60).to_string());
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
