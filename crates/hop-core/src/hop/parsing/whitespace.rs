use super::parsed_node::ParsedNode;
use crate::html::HtmlElement;

/// Normalize whitespace in a parsed node sequence.
///
/// The pass handles two concerns:
///
/// ## 1. Whitespace Trimming
///
/// - Trim the start of a Text that begins its sequence or follows a Newline
/// - Trim the end of a Text that ends its sequence or precedes a Newline
/// - Drop Text nodes that are empty after trimming
///
/// ## 2. Newline-to-Space Conversion
///
/// - Keep a Newline only between two Text nodes. A newline beside anything
///   else, a tag or an interpolation, emits nothing.
///
/// The children of raw text elements (`<script>`, `<style>`) are left alone.
pub fn normalize(nodes: &mut Vec<ParsedNode>) {
    trim_text(nodes);
    drop_newlines(nodes);
    for node in nodes.iter_mut() {
        normalize_children(node);
    }
}

fn normalize_children(node: &mut ParsedNode) {
    match node {
        // The content of a raw text element is passed through verbatim.
        ParsedNode::Html {
            element: HtmlElement::Script | HtmlElement::Style,
            ..
        } => {}
        ParsedNode::Html { children, .. }
        | ParsedNode::Fragment { children, .. }
        | ParsedNode::If { children, .. }
        | ParsedNode::For { children, .. }
        | ParsedNode::Let { children, .. } => normalize(children),
        ParsedNode::ComponentInvocation { children, .. } => {
            if let Some(children) = children {
                normalize(children);
            }
        }
        ParsedNode::Match { cases, .. } => {
            for case in cases {
                normalize(&mut case.children);
            }
        }
        ParsedNode::Text { .. }
        | ParsedNode::Newline { .. }
        | ParsedNode::TextExpression { .. }
        | ParsedNode::Comment { .. } => {}
    }
}

fn is_newline(node: &ParsedNode) -> bool {
    matches!(node, ParsedNode::Newline { .. })
}

fn trim_text(nodes: &mut Vec<ParsedNode>) {
    for i in 0..nodes.len() {
        let trim_start = i == 0 || is_newline(&nodes[i - 1]);
        let trim_end = i + 1 == nodes.len() || is_newline(&nodes[i + 1]);
        let ParsedNode::Text { range } = &nodes[i] else {
            continue;
        };
        let range = match (trim_start, trim_end) {
            (true, true) => range.trim(),
            (true, false) => range.trim_start(),
            (false, true) => range.trim_end(),
            (false, false) => continue,
        };
        nodes[i] = ParsedNode::Text { range };
    }
    nodes.retain(|node| match node {
        ParsedNode::Text { range } => !range.as_str().is_empty(),
        _ => true,
    });
}

fn drop_newlines(nodes: &mut Vec<ParsedNode>) {
    let keep: Vec<bool> = (0..nodes.len())
        .map(|i| {
            if !is_newline(&nodes[i]) {
                return true;
            }
            let preceded_by_text = nodes[..i]
                .iter()
                .rev()
                .find(|node| !is_newline(node))
                .is_some_and(|node| matches!(node, ParsedNode::Text { .. }));
            let followed_by_text = nodes
                .get(i + 1)
                .is_some_and(|node| matches!(node, ParsedNode::Text { .. }));
            preceded_by_text && followed_by_text
        })
        .collect();
    let mut keep = keep.into_iter();
    nodes.retain(|_| keep.next().unwrap());
}

#[cfg(test)]
mod tests {
    use crate::document::Document;
    use crate::document_id::DocumentId;
    use crate::hop::format;
    use crate::hop::parsing::parse;
    use crate::ir::runtime::evaluator;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
    use crate::program::Program;
    use crate::symbols::type_name::TypeName;
    use indoc::indoc;
    use std::collections::HashMap;

    fn check(source: &str, expected: &str) {
        assert_eq!(render(source), expected);

        // Formatting a view must not change what it renders.
        let formatted = reformat(source);
        assert_eq!(
            render(&formatted),
            expected,
            "render changed after formatting:\n{formatted}"
        );
    }

    fn reformat(source: &str) -> String {
        let document_id = DocumentId::new("test.hop").unwrap();
        let mut errors = Vec::new();
        let ast = parse::parse(
            document_id.clone(),
            Document::new(document_id, source.to_string()),
            &mut errors,
        );
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        format(&ast)
    }

    fn render(source: &str) -> String {
        let document_id = DocumentId::new("test.hop").unwrap();
        let mut program = Program::default();
        program.update_module(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );

        let parse_errors = program.get_parse_errors();
        assert!(
            parse_errors.values().all(|e| e.is_empty()),
            "parse errors: {parse_errors:?}"
        );
        let type_errors = program.get_type_errors();
        assert!(
            type_errors.values().all(|e| e.is_empty()),
            "type errors: {type_errors:?}"
        );

        let typed_asts = program.get_typed_modules().clone();
        let page_name = TypeName::new("Test").unwrap();
        let module = orchestrate_pure(
            &typed_asts,
            OrchestrateOptions {
                skip_html_structure: true,
                skip_optimization: true,
                ..Default::default()
            },
        );
        evaluator::evaluate_page(&module, &page_name, HashMap::new()).expect("evaluator failed")
    }

    #[test]
    fn trims_text_against_the_tags_around_it() {
        check(
            indoc! {"
                view Test {
                  <div>
                    hello
                  </div>
                }
            "},
            "<div>hello</div>",
        );
    }

    #[test]
    fn turns_a_newline_between_text_into_a_space() {
        check(
            indoc! {"
                view Test {
                  <div>
                    hello
                    world
                  </div>
                }
            "},
            "<div>hello world</div>",
        );
    }

    #[test]
    fn trims_trailing_whitespace_before_a_newline() {
        check(
            concat!(
                "view Test {\n",
                "  <div>\n",
                "    hello  \n",
                "    world\n",
                "  </div>\n",
                "}\n",
            ),
            "<div>hello world</div>",
        );
    }

    #[test]
    fn collapses_a_run_of_newlines_into_one_space() {
        check(
            indoc! {"
                view Test {
                  <div>
                    hello

                    world
                  </div>
                }
            "},
            "<div>hello world</div>",
        );
    }

    #[test]
    fn drops_a_newline_between_text_and_expression() {
        check(
            indoc! {r#"
                view Test {
                  <div>
                    hello
                    {"world"}
                  </div>
                }
            "#},
            "<div>helloworld</div>",
        );
    }

    #[test]
    fn drops_a_newline_next_to_a_tag() {
        check(
            indoc! {"
                view Test {
                  <div>
                    hello
                    <span>world</span>
                  </div>
                }
            "},
            "<div>hello<span>world</span></div>",
        );
    }

    #[test]
    fn keeps_a_space_before_a_tag_on_the_same_line() {
        check(
            indoc! {"
                view Test {
                  <div>hello <span>world</span></div>
                }
            "},
            "<div>hello <span>world</span></div>",
        );
    }

    #[test]
    fn trims_text_at_the_end_of_a_body() {
        check("view Test {hello }\n", "hello");
    }

    #[test]
    fn preserves_script_content_verbatim() {
        check(
            indoc! {"
                view Test {
                  <script>
                    let x = 1;
                  </script>
                }
            "},
            "<script>\n    let x = 1;\n  </script>",
        );
    }

    #[test]
    fn preserves_spaces_inside_expression() {
        check(
            indoc! {r#"
                view Test {
                  {"   "}
                }
            "#},
            "   ",
        );
    }

    #[test]
    fn preserves_content_betwen_two_interpolations_on_single_line() {
        check(
            indoc! {r#"
                view Test {
                  <let {first: String = "Hello", second: String = "World"}>
                    <div>{first} {second}</div>
                  </let>
                }
            "#},
            "<div>Hello World</div>",
        );
    }

    #[test]
    fn preserves_whitespace_before_tag_on_single_line() {
        check(
            indoc! {"
                view Test {
                  <>this looks <b>great</b></>
                }
            "},
            "this looks <b>great</b>",
        );
    }

    #[test]
    fn preserves_style_content_verbatim() {
        check(
            indoc! {"
                view Test {
                  <style>
                    .a { color: red; }
                  </style>
                }
            "},
            "<style>\n    .a { color: red; }\n  </style>",
        );
    }

    #[test]
    fn keeps_a_space_between_two_tags_on_the_same_line() {
        check(
            indoc! {"
                view Test {
                  <><b>b</b> <i>i</i></>
                }
            "},
            "<b>b</b> <i>i</i>",
        );
    }

    #[test]
    fn drops_a_line_break_between_two_tags() {
        check(
            indoc! {"
                view Test {
                  <>
                    <b>b</b>
                    <i>i</i>
                  </>
                }
            "},
            "<b>b</b><i>i</i>",
        );
    }

    #[test]
    fn keeps_a_space_between_two_expressions_on_the_same_line() {
        check(
            indoc! {r#"
                view Test {
                  <>{"a"} {"b"}</>
                }
            "#},
            "a b",
        );
    }

    #[test]
    fn drops_a_newline_between_two_expressions() {
        check(
            indoc! {r#"
                view Test {
                  <>
                    {"a"}
                    {"b"}
                  </>
                }
            "#},
            "ab",
        );
    }

    #[test]
    fn keeps_a_space_between_a_tag_and_an_expression() {
        check(
            indoc! {r#"
                view Test {
                  <><b>b</b> {"i"}</>
                }
            "#},
            "<b>b</b> i",
        );
    }

    #[test]
    fn keeps_a_run_of_spaces_beside_a_tag() {
        check(
            indoc! {"
                view Test {
                  <>a  <b>x</b></>
                }
            "},
            "a  <b>x</b>",
        );
    }

    #[test]
    fn keeps_whitespace_on_the_side_that_has_no_linebreak() {
        check(
            indoc! {r#"
                view Test {
                  <><b>x</b>  a {"y"}</>
                }
            "#},
            "<b>x</b>  a y",
        );
    }

    #[test]
    fn renders_a_fragment_as_its_children() {
        check(
            indoc! {"
                view Test {
                  <><b>x</b><i>y</i></>
                }
            "},
            "<b>x</b><i>y</i>",
        );
    }

    #[test]
    fn renders_an_empty_fragment_as_nothing() {
        check(
            indoc! {"
                view Test {
                  <></>
                }
            "},
            "",
        );
    }

    #[test]
    fn trims_the_children_of_a_fragment_against_its_tags() {
        check(
            indoc! {"
                view Test {
                  <div>
                    hello
                    <>
                      world
                    </>
                  </div>
                }
            "},
            "<div>helloworld</div>",
        );
    }

    #[test]
    fn keeps_a_space_written_beside_a_fragment() {
        check(
            indoc! {"
                view Test {
                  <>hello <>world</></>
                }
            "},
            "hello world",
        );
    }
}
