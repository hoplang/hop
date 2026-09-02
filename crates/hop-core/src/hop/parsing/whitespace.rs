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
/// - Keep a Newline only between two pieces of inline content, i.e. Text or
///   TextExpression.
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

fn is_inline(node: &ParsedNode) -> bool {
    matches!(
        node,
        ParsedNode::Text { .. } | ParsedNode::TextExpression { .. }
    )
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
            let preceded_by_inline = nodes[..i]
                .iter()
                .rev()
                .find(|node| !is_newline(node))
                .is_some_and(is_inline);
            let followed_by_inline = nodes.get(i + 1).is_some_and(is_inline);
            preceded_by_inline && followed_by_inline
        })
        .collect();
    let mut keep = keep.into_iter();
    nodes.retain(|_| keep.next().unwrap());
}

#[cfg(test)]
mod tests {
    use crate::document::Document;
    use crate::document_id::DocumentId;
    use crate::ir::runtime::evaluator;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
    use crate::program::Program;
    use crate::symbols::type_name::TypeName;
    use indoc::indoc;
    use std::collections::HashMap;

    fn check(source: &str, expected: &str) {
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
        let render = |skip_optimization| {
            let module = orchestrate_pure(
                &typed_asts,
                OrchestrateOptions {
                    skip_html_structure: true,
                    skip_optimization,
                    ..Default::default()
                },
            );
            evaluator::evaluate_page(&module, &page_name, HashMap::new()).expect("evaluator failed")
        };

        let unoptimized = render(true);
        let optimized = render(false);
        assert_eq!(
            unoptimized, optimized,
            "optimization changed the rendered output"
        );
        assert_eq!(unoptimized, expected);
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
    fn turns_a_newline_between_text_and_expression_into_a_space() {
        check(
            indoc! {r#"
                view Test {
                  <div>
                    hello
                    {"world"}
                  </div>
                }
            "#},
            "<div>hello world</div>",
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
}
