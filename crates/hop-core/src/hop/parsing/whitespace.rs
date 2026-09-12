use super::parsed_node::ParsedNode;
use crate::html::HtmlElementKind;

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
fn normalize(nodes: &mut Vec<ParsedNode>) {
    trim_text(nodes);
    drop_newlines(nodes);
    for node in nodes.iter_mut() {
        normalize_node(node);
    }
}

/// Normalize whitespace inside a single node.
pub fn normalize_node(node: &mut ParsedNode) {
    match node {
        // The content of a raw text element is passed through verbatim.
        ParsedNode::HtmlElement {
            kind: HtmlElementKind::Script | HtmlElementKind::Style,
            ..
        } => {}
        ParsedNode::HtmlElement { children, .. }
        | ParsedNode::Fragment { children, .. }
        | ParsedNode::For { children, .. } => normalize(children),
        ParsedNode::FunctionInvocation { children, .. } => {
            if let Some(children) = children {
                normalize(children);
            }
        }
        ParsedNode::Text { .. }
        | ParsedNode::Newline { .. }
        | ParsedNode::Interpolation { .. }
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
    use crate::parse_error::ParseErrors;
    use crate::program::Program;
    use crate::symbols::type_name::TypeName;
    use indoc::indoc;
    use std::collections::HashMap;

    fn check(source: &str, expected: &str) {
        assert_eq!(render(source), expected);

        // Formatting a page must not change what it renders.
        let formatted = reformat(source);
        assert_eq!(
            render(&formatted),
            expected,
            "render changed after formatting:\n{formatted}"
        );
    }

    fn reformat(source: &str) -> String {
        let document_id = DocumentId::new("test.hop").unwrap();
        let mut errors = ParseErrors::new();
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
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello
                    </div>
                  }
                }
            "},
            "<div>hello</div>",
        );
    }

    #[test]
    fn turns_a_newline_between_text_into_a_space() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello
                      world
                    </div>
                  }
                }
            "},
            "<div>hello world</div>",
        );
    }

    #[test]
    fn trims_trailing_whitespace_before_a_newline() {
        check(
            concat!(
                "page Test() {\n",
                "  fn body() -> Html {\n",
                "    <div>\n",
                "      hello  \n",
                "      world\n",
                "    </div>\n",
                "  }\n",
                "}\n",
            ),
            "<div>hello world</div>",
        );
    }

    #[test]
    fn collapses_a_run_of_newlines_into_one_space() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello

                      world
                    </div>
                  }
                }
            "},
            "<div>hello world</div>",
        );
    }

    #[test]
    fn drops_a_newline_between_text_and_expression() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello
                      {"world"}
                    </div>
                  }
                }
            "#},
            "<div>helloworld</div>",
        );
    }

    #[test]
    fn drops_a_newline_next_to_a_tag() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello
                      <span>world</span>
                    </div>
                  }
                }
            "},
            "<div>hello<span>world</span></div>",
        );
    }

    #[test]
    fn keeps_a_space_before_a_tag_on_the_same_line() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>hello <span>world</span></div>
                  }
                }
            "},
            "<div>hello <span>world</span></div>",
        );
    }

    #[test]
    fn trims_text_at_the_end_of_a_body() {
        check("page Test() { fn body() -> Html {<>hello </>} }\n", "hello");
    }

    #[test]
    fn preserves_script_content_verbatim() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <script>
                      let x = 1;
                    </script>
                  }
                }
            "},
            "<script>\n      let x = 1;\n    </script>",
        );
    }

    #[test]
    fn preserves_spaces_inside_expression() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <>{"   "}</>
                  }
                }
            "#},
            "   ",
        );
    }

    #[test]
    fn preserves_content_betwen_two_interpolations_on_single_line() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    let first: String = "Hello";
                    let second: String = "World";
                    <div>{first} {second}</div>
                  }
                }
            "#},
            "<div>Hello World</div>",
        );
    }

    #[test]
    fn preserves_whitespace_before_tag_on_single_line() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>this looks <b>great</b></>
                  }
                }
            "},
            "this looks <b>great</b>",
        );
    }

    #[test]
    fn preserves_style_content_verbatim() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <style>
                      .a { color: red; }
                    </style>
                  }
                }
            "},
            "<style>\n      .a { color: red; }\n    </style>",
        );
    }

    #[test]
    fn keeps_a_space_between_two_tags_on_the_same_line() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <><b>b</b> <i>i</i></>
                  }
                }
            "},
            "<b>b</b> <i>i</i>",
        );
    }

    #[test]
    fn drops_a_line_break_between_two_tags() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>
                      <b>b</b>
                      <i>i</i>
                    </>
                  }
                }
            "},
            "<b>b</b><i>i</i>",
        );
    }

    #[test]
    fn keeps_a_space_between_two_expressions_on_the_same_line() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <>{"a"} {"b"}</>
                  }
                }
            "#},
            "a b",
        );
    }

    #[test]
    fn drops_a_newline_between_two_expressions() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <>
                      {"a"}
                      {"b"}
                    </>
                  }
                }
            "#},
            "ab",
        );
    }

    #[test]
    fn keeps_a_space_between_a_tag_and_an_expression() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <><b>b</b> {"i"}</>
                  }
                }
            "#},
            "<b>b</b> i",
        );
    }

    #[test]
    fn keeps_a_run_of_spaces_beside_a_tag() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>a  <b>x</b></>
                  }
                }
            "},
            "a  <b>x</b>",
        );
    }

    #[test]
    fn keeps_whitespace_on_the_side_that_has_no_linebreak() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <><b>x</b>  a {"y"}</>
                  }
                }
            "#},
            "<b>x</b>  a y",
        );
    }

    #[test]
    fn renders_a_fragment_as_its_children() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <><b>x</b><i>y</i></>
                  }
                }
            "},
            "<b>x</b><i>y</i>",
        );
    }

    #[test]
    fn renders_an_empty_fragment_as_nothing() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <></>
                  }
                }
            "},
            "",
        );
    }

    #[test]
    fn trims_the_children_of_a_fragment_against_its_tags() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello
                      <>
                        world
                      </>
                    </div>
                  }
                }
            "},
            "<div>helloworld</div>",
        );
    }

    #[test]
    fn keeps_a_space_written_beside_a_fragment() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <>hello <>world</></>
                  }
                }
            "},
            "hello world",
        );
    }

    #[test]
    fn drops_spaces_written_inside_an_interpolation() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <div>a{ "b" }c</div>
                  }
                }
            "#},
            "<div>abc</div>",
        );
    }

    #[test]
    fn drops_line_breaks_written_inside_an_interpolation() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <div>a{
                      "b"
                    }c</div>
                  }
                }
            "#},
            "<div>abc</div>",
        );
    }

    #[test]
    fn drops_a_newline_beside_a_fragment_valued_expression() {
        check(
            indoc! {"
                fn Wrap(children: Html) -> Html {
                  <div>
                    hello
                    {children}
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrap><b>w</b></Wrap>
                  }
                }
            "},
            "<div>hello<b>w</b></div>",
        );
    }

    #[test]
    fn keeps_a_space_beside_a_fragment_valued_expression_on_the_same_line() {
        check(
            indoc! {"
                fn Wrap(children: Html) -> Html {
                  <div>hello {children}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <Wrap><b>w</b></Wrap>
                  }
                }
            "},
            "<div>hello <b>w</b></div>",
        );
    }

    #[test]
    fn trims_text_in_markup_written_in_expression_position() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>{<span> hello </span>}</div>
                  }
                }
            "},
            "<div><span>hello</span></div>",
        );
    }

    #[test]
    fn drops_line_breaks_in_markup_written_in_expression_position() {
        check(
            indoc! {"
                fn card() -> Html {
                  <div>
                    hello
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <>{card()}</>
                  }
                }
            "},
            "<div>hello</div>",
        );
    }

    #[test]
    fn keeps_a_line_break_between_two_texts_in_expression_position() {
        check(
            indoc! {"
                fn card() -> Html {
                  <div>
                    hello
                    world
                  </div>
                }

                page Test() {
                  fn body() -> Html {
                    <>{card()}</>
                  }
                }
            "},
            "<div>hello world</div>",
        );
    }

    #[test]
    fn normalizes_markup_on_both_sides_of_an_interpolation() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>
                      hello
                      {<span>
                        world
                      </span>}
                    </div>
                  }
                }
            "},
            "<div>hello<span>world</span></div>",
        );
    }

    #[test]
    fn adds_no_whitespace_to_markup_laid_out_inline() {
        check(
            indoc! {"
                fn Card(slot: Html) -> Html {
                  <div>{slot}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card slot={<span>a<b>c</b></span>}/>
                  }
                }
            "},
            "<div><span>a<b>c</b></span></div>",
        );
    }

    #[test]
    fn keeps_significant_whitespace_in_markup_laid_out_inline() {
        check(
            indoc! {"
                fn Card(slot: Html) -> Html {
                  <div>{slot}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <Card slot={<span>a <b>c</b></span>}/>
                  }
                }
            "},
            "<div><span>a <b>c</b></span></div>",
        );
    }

    #[test]
    fn leaves_raw_text_content_in_expression_position_alone() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <div>{<style>  a  </style>}</div>
                  }
                }
            "},
            "<div><style>  a  </style></div>",
        );
    }

    #[test]
    fn keeps_significant_whitespace_in_markup_inside_broken_match_arms() {
        check(
            indoc! {r#"
                fn badge(on: Bool) -> Html {
                  match on {true => <span class="a-fairly-long-class">yes <b>indeed</b></span>, false => <i>no</i>}
                }

                page Test() {
                  fn body() -> Html {
                    <div>{badge(true)}{badge(false)}</div>
                  }
                }
            "#},
            "<div><span class=\"a-fairly-long-class\">yes <b>indeed</b></span><i>no</i></div>",
        );
    }

    #[test]
    fn keeps_significant_whitespace_in_markup_passed_as_call_arguments() {
        check(
            indoc! {r#"
                fn pair(a: Html, b: Html) -> Html {
                  <div>{a}{b}</div>
                }

                page Test() {
                  fn body() -> Html {
                    <>{pair(<span>first <b>one</b></span>, <span>second one</span>)}</>
                  }
                }
            "#},
            "<div><span>first <b>one</b></span><span>second one</span></div>",
        );
    }
}
