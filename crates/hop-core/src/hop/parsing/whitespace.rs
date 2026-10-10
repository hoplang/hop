use super::parsed_markup::ParsedMarkup;
use crate::html::HtmlElementKind;

/// Normalize whitespace in a sequence of parsed markup.
///
/// The pass handles two concerns:
///
/// ## 1. Whitespace Trimming
///
/// - Trim the start of a Text that begins its sequence or follows a Newline
/// - Trim the end of a Text that ends its sequence or precedes a Newline
/// - Drop Texts that are empty after trimming
///
/// Texts in a row, which a comment split, are trimmed as one text.
///
/// ## 2. Newline-to-Space Conversion
///
/// - Keep a Newline only between two Texts, and of a run of Newlines
///   only the last. A newline beside anything else, a tag or an
///   interpolation, emits nothing.
///
/// A raw text element (`<script>`, `<style>`) may only hold whitespace, which
/// carries no meaning and is dropped. Content it is not allowed to hold is
/// passed through verbatim.
fn normalize(content: &mut Vec<ParsedMarkup>) {
    trim_text(content);
    drop_newlines(content);
    for markup in content.iter_mut() {
        normalize_markup(markup);
    }
}

/// Normalize whitespace inside a single piece of markup.
pub fn normalize_markup(markup: &mut ParsedMarkup) {
    match markup {
        // Whitespace between a raw text element's tags is not content, so it is
        // dropped. Anything else there has already been rejected by the parser,
        // and is kept as written so that normalizing never discards code.
        ParsedMarkup::Element {
            kind: HtmlElementKind::Script | HtmlElementKind::Style,
            children,
            ..
        } => children.retain(|child| match child {
            ParsedMarkup::Text { range } => !range.as_str().trim().is_empty(),
            _ => true,
        }),
        ParsedMarkup::Element { children, .. } | ParsedMarkup::Fragment { children, .. } => {
            normalize(children);
        }
        ParsedMarkup::Call { children, .. } => {
            if let Some(children) = children {
                normalize(children);
            }
        }
        ParsedMarkup::Text { .. }
        | ParsedMarkup::Newline { .. }
        | ParsedMarkup::Interpolation { .. } => {}
    }
}

fn trim_text(content: &mut Vec<ParsedMarkup>) {
    let mut trim = true;
    for markup in content.iter_mut() {
        match markup {
            ParsedMarkup::Text { range } if trim => {
                *range = range.trim_start();
                trim = range.as_str().is_empty();
            }
            ParsedMarkup::Newline { .. } => trim = true,
            _ => trim = false,
        }
    }
    let mut trim = true;
    for markup in content.iter_mut().rev() {
        match markup {
            ParsedMarkup::Text { range } if trim => {
                *range = range.trim_end();
                trim = range.as_str().is_empty();
            }
            ParsedMarkup::Newline { .. } => trim = true,
            _ => trim = false,
        }
    }
    content.retain(|markup| match markup {
        ParsedMarkup::Text { range } => !range.as_str().is_empty(),
        _ => true,
    });
}

fn drop_newlines(content: &mut Vec<ParsedMarkup>) {
    let mut kept = Vec::with_capacity(content.len());
    // The last Newline since the previous markup that is not a Newline.
    let mut newline = None;
    for markup in content.drain(..) {
        match markup {
            ParsedMarkup::Newline { .. } => newline = Some(markup),
            ParsedMarkup::Text { .. } => {
                if let Some(newline) = newline.take()
                    && matches!(kept.last(), Some(ParsedMarkup::Text { .. }))
                {
                    kept.push(newline);
                }
                kept.push(markup);
            }
            _ => {
                newline = None;
                kept.push(markup);
            }
        }
    }
    *content = kept;
}

#[cfg(test)]
mod tests {
    use crate::document::Document;
    use crate::hop::format;
    use crate::hop::parsing::parse;
    use crate::ir::pure_to_flat;
    use crate::ir::runtime::flat_evaluator;
    use crate::orchestrator::{OrchestrateOptions, orchestrate_pure};
    use crate::program::Program;
    use crate::root_contained_file_path::RootContainedFilePath;
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
        let document_id = RootContainedFilePath::new("test.hop").unwrap();
        let mut errors = Vec::new();
        let document = Document::new(document_id, source.to_string());
        let module = parse::parse(document, &mut errors);
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        format(&module)
    }

    fn render(source: &str) -> String {
        let document_id = RootContainedFilePath::new("test.hop").unwrap();
        let mut program = Program::new();
        program.update_hop_document(
            &document_id,
            Document::new(document_id.clone(), source.to_string()),
        );

        let diagnostics = program.diagnostics();
        assert!(diagnostics.is_empty(), "diagnostics: {diagnostics:?}");

        let typed_modules = program.typed_modules().clone();
        let page_name = TypeName::parse("Test").unwrap();
        let (module, pages) = orchestrate_pure(
            &typed_modules,
            OrchestrateOptions {
                ..Default::default()
            },
        );
        flat_evaluator::evaluate_page(
            &pure_to_flat(module),
            &pages,
            &page_name,
            HashMap::new(),
            None,
        )
        .expect("evaluator failed")
    }

    #[test]
    fn treats_a_comment_as_not_written() {
        check(
            indoc! {"
                page Test() {
                  fn body() -> Html {
                    <p>
                      one
                      <!-- two -->
                      three <!-- four -->\x20
                      five<!-- six -->seven
                      <!-- eight --> nine
                    </p>
                  }
                }
            "},
            "<p>one three fiveseven nine</p>",
        );
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
    fn strips_whitespace_between_a_script_tags() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <script src="/app.js">
                    </script>
                  }
                }
            "#},
            "<script src=\"/app.js\"></script>",
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
    fn strips_whitespace_between_script_tags_in_expression_position() {
        check(
            indoc! {r#"
                page Test() {
                  fn body() -> Html {
                    <div>{<script src="/app.js">  </script>}</div>
                  }
                }
            "#},
            "<div><script src=\"/app.js\"></script></div>",
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
