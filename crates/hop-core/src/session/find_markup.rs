use crate::document::DocumentPosition;
use crate::hop::parsing::{ParsedExpr, ParsedMarkup, ParsedModule};

/// Finds the deepest markup that contains the given position.
///
/// Upper bound on time complexity is max(depth of tree, number of top level markup expressions).
///
/// # Example
/// ```text
/// <div>
///     <span>text</span>
///                  ^
/// </div>
/// ```
/// returns
/// ```text
/// <div>
///     <span>text</span>
///     ^^^^^^^^^^^^^^^^^
/// </div>
/// ```
pub fn find_markup_at_position<'a>(
    module: &'a ParsedModule,
    position: &DocumentPosition,
) -> Option<&'a ParsedMarkup> {
    for n in module.page_declarations() {
        if n.range.contains_position(position) {
            if let Some(head) = &n.head
                && let Some(markup) = find_markup_at_position_in_expr(&head.body, position)
            {
                return Some(markup);
            }
            return find_markup_at_position_in_expr(&n.body.body, position);
        }
    }

    for n in module.function_declarations() {
        if n.range.contains_position(position) {
            return find_markup_at_position_in_expr(&n.body, position);
        }
    }

    None
}

fn find_markup_at_position_in_expr<'a>(
    expr: &'a ParsedExpr,
    position: &DocumentPosition,
) -> Option<&'a ParsedMarkup> {
    expr.markup()
        .into_iter()
        .find_map(|root| find_markup_at_position_in_markup(root, position))
}

fn find_markup_at_position_in_markup<'a>(
    markup: &'a ParsedMarkup,
    position: &DocumentPosition,
) -> Option<&'a ParsedMarkup> {
    if !markup.range().contains_position(position) {
        return None;
    }

    for expr in markup.expressions() {
        for root in expr.markup() {
            if let Some(found) = find_markup_at_position_in_markup(root, position) {
                return Some(found);
            }
        }
    }

    for child in markup.children() {
        if let Some(found) = find_markup_at_position_in_markup(child, position) {
            return Some(found);
        }
    }
    Some(markup)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostic;
    use crate::diagnostic_severity::DiagnosticSeverity;
    use crate::document_annotator::DocumentAnnotator;
    use crate::extract_position::extract_position;
    use crate::hop::parsing::parse;
    use crate::root_contained_file_path::RootContainedFilePath;
    use expect_test::{Expect, expect};
    use indoc::indoc;

    fn check_find_markup_at_position(input: &str, expected: Expect) {
        let document_id = RootContainedFilePath::new("test.hop").unwrap();
        let (document, position) =
            extract_position(document_id, input).expect("Position marker not found");
        let mut errors = Vec::new();
        let module = parse(document, &mut errors);

        assert!(errors.is_empty(), "Parse errors: {:?}", errors);

        let found_markup = find_markup_at_position(&module, &position);

        let output = if let Some(markup) = found_markup {
            DocumentAnnotator::new()
                .without_location()
                .annotate([Diagnostic {
                    message: "range".to_string(),
                    range: markup.range().clone(),
                    severity: DiagnosticSeverity::Error,
                }])
                .render()
        } else {
            "No markup found at position".to_string()
        };

        expected.assert_eq(&output);
    }

    #[test]
    fn should_find_text_content() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>Hello World</div>
                             ^
                }
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
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>Content</div>
                     ^
                }
            "},
            expect![[r#"
                range
                2 |     <div>Content</div>
                  |     ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_markup_call() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <FooBar>Content</FooBar>
                        ^
                }
            "},
            expect![[r#"
                range
                2 |     <FooBar>Content</FooBar>
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_nested_text_content() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>
                        <span>Nested text</span>
                                    ^
                    </div>
                }
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
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>Content</div>
                }
                ^
            "},
            expect!["No markup found at position"],
        );
    }

    #[test]
    fn should_find_expression_in_deeply_nested_structure() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>
                        {match condition {
                          true => {
                            for item in items {
                                <span>{item}</span>
                                        ^
                            }
                          },
                          false => <></>,
                        }}
                    </div>
                }
            "},
            expect![[r#"
                range
                 6 |                 <span>{item}</span>
                   |                       ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_void_element() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <p>Some text <br/> more text</p>
                                  ^
                }
            "},
            expect![[r#"
                range
                2 |     <p>Some text <br/> more text</p>
                  |                  ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_first_element_on_line_with_multiple_elements() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div><span>Hello</span> <strong>World</strong></div>
                           ^
                }
            "},
            expect![[r#"
                range
                2 |     <div><span>Hello</span> <strong>World</strong></div>
                  |          ^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_second_element_on_line_with_multiple_elements() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div><span>Hello</span> <strong>World</strong></div>
                                               ^
                }
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
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div><span>Hello</span> and <strong>World</strong></div>
                                            ^
                }
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
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>
                        <section>
                            <article>
                                {match condition {
                                  true => {
                                    for item in items {
                                        <header>
                                            <h1>
                                                <span>
                                                    <em>Deep {item.name} text</em>
                                                             ^
                                                </span>
                                            </h1>
                                        </header>
                                    }
                                  },
                                  false => <></>,
                                }}
                            </article>
                        </section>
                    </div>
                }
            "},
            expect![[r#"
                range
                11 |                                     <em>Deep {item.name} text</em>
                   |                                              ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_parent_element_in_deep_nesting() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
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
                }
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
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                                                                  ^
                }
            "},
            expect![[r#"
                range
                2 |     <p>Hello <em>{user.name}</em>, welcome to <strong>{site.title}</strong>!</p>
                  |                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_text_inside_function_with_inline_content() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div><UserCard data={user}><span>Content</span></UserCard> more text</div>
                                                      ^
                }
            "},
            expect![[r#"
                range
                2 |     <div><UserCard data={user}><span>Content</span></UserCard> more text</div>
                  |                                      ^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_element_in_nested_control_structures() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    match users {
                      true => {
                        for user in users {
                          match user.active {
                            true => {
                              for role in user.roles {
                                  <span>{role}</span>
                                     ^
                              }
                            },
                            false => <></>,
                          }
                        }
                      },
                      false => <></>,
                    }
                }
            "},
            expect![[r#"
                range
                 8 |                   <span>{role}</span>
                   |                   ^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_self_closing_element_with_attributes() {
        check_find_markup_at_position(
            indoc! {r#"
                fn Main() -> Html {
                    <div>
                        <input type="text" placeholder="Enter name" required />
                               ^
                        <br/>
                    </div>
                }
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
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <><div>First</div> <div>Second</div></>
                                       ^
                }
            "},
            expect![[r#"
                range
                2 |     <><div>First</div> <div>Second</div></>
                  |                        ^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_element_inside_match_arm() {
        check_find_markup_at_position(
            indoc! {"
                fn Main(x: Option[String]) -> Html {
                    match x {
                        Some(s) => {
                            <div>found</div>
                             ^
                        },
                        None => <></>,
                    }
                }
            "},
            expect![[r#"
                range
                4 |             <div>found</div>
                  |             ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_markup_inside_an_interpolation() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>{<span>text</span>}</div>
                                   ^
                }
            "},
            expect![[r#"
                range
                2 |     <div>{<span>text</span>}</div>
                  |                 ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_markup_inside_an_attribute_value() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <Card slot={<span>text</span>}></Card>
                                         ^
                }
            "},
            expect![[r#"
                range
                2 |     <Card slot={<span>text</span>}></Card>
                  |                       ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_the_outer_markup_when_the_position_misses_interpolated_markup() {
        check_find_markup_at_position(
            indoc! {"
                fn Main() -> Html {
                    <div>{<span>text</span>}</div>
                     ^
                }
            "},
            expect![[r#"
                range
                2 |     <div>{<span>text</span>}</div>
                  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }
}
