use super::Program;
use crate::document::{DocumentPosition, DocumentRange};

impl Program {
    /// Returns the range and the message to display when hovering the
    /// given position.
    pub fn hover_info(&self, position: &DocumentPosition) -> Option<(DocumentRange, String)> {
        self.hover_annotations
            .get(position.document_id())?
            .iter()
            .find(|a| a.range().contains_position(position))
            .map(|annotation| (annotation.range().clone(), annotation.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::{extract_markers_from_archive, program_from_archive};
    use crate::diagnostic::Diagnostic;
    use crate::diagnostic_severity::DiagnosticSeverity;
    use crate::document_annotator::DocumentAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::Archive;

    fn check(archive: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(archive));

        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let program = program_from_archive(&archive);

        let diagnostics = program.diagnostics();
        assert!(
            diagnostics.is_empty(),
            "Expected no diagnostics, got: {:?}",
            diagnostics
        );

        let (range, message) = program
            .hover_info(&markers[0])
            .expect("Expected hover info to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate([Diagnostic {
                message,
                range,
                severity: DiagnosticSeverity::Error,
            }])
            .render();

        expected.assert_eq(&output);
    }

    #[test]
    fn should_show_hover_info_for_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                fn Main(user: User) -> Html {
                        ^
                  <h1>Hello {user.name}</h1>
                }
            "#},
            expect![[r#"
                ```
                user : User
                ```
                  --> main.hop (line 2, col 9)
                2 | fn Main(user: User) -> Html {
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_variable_in_text_expression() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Greeting(name: String) -> Html {
                  <div>{name}</div>
                        ^
                }
            "#},
            expect![[r#"
                ```
                name : String
                ```
                  --> main.hop (line 2, col 9)
                2 |   <div>{name}</div>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_let_binding() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Greeting(name: String) -> Html {
                  let greeting = "Hello " + name;
                        ^
                  <div>{greeting}</div>
                }
            "#},
            expect![[r#"
                ```
                greeting : String
                ```
                  --> main.hop (line 2, col 7)
                2 |   let greeting = "Hello " + name;
                  |       ^^^^^^^^
            "#]],
        );
        check(
            indoc! {r#"
                -- main.hop --
                fn Greeting(name: String) -> Html {
                  let greeting = "Hello " + name;
                  <div>{greeting}</div>
                          ^
                }
            "#},
            expect![[r#"
                ```
                greeting : String
                ```
                  --> main.hop (line 3, col 9)
                3 |   <div>{greeting}</div>
                  |         ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_for_expression_loop_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Items(items: Array[String]) -> Html {
                  for item in items {
                      ^
                    <li>{item}</li>
                  }
                }
            "#},
            expect![[r#"
                ```
                item : String
                ```
                  --> main.hop (line 2, col 7)
                2 |   for item in items {
                  |       ^^^^
            "#]],
        );
        check(
            indoc! {r#"
                -- main.hop --
                fn Items(items: Array[String]) -> Html {
                  for item in items {
                    <li>{item}</li>
                           ^
                  }
                }
            "#},
            expect![[r#"
                ```
                item : String
                ```
                  --> main.hop (line 3, col 10)
                3 |     <li>{item}</li>
                  |          ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_record_literal_type_name() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                fn Main() -> Html {
                  let user: User = User{name: "John"};
                                   ^
                  <>{user.name}</>
                }
            "#},
            expect![[r#"
                ```
                User : User
                ```
                  --> main.hop (line 3, col 20)
                3 |   let user: User = User{name: "John"};
                  |                    ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_enum_literal_constructor() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                fn Main() -> Html {
                  let color: Color = Color::Red;
                                     ^
                  match color {
                    Color::Red => <>red</>,
                    _ => <>other</>,
                  }
                }
            "#},
            expect![[r#"
                ```
                Color : Color
                ```
                  --> main.hop (line 3, col 22)
                3 |   let color: Color = Color::Red;
                  |                      ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_enum_literal_constructor_with_fields() {
        check(
            indoc! {r#"
                -- main.hop --
                enum Outcome { Success{value: String}, Failure{message: String} }
                fn Main() -> Html {
                  let result: Outcome = Outcome::Success{value: "ok"};
                                        ^
                  match result {
                    Outcome::Success{value: v} => <>{v}</>,
                    Outcome::Failure{message: m} => <>{m}</>,
                  }
                }
            "#},
            expect![[r#"
                ```
                Outcome : Outcome
                ```
                  --> main.hop (line 3, col 25)
                3 |   let result: Outcome = Outcome::Success{value: "ok"};
                  |                         ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_len_on_array() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main(items: Array[String]) -> Html {
                  match items.len() == 0 {
                              ^
                    true => <>Empty</>,
                    false => <></>,
                  }
                }
            "#},
            expect![[r#"
                ```
                Array::len() -> Int
                ```

                Returns the number of elements in the array.
                  --> main.hop (line 2, col 15)
                2 |   match items.len() == 0 {
                  |               ^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_is_empty_on_array() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main(items: Array[String]) -> Html {
                  match items.is_empty() {
                              ^
                    true => <>Empty</>,
                    false => <></>,
                  }
                }
            "#},
            expect![[r#"
                ```
                Array::is_empty() -> Bool
                ```

                Returns `true` if the array is empty.
                  --> main.hop (line 2, col 15)
                2 |   match items.is_empty() {
                  |               ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_join_macro() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main(a: String, b: String) -> Html {
                  <div class={
                    join!(a, b)
                    ^
                  }>
                  </div>
                }
            "#},
            expect![[r#"
                ```
                join!(String, ...) -> String
                ```

                Joins strings with spaces.
                  --> main.hop (line 3, col 5)
                3 |     join!(a, b)
                  |     ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_asset_macro() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                  <img src={
                    asset!("/logo.svg")
                    ^
                  } />
                }
            "#},
            expect![[r#"
                ```
                asset!(literal: String) -> String
                ```

                The path must start with `/`, which denotes the project root. Resolves to a content-hashed URL prefixed by `assets.production_prefix` in production builds.
                  --> main.hop (line 3, col 5)
                3 |     asset!("/logo.svg")
                  |     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_format_macro() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main(a: String, b: Int) -> Html {
                  <div>
                    {format!("a: {}, b: {}", a, b)}
                      ^
                  </div>
                }
            "#},
            expect![[r#"
                ```
                format!(literal: String, ...) -> String
                ```

                Replaces each `{}` in the format string with the corresponding argument.
                  --> main.hop (line 3, col 6)
                3 |     {format!("a: {}, b: {}", a, b)}
                  |      ^^^^^^^
            "#]],
        );
    }
}
