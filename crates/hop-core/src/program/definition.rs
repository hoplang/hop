use super::Program;
use crate::document::{DocumentPosition, DocumentRange};

impl Program {
    pub fn definition_location(&self, position: &DocumentPosition) -> Option<DocumentRange> {
        self.definition_links
            .get(position.document_id())?
            .iter()
            .find(|link| link.use_range.contains_position(position))
            .map(|link| link.definition_range.clone())
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

    fn check(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));

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

        let range = program
            .definition_location(&markers[0])
            .expect("Expected definition location to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate([Diagnostic {
                message: "Definition".to_string(),
                range,
                severity: DiagnosticSeverity::Error,
            }])
            .render();

        expected.assert_eq(&output);
    }

    #[test]
    fn should_find_definition_from_function_invocation_opening_tag() {
        check(
            indoc! {r#"
                -- hop/components.hop --
                pub fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import hop::components::HelloWorld

                fn Main() -> Html {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 8)
                1 | pub fn HelloWorld() -> Html {
                  |        ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_function_invocation_closing_tag() {
        check(
            indoc! {r#"
                -- hop/components.hop --
                pub fn HelloWorld(children: Html) -> Html {
                  <h1>Hello World {children}</h1>
                }

                -- main.hop --
                import hop::components::HelloWorld

                fn Main() -> Html {
                  <HelloWorld>
                    World
                  </HelloWorld>
                     ^
                }
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 8)
                1 | pub fn HelloWorld(children: Html) -> Html {
                  |        ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_function_name() {
        check(
            indoc! {r#"
                -- hop/components.hop --
                pub fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import hop::components::HelloWorld
                                        ^

                fn Main() -> Html {
                  <HelloWorld />
                }
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 8)
                1 | pub fn HelloWorld() -> Html {
                  |        ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_record_name() {
        check(
            indoc! {r#"
                -- types.hop --
                pub record User {name: String, age: Int}
                -- main.hop --
                import types::User
                              ^

                fn Main(user: User) -> Html {
                  <div>{user.name}</div>
                }
            "#},
            expect![[r#"
                Definition
                  --> types.hop (line 1, col 12)
                1 | pub record User {name: String, age: Int}
                  |            ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_enum_name() {
        check(
            indoc! {r#"
                -- types.hop --
                pub enum Status { Active, Inactive }
                -- main.hop --
                import types::Status
                              ^

                fn Main(status: Status) -> Html {
                  match status {
                    Status::Active => <span>Active</span>,
                    Status::Inactive => <span>Inactive</span>,
                  }
                }
            "#},
            expect![[r#"
                Definition
                  --> types.hop (line 1, col 10)
                1 | pub enum Status { Active, Inactive }
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_record_pattern() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}

                fn Main(user: User) -> Html {
                  match user {
                    User {name} => <span>{name}</span>,
                    ^
                  }
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 8)
                1 | record User {name: String}
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_definition_opening_tag() {
        check(
            indoc! {r#"
                -- main.hop --
                fn HelloWorld() -> Html {
                     ^
                  <h1>Hello World</h1>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 4)
                1 | fn HelloWorld() -> Html {
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_function_definition_name() {
        check(
            indoc! {r#"
                -- main.hop --
                fn greeting() -> String {
                     ^
                  "Hello"
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 4)
                1 | fn greeting() -> String {
                  |    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_function_call() {
        check(
            indoc! {r#"
                -- main.hop --
                fn greeting() -> String {
                  "Hello"
                }

                fn message() -> String {
                  greeting()
                    ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 4)
                1 | fn greeting() -> String {
                  |    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_function_invocation_in_same_module_simple() {
        check(
            indoc! {r#"
                -- main.hop --
                fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                fn Main() -> Html {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 4)
                1 | fn HelloWorld() -> Html {
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_function_invocation_inside_match() {
        check(
            indoc! {r#"
                -- main.hop --
                fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                fn Main(x: Option[String]) -> Html {
                  match x {
                    Some(_) => {
                      <HelloWorld />
                       ^
                    },
                    None => <></>,
                  }
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 4)
                 1 | fn HelloWorld() -> Html {
                   |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_function_invocation_inside_page() {
        check(
            indoc! {r#"
                -- main.hop --
                fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                page Main() {
                  fn body() -> Html {
                    <HelloWorld />
                     ^
                  }
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 4)
                1 | fn HelloWorld() -> Html {
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_variable_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main(name: String) -> Html {
                  <span>{name}</span>
                         ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 9)
                1 | fn Main(name: String) -> Html {
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_for_loop_variable() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main(items: Array[String]) -> Html {
                  <ul>
                    {for item in items {
                      <li>{item}</li>
                            ^
                    }}
                  </ul>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 3, col 10)
                3 |     {for item in items {
                  |          ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_let_binding_reference() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                  let greeting: String = "Hello";
                  <span>{greeting}</span>
                         ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 2, col 7)
                2 |   let greeting: String = "Hello";
                  |       ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_type_reference_in_parameter() {
        check(
            indoc! {r#"
                -- main.hop --
                record User {name: String}

                fn Main(user: User) -> Html {
                              ^
                  <span>{user.name}</span>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 8)
                1 | record User {name: String}
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_type_reference_in_array() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {name: String}

                fn Main(items: Array[Item]) -> Html {
                                     ^
                  for item in items {
                    <span>{item.name}</span>
                  }
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 8)
                1 | record Item {name: String}
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_imported_type_reference() {
        check(
            indoc! {r#"
                -- types.hop --
                pub record User {name: String}

                -- main.hop --
                import types::User

                fn Main(user: User) -> Html {
                              ^
                  <span>{user.name}</span>
                }
            "#},
            expect![[r#"
                Definition
                  --> types.hop (line 1, col 12)
                1 | pub record User {name: String}
                  |            ^^^^
            "#]],
        );
    }
}
