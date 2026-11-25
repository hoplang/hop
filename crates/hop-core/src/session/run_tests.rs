use super::Session;
use crate::diagnostic::Diagnostic;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::ir::runtime::flat_evaluator::evaluate_test;
use crate::ir::runtime::test_run::TestRun;
use crate::root_contained_file_path::RootContainedFilePath;

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum RunTestsError {
    #[error("Cannot run tests: program has parse errors")]
    ParseErrors,

    #[error("Cannot run tests: program has type errors")]
    TypeErrors,
}

/// A test that ran, with a diagnostic for each of its assertions that
/// failed. The test passes if it has none.
#[derive(Debug, Clone)]
pub struct TestResult {
    /// The path of the test's module from the project root.
    pub module: String,
    pub name: String,
    pub failures: Vec<Diagnostic>,
}

impl Session {
    /// Run every test, or those of `module` if it is given, and of those the
    /// tests called `name` if it is given, in order of module and then of
    /// name. A test is a test fn without parameters.
    ///
    /// An assertion that fails does not end its test, so every assertion a
    /// test makes that fails is reported, as often as it fails. A test the
    /// evaluator cannot evaluate within its limits ends there, and that is
    /// reported at its name.
    pub fn run_tests(
        &self,
        module: Option<&RootContainedFilePath>,
        name: Option<&str>,
    ) -> Result<Vec<TestResult>, RunTestsError> {
        if self.parse_errors.values().any(|errors| !errors.is_empty()) {
            return Err(RunTestsError::ParseErrors);
        }
        if self
            .type_errors
            .values()
            .flatten()
            .any(|error| error.severity() == DiagnosticSeverity::Error)
        {
            return Err(RunTestsError::TypeErrors);
        }

        let (program, tests) = self.compile_tests(module, name);
        let results = (tests.tests.iter())
            .map(|test| {
                let mut run = TestRun::default();
                let evaluated = evaluate_test(&program, test.function, &mut run);
                let mut failures: Vec<Diagnostic> = (run.failures.iter())
                    .map(|failure| {
                        let assertion = &tests.assertions[failure.assertion.index()];
                        Diagnostic {
                            message: if assertion.reports_operands {
                                format!(
                                    "Assertion failed\n  left:  {}\n  right: {}",
                                    failure.left, failure.right
                                )
                            } else {
                                "Assertion failed".to_string()
                            },
                            range: assertion.range.clone(),
                            severity: DiagnosticSeverity::Error,
                        }
                    })
                    .collect();
                if let Err(error) = evaluated {
                    let declaration = self.parsed_modules[&test.module]
                        .function_declarations()
                        .find(|function| function.name == test.name)
                        .expect("the module declares the test");
                    failures.push(Diagnostic {
                        message: error.to_string(),
                        range: declaration.name_range.clone(),
                        severity: DiagnosticSeverity::Error,
                    });
                }
                TestResult {
                    module: test.module.as_str().to_string(),
                    name: test.name.to_string(),
                    failures,
                }
            })
            .collect();
        Ok(results)
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::session_from_archive;
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::Archive;

    fn check(input: &str, module: Option<&str>, name: Option<&str>, expected: Expect) {
        let session = session_from_archive(&Archive::from(input));
        let diagnostics = session.diagnostics();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let module = module.map(|module| RootContainedFilePath::new(module).unwrap());
        let results = session.run_tests(module.as_ref(), name).unwrap();
        let mut out = String::new();
        for result in results {
            let status = if result.failures.is_empty() {
                "ok"
            } else {
                "FAILED"
            };
            out.push_str(&format!("{} {} ... {status}\n", result.module, result.name));
            out.push_str(
                &DocumentAnnotator::new()
                    .with_location()
                    .annotate(result.failures)
                    .render(),
            );
        }
        expected.assert_eq(&out);
    }

    #[test]
    fn a_test_whose_assertions_hold_passes() {
        check(
            indoc! {r#"
                -- main.hop --
                fn double(x: Int) -> Int {
                  x * 2
                }

                test fn doubles() -> Tuple[] {
                  let four = double(2);
                  assert!(four > 3);
                  assert_eq!(four, 4);
                  assert_eq!(double(four), 8)
                }
            "#},
            None,
            None,
            expect![[r#"
                main.hop doubles ... ok
            "#]],
        );
    }

    #[test]
    fn every_assertion_that_fails_is_reported() {
        check(
            indoc! {r#"
                -- main.hop --
                record Item {
                  label: String,
                  count: Int,
                }

                enum Status {
                  Active,
                  Away {since: String},
                }

                test fn compares_data() -> Tuple[] {
                  let item = Item {label: "A", count: 1};
                  assert!(item.count > 1);
                  assert_eq!(item, Item {label: "B", count: 1});
                  assert_eq!(item, Item {label: "A", count: 1});
                  assert_eq!([Some(1), None], [Some(2), None]);
                  assert_eq!((1.5, "a\"b"), (1.5, "a\nb"));
                  assert_eq!(Status::Away {since: "A"}, Status::Active)
                }
            "#},
            None,
            None,
            expect![[r#"
                main.hop compares_data ... FAILED
                Assertion failed
                  --> main.hop (line 13, col 3)
                13 |   assert!(item.count > 1);
                   |   ^^^^^^^^^^^^^^^^^^^^^^^

                Assertion failed
                  left:  {label: "A", count: 1}
                  right: {label: "B", count: 1}
                  --> main.hop (line 14, col 3)
                14 |   assert_eq!(item, Item {label: "B", count: 1});
                   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Assertion failed
                  left:  [Some(1), None]
                  right: [Some(2), None]
                  --> main.hop (line 16, col 3)
                16 |   assert_eq!([Some(1), None], [Some(2), None]);
                   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Assertion failed
                  left:  (1.5, "a\"b")
                  right: (1.5, "a\nb")
                  --> main.hop (line 17, col 3)
                17 |   assert_eq!((1.5, "a\"b"), (1.5, "a\nb"));
                   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Assertion failed
                  left:  Away {since: "A"}
                  right: Active
                  --> main.hop (line 18, col 3)
                18 |   assert_eq!(Status::Away {since: "A"}, Status::Active)
                   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn an_assertion_is_made_where_it_is_evaluated() {
        check(
            indoc! {r#"
                -- main.hop --
                test fn is_positive(x: Int) -> Tuple[] {
                  assert!(x > 0)
                }

                test fn checks() -> Tuple[] {
                  is_positive(1);
                  is_positive(0);
                  is_positive(0 - 1);
                  match Some(2) {
                    Some(x) => assert_eq!(x, 3),
                    None => assert!(false),
                  }
                }
            "#},
            None,
            None,
            expect![[r#"
                main.hop checks ... FAILED
                Assertion failed
                  --> main.hop (line 2, col 3)
                 2 |   assert!(x > 0)
                   |   ^^^^^^^^^^^^^^

                Assertion failed
                  --> main.hop (line 2, col 3)
                 2 |   assert!(x > 0)
                   |   ^^^^^^^^^^^^^^

                Assertion failed
                  left:  2
                  right: 3
                  --> main.hop (line 10, col 16)
                10 |     Some(x) => assert_eq!(x, 3),
                   |                ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn tests_are_selected_by_module_and_by_name() {
        let input = indoc! {r#"
            -- a.hop --
            test fn first() -> Tuple[] {
              assert!(true)
            }

            test fn second() -> Tuple[] {
              assert!(false)
            }

            test fn third(x: Int) -> Tuple[] {
              assert!(x > 0)
            }

            -- b.hop --
            test fn first() -> Tuple[] {
              assert!(false)
            }
        "#};
        check(
            input,
            Some("a.hop"),
            None,
            expect![[r#"
                a.hop first ... ok
                a.hop second ... FAILED
                Assertion failed
                  --> a.hop (line 6, col 3)
                 6 |   assert!(false)
                   |   ^^^^^^^^^^^^^^
            "#]],
        );
        check(
            input,
            None,
            Some("first"),
            expect![[r#"
                a.hop first ... ok
                b.hop first ... FAILED
                Assertion failed
                  --> b.hop (line 2, col 3)
                2 |   assert!(false)
                  |   ^^^^^^^^^^^^^^
            "#]],
        );
        check(
            input,
            Some("b.hop"),
            Some("first"),
            expect![[r#"
                b.hop first ... FAILED
                Assertion failed
                  --> b.hop (line 2, col 3)
                2 |   assert!(false)
                  |   ^^^^^^^^^^^^^^
            "#]],
        );
        check(input, Some("b.hop"), Some("second"), expect![]);
        check(input, Some("a.hop"), Some("third"), expect![]);
    }

    #[test]
    fn a_test_the_evaluator_cannot_evaluate_is_reported() {
        check(
            indoc! {r#"
                -- main.hop --
                fn count_down(n: Int) -> Int {
                  match n <= 0 {
                    true => 0,
                    false => count_down(n - 1),
                  }
                }

                test fn counts_down() -> Tuple[] {
                  assert_eq!(count_down(3), 1);
                  assert_eq!(count_down(100), 0);
                  assert!(false)
                }
            "#},
            None,
            None,
            expect![[r#"
                main.hop counts_down ... FAILED
                Program exceeded the call depth limit of 12
                  --> main.hop (line 8, col 9)
                 8 | test fn counts_down() -> Tuple[] {
                   |         ^^^^^^^^^^^

                Assertion failed
                  left:  0
                  right: 1
                  --> main.hop (line 9, col 3)
                 9 |   assert_eq!(count_down(3), 1);
                   |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn tests_do_not_run_in_a_program_with_type_errors() {
        let session = session_from_archive(&Archive::from(indoc! {r#"
            -- main.hop --
            test fn adds() -> Tuple[] {
              assert_eq!(1 + 1, "2")
            }
        "#}));
        assert_eq!(
            session.run_tests(None, None).unwrap_err(),
            RunTestsError::TypeErrors
        );
    }
}
