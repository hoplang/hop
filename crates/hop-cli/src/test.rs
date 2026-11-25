use crate::project::Project;
use anyhow::Result;
use hop_core::{DiagnosticSeverity, DocumentAnnotator, Session, TestResult};

/// Run the tests of the project: those of `file` if it is given, and of
/// those the tests called `name` if it is given.
pub fn execute(
    project: &Project,
    file: Option<&str>,
    name: Option<&str>,
) -> Result<Vec<TestResult>> {
    let module = match file {
        Some(file) => Some(project.root().relativize(&std::path::absolute(file)?)?),
        None => None,
    };

    let mut session = Session::new();
    for document_id in project.documents()? {
        if document_id.extension() == Some("hop") {
            session.update_hop_document(&document_id, project.load_document(&document_id)?);
        }
    }

    let diagnostics = session.diagnostics();
    if diagnostics
        .iter()
        .any(|d| d.severity() == DiagnosticSeverity::Error)
    {
        let mut annotator = DocumentAnnotator::new()
            .with_severity_label()
            .with_lines_before(1)
            .with_location();
        annotator.annotate(diagnostics);
        anyhow::bail!("Compilation failed:\n{}", annotator.render());
    }

    Ok(session.run_tests(module.as_ref(), name)?)
}

/// A line for each test, with the assertions of a failed test that do not
/// hold beneath it.
pub fn render(results: &[TestResult]) -> String {
    let mut out = String::new();
    for result in results {
        let status = if result.failures.is_empty() {
            "ok"
        } else {
            "FAILED"
        };
        out.push_str(&format!("{} {} ... {status}\n", result.module, result.name));
        if !result.failures.is_empty() {
            let failures = DocumentAnnotator::new()
                .with_lines_before(1)
                .with_location()
                .annotate(result.failures.iter().cloned())
                .render();
            out.push('\n');
            for line in failures.lines() {
                if !line.is_empty() {
                    out.push_str("  ");
                    out.push_str(line);
                }
                out.push('\n');
            }
            out.push('\n');
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use tempfile::TempDir;
    use txtar::{Archive, write_archive_to_dir};

    fn check(input: &str, file: Option<&str>, name: Option<&str>, expected: Expect) {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::open(temp_dir.path()).unwrap();
        let file = file.map(|file| temp_dir.path().join(file));
        let file = file.as_ref().map(|file| file.to_str().unwrap());
        let actual = match execute(&project, file, name) {
            Ok(results) => render(&results),
            Err(error) => format!("{error}"),
        };
        expected.assert_eq(&actual);
    }

    const PROJECT: &str = indoc! {r#"
        -- hop.toml --
        [compile]
        target = "ts"
        output_path = "app.ts"
        -- main.hop --
        fn double(x: Int) -> Int {
          x * 2
        }

        test fn doubles() -> Tuple[] {
          assert_eq!(double(2), 4)
        }

        test fn triples() -> Tuple[] {
          assert!(double(1) > 1);
          assert_eq!(double(3), 9)
        }
        -- components/badge.hop --
        pub fn label(count: Int) -> String {
          format!("{} new", count)
        }

        test fn doubles() -> Tuple[] {
          assert_eq!(label(2), "2 new")
        }
    "#};

    #[test]
    fn should_run_every_test_of_the_project() {
        check(
            PROJECT,
            None,
            None,
            expect![[r#"
                components/badge.hop doubles ... ok
                main.hop doubles ... ok
                main.hop triples ... FAILED

                  Assertion failed
                    left:  6
                    right: 9
                    --> main.hop (line 11, col 3)
                  10 |   assert!(double(1) > 1);
                  11 |   assert_eq!(double(3), 9)
                     |   ^^^^^^^^^^^^^^^^^^^^^^^^

            "#]],
        );
    }

    #[test]
    fn should_run_the_tests_of_a_file_and_a_name() {
        check(
            PROJECT,
            Some("components/badge.hop"),
            None,
            expect![[r#"
                components/badge.hop doubles ... ok
            "#]],
        );
        check(
            PROJECT,
            None,
            Some("doubles"),
            expect![[r#"
                components/badge.hop doubles ... ok
                main.hop doubles ... ok
            "#]],
        );
        check(
            PROJECT,
            Some("main.hop"),
            Some("triples"),
            expect![[r#"
                main.hop triples ... FAILED

                  Assertion failed
                    left:  6
                    right: 9
                    --> main.hop (line 11, col 3)
                  10 |   assert!(double(1) > 1);
                  11 |   assert_eq!(double(3), 9)
                     |   ^^^^^^^^^^^^^^^^^^^^^^^^

            "#]],
        );
        check(PROJECT, Some("main.hop"), Some("missing"), expect![]);
    }

    #[test]
    fn should_not_run_tests_of_a_project_that_does_not_compile() {
        check(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                test fn doubles() -> Tuple[] {
                  assert_eq!(2, "2")
                }
            "#},
            None,
            None,
            expect![[r#"
                Compilation failed:
                error: Expected Int got String
                  --> main.hop (line 2, col 17)
                1 | test fn doubles() -> Tuple[] {
                2 |   assert_eq!(2, "2")
                  |                 ^^^
            "#]],
        );
    }
}
