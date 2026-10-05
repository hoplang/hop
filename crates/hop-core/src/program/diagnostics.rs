use super::Program;
use crate::diagnostic::Diagnostic;
use crate::root_contained_file_path::RootContainedFilePath;
use std::collections::BTreeSet;

impl Program {
    /// Every diagnostic across all hop and CSS documents, sorted by
    /// document id and position.
    ///
    /// Type errors are not reported for a document that has parse errors,
    /// since they may be nonsensical when parsing fails.
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.parse_errors
            .keys()
            .chain(self.type_errors.keys())
            .chain(self.css_errors.keys())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .flat_map(|document_id| self.document_diagnostics(document_id))
            .collect()
    }

    /// Every diagnostic for a single hop or CSS document, sorted by
    /// position. Returns an empty list for a document the program does not
    /// know about.
    ///
    /// Type errors are not reported for a document that has parse errors,
    /// since they may be nonsensical when parsing fails.
    pub fn document_diagnostics(&self, document_id: &RootContainedFilePath) -> Vec<Diagnostic> {
        let parse_errors = self
            .parse_errors
            .get(document_id)
            .map(Vec::as_slice)
            .unwrap_or_default();

        let type_errors = if parse_errors.is_empty() {
            self.type_errors
                .get(document_id)
                .map(Vec::as_slice)
                .unwrap_or_default()
        } else {
            &[]
        };

        let css_errors = self
            .css_errors
            .get(document_id)
            .map(Vec::as_slice)
            .unwrap_or_default();

        let mut diagnostics = parse_errors
            .iter()
            .map(|error| error.to_diagnostic())
            .chain(type_errors.iter().map(|error| error.to_diagnostic()))
            .chain(css_errors.iter().map(|error| error.to_diagnostic()))
            .collect::<Vec<_>>();

        diagnostics.sort_by(|a, b| {
            a.range()
                .start()
                .cmp(&b.range().start())
                .then(a.range().end().cmp(&b.range().end()))
        });

        diagnostics
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::program_from_archive;
    use crate::diagnostic_severity::DiagnosticSeverity;
    use crate::document_annotator::DocumentAnnotator;
    use crate::root_contained_file_path::RootContainedFilePath;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::Archive;

    fn check(input: &str, module: &str, expected: Expect) {
        let program = program_from_archive(&Archive::from(input));

        let diagnostics =
            program.document_diagnostics(&RootContainedFilePath::new(module).unwrap());

        if diagnostics.is_empty() {
            panic!("Expected diagnostics to be non-empty");
        }

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(diagnostics)
            .render();

        expected.assert_eq(&output);
    }

    #[test]
    fn should_warn_on_unused_import() {
        check(
            indoc! {r#"
                -- components.hop --
                pub fn HelloWorld() -> Html {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import components::HelloWorld

                fn Main() -> Html {
                  <span>No usage of HelloWorld</span>
                }
            "#},
            "main.hop",
            expect![[r#"
                Unused import 'HelloWorld'
                  --> main.hop (line 1, col 1)
                1 | import components::HelloWorld
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_not_warn_on_used_import() {
        let program = program_from_archive(&Archive::from(indoc! {r#"
            -- components.hop --
            pub fn HelloWorld() -> Html {
              <h1>Hello World</h1>
            }

            -- main.hop --
            import components::HelloWorld

            fn Main() -> Html {
              <HelloWorld />
            }
        "#}));

        let diagnostics = program.diagnostics();
        let warnings: Vec<_> = diagnostics
            .into_iter()
            .filter(|d| d.severity() == DiagnosticSeverity::Warning)
            .collect();
        assert!(
            warnings.is_empty(),
            "Expected no warnings for used import, got: {:?}",
            warnings
        );
    }

    #[test]
    fn should_report_parse_errors_as_diagnostics() {
        check(
            indoc! {r#"
                -- main.hop --
                fn Main() -> Html {
                  <div>
                  <span>unclosed span
                }
            "#},
            "main.hop",
            expect![[r#"
                Unclosed <div>
                  --> main.hop (line 2, col 4)
                2 |   <div>
                  |    ^^^

                Unclosed <span>
                  --> main.hop (line 3, col 4)
                3 |   <span>unclosed span
                  |    ^^^^
            "#]],
        );
    }
}
