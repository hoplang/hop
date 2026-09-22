use anyhow::Result;
use hop_core::{DocumentAnnotator, FormatError, Program, Project};

#[derive(Debug)]
pub struct FmtResult {
    pub files_formatted: usize,
    pub files_unchanged: usize,
}

pub fn execute(project: &Project, file: Option<&str>) -> Result<FmtResult> {
    let document_ids = {
        match file {
            Some(file_path) => {
                let path = std::path::absolute(file_path)?;
                let document_id = project.root().path_to_document_id(&path)?;
                vec![document_id]
            }
            None => project
                .documents()?
                .into_iter()
                .filter(|document_id| document_id.extension() == Some("hop"))
                .collect(),
        }
    };

    let mut program = Program::new();

    for document_id in &document_ids {
        program.update_hop_document(document_id, project.load_document(document_id)?);
    }

    // Format every module before writing any, so that a parse error in one
    // file leaves all files untouched.
    let mut formatted = Vec::new();
    let mut unparsable = Vec::new();
    for document_id in &document_ids {
        match program.format_hop_document(document_id) {
            Ok(source) => formatted.push((document_id, source)),
            Err(FormatError::HasParseErrors(_)) => unparsable.push(document_id),
            Err(err) => return Err(err.into()),
        }
    }

    if !unparsable.is_empty() {
        let mut annotator = DocumentAnnotator::new()
            .with_severity_label()
            .with_lines_before(1)
            .with_location();
        annotator.annotate(
            unparsable
                .iter()
                .flat_map(|document_id| program.document_diagnostics(document_id)),
        );
        anyhow::bail!("Formatting failed:\n{}", annotator.render());
    }

    let mut files_formatted = 0;
    let mut files_unchanged = 0;

    for (document_id, formatted) in formatted {
        let original = project.load_document(document_id)?;
        if formatted != original.as_str() {
            let path = project.root().document_id_to_path(document_id);
            std::fs::write(&path, &formatted)?;
            files_formatted += 1;
        } else {
            files_unchanged += 1;
        }
    }

    Ok(FmtResult {
        files_formatted,
        files_unchanged,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use tempfile::TempDir;
    use txtar::{Archive, read_archive_from_dir, write_archive_to_dir};

    fn check(input: &str, expected: Expect) {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();
        execute(&project, None).expect("formatting should succeed");
        let output_archive = read_archive_from_dir(temp_dir.path()).unwrap();
        expected.assert_eq(&output_archive.to_string());
    }

    fn check_file(input: &str, file: &str, expected: Expect) {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();
        let file_path = temp_dir.path().join(file);
        execute(&project, Some(file_path.to_str().unwrap())).expect("formatting should succeed");
        let output_archive = read_archive_from_dir(temp_dir.path()).unwrap();
        expected.assert_eq(&output_archive.to_string());
    }

    fn check_error(input: &str, expected_error: Expect, expected_files: Expect) {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();
        let err = match execute(&project, None) {
            Ok(_) => panic!("formatting should fail"),
            Err(e) => e,
        };
        expected_error.assert_eq(&format!("{err}"));
        let output_archive = read_archive_from_dir(temp_dir.path()).unwrap();
        expected_files.assert_eq(&output_archive.to_string());
    }

    #[test]
    fn should_format_unformatted_file() {
        check(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main(name: String, count: Int) -> Html {<div>{name}</div>}
            "#},
            expect![[r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main(
                  name: String,
                  count: Int,
                ) -> Html {
                  <div>
                    {name}
                  </div>
                }
            "#]],
        )
    }

    #[test]
    fn should_format_multiple_files() {
        check(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main() -> Html {<div>hello</div>}
                -- other.hop --
                fn Other() -> Html {<span>world</span>}
            "#},
            expect![[r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main() -> Html {
                  <div>
                    hello
                  </div>
                }
                -- other.hop --
                fn Other() -> Html {
                  <span>
                    world
                  </span>
                }
            "#]],
        )
    }

    #[test]
    fn should_format_single_file() {
        check_file(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main() -> Html {<div>hello</div>}
                -- other.hop --
                fn Other() -> Html {<span>world</span>}
            "#},
            "main.hop",
            expect![[r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main() -> Html {
                  <div>
                    hello
                  </div>
                }
                -- other.hop --
                fn Other() -> Html {<span>world</span>}
            "#]],
        )
    }

    #[test]
    fn should_not_format_any_file_if_some_file_has_parse_errors() {
        check_error(
            indoc! {r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main() -> Html {<div>hello</div>}
                -- broken.hop --
                fn Broken() -> Html {
                  <div>
            "#},
            expect![[r#"
                Formatting failed:
                error: Unmatched '{'
                  --> broken.hop (line 1, col 21)
                1 | fn Broken() -> Html {
                  |                     ^

                error: Unclosed <div>
                  --> broken.hop (line 2, col 4)
                1 | fn Broken() -> Html {
                2 |   <div>
                  |    ^^^
            "#]],
            expect![[r#"
                -- broken.hop --
                fn Broken() -> Html {
                  <div>
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                fn Main() -> Html {<div>hello</div>}
            "#]],
        )
    }
}
