use anyhow::Result;
use hop_core::document_annotator::DocumentAnnotator;
use hop_core::program::Program;
use hop_core::project::Project;
use std::path::PathBuf;

#[derive(Debug)]
pub struct FmtResult {
    pub files_formatted: usize,
    pub files_unchanged: usize,
}

pub fn execute(project: &Project, file: Option<&str>) -> Result<FmtResult> {
    let document_ids = {
        match file {
            Some(file_path) => {
                let document_id = project.path_to_document_id(&PathBuf::from(file_path))?;
                vec![document_id]
            }
            None => project.find_hop_modules()?,
        }
    };

    let mut program = Program::default();

    for document_id in &document_ids {
        program.update_module(document_id, project.load_document(document_id)?);
    }

    // Check for parse errors
    let mut annotator = DocumentAnnotator::new()
        .with_label("error")
        .with_lines_before(1)
        .with_location();

    for (document_id, errors) in program.get_parse_errors() {
        if !errors.is_empty() {
            annotator.annotate(document_id, errors);
        }
    }

    if !annotator.is_empty() {
        anyhow::bail!("Formatting failed:\n{}", annotator.render());
    }

    let mut files_formatted = 0;
    let mut files_unchanged = 0;

    for document_id in &document_ids {
        let original = project.load_document(document_id)?;
        let formatted = program.get_formatted_module(document_id)?;
        if formatted != original.as_str() {
            let path = project.document_id_to_path(document_id);
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
                component Main(name: String, count: Int) {<div>{name}</div>}
            "#},
            expect![[r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                component Main(
                  name: String,
                  count: Int,
                ) {
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
                component Main {<div>hello</div>}
                -- other.hop --
                component Other {<span>world</span>}
            "#},
            expect![[r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                component Main {
                  <div>
                    hello
                  </div>
                }
                -- other.hop --
                component Other {
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
                component Main {<div>hello</div>}
                -- other.hop --
                component Other {<span>world</span>}
            "#},
            "main.hop",
            expect![[r#"
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                component Main {
                  <div>
                    hello
                  </div>
                }
                -- other.hop --
                component Other {<span>world</span>}
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
                component Main {<div>hello</div>}
                -- broken.hop --
                component Broken {
                  <div>
            "#},
            expect![[r#"
                Formatting failed:
                error: Unmatched '{'
                  --> broken.hop (line 1, col 18)
                1 | component Broken {
                  |                  ^

                error: Unclosed <div>
                  --> broken.hop (line 2, col 4)
                1 | component Broken {
                2 |   <div>
                  |    ^^^
            "#]],
            expect![[r#"
                -- broken.hop --
                component Broken {
                  <div>
                -- hop.toml --
                [compile]
                target = "ts"
                output_path = "app.ts"
                -- main.hop --
                component Main {<div>hello</div>}
            "#]],
        )
    }
}
