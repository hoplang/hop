use crate::document::{Document, DocumentAnnotator};
use crate::error_collector::ErrorCollector;
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::symbols::module_name::ModuleName;
use crate::hop::syntax::format;
use crate::hop::syntax::parser;
use crate::parse_error::ParseError;
use anyhow::Result;
use rayon::prelude::*;
use std::path::PathBuf;

#[derive(Debug)]
pub struct FmtResult {
    pub files_formatted: usize,
    pub files_unchanged: usize,
}

struct FormattedModule {
    module_name: ModuleName,
    original: Document,
    formatted: String,
    errors: Vec<ParseError>,
}

enum ModuleResult {
    Success(FormattedModule),
    LoadError { path: PathBuf, error: anyhow::Error },
}

pub fn execute(project_root: &ProjectRoot, file: Option<&str>) -> Result<FmtResult> {
    let module_paths: Vec<PathBuf> = match file {
        Some(file_path) => vec![PathBuf::from(file_path)],
        None => {
            let paths = project_root.find_hop_files()?;
            if paths.is_empty() {
                anyhow::bail!("No .hop files found in project");
            }
            paths
        }
    };

    let results: Vec<ModuleResult> = module_paths
        .into_par_iter()
        .map(|path| {
            let (module_name, document) = match project_root.load_hop_module(&path) {
                Ok(result) => result,
                Err(e) => return ModuleResult::LoadError { path, error: e },
            };
            let mut errors = ErrorCollector::new();
            let ast = parser::parse(module_name.clone(), document.clone(), &mut errors);
            let formatted = format(ast);
            ModuleResult::Success(FormattedModule {
                module_name,
                original: document,
                formatted,
                errors: errors.to_vec(),
            })
        })
        .collect();

    // Check for errors
    let mut error_output_parts = Vec::new();
    let annotator = DocumentAnnotator::new()
        .with_label("error")
        .with_lines_before(1)
        .with_location();

    for result in &results {
        match result {
            ModuleResult::LoadError { path, error } => {
                anyhow::bail!("Failed to read {}: {}", path.display(), error);
            }
            ModuleResult::Success(m) if !m.errors.is_empty() => {
                let filename = format!("{}.hop", m.module_name);
                error_output_parts.push(annotator.annotate(Some(&filename), m.errors.iter()));
            }
            _ => {}
        }
    }

    if !error_output_parts.is_empty() {
        anyhow::bail!("Formatting failed:\n{}", error_output_parts.join("\n"));
    }

    // Write all results to disk (only if all loaded and parsed successfully)
    let mut files_formatted = 0;
    let mut files_unchanged = 0;

    for result in results {
        if let ModuleResult::Success(result) = result {
            if result.formatted != result.original.as_str() {
                let path = project_root.module_name_to_path(&result.module_name);
                std::fs::write(&path, &result.formatted)?;
                files_formatted += 1;
            } else {
                files_unchanged += 1;
            }
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
    use crate::test_utils::archive::temp_dir_from_archive;
    use indoc::indoc;
    use simple_txtar::Archive;
    use std::fs;

    #[test]
    fn formats_unformatted_file() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <Main {name: String, count: Int}><div>{name}</div></Main>
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project_root = ProjectRoot::from(&temp_dir).unwrap();

        let result = execute(&project_root, None).unwrap();

        assert_eq!(result.files_formatted, 1);
        assert_eq!(result.files_unchanged, 0);

        let formatted_content = fs::read_to_string(temp_dir.join("main.hop")).unwrap();
        assert_eq!(
            formatted_content,
            indoc! {r#"
                <Main {name: String, count: Int}>
                  <div>
                    {name}
                  </div>
                </Main>
            "#}
        );
    }

    #[test]
    fn formats_multiple_files() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <Main><div>hello</div></Main>
            -- other.hop --
            <Other><span>world</span></Other>
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project_root = ProjectRoot::from(&temp_dir).unwrap();

        let result = execute(&project_root, None).unwrap();

        assert_eq!(result.files_formatted, 2);
        assert_eq!(result.files_unchanged, 0);
    }

    #[test]
    fn formats_single_file() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <Main><div>hello</div></Main>
            -- other.hop --
            <Other><span>world</span></Other>
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project_root = ProjectRoot::from(&temp_dir).unwrap();

        let main_path = temp_dir.join("main.hop");
        let result = execute(&project_root, Some(main_path.to_str().unwrap())).unwrap();

        // Only main.hop should be formatted
        assert_eq!(result.files_formatted, 1);
        assert_eq!(result.files_unchanged, 0);

        // Verify main.hop was formatted
        let main_content = fs::read_to_string(&main_path).unwrap();
        assert!(main_content.contains("<Main>"));
        assert!(main_content.contains("  <div>"));

        // Verify other.hop was NOT formatted (still on one line)
        let other_content = fs::read_to_string(temp_dir.join("other.hop")).unwrap();
        assert_eq!(other_content, "<Other><span>world</span></Other>\n");
    }

    #[test]
    fn does_not_format_any_file_if_any_has_parse_errors() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <Main><div>hello</div></Main>
            -- broken.hop --
            <Broken><div>
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project_root = ProjectRoot::from(&temp_dir).unwrap();

        let result = execute(&project_root, None);

        // Should return an error because broken.hop has parse errors
        assert!(result.is_err());

        // Verify main.hop was NOT formatted (still on one line)
        let main_content = fs::read_to_string(temp_dir.join("main.hop")).unwrap();
        assert_eq!(main_content, "<Main><div>hello</div></Main>\n");
    }

    #[test]
    fn displays_parse_error_details() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- broken.hop --
            <Broken><div>
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project_root = ProjectRoot::from(&temp_dir).unwrap();

        let result = execute(&project_root, None);

        let error = result.unwrap_err();
        let error_message = error.to_string();

        // Error should include the filename
        assert!(
            error_message.contains("broken.hop"),
            "Error should include filename, got: {}",
            error_message
        );
        // Error should include source location
        assert!(
            error_message.contains("line"),
            "Error should include line number, got: {}",
            error_message
        );
        // Error should include the actual error message, not just "Parse errors in"
        assert!(
            !error_message.starts_with("Parse errors in"),
            "Error should show detailed message, not just 'Parse errors in', got: {}",
            error_message
        );
    }
}
