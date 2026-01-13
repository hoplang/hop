use crate::document::Document;
use crate::error_collector::ErrorCollector;
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::symbols::module_name::ModuleName;
use crate::hop::syntax::format;
use crate::hop::syntax::parser;
use crate::tui::timing::TimingCollector;
use anyhow::Result;
use std::path::Path;

pub struct FmtResult {
    pub files_formatted: usize,
    pub files_unchanged: usize,
    pub timer: TimingCollector,
}

pub fn execute(project_root: &ProjectRoot, file: Option<&str>) -> Result<FmtResult> {
    let mut timer = TimingCollector::new();

    timer.start_phase("load modules");
    let hop_modules: Vec<(ModuleName, Document)> = match file {
        Some(file_path) => {
            let (module_name, document) = project_root.load_hop_module(Path::new(file_path))?;
            vec![(module_name, document)]
        }
        None => {
            let modules = project_root.load_all_hop_modules()?;
            if modules.is_empty() {
                anyhow::bail!("No .hop files found in project");
            }
            modules.into_iter().collect()
        }
    };

    // First pass: parse all files and check for errors
    timer.start_phase("parse");
    let mut parsed_modules = Vec::new();
    for (module_name, document) in hop_modules {
        let mut errors = ErrorCollector::new();
        let ast = parser::parse(module_name.clone(), document.clone(), &mut errors);

        if !errors.is_empty() {
            anyhow::bail!("Parse errors in {}", module_name);
        }

        parsed_modules.push((module_name, document, ast));
    }

    // Second pass: format all files (only if all parsed successfully)
    timer.start_phase("format");
    let mut files_formatted = 0;
    let mut files_unchanged = 0;

    for (module_name, document, ast) in parsed_modules {
        let formatted = format(ast);

        if formatted != document.as_str() {
            let path = project_root.module_name_to_path(&module_name);
            std::fs::write(&path, &formatted)?;
            files_formatted += 1;
        } else {
            files_unchanged += 1;
        }
    }

    timer.end_phase();

    Ok(FmtResult {
        files_formatted,
        files_unchanged,
        timer,
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
}
