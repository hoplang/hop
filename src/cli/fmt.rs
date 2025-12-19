use crate::error_collector::ErrorCollector;
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::syntax::parse_error::ParseError;
use crate::hop::syntax::parser;
use anyhow::Result;

pub struct FmtResult {
    pub files_formatted: usize,
    pub files_unchanged: usize,
}

pub fn execute(project_root: &ProjectRoot) -> Result<FmtResult> {
    let hop_modules = project_root.load_all_hop_modules()?;

    if hop_modules.is_empty() {
        anyhow::bail!("No .hop files found in project");
    }

    let mut files_formatted = 0;
    let mut files_unchanged = 0;

    for (module_name, source) in hop_modules {
        let mut errors = ErrorCollector::<ParseError>::new();
        let ast = parser::parse(module_name.clone(), source.clone(), &mut errors);

        if !errors.is_empty() {
            // Skip files with parse errors
            continue;
        }

        let formatted = ast.to_doc().pretty(60).to_string();

        if formatted != source {
            let path = project_root.module_name_to_path(&module_name);
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

        let result = execute(&project_root).unwrap();

        assert_eq!(result.files_formatted, 1);
        assert_eq!(result.files_unchanged, 0);

        let formatted_content = fs::read_to_string(temp_dir.join("main.hop")).unwrap();
        assert_eq!(
            formatted_content,
            indoc! {r#"
                <Main {
                  name: String,
                  count: Int,
                }>
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

        let result = execute(&project_root).unwrap();

        assert_eq!(result.files_formatted, 2);
        assert_eq!(result.files_unchanged, 0);
    }

    #[test]
    fn skips_files_with_parse_errors() {
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

        let result = execute(&project_root).unwrap();

        // Only the valid file should be formatted
        assert_eq!(result.files_formatted, 1);
        assert_eq!(result.files_unchanged, 0);
    }
}
