//! Represents a hop project and provides methods for working with it.
//!
//! A [`Project`] is anchored to a directory containing a `hop.toml` configuration
//! file. It provides functionality for:
//!
//! - Converting between file paths and [`ModuleId`]
//! - Loading modules and configuration

use anyhow::Context;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::config::HopConfig;
use crate::document::Document;
use crate::document_id::DocumentId;

#[derive(Debug, Clone, PartialEq)]
pub struct Project {
    // Directory containing the hop.toml file
    project_root: PathBuf,
}

impl Project {
    /// Construct the project from a path.
    ///
    /// The path should be a directory and contain the config file.
    pub fn from(path: &Path) -> anyhow::Result<Project> {
        if !path.is_dir() {
            anyhow::bail!("{:?} is not a directory", &path)
        }
        let canonicalized = path
            .canonicalize()
            .with_context(|| format!("Failed to canonicalize path {:?}", &path))?;
        let config_file = canonicalized.join("hop.toml");
        if !config_file.exists() {
            anyhow::bail!("Expected to find hop.toml in {:?}", &path)
        }
        Ok(Project {
            project_root: canonicalized.clone(),
        })
    }

    /// Find the project root by traversing into superdirectories.
    pub fn find_traversing_superdirectories(start_path: &Path) -> anyhow::Result<Project> {
        let canonicalized = start_path
            .canonicalize()
            .with_context(|| format!("Failed to canonicalize path {:?}", &start_path))?;
        let mut current_dir = if canonicalized.is_file() {
            canonicalized
                .parent()
                .ok_or_else(|| anyhow::anyhow!("Can't get parent of path {:?}", &canonicalized))?
        } else {
            &canonicalized
        };

        loop {
            let config_file = current_dir.join("hop.toml");
            if config_file.exists() {
                return Ok(Project {
                    project_root: current_dir.to_path_buf(),
                });
            }
            current_dir = current_dir.parent().ok_or_else(|| {
                anyhow::anyhow!(
                    "Failed to locate hop.toml file in {:?} or any parent directory",
                    &start_path
                )
            })?;
        }
    }

    /// Find the project root by traversing into subdirectories.
    pub fn find_traversing_subdirectories(start_path: &Path) -> anyhow::Result<Project> {
        let canonicalized = start_path
            .canonicalize()
            .with_context(|| format!("Failed to canonicalize path {:?}", &start_path))?;

        let mut paths: Vec<PathBuf> = vec![canonicalized.clone()];

        while let Some(path) = paths.pop() {
            if path.is_dir() {
                if let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) {
                    if should_skip_directory(dir_name) {
                        continue;
                    }
                }

                let config_file = path.join("hop.toml");
                if config_file.exists() {
                    return Ok(Project { project_root: path });
                }

                if let Ok(entries) = std::fs::read_dir(&path) {
                    for entry in entries.flatten() {
                        let p = entry.path();
                        if p.is_dir() {
                            paths.push(p);
                        }
                    }
                }
            }
        }

        anyhow::bail!(
            "Failed to locate hop.toml file in {:?} or any subdirectory",
            &start_path
        )
    }

    pub fn get_project_root(&self) -> &Path {
        &self.project_root
    }

    /// Convert a file path to a ModuleId using this project root as reference
    pub fn path_to_document_id(&self, file_path: &Path) -> anyhow::Result<DocumentId> {
        let canonical = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.to_path_buf());
        let relative_path = canonical
            .strip_prefix(&self.project_root)
            .with_context(|| {
                format!(
                    "Path {:?} is not inside the project at {:?}",
                    file_path, self.project_root
                )
            })?;

        let module_str = relative_path.to_string_lossy().to_string();

        DocumentId::new(&module_str)
            .map_err(|e| anyhow::anyhow!("Invalid document id for path {:?}: {}", file_path, e))
    }

    /// Convert a ModuleId back to a file path
    pub fn document_id_to_path(&self, document_id: &DocumentId) -> PathBuf {
        self.project_root.join(document_id.as_str())
    }

    /// Load a single document from its module ID
    pub fn load_document(&self, document_id: &DocumentId) -> anyhow::Result<Document> {
        let path = self.document_id_to_path(document_id);
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read document {:?} at {:?}", document_id, path))?;
        Ok(Document::new(document_id.clone(), content))
    }

    /// Check if a document exists.
    pub fn document_exists(&self, document_id: &DocumentId) -> io::Result<bool> {
        let path = self.document_id_to_path(document_id);
        std::fs::exists(path)
    }

    /// Find all hop modules in this project.
    pub fn find_hop_modules(&self) -> anyhow::Result<Vec<DocumentId>> {
        self.find_files_by_extension("hop")
    }

    pub fn find_css_documents(&self) -> anyhow::Result<Vec<DocumentId>> {
        self.find_files_by_extension("css")
    }

    fn find_files_by_extension(&self, extension: &str) -> anyhow::Result<Vec<DocumentId>> {
        let mut document_ids = Vec::new();

        if !self.project_root.exists() || !self.project_root.is_dir() {
            return Ok(document_ids);
        }

        let mut paths: Vec<PathBuf> = Vec::new();
        paths.push(self.project_root.clone());

        while let Some(path) = paths.pop() {
            if path.is_dir() {
                if let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) {
                    if should_skip_directory(dir_name) {
                        continue;
                    }
                }

                let entries = std::fs::read_dir(&path)
                    .with_context(|| format!("Failed to read directory {:?}", &path))?;
                for entry in entries {
                    let p = entry.context("Failed to read directory entry")?.path();
                    paths.push(p);
                }
            } else if path.extension().and_then(|s| s.to_str()) == Some(extension) {
                document_ids.push(self.path_to_document_id(&path)?);
            }
        }

        Ok(document_ids)
    }

    /// Load the hop.toml configuration file from this project root
    pub fn load_config(&self) -> anyhow::Result<HopConfig> {
        let config_path = self.project_root.join("hop.toml");

        if !config_path.exists() {
            anyhow::bail!("hop.toml not found at {:?}", config_path);
        }

        let config_str = fs::read_to_string(&config_path)
            .with_context(|| format!("Failed to read hop.toml at {:?}", config_path))?;

        let config = HopConfig::from_toml_str(&config_str)
            .with_context(|| format!("Failed to parse hop.toml at {:?}", config_path))?;

        Ok(config)
    }

    pub fn get_css_input_path(&self) -> anyhow::Result<Option<PathBuf>> {
        let config = self.load_config()?;
        Ok(config.css.map(|c| self.project_root.join(c.input_path)))
    }

    pub fn get_js_input_path(&self) -> anyhow::Result<Option<PathBuf>> {
        let config = self.load_config()?;
        Ok(config.js.map(|j| self.project_root.join(j.input_path)))
    }

    pub fn get_output_path(&self) -> anyhow::Result<PathBuf> {
        let config = self.load_config()?;
        let resolved = config.get_resolved_config()?;
        Ok(self.project_root.join(&resolved.output_path))
    }

    pub fn write_output_path(&self, data: &str) -> anyhow::Result<PathBuf> {
        let path = self.get_output_path()?;
        // Preserve the file's mtime if the content is unchanged, so downstream
        // build tools (e.g. cargo) don't trigger unnecessary recompiles.
        if let Ok(existing) = fs::read(&path)
            && existing == data.as_bytes()
        {
            return Ok(path.canonicalize()?);
        }
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&path, data)?;
        Ok(path.canonicalize()?)
    }
}

/// Check if a directory should be skipped during file search
fn should_skip_directory(dir_name: &str) -> bool {
    matches!(
        dir_name,
        "target"
            | ".git"
            | "node_modules"
            | ".cargo"
            | ".rustup"
            | "dist"
            | "build"
            | ".next"
            | ".nuxt"
            | "coverage"
            | ".nyc_output"
            | ".pytest_cache"
            | "__pycache__"
            | ".venv"
            | "vendor"
            | ".idea"
            | ".vscode"
            | ".direnv"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use tempfile::TempDir;
    use txtar::{Archive, write_archive_to_dir};

    #[test]
    fn find_config_file() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- src/components/.gitkeep --

        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();

        // Test finding from nested directory
        let nested_dir = temp_dir.path().join("src").join("components");
        let found = Project::find_traversing_superdirectories(&nested_dir).unwrap();
        assert_eq!(found.project_root, temp_dir.path().canonicalize().unwrap());
    }

    #[test]
    fn find_config_file_downwards() {
        let archive = Archive::from(indoc! {r#"
            -- hop/hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- hop/main.hop --
            type User {
                name: String
            }
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();

        // Test finding from parent directory
        let found = Project::find_traversing_subdirectories(temp_dir.path()).unwrap();
        assert_eq!(
            found.project_root,
            temp_dir.path().join("hop").canonicalize().unwrap()
        );
    }

    #[test]
    fn find_config_file_not_found() {
        let archive = Archive::from(indoc! {r#"
            -- src/components/test.hop --
            <test-comp>Hello</test-comp>
            -- src/main.rs --
            fn main() {}
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();

        // Test that find_upwards fails when no hop.toml exists
        let nested_dir = temp_dir.path().join("src").join("components");
        let result = Project::find_traversing_superdirectories(&nested_dir);
        assert!(result.is_err());

        let error_message = result.unwrap_err().to_string();
        assert!(error_message.contains("Failed to locate hop.toml"));
    }

    #[test]
    fn path_to_document_id() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <main-component>Test</main-component>
            -- src/components/button.hop --
            <button-comp>Click</button-comp>
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // Test converting file paths to module names
        let button_path = temp_dir.path().join("src/components/button.hop");
        let document_id = project.path_to_document_id(&button_path).unwrap();
        assert_eq!(document_id.as_str(), "src/components/button.hop");

        let main_path = temp_dir.path().join("main.hop");
        let main_module = project.path_to_document_id(&main_path).unwrap();
        assert_eq!(main_module.as_str(), "main.hop");
    }

    #[test]
    fn path_to_document_id_outside_project() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // Try to convert a path outside the project
        let outside_path = PathBuf::from("/some/other/path/file.hop");
        let result = project.path_to_document_id(&outside_path);

        assert!(result.is_err());
        let error_message = result.unwrap_err().to_string();
        assert!(
            error_message.contains("is not inside the project"),
            "Expected error about path not inside project, got: {}",
            error_message
        );
    }

    #[test]
    fn document_id_to_path() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // Test converting module names back to paths
        let module = DocumentId::new("src/components/button.hop").unwrap();
        let path = project.document_id_to_path(&module);
        assert_eq!(
            path.strip_prefix(temp_dir.path().canonicalize().unwrap())
                .unwrap()
                .to_string_lossy()
                .replace('\\', "/"),
            "src/components/button.hop"
        );
    }

    #[test]
    fn load_module() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- src/components/button.hop --
            <button-comp>Click me!</button-comp>
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let document_id = DocumentId::new("src/components/button.hop").unwrap();
        let document = project.load_document(&document_id).unwrap();

        assert!(
            document
                .as_str()
                .contains("<button-comp>Click me!</button-comp>")
        );
    }

    #[test]
    fn load_module_not_found() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let document_id = DocumentId::new("nonexistent/module.hop").unwrap();
        let result = project.load_document(&document_id);

        assert!(result.is_err());
    }

    #[test]
    fn write_output_path_preserves_file_when_unchanged() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let path = project.write_output_path("hello").unwrap();
        let mtime_before = std::fs::metadata(&path).unwrap().modified().unwrap();

        // Sleep longer than typical filesystem mtime resolution.
        std::thread::sleep(std::time::Duration::from_millis(5));

        project.write_output_path("hello").unwrap();
        let mtime_after_same = std::fs::metadata(&path).unwrap().modified().unwrap();
        assert_eq!(
            mtime_before, mtime_after_same,
            "mtime should be preserved when content is unchanged"
        );

        project.write_output_path("changed").unwrap();
        let mtime_after_change = std::fs::metadata(&path).unwrap().modified().unwrap();
        assert_ne!(
            mtime_before, mtime_after_change,
            "mtime should update when content changes"
        );
    }

    #[test]
    fn find_modules() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- src/main.hop --
            <main-comp>Main</main-comp>
            -- src/components/button.hop --
            <button-comp>Click me!</button-comp>
            -- src/components/header.hop --
            <header-comp>Welcome</header-comp>
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let mut modules = project.find_hop_modules().unwrap();
        modules.sort();

        assert_eq!(modules.len(), 3);
        assert_eq!(
            modules[0],
            DocumentId::new("src/components/button.hop").unwrap()
        );
        assert_eq!(
            modules[1],
            DocumentId::new("src/components/header.hop").unwrap()
        );
        assert_eq!(modules[2], DocumentId::new("src/main.hop").unwrap());
    }

    #[test]
    fn skip_directories() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- src/main.hop --
            <main-comp>Main</main-comp>
            -- node_modules/package/index.hop --
            <should-not-find>This should be skipped</should-not-find>
            -- .git/hooks/pre-commit.hop --
            <should-not-find>This should also be skipped</should-not-find>
            -- target/debug/test.hop --
            <should-not-find>Skip this too</should-not-find>
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // Test that find_modules correctly skips certain directories
        let modules = project.find_hop_modules().unwrap();

        // Should only load 1 module (from src/main.hop)
        assert_eq!(modules.len(), 1);

        // Check which modules were loaded
        assert_eq!(modules[0], DocumentId::new("src/main.hop").unwrap());

        // Should NOT contain modules from skipped directories
        let document_ids: Vec<String> = modules.iter().map(|m| m.to_string()).collect();
        assert!(!document_ids.iter().any(|m| m.contains("node_modules")));
        assert!(!document_ids.iter().any(|m| m.contains(".git")));
        assert!(!document_ids.iter().any(|m| m.contains("target")));
    }

    #[test]
    fn load_config_missing_hop_toml_error() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <main-component>Test</main-component>
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // Delete the hop.toml to test the error case
        std::fs::remove_file(temp_dir.path().join("hop.toml")).unwrap();

        let result = project.load_config();
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("hop.toml not found")
        );
    }

    #[test]
    fn load_config_with_empty_hop_toml() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            # Empty config file
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // Empty config should now parse successfully (build section is optional)
        let result = project.load_config();
        assert!(
            result.is_ok(),
            "Empty config should parse: {:?}",
            result.err()
        );
    }

    #[test]
    fn load_config_without_build_section() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let result = project.load_config();
        assert!(
            result.is_ok(),
            "Config without build section should succeed: {:?}",
            result.err()
        );
    }

    #[test]
    fn get_css_input_path_without_build_section() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            bundler = "tailwind_4"
            input_path = "styles/input.css"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let result = project.get_css_input_path();
        assert!(
            result.is_ok(),
            "get_css_input_path should succeed without build section: {:?}",
            result.err()
        );

        let path = result.unwrap();
        assert!(path.is_some());
        assert!(path.unwrap().ends_with("styles/input.css"));
    }

    #[test]
    fn get_output_path_errors_without_build_config() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let result = project.get_output_path();
        assert!(
            result.is_err(),
            "get_output_path should fail without build config"
        );
    }
}
