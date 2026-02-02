use anyhow::Context;
use std::path::{Path, PathBuf};
use tokio::fs as async_fs;

use crate::config::HopConfig;
use crate::document::Document;
use crate::hop::symbols::module_id::ModuleId;

#[derive(Debug, Clone, PartialEq)]
pub struct Project {
    // Directory containing the hop.toml file
    directory: PathBuf,
}

impl Project {
    /// Find the project root by traversing upwards.
    pub fn find_upwards(start_path: &Path) -> anyhow::Result<Project> {
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
                    directory: current_dir.to_path_buf(),
                });
            }
            current_dir = current_dir.parent().ok_or_else(|| {
                anyhow::anyhow!(
                    "Failed to locate hop.toml file in {:?} or any parent directory",
                    &start_path
                )
            })?
        }
    }

    /// Construct the project root from a path.
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
            directory: canonicalized.clone(),
        })
    }

    pub fn get_path(&self) -> &Path {
        &self.directory
    }

    /// Convert a file path to a ModuleId using this project root as reference
    pub fn path_to_module_id(&self, file_path: &Path) -> anyhow::Result<ModuleId> {
        let relative_path = file_path
            .strip_prefix(&self.directory)
            .with_context(|| format!("Failed to strip prefix from path {:?}", file_path))?;

        let module_str = relative_path
            .with_extension("")
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, "::");

        ModuleId::new(&module_str)
            .map_err(|e| anyhow::anyhow!("Invalid module name for path {:?}: {}", file_path, e))
    }

    /// Convert a ModuleId back to a file path
    pub fn module_id_to_path(&self, module_id: &ModuleId) -> PathBuf {
        let module_path = module_id
            .to_path()
            .replace('/', std::path::MAIN_SEPARATOR_STR);
        self.directory.join(format!("{}.hop", module_path))
    }

    /// Load a single hop module by its module ID
    pub fn load_module(&self, module_id: &ModuleId) -> anyhow::Result<Document> {
        let path = self.module_id_to_path(module_id);
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read module {:?} at {:?}", module_id, path))?;
        Ok(Document::new(module_id.clone(), content))
    }

    /// Find all module IDs in this project root
    pub fn find_modules(&self) -> anyhow::Result<Vec<ModuleId>> {
        let mut modules = Vec::new();

        if !self.directory.exists() || !self.directory.is_dir() {
            return Ok(modules);
        }

        let mut paths: Vec<PathBuf> = Vec::new();
        paths.push(self.directory.clone());

        while let Some(path) = paths.pop() {
            if path.is_dir() {
                // Check if we should skip this directory
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
            } else if path.extension().and_then(|s| s.to_str()) == Some("hop") {
                let module_id = self.path_to_module_id(&path)?;
                modules.push(module_id);
            }
        }

        Ok(modules)
    }

    /// Load the hop.toml configuration file from this project root
    pub async fn load_config(&self) -> anyhow::Result<HopConfig> {
        let config_path = self.directory.join("hop.toml");

        if !config_path.exists() {
            anyhow::bail!("hop.toml not found at {:?}", config_path);
        }

        let config_str = async_fs::read_to_string(&config_path)
            .await
            .with_context(|| format!("Failed to read hop.toml at {:?}", config_path))?;

        let config = HopConfig::from_toml_str(&config_str)
            .with_context(|| format!("Failed to parse hop.toml at {:?}", config_path))?;

        Ok(config)
    }

    pub async fn get_tailwind_input_path(&self) -> anyhow::Result<Option<PathBuf>> {
        let config = self.load_config().await?;
        Ok(config.css.tailwind.map(|p| self.directory.join(p.input)))
    }

    pub async fn get_output_path(&self) -> anyhow::Result<PathBuf> {
        let config = self.load_config().await?;
        let resolved = config.get_resolved_config()?;
        Ok(self.directory.join(&resolved.output_path))
    }

    pub async fn write_output_path(&self, data: &str) -> anyhow::Result<PathBuf> {
        let path = self.get_output_path().await?;
        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            async_fs::create_dir_all(parent).await?;
        }
        async_fs::write(&path, data).await?;
        Ok(path)
    }
}

/// Check if a directory should be skipped during .hop file search
pub(crate) fn should_skip_directory(dir_name: &str) -> bool {
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
    use crate::test_utils::archive::temp_dir_from_archive;
    use indoc::indoc;
    use simple_txtar::Archive;

    #[test]
    fn find_config_file() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
            -- src/components/.gitkeep --

        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();

        // Test finding from nested directory
        let nested_dir = temp_dir.join("src").join("components");
        let found = Project::find_upwards(&nested_dir).unwrap();
        assert_eq!(found.directory, temp_dir);

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn find_config_file_not_found() {
        let archive = Archive::from(indoc! {r#"
            -- src/components/test.hop --
            <test-comp>Hello</test-comp>
            -- src/main.rs --
            fn main() {}
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();

        // Test that find_upwards fails when no hop.toml exists
        let nested_dir = temp_dir.join("src").join("components");
        let result = Project::find_upwards(&nested_dir);
        assert!(result.is_err());

        let error_message = result.unwrap_err().to_string();
        assert!(error_message.contains("Failed to locate hop.toml"));

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn path_to_module_id() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <main-component>Test</main-component>
            -- src/components/button.hop --
            <button-comp>Click</button-comp>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Test converting file paths to module names
        let button_path = temp_dir.join("src/components/button.hop");
        let module_id = project.path_to_module_id(&button_path).unwrap();
        assert_eq!(module_id.to_path(), "src/components/button");

        let main_path = temp_dir.join("main.hop");
        let main_module = project.path_to_module_id(&main_path).unwrap();
        assert_eq!(main_module.to_path(), "main");

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn module_id_to_path() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Test converting module names back to paths
        let module = ModuleId::new("src::components::button").unwrap();
        let path = project.module_id_to_path(&module);
        assert_eq!(
            path.strip_prefix(&temp_dir)
                .unwrap()
                .to_string_lossy()
                .replace('\\', "/"),
            "src/components/button.hop"
        );

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn load_module() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
            -- src/components/button.hop --
            <button-comp>Click me!</button-comp>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let module_id = ModuleId::new("src::components::button").unwrap();
        let document = project.load_module(&module_id).unwrap();

        assert!(
            document
                .as_str()
                .contains("<button-comp>Click me!</button-comp>")
        );

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn load_module_not_found() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let module_id = ModuleId::new("nonexistent::module").unwrap();
        let result = project.load_module(&module_id);

        assert!(result.is_err());

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn find_modules() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
            -- src/main.hop --
            <main-comp>Main</main-comp>
            -- src/components/button.hop --
            <button-comp>Click me!</button-comp>
            -- src/components/header.hop --
            <header-comp>Welcome</header-comp>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let mut modules = project.find_modules().unwrap();
        modules.sort();

        assert_eq!(modules.len(), 3);
        assert_eq!(
            modules[0],
            ModuleId::new("src::components::button").unwrap()
        );
        assert_eq!(
            modules[1],
            ModuleId::new("src::components::header").unwrap()
        );
        assert_eq!(modules[2], ModuleId::new("src::main").unwrap());

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn files_with_dots_in_names_are_rejected() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
            -- src/foo.bar.hop --
            <foo-bar-comp>Component with dots</foo-bar-comp>
            -- src/utils/helper_test.hop --
            <test-helper>Test helper</test-helper>
            -- src/components/button_min.hop --
            <button-min>Minified button</button-min>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Files with dots in the name (excluding .hop extension) should fail validation
        let foo_bar_path = temp_dir.join("src/foo.bar.hop");
        let result = project.path_to_module_id(&foo_bar_path);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Module ID contains invalid character: '.'")
        );

        // Files with underscores should work fine
        let helper_test_path = temp_dir.join("src/utils/helper_test.hop");
        let helper_test_module = project.path_to_module_id(&helper_test_path).unwrap();
        assert_eq!(helper_test_module.to_path(), "src/utils/helper_test");

        let button_min_path = temp_dir.join("src/components/button_min.hop");
        let button_min_module = project.path_to_module_id(&button_min_path).unwrap();
        assert_eq!(button_min_module.to_path(), "src/components/button_min");

        // find_modules should fail when files produce invalid module names
        let result = project.find_modules();
        // This will fail because foo.bar.hop creates an invalid module name
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Invalid module name")
        );

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn skip_directories() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
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
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Test that find_modules correctly skips certain directories
        let modules = project.find_modules().unwrap();

        // Should only load 1 module (from src/main.hop)
        assert_eq!(modules.len(), 1);

        // Check which modules were loaded
        assert_eq!(modules[0], ModuleId::new("src::main").unwrap());

        // Should NOT contain modules from skipped directories
        let module_ids: Vec<String> = modules.iter().map(|m| m.to_string()).collect();
        assert!(!module_ids.iter().any(|m| m.contains("node_modules")));
        assert!(!module_ids.iter().any(|m| m.contains(".git")));
        assert!(!module_ids.iter().any(|m| m.contains("target")));

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn load_css_config() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"

            [build]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let config = project.load_config().await.unwrap();

        assert_eq!(config.css.mode, Some("tailwind4".to_string()));

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn load_config_missing_hop_toml_error() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <main-component>Test</main-component>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Delete the hop.toml to test the error case
        std::fs::remove_file(temp_dir.join("hop.toml")).unwrap();

        let result = project.load_config().await;
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("hop.toml not found")
        );

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn load_config_with_empty_hop_toml() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            # Empty config file
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Empty config should now parse successfully (build section is optional)
        let result = project.load_config().await;
        assert!(
            result.is_ok(),
            "Empty config should parse: {:?}",
            result.err()
        );

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn load_config_without_build_section() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let result = project.load_config().await;
        assert!(
            result.is_ok(),
            "Config without build section should succeed: {:?}",
            result.err()
        );

        let config = result.unwrap();
        assert_eq!(config.css.mode, Some("tailwind4".to_string()));

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn get_tailwind_input_path_without_build_section() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css.tailwind]
            input = "styles/input.css"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let result = project.get_tailwind_input_path().await;
        assert!(
            result.is_ok(),
            "get_tailwind_input_path should succeed without build section: {:?}",
            result.err()
        );

        let path = result.unwrap();
        assert!(path.is_some());
        assert!(path.unwrap().ends_with("styles/input.css"));

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn get_output_path_errors_without_build_config() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        let result = project.get_output_path().await;
        assert!(
            result.is_err(),
            "get_output_path should fail without build config"
        );

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }
}
