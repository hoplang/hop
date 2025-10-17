use anyhow::Context;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use tokio::fs as async_fs;

use super::config::HopConfig;
use crate::hop::module_name::ModuleName;

/// Check if a directory should be skipped during .hop file search
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

#[derive(Debug, Clone, PartialEq)]
pub struct ProjectRoot {
    // Directory containing the hop.toml file
    directory: PathBuf,
}

impl ProjectRoot {
    /// Find the project root by traversing upwards.
    pub fn find_upwards(start_path: &Path) -> anyhow::Result<ProjectRoot> {
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
                return Ok(ProjectRoot {
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
    pub fn from(path: &Path) -> anyhow::Result<ProjectRoot> {
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
        Ok(ProjectRoot {
            directory: canonicalized.to_path_buf(),
        })
    }

    pub fn get_path(&self) -> &Path {
        &self.directory
    }

    /// Convert a file path to a module name using this project root as reference
    /// Returns a module name with '/' separators (e.g., "src/components/header")
    pub fn path_to_module_name(&self, file_path: &Path) -> anyhow::Result<ModuleName> {
        let relative_path = file_path
            .strip_prefix(&self.directory)
            .with_context(|| format!("Failed to strip prefix from path {:?}", file_path))?;

        let module_str = relative_path
            .with_extension("")
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, "/");

        ModuleName::new(module_str)
            .map_err(|e| anyhow::anyhow!("Invalid module name for path {:?}: {}", file_path, e))
    }

    /// Convert a module name with '/' separators back to a file path
    /// (e.g., "src/components/header" -> "src/components/header.hop")
    pub fn module_name_to_path(&self, module_name: &ModuleName) -> PathBuf {
        let module_path = module_name
            .to_string()
            .replace('/', std::path::MAIN_SEPARATOR_STR);
        self.directory.join(format!("{}.hop", module_path))
    }

    /// Load all hop modules from this project root, returning a HashMap of module_name -> content
    pub fn load_all_hop_modules(&self) -> anyhow::Result<HashMap<ModuleName, String>> {
        let all_hop_files = self.find_hop_files()?;
        let mut modules = HashMap::new();

        for path in all_hop_files {
            let module_name = self.path_to_module_name(&path)?;
            let content = std::fs::read_to_string(&path)
                .with_context(|| format!("Failed to read file {:?}", path))?;
            modules.insert(module_name, content);
        }

        Ok(modules)
    }

    /// Recursively find all .hop files in this project root
    fn find_hop_files(&self) -> anyhow::Result<Vec<PathBuf>> {
        let mut hop_files = Vec::new();

        if !self.directory.exists() || !self.directory.is_dir() {
            return Ok(hop_files);
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
                hop_files.push(path.to_path_buf());
            }
        }

        Ok(hop_files)
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
        let (_, target_config) = config.get_target();
        Ok(self.directory.join(target_config.output()))
    }

    pub async fn write_output(&self, data: &str) -> anyhow::Result<()> {
        let path = self.get_output_path().await?;
        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            async_fs::create_dir_all(parent).await?;
        }
        async_fs::write(path, data).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::archive::temp_dir_from_archive;
    use indoc::indoc;
    use simple_txtar::Archive;

    #[test]
    fn test_find_config_file() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
            -- src/components/.gitkeep --

        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();

        // Test finding from nested directory
        let nested_dir = temp_dir.join("src").join("components");
        let found = ProjectRoot::find_upwards(&nested_dir).unwrap();
        assert_eq!(found.directory, temp_dir);

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_find_config_file_not_found() {
        let archive = Archive::from(indoc! {r#"
            -- src/components/test.hop --
            <test-comp>Hello</test-comp>
            -- src/main.rs --
            fn main() {}
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();

        // Test that find_upwards fails when no hop.toml exists
        let nested_dir = temp_dir.join("src").join("components");
        let result = ProjectRoot::find_upwards(&nested_dir);
        assert!(result.is_err());

        let error_message = result.unwrap_err().to_string();
        assert!(error_message.contains("Failed to locate hop.toml"));

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_path_to_module_name() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
            -- main.hop --
            <main-component>Test</main-component>
            -- src/components/button.hop --
            <button-comp>Click</button-comp>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        // Test converting file paths to module names
        let button_path = temp_dir.join("src/components/button.hop");
        let module_name = root.path_to_module_name(&button_path).unwrap();
        assert_eq!(module_name.as_str(), "src/components/button");

        let main_path = temp_dir.join("main.hop");
        let main_module = root.path_to_module_name(&main_path).unwrap();
        assert_eq!(main_module.as_str(), "main");

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_module_name_to_path() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        // Test converting module names back to paths
        let module = ModuleName::new("src/components/button".to_string()).unwrap();
        let path = root.module_name_to_path(&module);
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
    fn test_load_all_hop_modules() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
            -- src/main.hop --
            <main-comp>
              <button-comp />
              <header-comp />
            </main-comp>
            -- src/components/button.hop --
            <button-comp>Click me!</button-comp>
            -- src/components/header.hop --
            <header-comp>Welcome</header-comp>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        let modules = root.load_all_hop_modules().unwrap();

        // Should load exactly 3 modules
        assert_eq!(modules.len(), 3);

        // Check that specific modules are loaded with correct content
        assert!(modules.contains_key(&ModuleName::new("src/main".to_string()).unwrap()));
        assert!(
            modules.contains_key(&ModuleName::new("src/components/button".to_string()).unwrap())
        );
        assert!(
            modules.contains_key(&ModuleName::new("src/components/header".to_string()).unwrap())
        );

        // Verify content of one module
        let button_content = modules
            .get(&ModuleName::new("src/components/button".to_string()).unwrap())
            .unwrap();
        assert!(button_content.contains("<button-comp>Click me!</button-comp>"));

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_files_with_dots_in_names_are_rejected() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
            -- src/foo.bar.hop --
            <foo-bar-comp>Component with dots</foo-bar-comp>
            -- src/utils/helper_test.hop --
            <test-helper>Test helper</test-helper>
            -- src/components/button_min.hop --
            <button-min>Minified button</button-min>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        // Files with dots in the name (excluding .hop extension) should fail validation
        let foo_bar_path = temp_dir.join("src/foo.bar.hop");
        let result = root.path_to_module_name(&foo_bar_path);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Module name contains invalid character: '.'")
        );

        // Files with underscores should work fine
        let helper_test_path = temp_dir.join("src/utils/helper_test.hop");
        let helper_test_module = root.path_to_module_name(&helper_test_path).unwrap();
        assert_eq!(helper_test_module.as_str(), "src/utils/helper_test");

        let button_min_path = temp_dir.join("src/components/button_min.hop");
        let button_min_module = root.path_to_module_name(&button_min_path).unwrap();
        assert_eq!(button_min_module.as_str(), "src/components/button_min");

        // load_all_hop_modules should skip files that produce invalid module names
        let result = root.load_all_hop_modules();
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
    fn test_skip_directories() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
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
        let root = ProjectRoot::from(&temp_dir).unwrap();

        // Test that load_all_hop_modules correctly skips certain directories
        let modules = root.load_all_hop_modules().unwrap();

        // Should only load 1 module (from src/main.hop)
        assert_eq!(modules.len(), 1);

        // Check which modules were loaded
        assert!(modules.contains_key(&ModuleName::new("src/main".to_string()).unwrap()));

        // Should NOT contain modules from skipped directories
        let module_names: Vec<String> = modules.keys().map(|m| m.as_str().to_string()).collect();
        assert!(!module_names.iter().any(|m| m.contains("node_modules")));
        assert!(!module_names.iter().any(|m| m.contains(".git")));
        assert!(!module_names.iter().any(|m| m.contains("target")));

        // Clean up
        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn test_load_css_config() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"

            [target.typescript]
            output = "app.ts"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        let config = root.load_config().await.unwrap();

        assert_eq!(config.css.mode, Some("tailwind4".to_string()));

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    async fn test_load_config_missing_hop_toml_error() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [css]
            mode = "tailwind4"
            -- main.hop --
            <main-component>Test</main-component>
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        // Delete the hop.toml to test the error case
        std::fs::remove_file(temp_dir.join("hop.toml")).unwrap();

        let result = root.load_config().await;
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
    async fn test_load_config_with_empty_hop_toml() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            # Empty config file
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::from(&temp_dir).unwrap();

        let result = root.load_config().await;
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Failed to parse hop.toml"));

        std::fs::remove_dir_all(&temp_dir).unwrap();
    }
}
