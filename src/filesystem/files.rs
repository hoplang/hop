use anyhow::Context;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, PartialEq)]
pub struct ProjectRoot(PathBuf);

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
            let build_file = current_dir.join("build.hop");
            if build_file.exists() {
                return Ok(ProjectRoot(current_dir.to_path_buf()));
            }
            current_dir = current_dir.parent().ok_or_else(|| {
                anyhow::anyhow!(
                    "Failed to locate build.hop file in {:?} or any parent directory",
                    &start_path
                )
            })?
        }
    }
    /// Construct the project root from a path. The path should be a directory and contain the
    /// build file.
    pub fn from(path: &Path) -> anyhow::Result<ProjectRoot> {
        if !path.is_dir() {
            anyhow::bail!("{:?} is not a directory", &path)
        }
        let canonicalized = path
            .canonicalize()
            .with_context(|| format!("Failed to canonicalize path {:?}", &path))?;
        let build_file = canonicalized.join("build.hop");
        if !build_file.exists() {
            anyhow::bail!("Expected to find build.hop in {:?}", &path)
        }
        Ok(ProjectRoot(canonicalized.to_path_buf()))
    }

    pub fn get_path(&self) -> &Path {
        &self.0
    }
}

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

/// Recursively find all .hop files in a directory
pub fn find_hop_files(root: &ProjectRoot) -> anyhow::Result<Vec<PathBuf>> {
    let mut hop_files = Vec::new();

    let ProjectRoot(dir) = root;

    if !dir.exists() || !dir.is_dir() {
        return Ok(hop_files);
    }

    let mut paths: Vec<PathBuf> = Vec::new();
    paths.push(dir.to_path_buf());

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

/// Convert a file path to a module name using the base directory as reference
/// Returns a module name with '/' separators (e.g., "src/components/header")
pub fn path_to_module_name(file_path: &Path, root: &ProjectRoot) -> anyhow::Result<String> {
    let ProjectRoot(dir) = root;
    let relative_path = file_path
        .strip_prefix(dir)
        .with_context(|| format!("Failed to strip prefix from path {:?}", file_path))?;

    Ok(relative_path
        .with_extension("")
        .to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR, "/"))
}

/// Convert a module name with '/' separators back to a file path
/// (e.g., "src/components/header" -> "src/components/header.hop")
pub fn module_name_to_path(module_name: &str, root: &ProjectRoot) -> PathBuf {
    let ProjectRoot(base) = root;
    let module_path = module_name.replace('/', std::path::MAIN_SEPARATOR_STR);
    base.join(format!("{}.hop", module_path))
}

/// Load all hop modules from a base directory, returning a HashMap of module_name -> content
pub fn load_all_hop_modules(base_dir: &ProjectRoot) -> anyhow::Result<HashMap<String, String>> {
    let all_hop_files = find_hop_files(base_dir)?;
    let mut modules = HashMap::new();

    for path in all_hop_files {
        let module_name = path_to_module_name(&path, base_dir)?;
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read file {:?}", path))?;
        modules.insert(module_name, content);
    }

    Ok(modules)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;

    fn create_test_dir() -> std::io::Result<PathBuf> {
        let r = rand::random::<u64>();
        let temp_dir = env::temp_dir().join(format!("files_test_{}", r));
        fs::create_dir_all(&temp_dir)?;
        Ok(temp_dir)
    }

    #[test]
    fn test_find_build_file() {
        let temp_dir = create_test_dir().unwrap();

        // Create nested directory structure
        let nested_dir = temp_dir.join("src").join("components");
        fs::create_dir_all(&nested_dir).unwrap();

        // Create build.hop in root
        let build_file = temp_dir.join("build.hop");
        fs::write(&build_file, "test").unwrap();

        // Test finding from nested directory
        let found = ProjectRoot::find_upwards(&nested_dir).unwrap();
        assert_eq!(found.0, temp_dir);

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_find_build_file_not_found() {
        let temp_dir = create_test_dir().unwrap();

        // Create nested directory structure without build.hop
        let nested_dir = temp_dir.join("src").join("components");
        fs::create_dir_all(&nested_dir).unwrap();

        // Test that find_upwards fails when no build.hop exists
        let result = ProjectRoot::find_upwards(&nested_dir);
        assert!(result.is_err());

        let error_message = result.unwrap_err().to_string();
        assert!(error_message.contains("Failed to locate build.hop"));

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
