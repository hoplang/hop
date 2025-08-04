use anyhow::Context;
use std::path::{Path, PathBuf};

/// Find the build.hop file by walking up the directory tree from a given path
pub fn find_build_file(start_path: &Path) -> Option<PathBuf> {
    let mut current_dir = if start_path.is_file() {
        start_path.parent()?
    } else {
        start_path
    };

    loop {
        let build_file = current_dir.join("build.hop");
        if build_file.exists() {
            return Some(build_file);
        }

        current_dir = current_dir.parent()?;
    }
}

/// Recursively find all .hop files in a directory
pub fn find_hop_files(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut hop_files = Vec::new();

    if !dir.exists() || !dir.is_dir() {
        return Ok(hop_files);
    }

    let entries =
        std::fs::read_dir(dir).with_context(|| format!("Failed to read directory {:?}", dir))?;

    for entry in entries {
        let path = entry.context("Failed to read directory entry")?.path();

        if path.is_dir() {
            hop_files.extend(find_hop_files(&path)?);
        } else if path.extension().and_then(|s| s.to_str()) == Some("hop") {
            hop_files.push(path);
        }
    }
    Ok(hop_files)
}

/// Convert a file path to a module name using the base directory as reference
/// Returns a module name with '/' separators (e.g., "src/components/header")
pub fn path_to_module_name(file_path: &Path, base_dir: &Path) -> anyhow::Result<String> {
    let relative_path = file_path
        .strip_prefix(base_dir)
        .with_context(|| format!("Failed to strip prefix from path {:?}", file_path))?;

    Ok(relative_path
        .with_extension("")
        .to_string_lossy()
        .replace(std::path::MAIN_SEPARATOR, "/"))
}

/// Convert a module name with '/' separators back to a file path
/// (e.g., "src/components/header" -> "src/components/header.hop")
pub fn module_name_to_path(module_name: &str, base_dir: &Path) -> PathBuf {
    let module_path = module_name.replace('/', std::path::MAIN_SEPARATOR_STR);
    base_dir.join(format!("{}.hop", module_path))
}

/// Load all hop modules from a base directory, returning (module_name, content) pairs
pub fn load_all_hop_modules(base_dir: &Path) -> anyhow::Result<Vec<(String, String)>> {
    let all_hop_files = find_hop_files(base_dir)?;
    let mut modules = Vec::new();

    for path in all_hop_files {
        let module_name = path_to_module_name(&path, base_dir)?;
        let content = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read file {:?}", path))?;
        modules.push((module_name, content));
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
        let found = find_build_file(&nested_dir).unwrap();
        assert_eq!(found, build_file);

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[test]
    fn test_path_to_module_name() {
        let base_dir = Path::new("/project");
        let file_path = Path::new("/project/src/components/header.hop");

        let module_name = path_to_module_name(file_path, base_dir).unwrap();
        assert_eq!(module_name, "src/components/header");
    }

    #[test]
    fn test_module_name_to_path() {
        let base_dir = Path::new("/project");
        let module_name = "src/components/header";

        let path = module_name_to_path(module_name, base_dir);
        assert_eq!(path, Path::new("/project/src/components/header.hop"));
    }
}
