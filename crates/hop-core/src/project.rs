//! Represents a hop project and provides methods for working with it.
//!
//! A [`Project`] is anchored to a directory containing a `hop.toml` configuration
//! file. It provides functionality for:
//!
//! - Converting between file paths and [`ModuleId`]
//! - Loading modules and configuration

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::config::Config;
use crate::document::Document;
use crate::document_id::{DocumentId, DocumentIdError};

#[derive(Debug, thiserror::Error)]
pub enum ProjectError {
    #[error("Path {path:?} is not inside the project at {root:?}")]
    OutsideProject { path: PathBuf, root: PathBuf },

    #[error("Invalid document id for path {path:?}: {source}")]
    InvalidId {
        path: PathBuf,
        #[source]
        source: DocumentIdError,
    },

    #[error("{path:?} is not a directory")]
    NotADirectory { path: PathBuf },

    #[error("Failed to locate hop.toml starting from {path:?}")]
    ConfigNotFound { path: PathBuf },

    #[error("IO error on {path:?}")]
    Io {
        path: PathBuf,
        #[source]
        source: io::Error,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Project {
    // Directory containing the hop.toml file
    project_root: PathBuf,
}

impl Project {
    /// Construct the project from a path.
    ///
    /// The path should be a directory and contain the config file.
    pub fn from(path: &Path) -> Result<Project, ProjectError> {
        if !path.is_dir() {
            return Err(ProjectError::NotADirectory {
                path: path.to_path_buf(),
            });
        }
        let canonicalized = canonicalize(path)?;
        let config_file = canonicalized.join("hop.toml");
        if !config_file.exists() {
            return Err(ProjectError::ConfigNotFound {
                path: path.to_path_buf(),
            });
        }
        Ok(Project {
            project_root: canonicalized,
        })
    }

    /// Find the project root by traversing into superdirectories.
    pub fn find_traversing_superdirectories(start_path: &Path) -> Result<Project, ProjectError> {
        let canonicalized = canonicalize(start_path)?;
        let mut current_dir = if canonicalized.is_file() {
            canonicalized
                .parent()
                .ok_or_else(|| ProjectError::ConfigNotFound {
                    path: start_path.to_path_buf(),
                })?
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
            current_dir = current_dir
                .parent()
                .ok_or_else(|| ProjectError::ConfigNotFound {
                    path: start_path.to_path_buf(),
                })?;
        }
    }

    /// Find the project root by traversing into subdirectories.
    pub fn find_traversing_subdirectories(start_path: &Path) -> Result<Project, ProjectError> {
        let canonicalized = canonicalize(start_path)?;

        let mut paths: Vec<PathBuf> = vec![canonicalized];

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

        Err(ProjectError::ConfigNotFound {
            path: start_path.to_path_buf(),
        })
    }

    pub fn project_root(&self) -> &Path {
        &self.project_root
    }

    /// Convert a file path to a [`DocumentId`] using this project root as reference.
    pub fn path_to_document_id(&self, file_path: &Path) -> Result<DocumentId, ProjectError> {
        let canonical = file_path
            .canonicalize()
            .unwrap_or_else(|_| file_path.to_path_buf());
        let relative_path = canonical.strip_prefix(&self.project_root).map_err(|_| {
            ProjectError::OutsideProject {
                path: file_path.to_path_buf(),
                root: self.project_root.clone(),
            }
        })?;

        DocumentId::new(&relative_path.to_string_lossy()).map_err(|source| {
            ProjectError::InvalidId {
                path: file_path.to_path_buf(),
                source,
            }
        })
    }

    /// Convert a ModuleId back to a file path
    pub fn document_id_to_path(&self, document_id: &DocumentId) -> PathBuf {
        self.project_root.join(document_id.as_str())
    }

    /// Load a single document from its module ID
    pub fn load_document(&self, document_id: &DocumentId) -> Result<Document, ProjectError> {
        let path = self.document_id_to_path(document_id);
        let content =
            fs::read_to_string(&path).map_err(|source| ProjectError::Io { path, source })?;
        Ok(Document::new(document_id.clone(), content))
    }

    /// Find every hop module and CSS document in this project.
    pub fn documents(&self) -> Result<Vec<DocumentId>, ProjectError> {
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

                let entries = fs::read_dir(&path).map_err(|source| ProjectError::Io {
                    path: path.clone(),
                    source,
                })?;
                for entry in entries {
                    let p = entry
                        .map_err(|source| ProjectError::Io {
                            path: path.clone(),
                            source,
                        })?
                        .path();
                    paths.push(p);
                }
            } else if matches!(
                path.extension().and_then(|s| s.to_str()),
                Some("hop") | Some("css")
            ) {
                document_ids.push(self.path_to_document_id(&path)?);
            }
        }

        Ok(document_ids)
    }

    /// Load the hop.toml configuration file from this project root.
    pub fn load_config(&self) -> Result<Config, ProjectError> {
        let config_path = self.project_root.join("hop.toml");
        let config_str = fs::read_to_string(&config_path).map_err(|source| ProjectError::Io {
            path: config_path,
            source,
        })?;
        let document_id = DocumentId::new("hop.toml").expect("hop.toml is a valid document id");
        Ok(Config::new(Document::new(document_id, config_str)))
    }
}

fn canonicalize(path: &Path) -> Result<PathBuf, ProjectError> {
    path.canonicalize().map_err(|source| ProjectError::Io {
        path: path.to_path_buf(),
        source,
    })
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
        assert!(
            matches!(result, Err(ProjectError::ConfigNotFound { .. })),
            "Expected ConfigNotFound error, got: {:?}",
            result
        );
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

        assert!(
            matches!(result, Err(ProjectError::OutsideProject { .. })),
            "Expected OutsideProject error, got: {:?}",
            result
        );
    }

    #[test]
    fn path_to_document_id_invalid_name() {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        // A path inside the project whose name is not a valid document id
        let path = project.project_root().join("my component.hop");
        let result = project.path_to_document_id(&path);

        assert!(
            matches!(
                result,
                Err(ProjectError::InvalidId {
                    source: DocumentIdError::InvalidCharacter(' '),
                    ..
                })
            ),
            "Expected InvalidId error, got: {:?}",
            result
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
    fn find_documents() {
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
            -- src/styles.css --
            body { margin: 0; }
            -- README.md --
            Not a document.
        "#});
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();

        let mut documents = project.documents().unwrap();
        documents.sort();

        assert_eq!(
            documents,
            [
                "src/components/button.hop",
                "src/components/header.hop",
                "src/main.hop",
                "src/styles.css",
            ]
            .map(|id| DocumentId::new(id).unwrap())
        );
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

        // Test that documents correctly skips certain directories
        let modules = project.documents().unwrap();

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
        assert!(
            matches!(
                result,
                Err(ProjectError::Io { ref source, .. })
                    if source.kind() == io::ErrorKind::NotFound
            ),
            "Expected Io/NotFound error, got: {:?}",
            result
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
}
