//! Represents a hop project and provides methods for working with it.

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::config::Config;
use crate::document::Document;
use crate::document_id::DocumentId;
use crate::project_root::{ProjectRoot, ProjectRootError};

#[derive(Debug, thiserror::Error)]
pub enum ProjectError {
    #[error(transparent)]
    Root(#[from] ProjectRootError),

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

/// A hop project on disk.
#[derive(Debug, Clone, PartialEq)]
pub struct Project {
    root: ProjectRoot,
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
        let root = ProjectRoot::new(&absolute(path)?);
        if !root.as_path().join("hop.toml").exists() {
            return Err(ProjectError::ConfigNotFound {
                path: path.to_path_buf(),
            });
        }
        Ok(Project { root })
    }

    /// Find the project root by traversing into superdirectories.
    pub fn find_traversing_superdirectories(start_path: &Path) -> Result<Project, ProjectError> {
        let start = ProjectRoot::new(&absolute(start_path)?);
        let mut current_dir = if start.as_path().is_file() {
            start
                .as_path()
                .parent()
                .ok_or_else(|| ProjectError::ConfigNotFound {
                    path: start_path.to_path_buf(),
                })?
        } else {
            start.as_path()
        };

        loop {
            if current_dir.join("hop.toml").exists() {
                return Ok(Project {
                    root: ProjectRoot::new(current_dir),
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
        let mut paths: Vec<PathBuf> = vec![absolute(start_path)?];

        while let Some(path) = paths.pop() {
            if path.is_dir() {
                if let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) {
                    if should_skip_directory(dir_name) {
                        continue;
                    }
                }

                if path.join("hop.toml").exists() {
                    return Ok(Project {
                        root: ProjectRoot::new(&path),
                    });
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

    pub fn root(&self) -> &ProjectRoot {
        &self.root
    }

    pub fn load_document(&self, document_id: &DocumentId) -> Result<Document, ProjectError> {
        let path = self.root.document_id_to_path(document_id);
        let content =
            fs::read_to_string(&path).map_err(|source| ProjectError::Io { path, source })?;
        Ok(Document::new(document_id.clone(), content))
    }

    pub fn documents(&self) -> Result<Vec<DocumentId>, ProjectError> {
        let mut document_ids = Vec::new();

        let root = self.root.as_path();
        if !root.exists() || !root.is_dir() {
            return Ok(document_ids);
        }

        let mut paths: Vec<PathBuf> = vec![root.to_path_buf()];

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
                // Paths come from walking the root, which is absolute and
                // lexically normalized, so they share its prefix and need no
                // further resolution before being stripped to a document id.
                document_ids.push(self.root.path_to_document_id(&path)?);
            }
        }

        Ok(document_ids)
    }

    /// Load the hop.toml configuration file from this project root.
    pub fn load_config(&self) -> Result<Config, ProjectError> {
        let config_path = self.root.as_path().join("hop.toml");
        let config_str = fs::read_to_string(&config_path).map_err(|source| ProjectError::Io {
            path: config_path,
            source,
        })?;
        let document_id = DocumentId::new("hop.toml").expect("hop.toml is a valid document id");
        Ok(Config::new(Document::new(document_id, config_str)))
    }
}

/// Make `path` absolute by prepending the current directory. Purely lexical:
/// symlinks are left alone so the caller's spelling is preserved.
fn absolute(path: &Path) -> Result<PathBuf, ProjectError> {
    std::path::absolute(path).map_err(|source| ProjectError::Io {
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

    fn write(input: &str) -> TempDir {
        let archive = Archive::from(input);
        let temp_dir = TempDir::new().unwrap();
        write_archive_to_dir(&archive, temp_dir.path()).unwrap();
        temp_dir
    }

    fn project_from(input: &str) -> (TempDir, Project) {
        let temp_dir = write(input);
        let project = Project::from(temp_dir.path()).unwrap();
        (temp_dir, project)
    }

    #[test]
    fn find_config_file() {
        let temp_dir = write(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- src/components/.gitkeep --

        "#});

        // Test finding from nested directory
        let nested_dir = temp_dir.path().join("src").join("components");
        let found = Project::find_traversing_superdirectories(&nested_dir).unwrap();
        assert_eq!(found.root().as_path(), temp_dir.path());
    }

    #[test]
    fn find_config_file_downwards() {
        let temp_dir = write(indoc! {r#"
            -- hop/hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- hop/main.hop --
            type User {
                name: String
            }
        "#});

        // Test finding from parent directory
        let found = Project::find_traversing_subdirectories(temp_dir.path()).unwrap();
        assert_eq!(found.root().as_path(), temp_dir.path().join("hop"));
    }

    #[test]
    fn find_config_file_not_found() {
        let temp_dir = write(indoc! {r#"
            -- src/components/test.hop --
            <test-comp>Hello</test-comp>
            -- src/main.rs --
            fn main() {}
        "#});

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
    fn load_module() {
        let (_temp_dir, project) = project_from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- src/components/button.hop --
            <button-comp>Click me!</button-comp>
        "#});

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
        let (_temp_dir, project) = project_from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
        "#});

        let document_id = DocumentId::new("nonexistent/module.hop").unwrap();
        let result = project.load_document(&document_id);

        assert!(result.is_err());
    }

    #[test]
    fn find_documents() {
        let (_temp_dir, project) = project_from(indoc! {r#"
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
        let (_temp_dir, project) = project_from(indoc! {r#"
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
        let (temp_dir, project) = project_from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "ts"
            output_path = "app.ts"
            -- main.hop --
            <main-component>Test</main-component>
        "#});

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
        let (_temp_dir, project) = project_from(indoc! {r#"
            -- hop.toml --
            # Empty config file
        "#});

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
        let (_temp_dir, project) = project_from(indoc! {r#"
            -- hop.toml --
        "#});

        let result = project.load_config();
        assert!(
            result.is_ok(),
            "Config without build section should succeed: {:?}",
            result.err()
        );
    }
}
