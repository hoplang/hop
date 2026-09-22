use std::path::{Component, Path, PathBuf};

use crate::asset_path::AssetPath;
use crate::document_id::{DocumentId, DocumentIdError};

#[derive(Debug, thiserror::Error)]
pub enum ProjectRootError {
    #[error("Path {path:?} is not inside the project at {root:?}")]
    OutsideProject { path: PathBuf, root: PathBuf },

    #[error("Invalid document id for path {path:?}: {source}")]
    InvalidId {
        path: PathBuf,
        #[source]
        source: DocumentIdError,
    },
}

/// The directory that contains a project's `hop.toml` file.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProjectRoot {
    path: PathBuf,
}

impl ProjectRoot {
    /// Wrap an already located root directory.
    ///
    /// The `path` is expected to be absolute.
    /// Components equal to `.` and `..` are folded without touching the filesystem.
    pub fn new(path: &Path) -> ProjectRoot {
        ProjectRoot {
            path: normalize(path),
        }
    }

    pub fn as_path(&self) -> &Path {
        &self.path
    }

    /// The [`DocumentId`] of the project's `hop.toml` file.
    pub fn config(&self) -> DocumentId {
        DocumentId::new("hop.toml").expect("hop.toml is a valid document id")
    }

    /// The path of the project's `hop.toml` file.
    pub fn config_path(&self) -> PathBuf {
        self.document_id_to_path(&self.config())
    }

    /// Convert an absolute file path to a [`DocumentId`].
    pub fn path_to_document_id(&self, path: &Path) -> Result<DocumentId, ProjectRootError> {
        let normalized = normalize(path);
        let relative_path =
            normalized
                .strip_prefix(&self.path)
                .map_err(|_| ProjectRootError::OutsideProject {
                    path: path.to_path_buf(),
                    root: self.path.clone(),
                })?;

        DocumentId::new(&relative_path.to_string_lossy()).map_err(|source| {
            ProjectRootError::InvalidId {
                path: path.to_path_buf(),
                source,
            }
        })
    }

    pub fn document_id_to_path(&self, document_id: &DocumentId) -> PathBuf {
        self.path.join(document_id.as_str())
    }

    /// Convert an [`AssetPath`] to an absolute file path.
    ///
    /// Leading `..` components in the asset path are folded into the root,
    /// so the result may lie outside the project.
    pub fn asset_path_to_path(&self, asset_path: &AssetPath) -> PathBuf {
        normalize(&self.path.join(asset_path.as_str()))
    }
}

/// Fold `.` and `..` components without touching the filesystem.
///
/// A `..` directly under the root is dropped, as the OS would do.
fn normalize(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                if !matches!(
                    normalized.components().next_back(),
                    None | Some(Component::RootDir) | Some(Component::Prefix(_))
                ) {
                    normalized.pop();
                }
            }
            other => normalized.push(other),
        }
    }
    normalized
}

#[cfg(test)]
mod tests {
    use super::*;

    fn root() -> ProjectRoot {
        ProjectRoot::new(Path::new("/projects/app"))
    }

    #[test]
    fn new_folds_relative_components() {
        assert_eq!(
            ProjectRoot::new(Path::new("/projects/./tmp/../app")).as_path(),
            Path::new("/projects/app")
        );
    }

    #[test]
    fn path_to_document_id_folds_relative_components() {
        let document_id = root()
            .path_to_document_id(Path::new("/projects/app/src/../main.hop"))
            .unwrap();
        assert_eq!(document_id.as_str(), "main.hop");
    }

    #[test]
    fn path_to_document_id_does_not_escape_the_root() {
        let result = root().path_to_document_id(Path::new("/projects/app/../other/main.hop"));
        assert!(
            matches!(result, Err(ProjectRootError::OutsideProject { .. })),
            "Expected OutsideProject error, got: {:?}",
            result
        );
    }

    #[test]
    fn path_to_document_id() {
        let document_id = root()
            .path_to_document_id(Path::new("/projects/app/src/components/button.hop"))
            .unwrap();
        assert_eq!(document_id.as_str(), "src/components/button.hop");

        let document_id = root()
            .path_to_document_id(Path::new("/projects/app/main.hop"))
            .unwrap();
        assert_eq!(document_id.as_str(), "main.hop");
    }

    #[test]
    fn path_to_document_id_outside_project() {
        let result = root().path_to_document_id(Path::new("/some/other/path/file.hop"));
        assert!(
            matches!(result, Err(ProjectRootError::OutsideProject { .. })),
            "Expected OutsideProject error, got: {:?}",
            result
        );
    }

    #[test]
    fn path_to_document_id_invalid_name() {
        let result = root().path_to_document_id(Path::new("/projects/app/my component.hop"));
        assert!(
            matches!(
                result,
                Err(ProjectRootError::InvalidId {
                    source: DocumentIdError::InvalidCharacter(' '),
                    ..
                })
            ),
            "Expected InvalidId error, got: {:?}",
            result
        );
    }

    #[test]
    fn config_path() {
        assert_eq!(
            root().config_path(),
            PathBuf::from("/projects/app/hop.toml")
        );
    }

    #[test]
    fn document_id_to_path() {
        let document_id = DocumentId::new("src/components/button.hop").unwrap();
        assert_eq!(
            root().document_id_to_path(&document_id),
            PathBuf::from("/projects/app/src/components/button.hop")
        );
    }

    #[test]
    fn asset_path_to_path() {
        let inside = AssetPath::new("/icons/star.svg").unwrap();
        assert_eq!(
            root().asset_path_to_path(&inside),
            PathBuf::from("/projects/app/icons/star.svg")
        );

        let outside = AssetPath::new("/../shared/logo.svg").unwrap();
        assert_eq!(
            root().asset_path_to_path(&outside),
            PathBuf::from("/projects/shared/logo.svg")
        );
    }
}
