use std::path::{Component, Path, PathBuf};

use crate::root_contained_file_path::RootContainedFilePath;
use crate::root_relative_path::{RootRelativePath, RootRelativePathError};

#[derive(Debug, thiserror::Error)]
pub enum ProjectRootError {
    #[error("Path {path:?} is not inside the project at {root:?}")]
    OutsideProject { path: PathBuf, root: PathBuf },

    #[error("Path {path:?} is not valid UTF-8")]
    NotUtf8 { path: PathBuf },

    #[error("Path {path:?} cannot name a project file")]
    InvalidPath {
        path: PathBuf,
        #[source]
        source: RootRelativePathError,
    },
}

/// The absolute directory that a project's paths are resolved against.
#[derive(Debug, Clone)]
pub struct ProjectRoot {
    path: PathBuf,
}

impl ProjectRoot {
    /// Construct a project root from an absolute directory path.
    ///
    /// # Panics
    ///
    /// If `path` is relative.
    pub fn new(path: &Path) -> ProjectRoot {
        assert!(
            path.is_absolute(),
            "project root must be an absolute path, got {path:?}"
        );
        ProjectRoot {
            path: normalize(path),
        }
    }

    /// The absolute directory path this root was constructed from.
    pub fn as_path(&self) -> &Path {
        &self.path
    }

    /// The [`RootContainedFilePath`] of the project's `hop.toml` file.
    pub fn config(&self) -> RootContainedFilePath {
        RootContainedFilePath::new("hop.toml").expect("hop.toml is a valid project file path")
    }

    /// Convert an absolute file path to a [`RootContainedFilePath`].
    pub fn relativize(&self, path: &Path) -> Result<RootContainedFilePath, ProjectRootError> {
        let normalized = normalize(path);
        let relative_path =
            normalized
                .strip_prefix(&self.path)
                .map_err(|_| ProjectRootError::OutsideProject {
                    path: path.to_path_buf(),
                    root: self.path.clone(),
                })?;

        let components = relative_path
            .components()
            .map(|component| component.as_os_str().to_str())
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| ProjectRootError::NotUtf8 {
                path: path.to_path_buf(),
            })?;

        RootContainedFilePath::new(&components.join("/")).map_err(|source| {
            ProjectRootError::InvalidPath {
                path: path.to_path_buf(),
                source,
            }
        })
    }

    /// Convert a [`RootRelativePath`], or a path type that wraps one, to an
    /// absolute file path.
    ///
    /// Leading `..` components in the path are folded into the root, so the
    /// result may lie outside the project root.
    pub fn resolve(&self, path: impl AsRef<RootRelativePath>) -> PathBuf {
        normalize(&self.path.join(path.as_ref().as_str()))
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
    #[should_panic(expected = "project root must be an absolute path")]
    fn new_rejects_a_relative_path() {
        ProjectRoot::new(Path::new("projects/app"));
    }

    #[test]
    fn new_folds_relative_components() {
        assert_eq!(
            ProjectRoot::new(Path::new("/projects/./tmp/../app")).as_path(),
            Path::new("/projects/app")
        );
    }

    #[test]
    fn relativize_folds_relative_components() {
        let document_id = root()
            .relativize(Path::new("/projects/app/src/../main.hop"))
            .unwrap();
        assert_eq!(document_id.as_str(), "main.hop");
    }

    #[test]
    fn relativize_does_not_escape_the_root() {
        let result = root().relativize(Path::new("/projects/app/../other/main.hop"));
        assert!(
            matches!(result, Err(ProjectRootError::OutsideProject { .. })),
            "Expected OutsideProject error, got: {:?}",
            result
        );
    }

    #[test]
    fn relativize() {
        let document_id = root()
            .relativize(Path::new("/projects/app/src/components/button.hop"))
            .unwrap();
        assert_eq!(document_id.as_str(), "src/components/button.hop");

        let document_id = root()
            .relativize(Path::new("/projects/app/main.hop"))
            .unwrap();
        assert_eq!(document_id.as_str(), "main.hop");
    }

    #[test]
    fn relativize_outside_project() {
        let result = root().relativize(Path::new("/some/other/path/file.hop"));
        assert!(
            matches!(result, Err(ProjectRootError::OutsideProject { .. })),
            "Expected OutsideProject error, got: {:?}",
            result
        );
    }

    #[test]
    fn relativize_invalid_name() {
        let result = root().relativize(Path::new("/projects/app"));
        assert!(
            matches!(
                result,
                Err(ProjectRootError::InvalidPath {
                    source: RootRelativePathError::Empty,
                    ..
                })
            ),
            "Expected InvalidPath error, got: {:?}",
            result
        );
    }

    #[test]
    #[cfg(unix)]
    fn relativize_rejects_non_utf8() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        let path = Path::new(OsStr::from_bytes(b"/projects/app/caf\xE9.hop"));
        let result = root().relativize(path);
        assert!(
            matches!(result, Err(ProjectRootError::NotUtf8 { .. })),
            "Expected NotUtf8 error, got: {:?}",
            result
        );
    }

    #[test]
    fn config() {
        assert_eq!(
            root().resolve(root().config()),
            PathBuf::from("/projects/app/hop.toml")
        );
    }

    #[test]
    fn resolve() {
        let inside = RootRelativePath::from_root_anchored("/icons/star.svg").unwrap();
        assert_eq!(
            root().resolve(&inside),
            PathBuf::from("/projects/app/icons/star.svg")
        );

        let outside = RootRelativePath::from_root_anchored("/../shared/logo.svg").unwrap();
        assert_eq!(
            root().resolve(&outside),
            PathBuf::from("/projects/shared/logo.svg")
        );

        let output_dir = RootRelativePath::new("dist/public").unwrap();
        assert_eq!(
            root().resolve(&output_dir),
            PathBuf::from("/projects/app/dist/public")
        );

        let sibling = RootRelativePath::new("../assets").unwrap();
        assert_eq!(root().resolve(&sibling), PathBuf::from("/projects/assets"));
    }
}
