use crate::root_relative_path::{RootRelativePath, RootRelativePathError};

/// A [RootRelativePath] that is guaranteed to name a file.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RootRelativeFilePath(RootRelativePath);

impl RootRelativeFilePath {
    /// Parse a plain relative path.
    ///
    /// The path must not start with `/`, so `app.ts` and `../generated/app.ts`
    /// are accepted while `/etc/hop/app.ts` is rejected.
    pub(crate) fn new(path: &str) -> Result<Self, RootRelativePathError> {
        Self::from_root_relative(RootRelativePath::new(path)?)
    }

    /// Parse a root-anchored path, as written in `asset!("/logo.svg")`.
    ///
    /// The path must start with `/`, which denotes the project root and is
    /// stripped, so `/icons/star.svg` resolves to `icons/star.svg` and
    /// `/../shared/logo.svg` to `../shared/logo.svg`.
    pub(crate) fn from_root_anchored(reference: &str) -> Result<Self, RootRelativePathError> {
        Self::from_root_relative(RootRelativePath::from_root_anchored(reference)?)
    }

    fn from_root_relative(path: RootRelativePath) -> Result<Self, RootRelativePathError> {
        if path.file_name().is_none() {
            return Err(RootRelativePathError::NoFileName);
        }
        Ok(RootRelativeFilePath(path))
    }

    /// The final component of the path, e.g. `logo.svg` for `../shared/logo.svg`.
    pub fn file_name(&self) -> &str {
        self.0
            .file_name()
            .expect("checked by from_root_relative at construction")
    }

    /// The part of the file name after its last `.`, e.g. `hop` for
    /// `components/button.hop`.
    pub fn extension(&self) -> Option<&str> {
        let (stem, extension) = self.file_name().rsplit_once('.')?;
        (!stem.is_empty()).then_some(extension)
    }

    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<RootRelativePath> for RootRelativeFilePath {
    fn as_ref(&self) -> &RootRelativePath {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn names_the_final_component() {
        let inside = RootRelativeFilePath::from_root_anchored("/icons/star.svg").unwrap();
        assert_eq!(inside.as_str(), "icons/star.svg");
        assert_eq!(inside.file_name(), "star.svg");

        let outside = RootRelativeFilePath::from_root_anchored("/../shared/logo.svg").unwrap();
        assert_eq!(outside.as_str(), "../shared/logo.svg");
        assert_eq!(outside.file_name(), "logo.svg");
    }

    #[test]
    fn rejects_paths_that_name_no_file() {
        for reference in ["/.", "/..", "/icons/..", "/../.."] {
            assert_eq!(
                RootRelativeFilePath::from_root_anchored(reference),
                Err(RootRelativePathError::NoFileName),
                "expected {reference} to name no file"
            );
        }
    }

    #[test]
    fn plain_paths_name_a_file() {
        let nested = RootRelativeFilePath::new("dist/app.ts").unwrap();
        assert_eq!(nested.as_str(), "dist/app.ts");
        assert_eq!(nested.file_name(), "app.ts");

        let above = RootRelativeFilePath::new("../generated/app.ts").unwrap();
        assert_eq!(above.file_name(), "app.ts");
    }

    #[test]
    fn plain_paths_that_name_no_file_are_rejected() {
        for path in [".", "..", "dist/..", "../.."] {
            assert_eq!(
                RootRelativeFilePath::new(path),
                Err(RootRelativePathError::NoFileName),
                "expected {path} to name no file"
            );
        }
        assert_eq!(
            RootRelativeFilePath::new("/etc/hop/app.ts"),
            Err(RootRelativePathError::MustBeRelative)
        );
    }

    #[test]
    fn underlying_parse_errors_propagate() {
        assert_eq!(
            RootRelativeFilePath::from_root_anchored(""),
            Err(RootRelativePathError::Empty)
        );
        assert_eq!(
            RootRelativeFilePath::from_root_anchored("logo.svg"),
            Err(RootRelativePathError::MustBeRootAnchored)
        );
        assert_eq!(
            RootRelativeFilePath::from_root_anchored("/icons/"),
            Err(RootRelativePathError::EndsWithSeparator)
        );
        assert_eq!(
            RootRelativeFilePath::from_root_anchored("/my\\logo.svg"),
            Err(RootRelativePathError::InvalidCharacter('\\'))
        );
    }
}
