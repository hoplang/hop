use crate::root_relative_file_path::RootRelativeFilePath;
use crate::root_relative_path::{RootRelativePath, RootRelativePathError};

/// A [RootRelativeFilePath] that stays inside the
/// [ProjectRoot](crate::ProjectRoot).
///
/// Identifies a [Document](crate::Document).
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RootContainedFilePath(RootRelativeFilePath);

impl RootContainedFilePath {
    /// Parse a relative path that names a file inside the project root.
    ///
    /// `.` and `..` components are folded, so `src/../main.hop` becomes
    /// `main.hop`, while `../secrets.css` is rejected.
    pub(crate) fn new(path: &str) -> Result<Self, RootRelativePathError> {
        let path = RootRelativeFilePath::new(path)?;
        // Normalization leaves `..` components only at the start.
        if path.as_str().starts_with("../") {
            return Err(RootRelativePathError::EscapesRoot);
        }
        Ok(RootContainedFilePath(path))
    }

    /// See [RootRelativeFilePath::file_name].
    pub fn file_name(&self) -> &str {
        self.0.file_name()
    }

    /// See [RootRelativeFilePath::extension].
    pub fn extension(&self) -> Option<&str> {
        self.0.extension()
    }

    pub(crate) fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<RootRelativePath> for RootContainedFilePath {
    fn as_ref(&self) -> &RootRelativePath {
        self.0.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn accept(input: &str) {
        assert!(RootContainedFilePath::new(input).is_ok());
    }

    fn accept_as(input: &str, expected: &str) {
        assert_eq!(
            RootContainedFilePath::new(input).unwrap().as_str(),
            expected
        );
    }

    fn reject(input: &str, expected: RootRelativePathError) {
        assert_eq!(RootContainedFilePath::new(input), Err(expected));
    }

    #[test]
    fn accepts_simple_document_id() {
        accept("utils.hop");
    }

    #[test]
    fn accepts_document_id_with_path() {
        accept("components/button.hop");
        accept("hop/ui.hop");
    }

    #[test]
    fn accepts_document_id_with_hyphen() {
        accept("my-component.hop");
    }

    #[test]
    fn accepts_document_id_with_underscore() {
        accept("my_component.hop");
    }

    #[test]
    fn accepts_deeply_nested_document_id() {
        accept("a/b/c/d.hop");
    }

    #[test]
    fn rejects_empty_document_id() {
        reject("", RootRelativePathError::Empty);
    }

    #[test]
    fn rejects_document_id_starting_with_separator() {
        reject("/utils.hop", RootRelativePathError::MustBeRelative);
    }

    #[test]
    fn rejects_document_id_with_empty_component() {
        reject(
            "utils//components.hop",
            RootRelativePathError::EmptyComponent,
        );
    }

    #[test]
    fn accepts_document_id_with_space() {
        accept("my component.hop");
    }

    #[test]
    fn rejects_document_id_with_colon() {
        reject(
            "my::component.hop",
            RootRelativePathError::InvalidCharacter(':'),
        );
    }

    #[test]
    fn folds_parent_component() {
        accept_as("src/../main.hop", "main.hop");
    }

    #[test]
    fn folds_current_component() {
        accept_as("./main.hop", "main.hop");
        accept_as("src/./main.hop", "src/main.hop");
    }

    #[test]
    fn rejects_document_id_outside_the_root() {
        reject("../secrets.css", RootRelativePathError::EscapesRoot);
    }

    #[test]
    fn rejects_document_id_that_names_no_file() {
        reject("src/..", RootRelativePathError::NoFileName);
    }

    #[test]
    fn accepts_dotfiles_and_multiple_dots() {
        accept(".hidden.hop");
        accept("...hop");
        accept("a.b.c.hop");
    }
}
