use std::fmt;
use std::sync::Arc;
use thiserror::Error;

/// The file path of an asset file, relative to the [ProjectRoot](crate::ProjectRoot).
///
/// Unlike a [DocumentId](crate::DocumentId), an [AssetPath] may point outside
/// the project root via leading `..` components (e.g. `../shared/logo.svg`).
///
/// The path is always normalized: it contains no `.` components, and `..`
/// components only appear at the start.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AssetPath(Arc<str>);

impl AssetPath {
    /// Parse the path written in an `asset!("...")` or `--asset("...")` call.
    ///
    /// The path must start with `/`, which denotes the project root, so
    /// `/icons/star.svg` resolves to `icons/star.svg` and `/../shared/logo.svg`
    /// to `../shared/logo.svg`.
    pub(crate) fn new(reference: &str) -> Result<Self, AssetPathError> {
        if reference.is_empty() {
            return Err(AssetPathError::Empty);
        }
        let Some(relative) = reference.strip_prefix('/') else {
            return Err(AssetPathError::MustBeAbsolute);
        };
        if reference.ends_with('/') {
            return Err(AssetPathError::EndsWithSeparator);
        }

        let mut components: Vec<&str> = Vec::new();

        for component in relative.split('/') {
            match component {
                "" => return Err(AssetPathError::EmptyComponent),
                "." => {}
                ".." => match components.last() {
                    Some(&"..") | None => components.push(".."),
                    Some(_) => {
                        components.pop();
                    }
                },
                _ => {
                    // Only reject what breaks path handling. Anything else
                    // (spaces, `#`, non-ASCII, ...) is allowed here and
                    // sanitized when the output filename is derived.
                    if let Some(c) = component.chars().find(|c| *c == '\\' || c.is_control()) {
                        return Err(AssetPathError::InvalidCharacter(c));
                    }
                    components.push(component);
                }
            }
        }

        match components.last() {
            Some(&"..") | None => Err(AssetPathError::NoFileName),
            Some(_) => Ok(AssetPath(Arc::from(components.join("/")))),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// The final component of the path, e.g. `logo.svg` for `../shared/logo.svg`.
    pub fn file_name(&self) -> &str {
        self.0.rsplit('/').next().unwrap_or(&self.0)
    }

    /// Whether the path escapes the project root via leading `..` components.
    pub fn is_outside_root(&self) -> bool {
        self.0.starts_with("../")
    }
}

impl fmt::Display for AssetPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Error type for invalid [AssetPaths](crate::AssetPath).
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum AssetPathError {
    #[error("asset path cannot be empty")]
    Empty,

    #[error("asset path must start with '/'")]
    MustBeAbsolute,

    #[error("asset path cannot end with '/'")]
    EndsWithSeparator,

    #[error("asset path contains an empty component")]
    EmptyComponent,

    #[error("asset path contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("asset path does not name a file")]
    NoFileName,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(reference: &str) -> Result<String, AssetPathError> {
        AssetPath::new(reference).map(|p| p.to_string())
    }

    fn accept(reference: &str, expected: &str) {
        assert_eq!(parse(reference).as_deref(), Ok(expected));
    }

    fn reject(reference: &str, expected: AssetPathError) {
        assert_eq!(parse(reference), Err(expected));
    }

    #[test]
    fn parses_root_absolute_paths() {
        accept("/logo.svg", "logo.svg");
        accept("/icons/star.svg", "icons/star.svg");
    }

    #[test]
    fn rejects_paths_without_a_leading_slash() {
        reject("logo.svg", AssetPathError::MustBeAbsolute);
        reject("./logo.svg", AssetPathError::MustBeAbsolute);
        reject("../shared/logo.svg", AssetPathError::MustBeAbsolute);
    }

    #[test]
    fn paths_may_escape_the_project_root() {
        accept("/../logo.svg", "../logo.svg");
        accept("/../shared/logo.svg", "../shared/logo.svg");
        accept("/../../logo.svg", "../../logo.svg");
        accept("/../a/../../b/x.png", "../../b/x.png");
    }

    #[test]
    fn accepts_spaces_and_other_url_unsafe_characters() {
        accept("/my logo.svg", "my logo.svg");
        accept("/fonts/Inter Variable.woff2", "fonts/Inter Variable.woff2");
        accept("/logo#1.svg", "logo#1.svg");
        accept("/ünicode.svg", "ünicode.svg");
    }

    #[test]
    fn dot_components_are_folded() {
        accept("/./icons/./star.svg", "icons/star.svg");
        accept("/icons/../logo.svg", "logo.svg");
    }

    #[test]
    fn rejects_malformed_paths() {
        reject("", AssetPathError::Empty);
        reject("/icons/", AssetPathError::EndsWithSeparator);
        reject("/icons//star.svg", AssetPathError::EmptyComponent);
        reject("/my\\logo.svg", AssetPathError::InvalidCharacter('\\'));
        reject("/my\nlogo.svg", AssetPathError::InvalidCharacter('\n'));
        reject("/", AssetPathError::EndsWithSeparator);
        reject("/..", AssetPathError::NoFileName);
        reject("/icons/..", AssetPathError::NoFileName);
    }

    #[test]
    fn file_name_and_outside_root() {
        let inside = AssetPath::new("/icons/star.svg").unwrap();
        assert_eq!(inside.file_name(), "star.svg");
        assert!(!inside.is_outside_root());

        let outside = AssetPath::new("/../shared/logo.svg").unwrap();
        assert_eq!(outside.file_name(), "logo.svg");
        assert!(outside.is_outside_root());
    }
}
