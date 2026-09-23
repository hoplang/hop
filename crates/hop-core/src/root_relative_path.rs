use std::sync::Arc;
use thiserror::Error;

/// A path relative to the [ProjectRoot](crate::ProjectRoot), which may point
/// outside it via leading `..` components (e.g. `../shared/assets`).
///
/// The path is always normalized: `..` components only appear at the start,
/// and the only `.` component is the path `.` itself, which denotes the root.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RootRelativePath(Arc<str>);

impl RootRelativePath {
    /// Parse a plain relative path, as written in `hop.toml`.
    ///
    /// The path must not start with `/`, so `dist/public` and `../assets`
    /// are accepted while `/var/www` is rejected.
    pub(crate) fn new(path: &str) -> Result<Self, RootRelativePathError> {
        if path.is_empty() {
            return Err(RootRelativePathError::Empty);
        }
        if path.starts_with('/') {
            return Err(RootRelativePathError::MustBeRelative);
        }
        Self::normalize(path)
    }

    /// Parse a root-anchored path, as written in `asset!("/logo.svg")`.
    ///
    /// The path must start with `/`, which denotes the project root and is
    /// stripped, so `/icons/star.svg` resolves to `icons/star.svg` and
    /// `/../shared/logo.svg` to `../shared/logo.svg`.
    pub(crate) fn from_root_anchored(reference: &str) -> Result<Self, RootRelativePathError> {
        if reference.is_empty() {
            return Err(RootRelativePathError::Empty);
        }
        let Some(relative) = reference.strip_prefix('/') else {
            return Err(RootRelativePathError::MustBeRootAnchored);
        };
        if reference.ends_with('/') {
            return Err(RootRelativePathError::EndsWithSeparator);
        }
        Self::normalize(relative)
    }

    /// Fold `.` and `..` components of a non-empty path without a leading `/`.
    fn normalize(path: &str) -> Result<Self, RootRelativePathError> {
        if path.ends_with('/') {
            return Err(RootRelativePathError::EndsWithSeparator);
        }

        let mut components: Vec<&str> = Vec::new();

        for component in path.split('/') {
            match component {
                "" => return Err(RootRelativePathError::EmptyComponent),
                "." => {}
                ".." => match components.last() {
                    Some(&"..") | None => components.push(".."),
                    Some(_) => {
                        components.pop();
                    }
                },
                _ => {
                    // Only reject what breaks path handling: `\` is a
                    // separator on Windows, and `:` starts a drive prefix
                    // (`C:/secrets.css`) or names an alternate data stream
                    // (`logo.svg:x`) there. Anything else (spaces, `#`,
                    // non-ASCII, ...) is allowed here and sanitized when the
                    // output filename is derived.
                    if let Some(c) = component
                        .chars()
                        .find(|c| *c == '\\' || *c == ':' || c.is_control())
                    {
                        return Err(RootRelativePathError::InvalidCharacter(c));
                    }
                    components.push(component);
                }
            }
        }

        // `.` and `dist/..` fold to nothing: that is the root itself.
        if components.is_empty() {
            components.push(".");
        }

        Ok(RootRelativePath(Arc::from(components.join("/"))))
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }

    /// The final component of the path, e.g. `logo.svg` for `../shared/logo.svg`.
    ///
    /// `None` when the path names no file, i.e. it is `.`, `..` or `../..`.
    /// See [RootRelativeFilePath](crate::RootRelativeFilePath) for a path that
    /// always names one.
    pub(crate) fn file_name(&self) -> Option<&str> {
        self.0
            .rsplit('/')
            .next()
            .filter(|c| *c != "." && *c != "..")
    }
}

impl AsRef<RootRelativePath> for RootRelativePath {
    fn as_ref(&self) -> &RootRelativePath {
        self
    }
}

/// Error type for invalid [RootRelativePaths](crate::RootRelativePath).
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum RootRelativePathError {
    #[error("path cannot be empty")]
    Empty,

    #[error("path must not start with '/'")]
    MustBeRelative,

    #[error("path must start with '/'")]
    MustBeRootAnchored,

    #[error("path cannot end with '/'")]
    EndsWithSeparator,

    #[error("path contains an empty component")]
    EmptyComponent,

    #[error("path contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("path does not name a file")]
    NoFileName,

    #[error("path must not point outside the project root")]
    EscapesRoot,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_anchored(reference: &str) -> Result<String, RootRelativePathError> {
        RootRelativePath::from_root_anchored(reference).map(|p| p.as_str().to_string())
    }

    fn accept_anchored(reference: &str, expected: &str) {
        assert_eq!(parse_anchored(reference).as_deref(), Ok(expected));
    }

    fn reject_anchored(reference: &str, expected: RootRelativePathError) {
        assert_eq!(parse_anchored(reference), Err(expected));
    }

    fn parse(path: &str) -> Result<String, RootRelativePathError> {
        RootRelativePath::new(path).map(|p| p.as_str().to_string())
    }

    fn accept(path: &str, expected: &str) {
        assert_eq!(parse(path).as_deref(), Ok(expected));
    }

    fn reject(path: &str, expected: RootRelativePathError) {
        assert_eq!(parse(path), Err(expected));
    }

    #[test]
    fn parses_root_absolute_paths() {
        accept_anchored("/logo.svg", "logo.svg");
        accept_anchored("/icons/star.svg", "icons/star.svg");
    }

    #[test]
    fn rejects_paths_without_a_leading_slash() {
        reject_anchored("logo.svg", RootRelativePathError::MustBeRootAnchored);
        reject_anchored("./logo.svg", RootRelativePathError::MustBeRootAnchored);
        reject_anchored(
            "../shared/logo.svg",
            RootRelativePathError::MustBeRootAnchored,
        );
    }

    #[test]
    fn paths_may_escape_the_project_root() {
        accept_anchored("/../logo.svg", "../logo.svg");
        accept_anchored("/../shared/logo.svg", "../shared/logo.svg");
        accept_anchored("/../../logo.svg", "../../logo.svg");
        accept_anchored("/../a/../../b/x.png", "../../b/x.png");
        accept_anchored("/..", "..");
        accept_anchored("/../..", "../..");
    }

    #[test]
    fn accepts_spaces_and_other_url_unsafe_characters() {
        accept_anchored("/my logo.svg", "my logo.svg");
        accept_anchored("/fonts/Inter Variable.woff2", "fonts/Inter Variable.woff2");
        accept_anchored("/logo#1.svg", "logo#1.svg");
        accept_anchored("/ünicode.svg", "ünicode.svg");
    }

    #[test]
    fn dot_components_are_folded() {
        accept_anchored("/./icons/./star.svg", "icons/star.svg");
        accept_anchored("/icons/../logo.svg", "logo.svg");
    }

    #[test]
    fn the_root_itself_is_a_dot() {
        accept_anchored("/.", ".");
        accept_anchored("/icons/..", ".");
        accept(".", ".");
        accept("dist/..", ".");
        accept("./dist/..", ".");
    }

    #[test]
    fn rejects_malformed_paths() {
        reject_anchored("", RootRelativePathError::Empty);
        reject_anchored("/icons/", RootRelativePathError::EndsWithSeparator);
        reject_anchored("/icons//star.svg", RootRelativePathError::EmptyComponent);
        reject_anchored(
            "/my\\logo.svg",
            RootRelativePathError::InvalidCharacter('\\'),
        );
        reject_anchored(
            "/my\nlogo.svg",
            RootRelativePathError::InvalidCharacter('\n'),
        );
        reject_anchored("/", RootRelativePathError::EndsWithSeparator);
    }

    #[test]
    fn rejects_colons() {
        reject(
            "C:/secrets.css",
            RootRelativePathError::InvalidCharacter(':'),
        );
        reject(
            "c:secrets.css",
            RootRelativePathError::InvalidCharacter(':'),
        );
        reject(
            "icons/star.svg:x",
            RootRelativePathError::InvalidCharacter(':'),
        );
        reject_anchored("/C:/logo.svg", RootRelativePathError::InvalidCharacter(':'));
    }

    #[test]
    fn plain_paths_are_relative() {
        accept("dist", "dist");
        accept("dist/public", "dist/public");
        accept("../assets", "../assets");
        accept("../../generated/app.ts", "../../generated/app.ts");
    }

    #[test]
    fn plain_paths_reject_absolute_and_empty() {
        reject("/dist", RootRelativePathError::MustBeRelative);
        reject("/", RootRelativePathError::MustBeRelative);
        reject("", RootRelativePathError::Empty);
    }

    #[test]
    fn plain_paths_are_normalized() {
        accept("./dist", "dist");
        accept("./dist/../public", "public");
        accept("dist/./public", "dist/public");
    }

    #[test]
    fn plain_paths_reject_malformed_paths() {
        reject("dist/", RootRelativePathError::EndsWithSeparator);
        reject("dist//public", RootRelativePathError::EmptyComponent);
        reject(
            "dist\\public",
            RootRelativePathError::InvalidCharacter('\\'),
        );
    }

    #[test]
    fn file_name() {
        let named = RootRelativePath::new("icons/star.svg").unwrap();
        assert_eq!(named.file_name(), Some("star.svg"));

        let above = RootRelativePath::new("../shared/logo.svg").unwrap();
        assert_eq!(above.file_name(), Some("logo.svg"));

        let root = RootRelativePath::new(".").unwrap();
        assert_eq!(root.file_name(), None);

        let parent = RootRelativePath::new("..").unwrap();
        assert_eq!(parent.file_name(), None);

        let grandparent = RootRelativePath::new("../..").unwrap();
        assert_eq!(grandparent.file_name(), None);
    }
}
