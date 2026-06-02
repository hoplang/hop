use std::fmt;
use std::sync::Arc;
use thiserror::Error;

/// A type-safe wrapper for documents IDs in the hop system.
/// Document IDs represent the path to a document relative to the project root.
/// Document IDs do _not_ have a leading slash.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DocumentId(Arc<String>);

impl DocumentId {
    pub fn new(name: &str) -> Result<Self, DocumentIdError> {
        Self::validate(name)?;
        Ok(DocumentId(Arc::new(name.to_string())))
    }

    /// Validate a module ID string
    fn validate(name: &str) -> Result<(), DocumentIdError> {
        if name.is_empty() {
            return Err(DocumentIdError::Empty);
        }

        if name.starts_with('/') {
            return Err(DocumentIdError::StartsWithSeparator);
        }
        if name.ends_with('/') {
            return Err(DocumentIdError::EndsWithSeparator);
        }

        for component in name.split('/') {
            if component.is_empty() {
                return Err(DocumentIdError::EmptyComponent);
            }

            for c in component.chars() {
                if !c.is_alphanumeric() && c != '-' && c != '_' && c != '.' {
                    return Err(DocumentIdError::InvalidCharacter(c));
                }
            }
        }

        Ok(())
    }

    /// Convert to a module-style identifier with '::' separator
    pub(crate) fn to_module_id(&self) -> String {
        self.0.trim_end_matches(".hop").replace("/", "::")
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Returns the file extension, if any (e.g. "hop" for "components/button.hop")
    pub fn extension(&self) -> Option<&str> {
        let s = self.0.as_str();
        match s.rfind('.') {
            Some(pos) if pos < s.len() - 1 => Some(&s[pos + 1..]),
            _ => None,
        }
    }
}

impl fmt::Display for DocumentId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Error type for invalid module IDs
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum DocumentIdError {
    #[error("Document ID cannot be empty")]
    Empty,

    #[error("Document ID cannot start with '/'")]
    StartsWithSeparator,

    #[error("Document ID cannot end with '/'")]
    EndsWithSeparator,

    #[error("Document ID contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Document ID contains empty component")]
    EmptyComponent,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_accept_simple_document_id() {
        assert!(DocumentId::new("utils.hop").is_ok());
    }

    #[test]
    fn should_accept_document_id_with_path() {
        assert!(DocumentId::new("components/button.hop").is_ok());
        assert!(DocumentId::new("hop/ui.hop").is_ok());
    }

    #[test]
    fn should_accept_document_id_with_hyphen() {
        assert!(DocumentId::new("my-component.hop").is_ok());
    }

    #[test]
    fn should_accept_document_id_with_underscore() {
        assert!(DocumentId::new("my_component.hop").is_ok());
    }

    #[test]
    fn should_accept_deeply_nested_document_id() {
        assert!(DocumentId::new("a/b/c/d.hop").is_ok());
    }

    #[test]
    fn should_reject_empty_document_id() {
        assert_eq!(DocumentId::new(""), Err(DocumentIdError::Empty));
    }

    #[test]
    fn should_reject_document_id_starting_with_separator() {
        assert_eq!(
            DocumentId::new("/utils.hop"),
            Err(DocumentIdError::StartsWithSeparator)
        );
    }

    #[test]
    fn should_reject_document_id_with_empty_component() {
        assert_eq!(
            DocumentId::new("utils//components.hop"),
            Err(DocumentIdError::EmptyComponent)
        );
    }

    #[test]
    fn should_reject_document_id_with_space() {
        assert_eq!(
            DocumentId::new("my component.hop"),
            Err(DocumentIdError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn should_reject_document_id_with_colon_separator() {
        assert_eq!(
            DocumentId::new("my::component.hop"),
            Err(DocumentIdError::InvalidCharacter(':'))
        );
    }
}
