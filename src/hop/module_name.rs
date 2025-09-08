use std::fmt;

/// A type-safe wrapper for module names in the hop system.
/// Module names represent the path to a module relative to the project root,
/// without the .hop extension and without the @/ prefix.
///
/// Examples: "components/button", "utils", "hop/ui"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(String);

impl ModuleName {
    /// Create a new ModuleName from a string, validating it
    ///
    /// Returns None if the module name is invalid.
    /// Valid module names:
    /// - Must not be empty
    /// - Must not start or end with '/'
    /// - Must not contain '//', '..' or '.'
    /// - Must not contain '@'
    /// - Must only contain alphanumeric characters, '-', '_', and '/'
    pub fn new(name: String) -> Option<Self> {
        if Self::is_valid(&name) {
            Some(ModuleName(name))
        } else {
            None
        }
    }

    /// Check if a module name is valid
    fn is_valid(name: &str) -> bool {
        // Must not be empty
        if name.is_empty() {
            return false;
        }

        // Must not start or end with '/'
        if name.starts_with('/') || name.ends_with('/') {
            return false;
        }

        // Must not contain '//', '..', or standalone '.'
        if name.contains("//") || name.contains("..") {
            return false;
        }

        // Must not contain '@' (this is stripped in import paths)
        if name.contains('@') {
            return false;
        }

        // Check for path components that are just '.'
        for component in name.split('/') {
            if component.is_empty() || component == "." {
                return false;
            }

            // Each component should only contain valid characters
            if !component
                .chars()
                .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
            {
                return false;
            }
        }

        true
    }

    /// Get the module name as a string slice
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for ModuleName {
    fn from(s: String) -> Self {
        ModuleName::new(s).expect("Invalid module name")
    }
}

impl From<&str> for ModuleName {
    fn from(s: &str) -> Self {
        ModuleName::new(s.to_string()).expect("Invalid module name")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_module_names() {
        assert!(ModuleName::new("utils".to_string()).is_some());
        assert!(ModuleName::new("components/button".to_string()).is_some());
        assert!(ModuleName::new("hop/ui".to_string()).is_some());
        assert!(ModuleName::new("my-component".to_string()).is_some());
        assert!(ModuleName::new("my_component".to_string()).is_some());
        assert!(ModuleName::new("a/b/c/d".to_string()).is_some());
    }

    #[test]
    fn test_invalid_module_names() {
        assert!(ModuleName::new("".to_string()).is_none());
        assert!(ModuleName::new("/utils".to_string()).is_none());
        assert!(ModuleName::new("utils/".to_string()).is_none());
        assert!(ModuleName::new("utils//components".to_string()).is_none());
        assert!(ModuleName::new("../parent".to_string()).is_none());
        assert!(ModuleName::new("./current".to_string()).is_none());
        assert!(ModuleName::new("@/utils".to_string()).is_none());
        assert!(ModuleName::new("utils/./current".to_string()).is_none());
        assert!(ModuleName::new("my component".to_string()).is_none()); // spaces not allowed
        assert!(ModuleName::new("my!component".to_string()).is_none()); // special chars not allowed
    }
}
