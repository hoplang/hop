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
    pub fn new(name: impl Into<String>) -> Option<Self> {
        let name = name.into();
        if Self::is_valid(&name) {
            Some(ModuleName(name))
        } else {
            None
        }
    }

    /// Create a ModuleName from an import path (strips @/ prefix if present)
    /// Returns None if the resulting module name is invalid
    pub fn from_import_path(path: &str) -> Option<Self> {
        let cleaned = path.strip_prefix("@/").unwrap_or(path);
        Self::new(cleaned)
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
            if !component.chars().all(|c| c.is_alphanumeric() || c == '-' || c == '_') {
                return false;
            }
        }

        true
    }

    /// Get the module name as a string slice
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Get the filename for this module (adds .hop extension)
    pub fn to_filename(&self) -> String {
        format!("{}.hop", self.0)
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
        ModuleName::new(s).expect("Invalid module name")
    }
}

impl AsRef<str> for ModuleName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_module_names() {
        assert!(ModuleName::new("utils").is_some());
        assert!(ModuleName::new("components/button").is_some());
        assert!(ModuleName::new("hop/ui").is_some());
        assert!(ModuleName::new("my-component").is_some());
        assert!(ModuleName::new("my_component").is_some());
        assert!(ModuleName::new("a/b/c/d").is_some());
    }

    #[test]
    fn test_invalid_module_names() {
        assert!(ModuleName::new("").is_none());
        assert!(ModuleName::new("/utils").is_none());
        assert!(ModuleName::new("utils/").is_none());
        assert!(ModuleName::new("utils//components").is_none());
        assert!(ModuleName::new("../parent").is_none());
        assert!(ModuleName::new("./current").is_none());
        assert!(ModuleName::new("@/utils").is_none());
        assert!(ModuleName::new("utils/./current").is_none());
        assert!(ModuleName::new("my component").is_none()); // spaces not allowed
        assert!(ModuleName::new("my!component").is_none()); // special chars not allowed
    }

    #[test]
    fn test_from_import_path() {
        assert_eq!(
            ModuleName::from_import_path("@/utils").unwrap().as_str(),
            "utils"
        );
        assert_eq!(
            ModuleName::from_import_path("@/components/button").unwrap().as_str(),
            "components/button"
        );
        // Works without @/ prefix too
        assert_eq!(
            ModuleName::from_import_path("utils").unwrap().as_str(),
            "utils"
        );
    }

    #[test]
    fn test_to_filename() {
        let module = ModuleName::new("components/button").unwrap();
        assert_eq!(module.to_filename(), "components/button.hop");
    }
}