use std::fmt;
use thiserror::Error;

/// Error type for invalid module IDs
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidModuleIdError {
    #[error("Module name cannot be empty")]
    Empty,

    #[error("Module name cannot start with '/'")]
    StartsWithSlash,

    #[error("Module name cannot end with '/'")]
    EndsWithSlash,

    #[error("Module name cannot contain '//'")]
    ContainsDoubleSlash,

    #[error("Module name cannot contain '..'")]
    ContainsParentDirectory,

    #[error("Module name cannot contain path component '.'")]
    ContainsCurrentDirectory,

    #[error("Module name cannot contain '@'")]
    ContainsAtSymbol,

    #[error("Module name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Module name contains empty path component")]
    EmptyComponent,
}

/// A type-safe wrapper for module IDs in the hop system.
/// Module IDs represent the path to a module relative to the project root,
/// without the .hop extension and without the @/ prefix.
///
/// Examples: "components/button", "utils", "hop/ui"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleId(Vec<String>);

impl ModuleId {
    /// Create a new ModuleId from a string, validating it
    ///
    /// Returns an error describing why the module ID is invalid.
    /// Valid module IDs:
    /// - Must not be empty
    /// - Must not start or end with '/'
    /// - Must not contain '//', '..' or '.'
    /// - Must not contain '@'
    /// - Must only contain alphanumeric characters, '-', '_', and '/'
    pub fn new(name: &str) -> Result<Self, InvalidModuleIdError> {
        let components = Self::validate(name)?;
        Ok(ModuleId(components))
    }

    /// Validate a module ID string and return the components
    fn validate(name: &str) -> Result<Vec<String>, InvalidModuleIdError> {
        // Must not be empty
        if name.is_empty() {
            return Err(InvalidModuleIdError::Empty);
        }

        // Must not start or end with '/'
        if name.starts_with('/') {
            return Err(InvalidModuleIdError::StartsWithSlash);
        }
        if name.ends_with('/') {
            return Err(InvalidModuleIdError::EndsWithSlash);
        }

        // Must not contain '//', '..', or standalone '.'
        if name.contains("//") {
            return Err(InvalidModuleIdError::ContainsDoubleSlash);
        }
        if name.contains("..") {
            return Err(InvalidModuleIdError::ContainsParentDirectory);
        }

        // Must not contain '@' (this is stripped in import paths)
        if name.contains('@') {
            return Err(InvalidModuleIdError::ContainsAtSymbol);
        }

        let mut components = Vec::new();

        // Check for path components that are just '.'
        for component in name.split('/') {
            if component.is_empty() {
                return Err(InvalidModuleIdError::EmptyComponent);
            }
            if component == "." {
                return Err(InvalidModuleIdError::ContainsCurrentDirectory);
            }

            // Each component should only contain valid characters
            for c in component.chars() {
                if !c.is_alphanumeric() && c != '-' && c != '_' {
                    return Err(InvalidModuleIdError::InvalidCharacter(c));
                }
            }

            components.push(component.to_string());
        }

        Ok(components)
    }

    /// Get the module ID as a path string with '/' separator
    pub fn to_path(&self) -> String {
        self.0.join("/")
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_accept_simple_module_name() {
        assert!(ModuleId::new("utils").is_ok());
    }

    #[test]
    fn should_accept_module_name_with_path() {
        assert!(ModuleId::new("components/button").is_ok());
        assert!(ModuleId::new("hop/ui").is_ok());
    }

    #[test]
    fn should_accept_module_name_with_hyphen() {
        assert!(ModuleId::new("my-component").is_ok());
    }

    #[test]
    fn should_accept_module_name_with_underscore() {
        assert!(ModuleId::new("my_component").is_ok());
    }

    #[test]
    fn should_accept_deeply_nested_module_name() {
        assert!(ModuleId::new("a/b/c/d").is_ok());
    }

    #[test]
    fn should_reject_empty_module_name() {
        assert_eq!(ModuleId::new(""), Err(InvalidModuleIdError::Empty));
    }

    #[test]
    fn should_reject_module_name_starting_with_slash() {
        assert_eq!(
            ModuleId::new("/utils"),
            Err(InvalidModuleIdError::StartsWithSlash)
        );
    }

    #[test]
    fn should_reject_module_name_ending_with_slash() {
        assert_eq!(
            ModuleId::new("utils/"),
            Err(InvalidModuleIdError::EndsWithSlash)
        );
    }

    #[test]
    fn should_reject_module_name_with_double_slash() {
        assert_eq!(
            ModuleId::new("utils//components"),
            Err(InvalidModuleIdError::ContainsDoubleSlash)
        );
    }

    #[test]
    fn should_reject_module_name_with_parent_directory() {
        assert_eq!(
            ModuleId::new("../parent"),
            Err(InvalidModuleIdError::ContainsParentDirectory)
        );
    }

    #[test]
    fn should_reject_module_name_with_current_directory() {
        assert_eq!(
            ModuleId::new("./current"),
            Err(InvalidModuleIdError::ContainsCurrentDirectory)
        );
    }

    #[test]
    fn should_reject_module_name_with_at_symbol() {
        assert_eq!(
            ModuleId::new("@/utils"),
            Err(InvalidModuleIdError::ContainsAtSymbol)
        );
    }

    #[test]
    fn should_reject_module_name_with_current_directory_in_path() {
        assert_eq!(
            ModuleId::new("utils/./current"),
            Err(InvalidModuleIdError::ContainsCurrentDirectory)
        );
    }

    #[test]
    fn should_reject_module_name_with_space() {
        assert_eq!(
            ModuleId::new("my component"),
            Err(InvalidModuleIdError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn should_reject_module_name_with_exclamation_mark() {
        assert_eq!(
            ModuleId::new("my!component"),
            Err(InvalidModuleIdError::InvalidCharacter('!'))
        );
    }
}
