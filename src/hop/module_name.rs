use std::fmt;
use thiserror::Error;

/// Error type for invalid module names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidModuleNameError {
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

/// A type-safe wrapper for module names in the hop system.
/// Module names represent the path to a module relative to the project root,
/// without the .hop extension and without the @/ prefix.
///
/// Examples: "components/button", "utils", "hop/ui"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(Vec<String>);

impl ModuleName {
    /// Create a new ModuleName from a string, validating it
    ///
    /// Returns an error describing why the module name is invalid.
    /// Valid module names:
    /// - Must not be empty
    /// - Must not start or end with '/'
    /// - Must not contain '//', '..' or '.'
    /// - Must not contain '@'
    /// - Must only contain alphanumeric characters, '-', '_', and '/'
    pub fn new(name: &str) -> Result<Self, InvalidModuleNameError> {
        let components = Self::validate(name)?;
        Ok(ModuleName(components))
    }

    /// Validate a module name string and return the components
    fn validate(name: &str) -> Result<Vec<String>, InvalidModuleNameError> {
        // Must not be empty
        if name.is_empty() {
            return Err(InvalidModuleNameError::Empty);
        }

        // Must not start or end with '/'
        if name.starts_with('/') {
            return Err(InvalidModuleNameError::StartsWithSlash);
        }
        if name.ends_with('/') {
            return Err(InvalidModuleNameError::EndsWithSlash);
        }

        // Must not contain '//', '..', or standalone '.'
        if name.contains("//") {
            return Err(InvalidModuleNameError::ContainsDoubleSlash);
        }
        if name.contains("..") {
            return Err(InvalidModuleNameError::ContainsParentDirectory);
        }

        // Must not contain '@' (this is stripped in import paths)
        if name.contains('@') {
            return Err(InvalidModuleNameError::ContainsAtSymbol);
        }

        let mut components = Vec::new();

        // Check for path components that are just '.'
        for component in name.split('/') {
            if component.is_empty() {
                return Err(InvalidModuleNameError::EmptyComponent);
            }
            if component == "." {
                return Err(InvalidModuleNameError::ContainsCurrentDirectory);
            }

            // Each component should only contain valid characters
            for c in component.chars() {
                if !c.is_alphanumeric() && c != '-' && c != '_' {
                    return Err(InvalidModuleNameError::InvalidCharacter(c));
                }
            }

            components.push(component.to_string());
        }

        Ok(components)
    }

    /// Get the module name as a joined string with '::' separator
    pub fn to_string(&self) -> String {
        self.0.join("::")
    }

    /// Get the module name as a path string with '/' separator
    pub fn to_path(&self) -> String {
        self.0.join("/")
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_accept_simple_module_name() {
        assert!(ModuleName::new("utils").is_ok());
    }

    #[test]
    fn should_accept_module_name_with_path() {
        assert!(ModuleName::new("components/button").is_ok());
        assert!(ModuleName::new("hop/ui").is_ok());
    }

    #[test]
    fn should_accept_module_name_with_hyphen() {
        assert!(ModuleName::new("my-component").is_ok());
    }

    #[test]
    fn should_accept_module_name_with_underscore() {
        assert!(ModuleName::new("my_component").is_ok());
    }

    #[test]
    fn should_accept_deeply_nested_module_name() {
        assert!(ModuleName::new("a/b/c/d").is_ok());
    }

    #[test]
    fn should_reject_empty_module_name() {
        assert_eq!(ModuleName::new(""), Err(InvalidModuleNameError::Empty));
    }

    #[test]
    fn should_reject_module_name_starting_with_slash() {
        assert_eq!(
            ModuleName::new("/utils"),
            Err(InvalidModuleNameError::StartsWithSlash)
        );
    }

    #[test]
    fn should_reject_module_name_ending_with_slash() {
        assert_eq!(
            ModuleName::new("utils/"),
            Err(InvalidModuleNameError::EndsWithSlash)
        );
    }

    #[test]
    fn should_reject_module_name_with_double_slash() {
        assert_eq!(
            ModuleName::new("utils//components"),
            Err(InvalidModuleNameError::ContainsDoubleSlash)
        );
    }

    #[test]
    fn should_reject_module_name_with_parent_directory() {
        assert_eq!(
            ModuleName::new("../parent"),
            Err(InvalidModuleNameError::ContainsParentDirectory)
        );
    }

    #[test]
    fn should_reject_module_name_with_current_directory() {
        assert_eq!(
            ModuleName::new("./current"),
            Err(InvalidModuleNameError::ContainsCurrentDirectory)
        );
    }

    #[test]
    fn should_reject_module_name_with_at_symbol() {
        assert_eq!(
            ModuleName::new("@/utils"),
            Err(InvalidModuleNameError::ContainsAtSymbol)
        );
    }

    #[test]
    fn should_reject_module_name_with_current_directory_in_path() {
        assert_eq!(
            ModuleName::new("utils/./current"),
            Err(InvalidModuleNameError::ContainsCurrentDirectory)
        );
    }

    #[test]
    fn should_reject_module_name_with_space() {
        assert_eq!(
            ModuleName::new("my component"),
            Err(InvalidModuleNameError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn should_reject_module_name_with_exclamation_mark() {
        assert_eq!(
            ModuleName::new("my!component"),
            Err(InvalidModuleNameError::InvalidCharacter('!'))
        );
    }
}
