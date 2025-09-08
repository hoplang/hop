use std::fmt;
use std::error::Error;

/// Error type for invalid module names
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InvalidModuleNameError {
    Empty,
    StartsWithSlash,
    EndsWithSlash,
    ContainsDoubleSlash,
    ContainsParentDirectory,
    ContainsCurrentDirectory,
    ContainsAtSymbol,
    InvalidCharacter(char),
    EmptyComponent,
}

impl fmt::Display for InvalidModuleNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => write!(f, "Module name cannot be empty"),
            Self::StartsWithSlash => write!(f, "Module name cannot start with '/'"),
            Self::EndsWithSlash => write!(f, "Module name cannot end with '/'"),
            Self::ContainsDoubleSlash => write!(f, "Module name cannot contain '//'"),
            Self::ContainsParentDirectory => write!(f, "Module name cannot contain '..'"),
            Self::ContainsCurrentDirectory => write!(f, "Module name cannot contain path component '.'"),
            Self::ContainsAtSymbol => write!(f, "Module name cannot contain '@'"),
            Self::InvalidCharacter(c) => write!(f, "Module name contains invalid character: '{}'", c),
            Self::EmptyComponent => write!(f, "Module name contains empty path component"),
        }
    }
}

impl Error for InvalidModuleNameError {}

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
    /// Returns an error describing why the module name is invalid.
    /// Valid module names:
    /// - Must not be empty
    /// - Must not start or end with '/'
    /// - Must not contain '//', '..' or '.'
    /// - Must not contain '@'
    /// - Must only contain alphanumeric characters, '-', '_', and '/'
    pub fn new(name: String) -> Result<Self, InvalidModuleNameError> {
        Self::validate(&name)?;
        Ok(ModuleName(name))
    }

    /// Validate a module name string
    fn validate(name: &str) -> Result<(), InvalidModuleNameError> {
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
        }

        Ok(())
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_module_names() {
        assert!(ModuleName::new("utils".to_string()).is_ok());
        assert!(ModuleName::new("components/button".to_string()).is_ok());
        assert!(ModuleName::new("hop/ui".to_string()).is_ok());
        assert!(ModuleName::new("my-component".to_string()).is_ok());
        assert!(ModuleName::new("my_component".to_string()).is_ok());
        assert!(ModuleName::new("a/b/c/d".to_string()).is_ok());
    }

    #[test]
    fn test_invalid_module_names() {
        assert_eq!(ModuleName::new("".to_string()), Err(InvalidModuleNameError::Empty));
        assert_eq!(ModuleName::new("/utils".to_string()), Err(InvalidModuleNameError::StartsWithSlash));
        assert_eq!(ModuleName::new("utils/".to_string()), Err(InvalidModuleNameError::EndsWithSlash));
        assert_eq!(ModuleName::new("utils//components".to_string()), Err(InvalidModuleNameError::ContainsDoubleSlash));
        assert_eq!(ModuleName::new("../parent".to_string()), Err(InvalidModuleNameError::ContainsParentDirectory));
        assert_eq!(ModuleName::new("./current".to_string()), Err(InvalidModuleNameError::ContainsCurrentDirectory));
        assert_eq!(ModuleName::new("@/utils".to_string()), Err(InvalidModuleNameError::ContainsAtSymbol));
        assert_eq!(ModuleName::new("utils/./current".to_string()), Err(InvalidModuleNameError::ContainsCurrentDirectory));
        assert_eq!(ModuleName::new("my component".to_string()), Err(InvalidModuleNameError::InvalidCharacter(' ')));
        assert_eq!(ModuleName::new("my!component".to_string()), Err(InvalidModuleNameError::InvalidCharacter('!')));
    }
}
