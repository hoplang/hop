use std::fmt;
use thiserror::Error;

/// Error type for invalid module IDs
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidModuleIdError {
    #[error("Module ID cannot be empty")]
    Empty,

    #[error("Module ID cannot start with '::'")]
    StartsWithSeparator,

    #[error("Module ID cannot end with '::'")]
    EndsWithSeparator,

    #[error("Module ID contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Module ID contains empty component")]
    EmptyComponent,
}

/// A type-safe wrapper for module IDs in the hop system.
/// Module IDs represent the path to a module relative to the project root,
/// without the .hop extension. Internally stored with `::` separators.
///
/// Examples: "components::button", "utils", "hop::ui"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleId(String);

impl ModuleId {
    /// Create a new ModuleId from a string using '::' separators, validating it
    ///
    /// Returns an error describing why the module ID is invalid.
    /// Valid module IDs:
    /// - Must not be empty
    /// - Must not start or end with '::'
    /// - Components must only contain alphanumeric characters, '-', and '_'
    pub fn new(name: &str) -> Result<Self, InvalidModuleIdError> {
        Self::validate(name)?;
        Ok(ModuleId(name.to_string()))
    }

    /// Validate a module ID string
    fn validate(name: &str) -> Result<(), InvalidModuleIdError> {
        if name.is_empty() {
            return Err(InvalidModuleIdError::Empty);
        }

        if name.starts_with("::") {
            return Err(InvalidModuleIdError::StartsWithSeparator);
        }
        if name.ends_with("::") {
            return Err(InvalidModuleIdError::EndsWithSeparator);
        }

        for component in name.split("::") {
            if component.is_empty() {
                return Err(InvalidModuleIdError::EmptyComponent);
            }

            for c in component.chars() {
                if !c.is_alphanumeric() && c != '-' && c != '_' {
                    return Err(InvalidModuleIdError::InvalidCharacter(c));
                }
            }
        }

        Ok(())
    }

    /// Get the module ID as a path string with '/' separator
    pub fn to_path(&self) -> String {
        self.0.replace("::", "/")
    }
}

impl fmt::Display for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_accept_simple_module_id() {
        assert!(ModuleId::new("utils").is_ok());
    }

    #[test]
    fn should_accept_module_id_with_path() {
        assert!(ModuleId::new("components::button").is_ok());
        assert!(ModuleId::new("hop::ui").is_ok());
    }

    #[test]
    fn should_accept_module_id_with_hyphen() {
        assert!(ModuleId::new("my-component").is_ok());
    }

    #[test]
    fn should_accept_module_id_with_underscore() {
        assert!(ModuleId::new("my_component").is_ok());
    }

    #[test]
    fn should_accept_deeply_nested_module_id() {
        assert!(ModuleId::new("a::b::c::d").is_ok());
    }

    #[test]
    fn should_reject_empty_module_id() {
        assert_eq!(ModuleId::new(""), Err(InvalidModuleIdError::Empty));
    }

    #[test]
    fn should_reject_module_id_starting_with_separator() {
        assert_eq!(
            ModuleId::new("::utils"),
            Err(InvalidModuleIdError::StartsWithSeparator)
        );
    }

    #[test]
    fn should_reject_module_id_ending_with_separator() {
        assert_eq!(
            ModuleId::new("utils::"),
            Err(InvalidModuleIdError::EndsWithSeparator)
        );
    }

    #[test]
    fn should_reject_module_id_with_empty_component() {
        assert_eq!(
            ModuleId::new("utils::::components"),
            Err(InvalidModuleIdError::EmptyComponent)
        );
    }

    #[test]
    fn should_reject_module_id_with_space() {
        assert_eq!(
            ModuleId::new("my component"),
            Err(InvalidModuleIdError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn should_reject_module_id_with_slash() {
        assert_eq!(
            ModuleId::new("my/component"),
            Err(InvalidModuleIdError::InvalidCharacter('/'))
        );
    }

    #[test]
    fn should_reject_module_id_with_dot() {
        assert_eq!(
            ModuleId::new("my.component"),
            Err(InvalidModuleIdError::InvalidCharacter('.'))
        );
    }
}
