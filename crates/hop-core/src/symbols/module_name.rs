use std::fmt;
use std::sync::Arc;
use thiserror::Error;

use crate::document_id::DocumentId;

/// Error type for invalid module IDs
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidModuleNameError {
    #[error("Module name cannot be empty")]
    Empty,

    #[error("Module name cannot start with '::'")]
    StartsWithSeparator,

    #[error("Module name cannot end with '::'")]
    EndsWithSeparator,

    #[error("Module name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Module name contains empty component")]
    EmptyComponent,
}

/// A type-safe wrapper for module IDs in the hop system.
/// Module IDs represent the path to a module relative to the project root,
/// without the .hop extension. Internally stored with `::` separators.
///
/// Examples: "components::button", "utils", "hop::ui"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(Arc<String>);

impl ModuleName {
    /// Create a new ModuleName from a string using '::' separators, validating it
    ///
    /// Returns an error describing why the module ID is invalid.
    /// Valid module IDs:
    /// - Must not be empty
    /// - Must not start or end with '::'
    /// - Components must only contain alphanumeric characters, '-', and '_'
    pub fn new(name: &str) -> Result<Self, InvalidModuleNameError> {
        Self::validate(name)?;
        Ok(ModuleName(Arc::new(name.to_string())))
    }

    pub fn to_document_id(&self) -> DocumentId {
        DocumentId::new(&format!("{}.hop", self.0.replace("::", "/"))).unwrap()
    }

    /// Validate a module ID string
    fn validate(name: &str) -> Result<(), InvalidModuleNameError> {
        if name.is_empty() {
            return Err(InvalidModuleNameError::Empty);
        }

        if name.starts_with("::") {
            return Err(InvalidModuleNameError::StartsWithSeparator);
        }
        if name.ends_with("::") {
            return Err(InvalidModuleNameError::EndsWithSeparator);
        }

        for component in name.split("::") {
            if component.is_empty() {
                return Err(InvalidModuleNameError::EmptyComponent);
            }

            for c in component.chars() {
                if !c.is_alphanumeric() && c != '-' && c != '_' {
                    return Err(InvalidModuleNameError::InvalidCharacter(c));
                }
            }
        }

        Ok(())
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

    fn accept(input: &str) {
        assert!(ModuleName::new(input).is_ok());
    }

    fn reject(input: &str, expected: InvalidModuleNameError) {
        assert_eq!(ModuleName::new(input), Err(expected));
    }

    #[test]
    fn accepts_simple_module_id() {
        accept("utils");
    }

    #[test]
    fn accepts_module_id_with_path() {
        accept("components::button");
        accept("hop::ui");
    }

    #[test]
    fn accepts_module_id_with_hyphen() {
        accept("my-component");
    }

    #[test]
    fn accepts_module_id_with_underscore() {
        accept("my_component");
    }

    #[test]
    fn accepts_deeply_nested_module_id() {
        accept("a::b::c::d");
    }

    #[test]
    fn rejects_empty_module_id() {
        reject("", InvalidModuleNameError::Empty);
    }

    #[test]
    fn rejects_module_id_starting_with_separator() {
        reject("::utils", InvalidModuleNameError::StartsWithSeparator);
    }

    #[test]
    fn rejects_module_id_ending_with_separator() {
        reject("utils::", InvalidModuleNameError::EndsWithSeparator);
    }

    #[test]
    fn rejects_module_id_with_empty_component() {
        reject(
            "utils::::components",
            InvalidModuleNameError::EmptyComponent,
        );
    }

    #[test]
    fn rejects_module_id_with_space() {
        reject(
            "my component",
            InvalidModuleNameError::InvalidCharacter(' '),
        );
    }

    #[test]
    fn rejects_module_id_with_slash() {
        reject(
            "my/component",
            InvalidModuleNameError::InvalidCharacter('/'),
        );
    }

    #[test]
    fn rejects_module_id_with_dot() {
        reject(
            "my.component",
            InvalidModuleNameError::InvalidCharacter('.'),
        );
    }
}
