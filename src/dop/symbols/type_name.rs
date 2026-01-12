use std::fmt::{self, Display};

use crate::document::document::CheapString;
use thiserror::Error;

/// Error type for invalid type names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidTypeNameError {
    #[error("Type name must start with an uppercase letter")]
    DoesNotStartWithUppercase,

    #[error("Type name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Type name cannot be empty")]
    Empty,
}

/// A TypeName represents a validated type name in dop.
/// Type names must be PascalCase (start with uppercase letter).
#[derive(Debug, Clone)]
pub struct TypeName {
    value: CheapString,
}

impl TypeName {
    /// Create a new TypeName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidTypeNameError> {
        Self::validate(name)?;
        Ok(TypeName {
            value: CheapString::new(name.to_string()),
        })
    }

    /// Create a new TypeName from a CheapString, validating it
    pub fn from_cheap_string(name: CheapString) -> Result<Self, InvalidTypeNameError> {
        Self::validate(name.as_str())?;
        Ok(TypeName { value: name })
    }

    /// Validate a type name string (PascalCase)
    fn validate(name: &str) -> Result<(), InvalidTypeNameError> {
        if name.is_empty() {
            return Err(InvalidTypeNameError::Empty);
        }

        let mut chars = name.chars();
        let first_char = chars.next().unwrap();

        // First character must be an uppercase letter
        if !first_char.is_ascii_uppercase() {
            return Err(InvalidTypeNameError::DoesNotStartWithUppercase);
        }

        // Remaining characters must be alphanumeric
        for c in chars {
            if !c.is_ascii_alphanumeric() {
                return Err(InvalidTypeNameError::InvalidCharacter(c));
            }
        }

        Ok(())
    }

    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl PartialEq for TypeName {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for TypeName {}

impl PartialOrd for TypeName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TypeName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl TryFrom<String> for TypeName {
    type Error = InvalidTypeNameError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        TypeName::new(&s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_accept_simple_type_name() {
        assert!(TypeName::new("User").is_ok());
    }

    #[test]
    fn should_accept_pascal_case_type_name() {
        assert!(TypeName::new("UserProfile").is_ok());
    }

    #[test]
    fn should_accept_single_letter_type_name() {
        assert!(TypeName::new("X").is_ok());
    }

    #[test]
    fn should_accept_type_name_with_digits() {
        assert!(TypeName::new("User123").is_ok());
    }

    #[test]
    fn should_accept_all_uppercase_type_name() {
        assert!(TypeName::new("ABC").is_ok());
    }

    #[test]
    fn should_reject_lowercase_type_name() {
        assert_eq!(
            TypeName::new("user"),
            Err(InvalidTypeNameError::DoesNotStartWithUppercase)
        );
    }

    #[test]
    fn should_reject_type_name_starting_with_digit() {
        assert_eq!(
            TypeName::new("123User"),
            Err(InvalidTypeNameError::DoesNotStartWithUppercase)
        );
    }

    #[test]
    fn should_reject_type_name_starting_with_underscore() {
        assert_eq!(
            TypeName::new("_User"),
            Err(InvalidTypeNameError::DoesNotStartWithUppercase)
        );
    }

    #[test]
    fn should_reject_type_name_with_underscore() {
        assert_eq!(
            TypeName::new("User_Profile"),
            Err(InvalidTypeNameError::InvalidCharacter('_'))
        );
    }

    #[test]
    fn should_reject_type_name_with_hyphen() {
        assert_eq!(
            TypeName::new("User-Profile"),
            Err(InvalidTypeNameError::InvalidCharacter('-'))
        );
    }

    #[test]
    fn should_reject_type_name_with_space() {
        assert_eq!(
            TypeName::new("User Profile"),
            Err(InvalidTypeNameError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn should_reject_empty_type_name() {
        assert_eq!(TypeName::new(""), Err(InvalidTypeNameError::Empty));
    }
}
