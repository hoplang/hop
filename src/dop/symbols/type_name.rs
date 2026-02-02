use std::fmt::{self, Display};

use crate::document::CheapString;
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
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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

    /// Get a clone of the inner CheapString
    pub fn to_cheap_string(&self) -> CheapString {
        self.value.clone()
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

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

impl AsRef<str> for TypeName {
    fn as_ref(&self) -> &str {
        self.value.as_str()
    }
}

impl TypeName {
    /// Convert the type name to PascalCase
    ///
    /// Since TypeName is already in PascalCase, this just returns a copy of the string.
    /// This method exists for clarity and consistency with other case conversion methods.
    ///
    /// Examples:
    /// - "UserProfile" -> "UserProfile"
    /// - "Button" -> "Button"
    /// - "NavBar" -> "NavBar"
    pub fn to_pascal_case(&self) -> String {
        self.value.as_str().to_string()
    }

    /// Convert the type name to snake_case
    ///
    /// Examples:
    /// - "UserProfile" -> "user_profile"
    /// - "Button" -> "button"
    /// - "NavBar" -> "nav_bar"
    pub fn to_snake_case(&self) -> String {
        let mut result = String::new();
        let mut prev_was_lowercase = false;

        for (i, ch) in self.value.as_str().chars().enumerate() {
            if ch.is_uppercase() {
                // Add underscore before uppercase if not first char and prev was lowercase
                if i > 0 && prev_was_lowercase {
                    result.push('_');
                }
                result.push(ch.to_ascii_lowercase());
                prev_was_lowercase = true;
            } else {
                result.push(ch);
                prev_was_lowercase = ch.is_lowercase();
            }
        }

        result
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
