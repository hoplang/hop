use std::fmt::{self, Display};
use thiserror::Error;

/// Error type for invalid property names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidPropertyNameError {
    #[error("Property name cannot start with a digit")]
    StartsWithDigit,

    #[error("Property name cannot start with underscore")]
    StartsWithUnderscore,

    #[error("Property name cannot end with underscore")]
    EndsWithUnderscore,

    #[error("Property name contains consecutive underscores")]
    ConsecutiveUnderscores,

    #[error("Property name must be lowercase (found uppercase: '{0}')")]
    NotSnakeCase(char),

    #[error("Property name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Property name cannot be empty")]
    Empty,
}

/// A PropertyName represents a validated property name in dop.
/// Property names follow the same snake_case rules as variable names.
#[derive(Debug, Clone)]
pub struct PropertyName {
    value: String,
}

impl PropertyName {
    /// Create a new PropertyName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidPropertyNameError> {
        Self::validate(name)?;
        Ok(PropertyName {
            value: name.to_string(),
        })
    }

    /// Validate a property name string (snake_case only)
    fn validate(name: &str) -> Result<(), InvalidPropertyNameError> {
        if name.is_empty() {
            return Err(InvalidPropertyNameError::Empty);
        }

        // Check for leading/trailing underscores
        if name.starts_with('_') {
            return Err(InvalidPropertyNameError::StartsWithUnderscore);
        }

        if name.ends_with('_') {
            return Err(InvalidPropertyNameError::EndsWithUnderscore);
        }

        // Check for consecutive underscores
        if name.contains("__") {
            return Err(InvalidPropertyNameError::ConsecutiveUnderscores);
        }

        let mut chars = name.chars();
        let first_char = chars.next().unwrap();

        // First character must be a lowercase letter
        if first_char.is_ascii_digit() {
            return Err(InvalidPropertyNameError::StartsWithDigit);
        }

        if !first_char.is_ascii_lowercase() {
            if first_char.is_ascii_uppercase() {
                return Err(InvalidPropertyNameError::NotSnakeCase(first_char));
            } else {
                return Err(InvalidPropertyNameError::InvalidCharacter(first_char));
            }
        }

        // Remaining characters must be lowercase letters, digits, or underscores
        for c in chars {
            if c.is_ascii_uppercase() {
                return Err(InvalidPropertyNameError::NotSnakeCase(c));
            } else if !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '_' {
                return Err(InvalidPropertyNameError::InvalidCharacter(c));
            }
        }

        Ok(())
    }

    pub fn as_str(&self) -> &str {
        &self.value
    }
}

impl Display for PropertyName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.value)
    }
}

impl PartialEq for PropertyName {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for PropertyName {}

impl PartialOrd for PropertyName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PropertyName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl TryFrom<String> for PropertyName {
    type Error = InvalidPropertyNameError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        PropertyName::new(&s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_property_names() {
        assert!(PropertyName::new("valid_name").is_ok());
        assert!(PropertyName::new("x").is_ok());
        assert!(PropertyName::new("name_with_underscores").is_ok());
        assert!(PropertyName::new("name123").is_ok());
        assert!(PropertyName::new("foo_bar").is_ok());
        assert!(PropertyName::new("foo_bar_123").is_ok());
        assert!(PropertyName::new("a1_b2_c3").is_ok());
    }

    #[test]
    fn test_invalid_property_names() {
        // Original invalid cases
        assert_eq!(
            PropertyName::new("123invalid"),
            Err(InvalidPropertyNameError::StartsWithDigit)
        );
        assert_eq!(
            PropertyName::new("_starts_with_underscore"),
            Err(InvalidPropertyNameError::StartsWithUnderscore)
        );
        assert_eq!(
            PropertyName::new("has-dash"),
            Err(InvalidPropertyNameError::InvalidCharacter('-'))
        );
        assert_eq!(
            PropertyName::new("has space"),
            Err(InvalidPropertyNameError::InvalidCharacter(' '))
        );
        assert_eq!(PropertyName::new(""), Err(InvalidPropertyNameError::Empty));

        // Snake_case validation tests
        assert_eq!(
            PropertyName::new("foo_bar_"),
            Err(InvalidPropertyNameError::EndsWithUnderscore)
        );
        assert_eq!(
            PropertyName::new("foo__bar"),
            Err(InvalidPropertyNameError::ConsecutiveUnderscores)
        );
        assert_eq!(
            PropertyName::new("FooBar"),
            Err(InvalidPropertyNameError::NotSnakeCase('F'))
        );
        assert_eq!(
            PropertyName::new("foo_Bar"),
            Err(InvalidPropertyNameError::NotSnakeCase('B'))
        );
        assert_eq!(
            PropertyName::new("validName"),
            Err(InvalidPropertyNameError::NotSnakeCase('N'))
        );
    }
}