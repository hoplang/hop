use std::fmt::{self, Display};

use crate::document::CheapString;
use thiserror::Error;

/// Error type for invalid field names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidFieldNameError {
    #[error("Field name cannot start with a digit")]
    StartsWithDigit,

    #[error("Field name cannot start with underscore")]
    StartsWithUnderscore,

    #[error("Field name cannot end with underscore")]
    EndsWithUnderscore,

    #[error("Field name contains consecutive underscores")]
    ConsecutiveUnderscores,

    #[error("Field name must be lowercase (found uppercase: '{0}')")]
    NotSnakeCase(char),

    #[error("Field name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Field name cannot be empty")]
    Empty,
}

/// A FieldName represents a validated field name.
/// Field names follow the same snake_case rules as variable names.
#[derive(Debug, Clone)]
pub struct FieldName {
    value: CheapString,
}

impl FieldName {
    /// Create a new FieldName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidFieldNameError> {
        Self::validate(name)?;
        Ok(FieldName {
            value: CheapString::new(name.to_string()),
        })
    }

    /// Create a new FieldName from a CheapString, validating it
    pub fn from_cheap_string(name: CheapString) -> Result<Self, InvalidFieldNameError> {
        Self::validate(name.as_str())?;
        Ok(FieldName { value: name })
    }

    /// Validate a field name string (snake_case only)
    fn validate(name: &str) -> Result<(), InvalidFieldNameError> {
        if name.is_empty() {
            return Err(InvalidFieldNameError::Empty);
        }

        // Check for leading/trailing underscores
        if name.starts_with('_') {
            return Err(InvalidFieldNameError::StartsWithUnderscore);
        }

        if name.ends_with('_') {
            return Err(InvalidFieldNameError::EndsWithUnderscore);
        }

        // Check for consecutive underscores
        if name.contains("__") {
            return Err(InvalidFieldNameError::ConsecutiveUnderscores);
        }

        let mut chars = name.chars();
        let first_char = chars.next().unwrap();

        // First character must be a lowercase letter
        if first_char.is_ascii_digit() {
            return Err(InvalidFieldNameError::StartsWithDigit);
        }

        if !first_char.is_ascii_lowercase() {
            if first_char.is_ascii_uppercase() {
                return Err(InvalidFieldNameError::NotSnakeCase(first_char));
            } else {
                return Err(InvalidFieldNameError::InvalidCharacter(first_char));
            }
        }

        // Remaining characters must be lowercase letters, digits, or underscores
        for c in chars {
            if c.is_ascii_uppercase() {
                return Err(InvalidFieldNameError::NotSnakeCase(c));
            } else if !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '_' {
                return Err(InvalidFieldNameError::InvalidCharacter(c));
            }
        }

        Ok(())
    }

    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }

    /// Convert the field name to PascalCase
    pub fn to_pascal_case(&self) -> String {
        self.value
            .as_str()
            .split('_')
            .map(|segment| {
                let mut chars = segment.chars();
                match chars.next() {
                    Some(first) => {
                        let mut result = first.to_uppercase().to_string();
                        result.push_str(chars.as_str());
                        result
                    }
                    None => String::new(),
                }
            })
            .collect()
    }
}

impl Display for FieldName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl std::hash::Hash for FieldName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialEq for FieldName {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for FieldName {}

impl PartialOrd for FieldName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for FieldName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl TryFrom<String> for FieldName {
    type Error = InvalidFieldNameError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        FieldName::new(&s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn accept(input: &str) {
        assert!(FieldName::new(input).is_ok());
    }

    fn reject(input: &str, expected: InvalidFieldNameError) {
        assert_eq!(FieldName::new(input), Err(expected));
    }

    #[test]
    fn accepts_simple_field_name() {
        accept("valid_name");
    }

    #[test]
    fn accepts_single_letter_field_name() {
        accept("x");
    }

    #[test]
    fn accepts_field_name_with_underscores() {
        accept("name_with_underscores");
    }

    #[test]
    fn accepts_field_name_with_trailing_digits() {
        accept("name123");
    }

    #[test]
    fn accepts_snake_case_field_name() {
        accept("foo_bar");
    }

    #[test]
    fn accepts_snake_case_field_name_with_digits() {
        accept("foo_bar_123");
    }

    #[test]
    fn accepts_alphanumeric_snake_case_field_name() {
        accept("a1_b2_c3");
    }

    #[test]
    fn rejects_field_name_starting_with_digit() {
        reject("123invalid", InvalidFieldNameError::StartsWithDigit);
    }

    #[test]
    fn rejects_field_name_starting_with_underscore() {
        reject(
            "_starts_with_underscore",
            InvalidFieldNameError::StartsWithUnderscore,
        );
    }

    #[test]
    fn rejects_field_name_with_dash() {
        reject("has-dash", InvalidFieldNameError::InvalidCharacter('-'));
    }

    #[test]
    fn rejects_field_name_with_space() {
        reject("has space", InvalidFieldNameError::InvalidCharacter(' '));
    }

    #[test]
    fn rejects_empty_field_name() {
        reject("", InvalidFieldNameError::Empty);
    }

    #[test]
    fn rejects_field_name_ending_with_underscore() {
        reject("foo_bar_", InvalidFieldNameError::EndsWithUnderscore);
    }

    #[test]
    fn rejects_field_name_with_consecutive_underscores() {
        reject("foo__bar", InvalidFieldNameError::ConsecutiveUnderscores);
    }

    #[test]
    fn rejects_pascal_case_field_name() {
        reject("FooBar", InvalidFieldNameError::NotSnakeCase('F'));
    }

    #[test]
    fn rejects_field_name_with_uppercase_after_underscore() {
        reject("foo_Bar", InvalidFieldNameError::NotSnakeCase('B'));
    }

    #[test]
    fn rejects_camel_case_field_name() {
        reject("validName", InvalidFieldNameError::NotSnakeCase('N'));
    }

    #[test]
    fn to_pascal_case() {
        assert_eq!(
            FieldName::new("foo_bar").unwrap().to_pascal_case(),
            "FooBar"
        );
        assert_eq!(
            FieldName::new("hello_world").unwrap().to_pascal_case(),
            "HelloWorld"
        );
        assert_eq!(FieldName::new("x").unwrap().to_pascal_case(), "X");
        assert_eq!(
            FieldName::new("foo_bar_baz").unwrap().to_pascal_case(),
            "FooBarBaz"
        );
    }
}
