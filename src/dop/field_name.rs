use std::fmt::{self, Display};
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

/// A FieldName represents a validated field name in dop.
/// Field names follow the same snake_case rules as variable names.
#[derive(Debug, Clone)]
pub struct FieldName {
    value: String,
}

impl FieldName {
    /// Create a new FieldName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidFieldNameError> {
        Self::validate(name)?;
        Ok(FieldName {
            value: name.to_string(),
        })
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
        &self.value
    }

    /// Convert the field name to PascalCase
    pub fn to_pascal_case(&self) -> String {
        self.value
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
        f.write_str(&self.value)
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

    #[test]
    fn test_valid_field_names() {
        assert!(FieldName::new("valid_name").is_ok());
        assert!(FieldName::new("x").is_ok());
        assert!(FieldName::new("name_with_underscores").is_ok());
        assert!(FieldName::new("name123").is_ok());
        assert!(FieldName::new("foo_bar").is_ok());
        assert!(FieldName::new("foo_bar_123").is_ok());
        assert!(FieldName::new("a1_b2_c3").is_ok());
    }

    #[test]
    fn test_invalid_field_names() {
        // Original invalid cases
        assert_eq!(
            FieldName::new("123invalid"),
            Err(InvalidFieldNameError::StartsWithDigit)
        );
        assert_eq!(
            FieldName::new("_starts_with_underscore"),
            Err(InvalidFieldNameError::StartsWithUnderscore)
        );
        assert_eq!(
            FieldName::new("has-dash"),
            Err(InvalidFieldNameError::InvalidCharacter('-'))
        );
        assert_eq!(
            FieldName::new("has space"),
            Err(InvalidFieldNameError::InvalidCharacter(' '))
        );
        assert_eq!(FieldName::new(""), Err(InvalidFieldNameError::Empty));

        // Snake_case validation tests
        assert_eq!(
            FieldName::new("foo_bar_"),
            Err(InvalidFieldNameError::EndsWithUnderscore)
        );
        assert_eq!(
            FieldName::new("foo__bar"),
            Err(InvalidFieldNameError::ConsecutiveUnderscores)
        );
        assert_eq!(
            FieldName::new("FooBar"),
            Err(InvalidFieldNameError::NotSnakeCase('F'))
        );
        assert_eq!(
            FieldName::new("foo_Bar"),
            Err(InvalidFieldNameError::NotSnakeCase('B'))
        );
        assert_eq!(
            FieldName::new("validName"),
            Err(InvalidFieldNameError::NotSnakeCase('N'))
        );
    }

    #[test]
    fn test_to_pascal_case() {
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
