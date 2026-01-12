use std::fmt::{self, Display};

use crate::document::document::CheapString;
use thiserror::Error;

/// Error type for invalid variable names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidVarNameError {
    #[error("Variable name cannot start with a digit")]
    StartsWithDigit,

    #[error("Variable name cannot start with underscore")]
    StartsWithUnderscore,

    #[error("Variable name cannot end with underscore")]
    EndsWithUnderscore,

    #[error("Variable name contains consecutive underscores")]
    ConsecutiveUnderscores,

    #[error("Variable name must be lowercase (found uppercase: '{0}')")]
    NotSnakeCase(char),

    #[error("Variable name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Variable name cannot be empty")]
    Empty,
}

/// A VarName represents a validated variable name in dop.
#[derive(Debug, Clone)]
pub struct VarName {
    value: CheapString,
}

impl VarName {
    /// Create a new VarName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidVarNameError> {
        Self::validate(name)?;
        Ok(VarName {
            value: CheapString::new(name.to_string()),
        })
    }

    /// Create a new VarName from a CheapString, validating it
    pub fn from_cheap_string(name: CheapString) -> Result<Self, InvalidVarNameError> {
        Self::validate(name.as_str())?;
        Ok(VarName { value: name })
    }

    /// Validate a variable name string (snake_case only)
    fn validate(name: &str) -> Result<(), InvalidVarNameError> {
        if name.is_empty() {
            return Err(InvalidVarNameError::Empty);
        }

        // Check for leading/trailing underscores
        if name.starts_with('_') {
            return Err(InvalidVarNameError::StartsWithUnderscore);
        }

        if name.ends_with('_') {
            return Err(InvalidVarNameError::EndsWithUnderscore);
        }

        // Check for consecutive underscores
        if name.contains("__") {
            return Err(InvalidVarNameError::ConsecutiveUnderscores);
        }

        let mut chars = name.chars();
        let first_char = chars.next().unwrap();

        // First character must be a lowercase letter
        if first_char.is_ascii_digit() {
            return Err(InvalidVarNameError::StartsWithDigit);
        }

        if !first_char.is_ascii_lowercase() {
            if first_char.is_ascii_uppercase() {
                return Err(InvalidVarNameError::NotSnakeCase(first_char));
            } else {
                return Err(InvalidVarNameError::InvalidCharacter(first_char));
            }
        }

        // Remaining characters must be lowercase letters, digits, or underscores
        for c in chars {
            if c.is_ascii_uppercase() {
                return Err(InvalidVarNameError::NotSnakeCase(c));
            } else if !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '_' {
                return Err(InvalidVarNameError::InvalidCharacter(c));
            }
        }

        Ok(())
    }
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }

    /// Convert the variable name to PascalCase
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

impl Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl PartialEq for VarName {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for VarName {}

impl TryFrom<String> for VarName {
    type Error = InvalidVarNameError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        VarName::new(&s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_var_names() {
        assert!(VarName::new("valid_name").is_ok());
        assert!(VarName::new("x").is_ok());
        assert!(VarName::new("name_with_underscores").is_ok());
        assert!(VarName::new("name123").is_ok());
        assert!(VarName::new("foo_bar").is_ok());
        assert!(VarName::new("foo_bar_123").is_ok());
        assert!(VarName::new("a1_b2_c3").is_ok());
    }

    #[test]
    fn invalid_var_names() {
        // Original invalid cases
        assert_eq!(
            VarName::new("123invalid"),
            Err(InvalidVarNameError::StartsWithDigit)
        );
        assert_eq!(
            VarName::new("_starts_with_underscore"),
            Err(InvalidVarNameError::StartsWithUnderscore)
        );
        assert_eq!(
            VarName::new("has-dash"),
            Err(InvalidVarNameError::InvalidCharacter('-'))
        );
        assert_eq!(
            VarName::new("has space"),
            Err(InvalidVarNameError::InvalidCharacter(' '))
        );
        assert_eq!(VarName::new(""), Err(InvalidVarNameError::Empty));

        // New snake_case validation tests
        assert_eq!(
            VarName::new("foo_bar_"),
            Err(InvalidVarNameError::EndsWithUnderscore)
        );
        assert_eq!(
            VarName::new("foo__bar"),
            Err(InvalidVarNameError::ConsecutiveUnderscores)
        );
        assert_eq!(
            VarName::new("FooBar"),
            Err(InvalidVarNameError::NotSnakeCase('F'))
        );
        assert_eq!(
            VarName::new("foo_Bar"),
            Err(InvalidVarNameError::NotSnakeCase('B'))
        );
        assert_eq!(
            VarName::new("validName"),
            Err(InvalidVarNameError::NotSnakeCase('N'))
        );
    }
}
