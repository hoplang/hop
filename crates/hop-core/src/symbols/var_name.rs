use std::fmt::{self, Display};

use crate::document::CheapString;
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

/// A VarName represents a validated variable name.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
}

impl Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl TryFrom<String> for VarName {
    type Error = InvalidVarNameError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        VarName::new(&s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn accept(input: &str) {
        assert!(VarName::new(input).is_ok());
    }

    fn reject(input: &str, expected: InvalidVarNameError) {
        assert_eq!(VarName::new(input), Err(expected));
    }

    #[test]
    fn accepts_simple_var_name() {
        accept("valid_name");
    }

    #[test]
    fn accepts_single_letter_var_name() {
        accept("x");
    }

    #[test]
    fn accepts_var_name_with_underscores() {
        accept("name_with_underscores");
    }

    #[test]
    fn accepts_var_name_with_trailing_digits() {
        accept("name123");
    }

    #[test]
    fn accepts_snake_case_var_name() {
        accept("foo_bar");
    }

    #[test]
    fn accepts_snake_case_var_name_with_digits() {
        accept("foo_bar_123");
    }

    #[test]
    fn accepts_alphanumeric_snake_case_var_name() {
        accept("a1_b2_c3");
    }

    #[test]
    fn rejects_var_name_starting_with_digit() {
        reject("123invalid", InvalidVarNameError::StartsWithDigit);
    }

    #[test]
    fn rejects_var_name_starting_with_underscore() {
        reject(
            "_starts_with_underscore",
            InvalidVarNameError::StartsWithUnderscore,
        );
    }

    #[test]
    fn rejects_var_name_with_dash() {
        reject("has-dash", InvalidVarNameError::InvalidCharacter('-'));
    }

    #[test]
    fn rejects_var_name_with_space() {
        reject("has space", InvalidVarNameError::InvalidCharacter(' '));
    }

    #[test]
    fn rejects_empty_var_name() {
        reject("", InvalidVarNameError::Empty);
    }

    #[test]
    fn rejects_var_name_ending_with_underscore() {
        reject("foo_bar_", InvalidVarNameError::EndsWithUnderscore);
    }

    #[test]
    fn rejects_var_name_with_consecutive_underscores() {
        reject("foo__bar", InvalidVarNameError::ConsecutiveUnderscores);
    }

    #[test]
    fn rejects_pascal_case_var_name() {
        reject("FooBar", InvalidVarNameError::NotSnakeCase('F'));
    }

    #[test]
    fn rejects_var_name_with_uppercase_after_underscore() {
        reject("foo_Bar", InvalidVarNameError::NotSnakeCase('B'));
    }

    #[test]
    fn rejects_camel_case_var_name() {
        reject("validName", InvalidVarNameError::NotSnakeCase('N'));
    }
}
