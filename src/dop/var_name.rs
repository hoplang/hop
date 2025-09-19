use std::fmt::{self, Display};
use thiserror::Error;

/// Error type for invalid variable names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidVarNameError {
    #[error("Variable name cannot start with a digit")]
    StartsWithDigit,

    #[error("Variable name cannot start with underscore")]
    StartsWithUnderscore,

    #[error("Variable name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Variable name cannot be empty")]
    Empty,
}

/// A VarName represents a validated variable name in dop.
#[derive(Debug, Clone)]
pub struct VarName {
    value: String,
}

impl VarName {
    /// Create a new VarName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidVarNameError> {
        Self::validate(name)?;
        Ok(VarName { value: name.to_string() })
    }

    /// Validate a variable name string
    fn validate(name: &str) -> Result<(), InvalidVarNameError> {
        if name.is_empty() {
            return Err(InvalidVarNameError::Empty);
        }

        let mut chars = name.chars();
        let first_char = chars.next().unwrap();

        if first_char.is_ascii_digit() {
            return Err(InvalidVarNameError::StartsWithDigit);
        }

        if first_char == '_' {
            return Err(InvalidVarNameError::StartsWithUnderscore);
        }

        if !first_char.is_ascii_alphabetic() {
            return Err(InvalidVarNameError::InvalidCharacter(first_char));
        }

        for c in chars {
            if !c.is_ascii_alphanumeric() && c != '_' {
                return Err(InvalidVarNameError::InvalidCharacter(c));
            }
        }

        Ok(())
    }
    pub fn as_str(&self) -> &str {
        &self.value
    }
}

impl Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.value)
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
    fn test_valid_var_names() {
        assert!(VarName::new("validName").is_ok());
        assert!(VarName::new("x").is_ok());
        assert!(VarName::new("name_with_underscores").is_ok());
        assert!(VarName::new("name123").is_ok());
    }

    #[test]
    fn test_invalid_var_names() {
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
        assert_eq!(
            VarName::new(""),
            Err(InvalidVarNameError::Empty)
        );
    }
}
