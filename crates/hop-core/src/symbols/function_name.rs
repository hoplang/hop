use std::fmt::{self, Display};

use crate::document::CheapString;
use crate::symbols::type_name::TypeName;
use crate::symbols::var_name::VarName;
use thiserror::Error;

/// Error type for invalid function names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidFunctionNameError {
    #[error("Function name must start with an ASCII letter")]
    DoesNotStartWithLetter,

    #[error("Function name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Function name cannot be empty")]
    Empty,
}

/// A FunctionName represents a validated function name.
///
/// Unlike `TypeName` (PascalCase-only) or `VarName` (snake_case-only), a
/// FunctionName accepts both: it is the IR-level name for both recursive
/// components (PascalCase) and source-level functions (snake_case).
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionName {
    value: CheapString,
}

impl FunctionName {
    /// Create a new FunctionName from a string, validating it
    pub fn new(name: &str) -> Result<Self, InvalidFunctionNameError> {
        Self::validate(name)?;
        Ok(FunctionName {
            value: CheapString::new(name.to_string()),
        })
    }

    /// Validate a function name string (ASCII letter followed by ASCII
    /// alphanumerics/underscores)
    fn validate(name: &str) -> Result<(), InvalidFunctionNameError> {
        if name.is_empty() {
            return Err(InvalidFunctionNameError::Empty);
        }

        let mut chars = name.chars();
        let first_char = chars.next().unwrap();

        if !first_char.is_ascii_alphabetic() {
            return Err(InvalidFunctionNameError::DoesNotStartWithLetter);
        }

        for c in chars {
            if !c.is_ascii_alphanumeric() && c != '_' {
                return Err(InvalidFunctionNameError::InvalidCharacter(c));
            }
        }

        Ok(())
    }

    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }

    /// Convert the function name to snake_case (identity for names that are
    /// already snake_case).
    pub fn to_snake_case(&self) -> String {
        let mut result = String::new();
        let mut prev_was_lowercase = false;

        for (i, ch) in self.value.as_str().chars().enumerate() {
            if ch.is_ascii_uppercase() {
                if i > 0 && prev_was_lowercase {
                    result.push('_');
                }
                result.push(ch.to_ascii_lowercase());
                prev_was_lowercase = true;
            } else {
                result.push(ch);
                prev_was_lowercase = ch.is_ascii_lowercase();
            }
        }

        result
    }

    /// Convert the function name to PascalCase (identity for names that are
    /// already PascalCase).
    pub fn to_pascal_case(&self) -> String {
        let mut result = String::new();
        let mut capitalize_next = true;

        for ch in self.value.as_str().chars() {
            if ch == '_' {
                capitalize_next = true;
            } else if capitalize_next {
                result.push(ch.to_ascii_uppercase());
                capitalize_next = false;
            } else {
                result.push(ch);
            }
        }

        result
    }
}

impl Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl AsRef<str> for FunctionName {
    fn as_ref(&self) -> &str {
        self.value.as_str()
    }
}

impl From<TypeName> for FunctionName {
    fn from(name: TypeName) -> Self {
        FunctionName::new(name.as_str())
            .expect("every valid TypeName should be a valid FunctionName")
    }
}

impl From<VarName> for FunctionName {
    fn from(name: VarName) -> Self {
        FunctionName::new(name.as_str())
            .expect("every valid VarName should be a valid FunctionName")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn accept(input: &str) {
        assert!(FunctionName::new(input).is_ok());
    }

    fn reject(input: &str, expected: InvalidFunctionNameError) {
        assert_eq!(FunctionName::new(input), Err(expected));
    }

    #[test]
    fn accepts_snake_case_function_name() {
        accept("format_price");
    }

    #[test]
    fn accepts_pascal_case_function_name() {
        accept("FormatPrice");
    }

    #[test]
    fn accepts_single_letter_function_name() {
        accept("x");
    }

    #[test]
    fn accepts_function_name_with_digits() {
        accept("format123");
    }

    #[test]
    fn rejects_function_name_starting_with_digit() {
        reject(
            "123format",
            InvalidFunctionNameError::DoesNotStartWithLetter,
        );
    }

    #[test]
    fn rejects_function_name_starting_with_underscore() {
        reject("_format", InvalidFunctionNameError::DoesNotStartWithLetter);
    }

    #[test]
    fn rejects_function_name_with_hyphen() {
        reject(
            "format-price",
            InvalidFunctionNameError::InvalidCharacter('-'),
        );
    }

    #[test]
    fn rejects_function_name_with_space() {
        reject(
            "format price",
            InvalidFunctionNameError::InvalidCharacter(' '),
        );
    }

    #[test]
    fn rejects_empty_function_name() {
        reject("", InvalidFunctionNameError::Empty);
    }

    #[test]
    fn to_snake_case_is_identity_for_snake_case_input() {
        assert_eq!(
            FunctionName::new("format_price").unwrap().to_snake_case(),
            "format_price"
        );
    }

    #[test]
    fn to_snake_case_converts_pascal_case_input() {
        assert_eq!(
            FunctionName::new("FormatPrice").unwrap().to_snake_case(),
            "format_price"
        );
    }

    #[test]
    fn to_pascal_case_is_identity_for_pascal_case_input() {
        assert_eq!(
            FunctionName::new("FormatPrice").unwrap().to_pascal_case(),
            "FormatPrice"
        );
    }

    #[test]
    fn to_pascal_case_converts_snake_case_input() {
        assert_eq!(
            FunctionName::new("format_price").unwrap().to_pascal_case(),
            "FormatPrice"
        );
    }

    #[test]
    fn from_type_name() {
        let type_name = TypeName::new("Counter").unwrap();
        assert_eq!(FunctionName::from(type_name).as_str(), "Counter");
    }

    #[test]
    fn from_var_name() {
        let var_name = VarName::new("format_price").unwrap();
        assert_eq!(FunctionName::from(var_name).as_str(), "format_price");
    }
}
