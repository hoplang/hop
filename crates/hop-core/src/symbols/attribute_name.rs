use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

use crate::document::CheapString;
use crate::symbols::var_name::VarName;
use thiserror::Error;

/// Error type for invalid attribute names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidAttributeNameError {
    #[error("Attribute name must start with a letter (found '{0}')")]
    StartsWithNonLetter(char),

    #[error("Attribute name contains invalid character: '{0}'")]
    InvalidCharacter(char),

    #[error("Attribute name cannot be empty")]
    Empty,
}

/// An AttributeName represents a validated attribute name, spelled as it was
/// written.
///
/// Two names are equal when they differ only in ASCII case, as HTML compares
/// attribute names, so `id` and `ID` name the same attribute.
#[derive(Debug, Clone)]
pub struct AttributeName {
    value: CheapString,
}

impl PartialEq for AttributeName {
    fn eq(&self, other: &Self) -> bool {
        self.value
            .as_str()
            .eq_ignore_ascii_case(other.value.as_str())
    }
}

impl Eq for AttributeName {}

/// Hashes the name in lowercase, so names that are equal hash the same.
impl Hash for AttributeName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for byte in self.value.as_str().bytes() {
            byte.to_ascii_lowercase().hash(state);
        }
    }
}

impl AttributeName {
    pub fn new(name: CheapString) -> Result<Self, InvalidAttributeNameError> {
        Self::validate(name.as_str())?;
        Ok(AttributeName { value: name })
    }

    #[cfg(test)]
    pub fn parse(name: &str) -> Result<Self, InvalidAttributeNameError> {
        Self::new(CheapString::new(name.to_string()))
    }

    /// Validate an attribute name: a letter followed by letters, digits and
    /// any of `-`, `_`, `:` and `.`.
    fn validate(name: &str) -> Result<(), InvalidAttributeNameError> {
        let mut chars = name.chars();
        let Some(first_char) = chars.next() else {
            return Err(InvalidAttributeNameError::Empty);
        };
        if !first_char.is_ascii_alphabetic() {
            return Err(InvalidAttributeNameError::StartsWithNonLetter(first_char));
        }
        for c in chars {
            if !c.is_ascii_alphanumeric() && !matches!(c, '-' | '_' | ':' | '.') {
                return Err(InvalidAttributeNameError::InvalidCharacter(c));
            }
        }
        Ok(())
    }

    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

/// Every variable name is also an attribute name, which lets a parameter be
/// passed as an attribute.
impl From<VarName> for AttributeName {
    fn from(name: VarName) -> Self {
        AttributeName {
            value: name.to_cheap_string(),
        }
    }
}

impl Display for AttributeName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn accept(input: &str) {
        assert!(AttributeName::parse(input).is_ok());
    }

    fn reject(input: &str, expected: InvalidAttributeNameError) {
        assert_eq!(AttributeName::parse(input), Err(expected));
    }

    #[test]
    fn accepts_simple_attribute_name() {
        accept("class");
    }

    #[test]
    fn accepts_attribute_name_with_dashes() {
        accept("aria-label");
    }

    #[test]
    fn accepts_attribute_name_with_colon_and_dot() {
        accept("xlink:href");
        accept("x.y");
    }

    #[test]
    fn accepts_mixed_case_attribute_name() {
        accept("viewBox");
    }

    #[test]
    fn accepts_attribute_name_with_digits_and_underscores() {
        accept("data_x1");
    }

    #[test]
    fn rejects_empty_attribute_name() {
        reject("", InvalidAttributeNameError::Empty);
    }

    #[test]
    fn rejects_attribute_name_starting_with_digit() {
        reject("1x", InvalidAttributeNameError::StartsWithNonLetter('1'));
    }

    #[test]
    fn rejects_attribute_name_starting_with_dash() {
        reject("-x", InvalidAttributeNameError::StartsWithNonLetter('-'));
    }

    #[test]
    fn rejects_attribute_name_with_space() {
        reject("my label", InvalidAttributeNameError::InvalidCharacter(' '));
    }

    #[test]
    fn rejects_attribute_name_with_quote() {
        reject("a\"b", InvalidAttributeNameError::InvalidCharacter('"'));
    }

    #[test]
    fn compares_attribute_names_ignoring_case() {
        let parse = |name| AttributeName::parse(name).unwrap();
        assert_eq!(parse("id"), parse("ID"));
        assert_eq!(parse("viewBox"), parse("viewbox"));
        assert_ne!(parse("id"), parse("idx"));
    }

    #[test]
    fn keeps_the_spelling_of_an_attribute_name() {
        assert_eq!(AttributeName::parse("viewBox").unwrap().as_str(), "viewBox");
    }
}
