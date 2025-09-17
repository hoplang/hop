use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::parse_error::ParseError;

/// A VarName represents a validated variable name in dop.
#[derive(Debug, Clone)]
pub struct VarName {
    value: DocumentRange,
}

impl VarName {
    pub fn new(value: DocumentRange) -> Result<Self, ParseError> {
        let mut chars = value.as_str().chars();
        if !chars.next().is_some_and(|c| c.is_ascii_alphabetic())
            || !chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            return Err(ParseError::InvalidVariableName {
                name: value.to_string_span(),
                range: value.clone(),
            });
        }
        Ok(VarName { value })
    }
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }
    pub fn range(&self) -> &DocumentRange {
        &self.value
    }
    pub fn to_string_span(&self) -> StringSpan {
        self.value.to_string_span()
    }
    pub fn value(&self) -> &DocumentRange {
        &self.value
    }
}

impl Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
}

impl TryFrom<String> for VarName {
    type Error = ParseError;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        let value = DocumentRange::new(s);
        VarName::new(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::document_cursor::DocumentRange;

    #[test]
    fn test_valid_var_names() {
        let range = DocumentRange::new("validName".to_string());
        assert!(VarName::new(range).is_ok());

        let range = DocumentRange::new("x".to_string());
        assert!(VarName::new(range).is_ok());

        let range = DocumentRange::new("name_with_underscores".to_string());
        assert!(VarName::new(range).is_ok());

        let range = DocumentRange::new("name123".to_string());
        assert!(VarName::new(range).is_ok());
    }

    #[test]
    fn test_invalid_var_names() {
        let range = DocumentRange::new("123invalid".to_string());
        assert!(VarName::new(range).is_err());

        let range = DocumentRange::new("_starts_with_underscore".to_string());
        assert!(VarName::new(range).is_err());

        let range = DocumentRange::new("has-dash".to_string());
        assert!(VarName::new(range).is_err());

        let range = DocumentRange::new("has space".to_string());
        assert!(VarName::new(range).is_err());
    }
}
