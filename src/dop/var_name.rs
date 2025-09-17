use std::fmt::{self, Display};

use crate::document::document_cursor::{DocumentRange, StringSpan};
use crate::dop::parse_error::ParseError;

/// A VarName represents a validated variable name in dop.
#[derive(Debug, Clone)]
pub struct VarName {
    value: DocumentRange,
}

impl Display for VarName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.value.as_str())
    }
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