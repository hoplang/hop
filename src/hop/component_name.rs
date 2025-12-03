use std::fmt;
use thiserror::Error;

/// Error type for invalid component names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidComponentNameError {
    #[error("Component name cannot be empty")]
    Empty,

    #[error("Component name must start with an uppercase letter")]
    NotPascalCase,

    #[error(
        "Component name contains invalid character: '{0}'. Only alphanumeric characters are allowed"
    )]
    InvalidCharacter(char),
}

/// A type-safe wrapper for component names in the hop system.
/// Component names must follow PascalCase convention and contain only alphanumeric characters.
///
/// Examples: "Button", "UserProfile", "NavBar", "Component123"
/// Invalid: "button", "user-profile", "nav_bar", "component-name"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ComponentName(String);

impl ComponentName {
    /// Create a new ComponentName from a string, validating it
    ///
    /// Returns an error describing why the component name is invalid.
    /// Valid component names:
    /// - Must not be empty
    /// - Must start with an uppercase letter (PascalCase)
    /// - Must contain only alphanumeric characters
    pub fn new(name: String) -> Result<Self, InvalidComponentNameError> {
        Self::validate(&name)?;
        Ok(ComponentName(name))
    }

    /// Validate a component name string
    fn validate(name: &str) -> Result<(), InvalidComponentNameError> {
        // Must not be empty
        if name.is_empty() {
            return Err(InvalidComponentNameError::Empty);
        }

        // Must start with an uppercase letter (PascalCase)
        let mut chars = name.chars();
        match chars.next() {
            Some(c) if c.is_ascii_uppercase() => {
                // Rest of the name should be alphanumeric
                for c in chars {
                    if !c.is_ascii_alphanumeric() {
                        return Err(InvalidComponentNameError::InvalidCharacter(c));
                    }
                }
            }
            _ => return Err(InvalidComponentNameError::NotPascalCase),
        }

        Ok(())
    }

    /// Get the component name as a string slice
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Convert the component name to PascalCase
    ///
    /// Since ComponentName is already in PascalCase, this just returns a copy of the string.
    /// This method exists for clarity and consistency with other case conversion methods.
    ///
    /// Examples:
    /// - "UserProfile" -> "UserProfile"
    /// - "Button" -> "Button"
    /// - "NavBar" -> "NavBar"
    pub fn to_pascal_case(&self) -> String {
        self.0.clone()
    }

    /// Convert the component name to camelCase
    ///
    /// Examples:
    /// - "UserProfile" -> "userProfile"
    /// - "Button" -> "button"
    /// - "NavBar" -> "navBar"
    pub fn to_camel_case(&self) -> String {
        if self.0.is_empty() {
            return String::new();
        }

        let mut chars = self.0.chars();
        let first = chars.next().unwrap().to_ascii_lowercase();
        let rest: String = chars.collect();
        format!("{}{}", first, rest)
    }

    /// Convert the component name to snake_case
    ///
    /// Examples:
    /// - "UserProfile" -> "user_profile"
    /// - "Button" -> "button"
    /// - "NavBar" -> "nav_bar"
    pub fn to_snake_case(&self) -> String {
        let mut result = String::new();
        let mut prev_was_lowercase = false;

        for (i, ch) in self.0.chars().enumerate() {
            if ch.is_uppercase() {
                // Add underscore before uppercase if not first char and prev was lowercase
                if i > 0 && prev_was_lowercase {
                    result.push('_');
                }
                result.push(ch.to_ascii_lowercase());
                prev_was_lowercase = true;
            } else {
                result.push(ch);
                prev_was_lowercase = ch.is_lowercase();
            }
        }

        result
    }
}

impl fmt::Display for ComponentName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for ComponentName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_component_names() {
        let cases = [
            "Button",
            "UserProfile",
            "NavBar",
            "Component123",
            "MyComponent",
            "A",
            "ABC123",
            "Main",
            "Foo",
            "Bar",
        ];

        for input in cases {
            assert!(
                ComponentName::new(input.to_string()).is_ok(),
                "{input:?} should be valid"
            );
        }
    }

    #[test]
    fn test_invalid_component_names() {
        let cases = [
            ("", InvalidComponentNameError::Empty),
            ("button", InvalidComponentNameError::NotPascalCase),
            ("userProfile", InvalidComponentNameError::NotPascalCase),
            ("navBar", InvalidComponentNameError::NotPascalCase),
            (
                "User-Profile",
                InvalidComponentNameError::InvalidCharacter('-'),
            ),
            (
                "User_Profile",
                InvalidComponentNameError::InvalidCharacter('_'),
            ),
            (
                "User Profile",
                InvalidComponentNameError::InvalidCharacter(' '),
            ),
            ("Button!", InvalidComponentNameError::InvalidCharacter('!')),
            ("Nav.Bar", InvalidComponentNameError::InvalidCharacter('.')),
        ];

        for (input, expected) in cases {
            assert_eq!(
                ComponentName::new(input.to_string()),
                Err(expected),
                "{input:?} should be invalid"
            );
        }
    }

    #[test]
    fn test_display() {
        let name = ComponentName::new("Button".to_string()).unwrap();
        assert_eq!(format!("{}", name), "Button");
        assert_eq!(name.to_string(), "Button");
    }

    #[test]
    fn test_as_str() {
        let name = ComponentName::new("UserProfile".to_string()).unwrap();
        assert_eq!(name.as_str(), "UserProfile");
        assert_eq!(name.as_ref(), "UserProfile");
    }

    #[test]
    fn test_case_conversions() {
        let cases = [
            ("Button", "Button", "button", "button"),
            ("UserProfile", "UserProfile", "userProfile", "user_profile"),
            ("NavBar", "NavBar", "navBar", "nav_bar"),
            ("Main", "Main", "main", "main"),
            ("Test", "Test", "test", "test"),
            (
                "TestAuthCheck",
                "TestAuthCheck",
                "testAuthCheck",
                "test_auth_check",
            ),
        ];

        for (input, pascal, camel, snake) in cases {
            let name = ComponentName::new(input.to_string()).unwrap();
            assert_eq!(name.to_pascal_case(), pascal, "{input} to_pascal_case");
            assert_eq!(name.to_camel_case(), camel, "{input} to_camel_case");
            assert_eq!(name.to_snake_case(), snake, "{input} to_snake_case");
        }
    }
}
