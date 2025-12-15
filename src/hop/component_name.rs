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
    fn should_accept_simple_pascal_case_name() {
        assert!(ComponentName::new("Button".to_string()).is_ok());
    }

    #[test]
    fn should_accept_multi_word_pascal_case_name() {
        assert!(ComponentName::new("UserProfile".to_string()).is_ok());
        assert!(ComponentName::new("NavBar".to_string()).is_ok());
        assert!(ComponentName::new("MyComponent".to_string()).is_ok());
    }

    #[test]
    fn should_accept_name_with_numbers() {
        assert!(ComponentName::new("Component123".to_string()).is_ok());
        assert!(ComponentName::new("ABC123".to_string()).is_ok());
    }

    #[test]
    fn should_accept_single_uppercase_letter() {
        assert!(ComponentName::new("A".to_string()).is_ok());
    }

    #[test]
    fn should_reject_empty_name() {
        assert_eq!(
            ComponentName::new("".to_string()),
            Err(InvalidComponentNameError::Empty)
        );
    }

    #[test]
    fn should_reject_name_that_starts_with_lowercase_letter() {
        assert_eq!(
            ComponentName::new("button".to_string()),
            Err(InvalidComponentNameError::NotPascalCase)
        );
    }

    #[test]
    fn should_reject_camel_case_name() {
        assert_eq!(
            ComponentName::new("userProfile".to_string()),
            Err(InvalidComponentNameError::NotPascalCase)
        );
        assert_eq!(
            ComponentName::new("navBar".to_string()),
            Err(InvalidComponentNameError::NotPascalCase)
        );
    }

    #[test]
    fn should_reject_name_with_hyphen() {
        assert_eq!(
            ComponentName::new("User-Profile".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('-'))
        );
    }

    #[test]
    fn should_reject_name_with_underscore() {
        assert_eq!(
            ComponentName::new("User_Profile".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('_'))
        );
    }

    #[test]
    fn should_reject_name_with_space() {
        assert_eq!(
            ComponentName::new("User Profile".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter(' '))
        );
    }

    #[test]
    fn should_reject_name_with_exclamation_mark() {
        assert_eq!(
            ComponentName::new("Button!".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('!'))
        );
    }

    #[test]
    fn should_reject_name_with_dot() {
        assert_eq!(
            ComponentName::new("Nav.Bar".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('.'))
        );
    }

    #[test]
    fn should_return_name_as_str() {
        let name = ComponentName::new("UserProfile".to_string()).unwrap();
        assert_eq!(name.as_str(), "UserProfile");
        assert_eq!(name.as_ref(), "UserProfile");
    }

    #[test]
    fn should_convert_to_pascal_case() {
        assert_eq!(
            ComponentName::new("Button".to_string())
                .unwrap()
                .to_pascal_case(),
            "Button"
        );
        assert_eq!(
            ComponentName::new("UserProfile".to_string())
                .unwrap()
                .to_pascal_case(),
            "UserProfile"
        );
    }

    #[test]
    fn should_convert_to_camel_case() {
        assert_eq!(
            ComponentName::new("Button".to_string())
                .unwrap()
                .to_camel_case(),
            "button"
        );
        assert_eq!(
            ComponentName::new("UserProfile".to_string())
                .unwrap()
                .to_camel_case(),
            "userProfile"
        );
        assert_eq!(
            ComponentName::new("NavBar".to_string())
                .unwrap()
                .to_camel_case(),
            "navBar"
        );
    }

    #[test]
    fn should_convert_to_snake_case() {
        assert_eq!(
            ComponentName::new("Button".to_string())
                .unwrap()
                .to_snake_case(),
            "button"
        );
        assert_eq!(
            ComponentName::new("UserProfile".to_string())
                .unwrap()
                .to_snake_case(),
            "user_profile"
        );
        assert_eq!(
            ComponentName::new("NavBar".to_string())
                .unwrap()
                .to_snake_case(),
            "nav_bar"
        );
        assert_eq!(
            ComponentName::new("TestAuthCheck".to_string())
                .unwrap()
                .to_snake_case(),
            "test_auth_check"
        );
    }
}
