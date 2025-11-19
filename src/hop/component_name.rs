use std::fmt;
use thiserror::Error;

/// Error type for invalid component names
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum InvalidComponentNameError {
    #[error("Component name cannot be empty")]
    Empty,

    #[error("Component name must start with an uppercase letter (PascalCase)")]
    NotPascalCase,

    #[error(
        "Component name contains invalid character: '{0}'. Only alphanumeric characters are allowed"
    )]
    InvalidCharacter(char),

    #[error("Component name '{0}' is reserved")]
    Reserved(String),
}

/// A type-safe wrapper for component names in the hop system.
/// Component names must follow PascalCase convention and contain only alphanumeric characters.
///
/// Examples: "Button", "UserProfile", "NavBar", "Component123"
/// Invalid: "button", "user-profile", "nav_bar", "component-name"
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ComponentName(String);

impl ComponentName {
    /// Reserved names that cannot be used as component names
    const RESERVED_NAMES: &'static [&'static str] = &[
        // Reserved for future use or special meaning
        "Component",
        "Element",
        "Fragment",
        "Slot",
        "Template",
    ];

    /// Create a new ComponentName from a string, validating it
    ///
    /// Returns an error describing why the component name is invalid.
    /// Valid component names:
    /// - Must not be empty
    /// - Must start with an uppercase letter (PascalCase)
    /// - Must contain only alphanumeric characters
    /// - Must not be a reserved name
    pub fn new(name: String) -> Result<Self, InvalidComponentNameError> {
        Self::validate(&name)?;
        Ok(ComponentName(name))
    }

    /// Check if a string is a valid component name (used for quick validation)
    pub fn is_valid(name: &str) -> bool {
        Self::validate(name).is_ok()
    }

    /// Check if a tag name looks like a component (starts with uppercase)
    /// This is a quick check used during parsing to determine if a tag should
    /// be treated as a component reference or an HTML element
    pub fn is_component_tag(name: &str) -> bool {
        name.chars().next().is_some_and(|c| c.is_ascii_uppercase())
    }

    /// Validate a component name string
    fn validate(name: &str) -> Result<(), InvalidComponentNameError> {
        // Must not be empty
        if name.is_empty() {
            return Err(InvalidComponentNameError::Empty);
        }

        // Check if it's a reserved name
        if Self::RESERVED_NAMES.contains(&name) {
            return Err(InvalidComponentNameError::Reserved(name.to_string()));
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
        assert!(ComponentName::new("Button".to_string()).is_ok());
        assert!(ComponentName::new("UserProfile".to_string()).is_ok());
        assert!(ComponentName::new("NavBar".to_string()).is_ok());
        assert!(ComponentName::new("Component123".to_string()).is_ok());
        assert!(ComponentName::new("MyComponent".to_string()).is_ok());
        assert!(ComponentName::new("A".to_string()).is_ok());
        assert!(ComponentName::new("ABC123".to_string()).is_ok());
        assert!(ComponentName::new("Main".to_string()).is_ok());
        assert!(ComponentName::new("Foo".to_string()).is_ok());
        assert!(ComponentName::new("Bar".to_string()).is_ok());
    }

    #[test]
    fn test_invalid_component_names() {
        // Empty name
        assert_eq!(
            ComponentName::new("".to_string()),
            Err(InvalidComponentNameError::Empty)
        );

        // Not PascalCase (lowercase start)
        assert_eq!(
            ComponentName::new("button".to_string()),
            Err(InvalidComponentNameError::NotPascalCase)
        );
        assert_eq!(
            ComponentName::new("userProfile".to_string()),
            Err(InvalidComponentNameError::NotPascalCase)
        );
        assert_eq!(
            ComponentName::new("navBar".to_string()),
            Err(InvalidComponentNameError::NotPascalCase)
        );

        // Contains invalid characters
        assert_eq!(
            ComponentName::new("User-Profile".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('-'))
        );
        assert_eq!(
            ComponentName::new("User_Profile".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('_'))
        );
        assert_eq!(
            ComponentName::new("User Profile".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter(' '))
        );
        assert_eq!(
            ComponentName::new("Button!".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('!'))
        );
        assert_eq!(
            ComponentName::new("Nav.Bar".to_string()),
            Err(InvalidComponentNameError::InvalidCharacter('.'))
        );

        // Reserved names
        assert_eq!(
            ComponentName::new("Component".to_string()),
            Err(InvalidComponentNameError::Reserved("Component".to_string()))
        );
        assert_eq!(
            ComponentName::new("Template".to_string()),
            Err(InvalidComponentNameError::Reserved("Template".to_string()))
        );
    }

    #[test]
    fn test_is_component_tag() {
        // Should return true for PascalCase
        assert!(ComponentName::is_component_tag("Button"));
        assert!(ComponentName::is_component_tag("UserProfile"));
        assert!(ComponentName::is_component_tag("A"));
        assert!(ComponentName::is_component_tag("Main"));

        // Should return false for lowercase
        assert!(!ComponentName::is_component_tag("button"));
        assert!(!ComponentName::is_component_tag("div"));
        assert!(!ComponentName::is_component_tag("span"));
        assert!(!ComponentName::is_component_tag(""));

        // Note: is_component_tag is just a quick check, not full validation
        // These return true even though they're not valid component names
        assert!(ComponentName::is_component_tag("User-Profile")); // Has dash
        assert!(ComponentName::is_component_tag("U")); // Single letter
    }

    #[test]
    fn test_is_valid() {
        assert!(ComponentName::is_valid("Button"));
        assert!(ComponentName::is_valid("UserProfile"));
        assert!(!ComponentName::is_valid("button"));
        assert!(!ComponentName::is_valid("User-Profile"));
        assert!(!ComponentName::is_valid(""));
        assert!(!ComponentName::is_valid("Component")); // Reserved
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
    fn test_to_pascal_case() {
        let button = ComponentName::new("Button".to_string()).unwrap();
        assert_eq!(button.to_pascal_case(), "Button");

        let user_profile = ComponentName::new("UserProfile".to_string()).unwrap();
        assert_eq!(user_profile.to_pascal_case(), "UserProfile");

        let nav_bar = ComponentName::new("NavBar".to_string()).unwrap();
        assert_eq!(nav_bar.to_pascal_case(), "NavBar");

        let main = ComponentName::new("Main".to_string()).unwrap();
        assert_eq!(main.to_pascal_case(), "Main");
    }

    #[test]
    fn test_to_camel_case() {
        let button = ComponentName::new("Button".to_string()).unwrap();
        assert_eq!(button.to_camel_case(), "button");

        let user_profile = ComponentName::new("UserProfile".to_string()).unwrap();
        assert_eq!(user_profile.to_camel_case(), "userProfile");

        let nav_bar = ComponentName::new("NavBar".to_string()).unwrap();
        assert_eq!(nav_bar.to_camel_case(), "navBar");

        let main = ComponentName::new("Main".to_string()).unwrap();
        assert_eq!(main.to_camel_case(), "main");

        let test = ComponentName::new("Test".to_string()).unwrap();
        assert_eq!(test.to_camel_case(), "test");
    }

    #[test]
    fn test_to_snake_case() {
        let button = ComponentName::new("Button".to_string()).unwrap();
        assert_eq!(button.to_snake_case(), "button");

        let user_profile = ComponentName::new("UserProfile".to_string()).unwrap();
        assert_eq!(user_profile.to_snake_case(), "user_profile");

        let nav_bar = ComponentName::new("NavBar".to_string()).unwrap();
        assert_eq!(nav_bar.to_snake_case(), "nav_bar");

        let main = ComponentName::new("Main".to_string()).unwrap();
        assert_eq!(main.to_snake_case(), "main");

        let test = ComponentName::new("Test".to_string()).unwrap();
        assert_eq!(test.to_snake_case(), "test");

        let test_auth_check = ComponentName::new("TestAuthCheck".to_string()).unwrap();
        assert_eq!(test_auth_check.to_snake_case(), "test_auth_check");
    }
}
