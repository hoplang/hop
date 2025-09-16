/// A utility for converting between different string casing conventions.
///
/// Supports conversions between:
/// - kebab-case
/// - camelCase
/// - PascalCase
/// - snake_case
/// - SCREAMING_SNAKE_CASE
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CasedString {
    /// The words that make up the string, stored in lowercase
    words: Vec<String>,
}

impl CasedString {
    /// Create a CasedString from a kebab-case string (e.g., "my-component-name")
    pub fn from_kebab_case(s: &str) -> Self {
        let words = s
            .split('-')
            .filter(|w| !w.is_empty())
            .map(|w| w.to_lowercase())
            .collect();
        Self { words }
    }

    /// Create a CasedString from a snake_case string (e.g., "my_component_name")
    pub fn from_snake_case(s: &str) -> Self {
        let words = s
            .split('_')
            .filter(|w| !w.is_empty())
            .map(|w| w.to_lowercase())
            .collect();
        Self { words }
    }

    /// Create a CasedString from a camelCase string (e.g., "myComponentName")
    pub fn from_camel_case(s: &str) -> Self {
        let mut words = Vec::new();
        let mut current_word = String::new();

        for (i, ch) in s.chars().enumerate() {
            if ch.is_uppercase() && i > 0 {
                if !current_word.is_empty() {
                    words.push(current_word.to_lowercase());
                    current_word = String::new();
                }
            }
            current_word.push(ch);
        }

        if !current_word.is_empty() {
            words.push(current_word.to_lowercase());
        }

        Self { words }
    }

    /// Create a CasedString from a PascalCase string (e.g., "MyComponentName")
    pub fn from_pascal_case(s: &str) -> Self {
        Self::from_camel_case(s)
    }

    /// Convert to kebab-case (e.g., "my-component-name")
    pub fn to_kebab_case(&self) -> String {
        self.words.join("-")
    }

    /// Convert to snake_case (e.g., "my_component_name")
    pub fn to_snake_case(&self) -> String {
        self.words.join("_")
    }

    /// Convert to SCREAMING_SNAKE_CASE (e.g., "MY_COMPONENT_NAME")
    pub fn to_screaming_snake_case(&self) -> String {
        self.words
            .iter()
            .map(|w| w.to_uppercase())
            .collect::<Vec<_>>()
            .join("_")
    }

    /// Convert to camelCase (e.g., "myComponentName")
    pub fn to_camel_case(&self) -> String {
        if self.words.is_empty() {
            return String::new();
        }

        let mut result = String::new();
        for (i, word) in self.words.iter().enumerate() {
            if i == 0 {
                result.push_str(word);
            } else {
                result.push_str(&Self::capitalize(word));
            }
        }
        result
    }

    /// Convert to PascalCase (e.g., "MyComponentName")
    pub fn to_pascal_case(&self) -> String {
        self.words
            .iter()
            .map(|w| Self::capitalize(w))
            .collect::<Vec<_>>()
            .join("")
    }

    /// Helper to capitalize the first letter of a word
    fn capitalize(word: &str) -> String {
        let mut chars = word.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_kebab_case() {
        let cs = CasedString::from_kebab_case("my-component-name");
        assert_eq!(cs.to_camel_case(), "myComponentName");
        assert_eq!(cs.to_pascal_case(), "MyComponentName");
        assert_eq!(cs.to_snake_case(), "my_component_name");
        assert_eq!(cs.to_screaming_snake_case(), "MY_COMPONENT_NAME");
        assert_eq!(cs.to_kebab_case(), "my-component-name");
    }

    #[test]
    fn test_from_snake_case() {
        let cs = CasedString::from_snake_case("my_component_name");
        assert_eq!(cs.to_camel_case(), "myComponentName");
        assert_eq!(cs.to_pascal_case(), "MyComponentName");
        assert_eq!(cs.to_kebab_case(), "my-component-name");
        assert_eq!(cs.to_screaming_snake_case(), "MY_COMPONENT_NAME");
        assert_eq!(cs.to_snake_case(), "my_component_name");
    }

    #[test]
    fn test_from_camel_case() {
        let cs = CasedString::from_camel_case("myComponentName");
        assert_eq!(cs.to_kebab_case(), "my-component-name");
        assert_eq!(cs.to_pascal_case(), "MyComponentName");
        assert_eq!(cs.to_snake_case(), "my_component_name");
        assert_eq!(cs.to_screaming_snake_case(), "MY_COMPONENT_NAME");
        assert_eq!(cs.to_camel_case(), "myComponentName");
    }

    #[test]
    fn test_from_pascal_case() {
        let cs = CasedString::from_pascal_case("MyComponentName");
        assert_eq!(cs.to_kebab_case(), "my-component-name");
        assert_eq!(cs.to_camel_case(), "myComponentName");
        assert_eq!(cs.to_snake_case(), "my_component_name");
        assert_eq!(cs.to_screaming_snake_case(), "MY_COMPONENT_NAME");
        assert_eq!(cs.to_pascal_case(), "MyComponentName");
    }

    #[test]
    fn test_single_word() {
        let cs = CasedString::from_kebab_case("component");
        assert_eq!(cs.to_camel_case(), "component");
        assert_eq!(cs.to_pascal_case(), "Component");
        assert_eq!(cs.to_snake_case(), "component");
        assert_eq!(cs.to_screaming_snake_case(), "COMPONENT");
        assert_eq!(cs.to_kebab_case(), "component");
    }

    #[test]
    fn test_empty_string() {
        let cs = CasedString::from_kebab_case("");
        assert_eq!(cs.to_camel_case(), "");
        assert_eq!(cs.to_pascal_case(), "");
        assert_eq!(cs.to_snake_case(), "");
        assert_eq!(cs.to_screaming_snake_case(), "");
        assert_eq!(cs.to_kebab_case(), "");
    }

    #[test]
    fn test_with_numbers() {
        let cs = CasedString::from_kebab_case("component-2-test");
        assert_eq!(cs.to_camel_case(), "component2Test");
        assert_eq!(cs.to_pascal_case(), "Component2Test");
    }
}