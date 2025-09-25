use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct HopConfig {
    #[serde(default)]
    pub css: CssConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CssConfig {
    pub mode: Option<String>,
}

impl HopConfig {
    /// Parse HopConfig from a TOML string
    pub fn from_toml_str(toml_str: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(toml_str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_parse_config_with_css_mode() {
        let toml_str = indoc! {r#"
            [css]
            mode = "tailwind4"
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        assert_eq!(config.css.mode, Some("tailwind4".to_string()));
    }

    #[test]
    fn test_parse_empty_config() {
        let toml_str = "";
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        assert!(config.css.mode.is_none());
    }

    #[test]
    fn test_parse_config_without_css_section() {
        let toml_str = indoc! {r#"
            # Just a comment
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        assert!(config.css.mode.is_none());
    }
}