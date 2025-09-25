use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum TargetLanguage {
    Js,
    Ts,
    Python,
    Go,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct HopConfig {
    #[serde(default)]
    pub css: CssConfig,

    /// Target configurations (exactly one must be specified)
    #[serde(default)]
    pub target: TargetSection,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TargetSection {
    /// JavaScript target config
    pub js: Option<TargetConfig>,

    /// TypeScript target config
    pub ts: Option<TargetConfig>,

    /// Python target config
    pub python: Option<TargetConfig>,

    /// Go target config
    pub go: Option<TargetConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CssConfig {
    pub mode: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TargetConfig {
    pub output: String,

    /// Shell commands to execute after compilation
    #[serde(default)]
    pub compile_and_run: Vec<String>,
}

#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("No targets defined in hop.toml. Please add a target section (e.g., [target.ts])")]
    NoTargets,
    #[error(
        "Multiple targets defined ({targets:?}). Please specify exactly one target in hop.toml"
    )]
    MultipleTargets { targets: Vec<String> },
}

impl HopConfig {
    /// Parse HopConfig from a TOML string and validate target count
    pub fn from_toml_str(toml_str: &str) -> anyhow::Result<Self> {
        let config: HopConfig = toml::from_str(toml_str)?;

        // Count defined targets and validate exactly one exists
        let mut targets = vec![];
        if config.target.js.is_some() {
            targets.push("js");
        }
        if config.target.ts.is_some() {
            targets.push("ts");
        }
        if config.target.python.is_some() {
            targets.push("python");
        }
        if config.target.go.is_some() {
            targets.push("go");
        }

        match targets.len() {
            0 => return Err(ConfigError::NoTargets.into()),
            1 => {} // Valid - continue
            _ => {
                return Err(ConfigError::MultipleTargets {
                    targets: targets.into_iter().map(String::from).collect(),
                }
                .into());
            }
        }

        Ok(config)
    }

    /// Get the target language and config (guaranteed to have exactly one after from_toml_str validation)
    pub fn get_target(&self) -> (TargetLanguage, &TargetConfig) {
        if let Some(ref config) = self.target.js {
            return (TargetLanguage::Js, config);
        }
        if let Some(ref config) = self.target.ts {
            return (TargetLanguage::Ts, config);
        }
        if let Some(ref config) = self.target.python {
            return (TargetLanguage::Python, config);
        }
        if let Some(ref config) = self.target.go {
            return (TargetLanguage::Go, config);
        }

        // This should never happen after from_toml_str validation
        panic!("No target found - this indicates a bug in from_toml_str validation")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_parse_config_with_css_mode_no_target_error() {
        let toml_str = indoc! {r#"
            [css]
            mode = "tailwind4"
        "#};
        let result = HopConfig::from_toml_str(toml_str);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("No targets"));
    }

    #[test]
    fn test_parse_empty_config_error() {
        let toml_str = "";
        let result = HopConfig::from_toml_str(toml_str);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("No targets"));
    }

    #[test]
    fn test_parse_config_without_css_section_error() {
        let toml_str = indoc! {r#"
            # Just a comment
        "#};
        let result = HopConfig::from_toml_str(toml_str);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("No targets"));
    }

    #[test]
    fn test_parse_config_with_single_target() {
        let toml_str = indoc! {r#"
            [css]
            mode = "tailwind4"

            [target.ts]
            output = "app.ts"
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        assert_eq!(config.css.mode, Some("tailwind4".to_string()));
        assert!(config.target.ts.is_some());
        assert_eq!(config.target.ts.unwrap().output, "app.ts");
    }

    #[test]
    fn test_get_target_single() {
        let toml_str = indoc! {r#"
            [target.ts]
            output = "app.ts"
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let (target_language, target_config) = config.get_target();
        assert_eq!(target_language, TargetLanguage::Ts);
        assert_eq!(target_config.output, "app.ts");
        assert!(target_config.compile_and_run.is_empty());
    }

    #[test]
    fn test_parse_multiple_targets_error() {
        let toml_str = indoc! {r#"
            [target.js]
            output = "app.js"

            [target.ts]
            output = "app.ts"
        "#};
        let result = HopConfig::from_toml_str(toml_str);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Multiple targets"));
        assert!(error_msg.contains("exactly one target"));
    }

    #[test]
    fn test_parse_config_with_compile_and_run() {
        let toml_str = indoc! {r#"
            [target.ts]
            output = "app.ts"
            compile_and_run = ["npm install", "npm start"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let (target_language, target_config) = config.get_target();
        assert_eq!(target_language, TargetLanguage::Ts);
        assert_eq!(target_config.output, "app.ts");
        assert_eq!(
            target_config.compile_and_run,
            vec!["npm install", "npm start"]
        );
    }

    #[test]
    fn test_parse_config_without_compile_and_run() {
        let toml_str = indoc! {r#"
            [target.js]
            output = "app.js"
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let target_config = &config.target.js.as_ref().unwrap();
        assert_eq!(target_config.output, "app.js");
        assert!(target_config.compile_and_run.is_empty());
    }
}
