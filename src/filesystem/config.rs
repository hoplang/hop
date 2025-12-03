use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum TargetLanguage {
    Javascript,
    Typescript,
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
#[serde(deny_unknown_fields)]
pub struct TargetSection {
    /// JavaScript target config
    pub javascript: Option<JavascriptTargetConfig>,

    /// TypeScript target config
    pub typescript: Option<TypescriptTargetConfig>,

    /// Python target config
    pub python: Option<PythonTargetConfig>,

    /// Go target config
    pub go: Option<GoTargetConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CssConfig {
    pub mode: Option<String>,

    /// Tailwind CSS configuration
    #[serde(default)]
    pub tailwind: Option<TailwindConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TailwindConfig {
    /// Path to the input CSS file for Tailwind
    pub input: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JavascriptTargetConfig {
    pub output_path: String,

    /// Shell commands to execute after compilation
    #[serde(default)]
    pub compile_and_run: Vec<String>,

    /// List of pages to export (format: "module/Component")
    /// Empty array means no pages are exported
    pub pages: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypescriptTargetConfig {
    pub output_path: String,

    /// Shell commands to execute after compilation
    #[serde(default)]
    pub compile_and_run: Vec<String>,

    /// List of pages to export (format: "module/Component")
    /// Empty array means no pages are exported
    pub pages: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PythonTargetConfig {
    pub output_path: String,

    /// Shell commands to execute after compilation
    #[serde(default)]
    pub compile_and_run: Vec<String>,

    /// List of pages to export (format: "module/Component")
    /// Empty array means no pages are exported
    pub pages: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GoTargetConfig {
    pub output_path: String,

    /// Shell commands to execute after compilation
    #[serde(default)]
    pub compile_and_run: Vec<String>,

    /// Package name for the Go file
    pub package: String,

    /// List of pages to export (format: "module/Component")
    /// Empty array means no pages are exported
    pub pages: Vec<String>,
}

/// Target configuration (tagged enum for each language)
#[derive(Debug, Clone)]
pub enum TargetConfig {
    Javascript(JavascriptTargetConfig),
    Typescript(TypescriptTargetConfig),
    Python(PythonTargetConfig),
    Go(GoTargetConfig),
}

#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error(
        "No targets defined in hop.toml. Please add a target section (e.g., [target.typescript])"
    )]
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
        if config.target.javascript.is_some() {
            targets.push("javascript");
        }
        if config.target.typescript.is_some() {
            targets.push("typescript");
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

    /// Get the target config (guaranteed to have exactly one after from_toml_str validation)
    pub fn get_target(&self) -> TargetConfig {
        if let Some(ref config) = self.target.javascript {
            return TargetConfig::Javascript(config.clone());
        }
        if let Some(ref config) = self.target.typescript {
            return TargetConfig::Typescript(config.clone());
        }
        if let Some(ref config) = self.target.python {
            return TargetConfig::Python(config.clone());
        }
        if let Some(ref config) = self.target.go {
            return TargetConfig::Go(config.clone());
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

            [target.typescript]
            output_path = "app.ts"
            pages = ["main/App"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        assert_eq!(config.css.mode, Some("tailwind4".to_string()));
        assert!(config.target.typescript.is_some());
        let ts_config = config.target.typescript.as_ref().unwrap();
        assert_eq!(ts_config.output_path, "app.ts");
        assert_eq!(ts_config.pages, vec!["main/App"]);
    }

    #[test]
    fn test_get_target_single() {
        let toml_str = indoc! {r#"
            [target.typescript]
            output_path = "app.ts"
            pages = ["main/App"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let target_config = config.get_target();
        assert!(matches!(target_config, TargetConfig::Typescript(_)));
        if let TargetConfig::Typescript(ts_config) = target_config {
            assert_eq!(ts_config.output_path, "app.ts");
            assert!(ts_config.compile_and_run.is_empty());
            assert_eq!(ts_config.pages, vec!["main/App"]);
        }
    }

    #[test]
    fn test_parse_multiple_targets_error() {
        let toml_str = indoc! {r#"
            [target.javascript]
            output_path = "app.js"
            pages = ["main/App"]

            [target.typescript]
            output_path = "app.ts"
            pages = ["main/App"]
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
            [target.typescript]
            output_path = "app.ts"
            pages = ["main/App"]
            compile_and_run = ["npm install", "npm start"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let target_config = config.get_target();
        assert!(matches!(target_config, TargetConfig::Typescript(_)));
        if let TargetConfig::Typescript(ts_config) = target_config {
            assert_eq!(ts_config.output_path, "app.ts");
            assert_eq!(ts_config.compile_and_run, vec!["npm install", "npm start"]);
        }
    }

    #[test]
    fn test_parse_config_without_compile_and_run() {
        let toml_str = indoc! {r#"
            [target.javascript]
            output_path = "app.js"
            pages = ["main/App"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let target_config = &config.target.javascript.as_ref().unwrap();
        assert_eq!(target_config.output_path, "app.js");
        assert!(target_config.compile_and_run.is_empty());
    }

    #[test]
    fn test_parse_config_with_tailwind() {
        let toml_str = indoc! {r#"
            [css.tailwind]
            input = "styles/input.css"

            [target.typescript]
            output_path = "app.ts"
            pages = ["main/App"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        assert!(config.css.tailwind.is_some());
        assert_eq!(config.css.tailwind.unwrap().input, "styles/input.css");
    }

    #[test]
    fn test_parse_config_with_unknown_target_error() {
        let toml_str = indoc! {r#"
            [target.typescritp]
            output_path = "app.ts"
            pages = ["main/App"]
        "#};
        let result = HopConfig::from_toml_str(toml_str);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("typescritp"));
        assert!(error_msg.contains("unknown field"));
    }

    #[test]
    fn test_parse_config_with_go_target() {
        let toml_str = indoc! {r#"
            [target.go]
            output_path = "main.go"
            package = "main"
            pages = ["main/App"]
        "#};
        let config = HopConfig::from_toml_str(toml_str).unwrap();
        let target_config = config.get_target();

        // Check that we can access the package field from the Go variant
        if let TargetConfig::Go(go_config) = target_config {
            assert_eq!(go_config.output_path, "main.go");
            assert_eq!(go_config.package, "main");
        } else {
            panic!("Expected Go target config");
        }
    }

    #[test]
    fn test_parse_config_with_go_target_missing_package() {
        let toml_str = indoc! {r#"
            [target.go]
            output_path = "main.go"
            pages = ["main/App"]
        "#};
        let result = HopConfig::from_toml_str(toml_str);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("package"));
    }
}
