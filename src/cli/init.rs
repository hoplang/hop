use anyhow::Result;
use std::fs;
use std::path::Path;

use crate::filesystem::config::{
    CssConfig, GoTargetConfig, HopConfig, JavascriptTargetConfig, PythonTargetConfig,
    TargetLanguage, TargetSection, TypescriptTargetConfig,
};

pub fn execute(template: &TargetLanguage) -> Result<()> {
    // Check if hop.toml already exists
    if Path::new("hop.toml").exists() {
        anyhow::bail!("hop.toml already exists in this directory.");
    }

    // Create HopConfig struct and serialize to TOML
    let config = create_hop_config(template);
    let hop_toml_content = toml::to_string_pretty(&config)
        .map_err(|e| anyhow::anyhow!("Failed to serialize config: {}", e))?;

    fs::write("hop.toml", hop_toml_content)?;

    // Create index.hop with a simple component
    let index_hop_content = r#"<page-index entrypoint>
  Welcome to hop!
</page-index>
"#;
    fs::write("index.hop", index_hop_content)?;

    let template_name = match template {
        TargetLanguage::Typescript => "TypeScript",
        TargetLanguage::Javascript => "JavaScript",
        TargetLanguage::Go => "Go",
        TargetLanguage::Python => "Python",
    };

    use colored::*;
    println!();
    println!("  {} Initialized {} project", "✓".green(), template_name);
    println!();
    println!("  Created:");
    println!("    hop.toml");
    println!("    index.hop");
    println!();
    println!("  Next steps:");
    println!("    1. Run 'hop dev' to start the development server");
    println!("    2. Run 'hop compile' to compile templates");
    println!();

    Ok(())
}

fn create_hop_config(template: &TargetLanguage) -> HopConfig {
    let css_config = CssConfig {
        mode: Some("tailwind4".to_string()),
        tailwind: None,
    };

    let mut target_section = TargetSection::default();
    match template {
        TargetLanguage::Typescript => {
            target_section.typescript = Some(TypescriptTargetConfig {
                output: "frontend.ts".to_string(),
                compile_and_run: Vec::new(),
                pages: vec!["main/App".to_string()],
            })
        }
        TargetLanguage::Javascript => {
            target_section.javascript = Some(JavascriptTargetConfig {
                output: "frontend.js".to_string(),
                compile_and_run: Vec::new(),
                pages: vec!["main/App".to_string()],
            })
        }
        TargetLanguage::Go => {
            target_section.go = Some(GoTargetConfig {
                output: "frontend.go".to_string(),
                compile_and_run: Vec::new(),
                package: "main".to_string(),
                pages: vec!["main/App".to_string()],
            })
        }
        TargetLanguage::Python => {
            target_section.python = Some(PythonTargetConfig {
                output: "frontend.py".to_string(),
                compile_and_run: Vec::new(),
                pages: vec!["main/App".to_string()],
            })
        }
    }

    HopConfig {
        css: css_config,
        target: target_section,
    }
}
