use anyhow::Result;
use std::fs;
use std::path::Path;

use crate::CompileLanguage;

pub fn execute(template: &CompileLanguage) -> Result<()> {
    // Check if hop.toml already exists
    if Path::new("hop.toml").exists() {
        anyhow::bail!("hop.toml already exists in this directory. Project already initialized.");
    }

    // Create hop.toml
    let hop_toml_content = r#"# Hop Configuration

[css]
mode = "tailwind4"
"#;
    fs::write("hop.toml", hop_toml_content)?;

    // Create index.hop with a simple component
    let index_hop_content = r#"<page-index entrypoint>
  Welcome to hop!
</page-index>
"#;
    fs::write("index.hop", index_hop_content)?;

    let template_name = match template {
        CompileLanguage::Ts => "TypeScript",
        CompileLanguage::Js => "JavaScript",
        CompileLanguage::Go => "Go",
        CompileLanguage::Py => "Python",
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
    println!("    2. Run 'hop compile {}' to compile templates",
        match template {
            CompileLanguage::Ts => "ts",
            CompileLanguage::Js => "js",
            CompileLanguage::Go => "go",
            CompileLanguage::Py => "py",
        });
    println!();

    Ok(())
}