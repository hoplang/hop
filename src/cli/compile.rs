use crate::document::DocumentAnnotator;
use crate::filesystem::config::TargetLanguage;
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::component_name::ComponentName;
use crate::hop::module_name::ModuleName;
use crate::hop::program::Program;
use crate::ir::{
    GoTranspiler, JsTranspiler, LanguageMode, PythonTranspiler, RecordInfo, Transpiler,
};
use crate::orchestrator::orchestrate;
use crate::tui::timing;
use anyhow::Result;
use std::path::{Path, PathBuf};
use tailwind_runner::{TailwindConfig, TailwindRunner};

pub struct CompileResult {
    pub entry_points: Vec<String>,
    pub timer: crate::tui::timing::TimingCollector,
    pub output_path: PathBuf,
}

async fn compile_tailwind(input_path: &Path) -> Result<String> {
    let cache_dir = PathBuf::from("/tmp/.hop-cache");
    tokio::fs::create_dir_all(&cache_dir).await?;

    let runner = TailwindRunner::new(cache_dir.clone()).await?;

    let tailwind_config = TailwindConfig {
        input: input_path.to_path_buf(),
        output: cache_dir.join("compiled-tailwind.css"),
    };

    // Run Tailwind compilation
    runner.run_once(&tailwind_config).await?;

    // Read and return the compiled CSS
    let css = tokio::fs::read_to_string(&tailwind_config.output).await?;
    Ok(css)
}

// Tailwind needs a file that contains at least the string `@import "tailwindcss";`
// to properly execute. This function creates such a file.
async fn create_default_tailwind_input() -> Result<PathBuf> {
    let cache_dir = PathBuf::from("/tmp/.hop-cache");
    tokio::fs::create_dir_all(&cache_dir).await?;

    let temp_input = cache_dir.join("default-input.css");
    let default_content = r#"@import "tailwindcss";"#;

    tokio::fs::write(&temp_input, default_content).await?;
    Ok(temp_input)
}

pub async fn execute(project_root: &ProjectRoot) -> Result<CompileResult> {
    let mut timer = timing::TimingCollector::new();

    // Load configuration
    let config = project_root.load_config().await?;
    let resolved = config.get_resolved_config();

    // Truncate output file before running Tailwind to prevent scanning old content
    project_root.write_output("").await?;

    // Compile Tailwind CSS if configured
    timer.start_phase("tailwind");
    let tailwind_css = if let Some(p) = project_root.get_tailwind_input_path().await? {
        // Use user-specified Tailwind input
        Some(compile_tailwind(&p).await?)
    } else {
        // Use default Tailwind configuration
        let default_input = create_default_tailwind_input().await?;
        Some(compile_tailwind(&default_input).await?)
    };

    // Load all .hop files
    let hop_modules = project_root.load_all_hop_modules()?;

    if hop_modules.is_empty() {
        anyhow::bail!("No .hop files found in project");
    }

    timer.start_phase("compiling");
    // Create Program and compile all modules
    let program = Program::new(hop_modules);

    // Check for compilation errors
    let mut error_output_parts = Vec::new();
    let annotator = DocumentAnnotator::new()
        .with_label("error")
        .with_lines_before(1)
        .with_location();

    // Check for parse errors
    for (module_name, errors) in program.get_parse_errors() {
        if !errors.is_empty() {
            let filename = format!("{}.hop", module_name);
            error_output_parts.push(annotator.annotate(Some(&filename), errors.iter()));
        }
    }

    // Check for type errors if there's no parse errors
    if error_output_parts.is_empty() {
        for (module_name, errors) in program.get_type_errors() {
            if !errors.is_empty() {
                let filename = format!("{}.hop", module_name);
                error_output_parts.push(annotator.annotate(Some(&filename), errors));
            }
        }
    }

    if !error_output_parts.is_empty() {
        return Err(anyhow::anyhow!(
            "Compilation failed:\n{}",
            error_output_parts.join("\n")
        ));
    }

    timer.start_phase("compiling to IR");

    let pages: Vec<(ModuleName, ComponentName)> = resolved
        .pages
        .iter()
        .map(|page| {
            let (module, component) = page.rsplit_once('/').ok_or_else(|| {
                anyhow::anyhow!(
                    "Invalid page format '{}'. Expected 'module/Component' (e.g., 'main/HomePage')",
                    page
                )
            })?;
            Ok((
                ModuleName::new(module)?,
                ComponentName::new(component.to_string())?,
            ))
        })
        .collect::<Result<Vec<_>>>()?;

    // Use orchestrate to handle inlining, compilation, and optimization
    let ir_entrypoints = orchestrate(
        program.get_typed_modules().clone(),
        tailwind_css.as_deref(),
        &pages,
    )?;

    // Collect record declarations from all modules
    let mut records: Vec<RecordInfo> = program
        .get_typed_modules()
        .values()
        .flat_map(|module| module.get_records())
        .map(|record| RecordInfo {
            name: record.name().to_string(),
            fields: record
                .declaration
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.field_type.clone()))
                .collect(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    // Generate code based on target language
    let generated_code = match resolved.target {
        TargetLanguage::Javascript => {
            timer.start_phase("transpiling to js");
            let transpiler = JsTranspiler::new(LanguageMode::JavaScript);
            transpiler.transpile_module(&ir_entrypoints, &records)
        }
        TargetLanguage::Typescript => {
            timer.start_phase("transpiling to ts");
            let transpiler = JsTranspiler::new(LanguageMode::TypeScript);
            transpiler.transpile_module(&ir_entrypoints, &records)
        }
        TargetLanguage::Go => {
            timer.start_phase("transpiling to go");
            let package = resolved
                .go_package
                .clone()
                .unwrap_or_else(|| "main".to_string());
            let transpiler = GoTranspiler::new(package);
            transpiler.transpile_module(&ir_entrypoints, &records)
        }
        TargetLanguage::Python => {
            timer.start_phase("transpiling to python");
            let transpiler = PythonTranspiler::new();
            transpiler.transpile_module(&ir_entrypoints, &records)
        }
    };

    timer.start_phase("writing output");

    let output_path = project_root.write_output(&generated_code).await?;

    let entry_points: Vec<String> = ir_entrypoints
        .iter()
        .map(|entrypoint| entrypoint.name.as_str().to_string())
        .collect();

    Ok(CompileResult {
        entry_points,
        timer,
        output_path,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::archive::temp_dir_from_archive;
    use indoc::indoc;
    use simple_txtar::Archive;
    use std::fs;

    #[tokio::test]
    #[ignore]
    async fn compile_with_custom_go_output_path() {
        // Create a temporary directory with hop.toml and a simple .hop file
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [compile]
            target = "go"
            output_path = "components/frontend.go"
            pages = ["main/HelloWorld"]

            [css]
            mode = "tailwind4"
            -- main.hop --
            <HelloWorld>Hello, World!</HelloWorld>
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project_root = ProjectRoot::from(&temp_dir).unwrap();

        // Execute the compile command
        let result = execute(&project_root).await;
        assert!(
            result.is_ok(),
            "Compilation should succeed: {:?}",
            result.err()
        );

        // Verify that the output file was created at the correct path
        let expected_output_path = temp_dir.join("components/frontend.go");
        assert!(
            expected_output_path.exists(),
            "Output file should exist at components/frontend.go"
        );

        // Verify that the generated file uses the correct package name (derived from directory)
        let generated_code = fs::read_to_string(&expected_output_path).unwrap();
        assert!(
            generated_code.starts_with("package components"),
            "Generated Go code should use 'package components' (derived from output path)"
        );

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
