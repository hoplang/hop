use crate::CompileLanguage;
use crate::document::DocumentAnnotator;
use crate::filesystem::config::TargetLanguage;
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::program::Program;
use crate::ir::{
    CompilationMode, GoTranspiler, JsTranspiler, LanguageMode, PythonTranspiler, Transpiler,
    orchestrator::orchestrate,
};
use crate::tui::timing;
use anyhow::Result;
use std::path::{Path, PathBuf};
use tailwind_runner::{TailwindConfig, TailwindRunner};

pub struct CompileResult {
    pub entry_points: Vec<String>,
    pub timer: crate::tui::timing::TimingCollector,
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

async fn create_default_tailwind_input() -> Result<PathBuf> {
    let cache_dir = PathBuf::from("/tmp/.hop-cache");
    tokio::fs::create_dir_all(&cache_dir).await?;

    let temp_input = cache_dir.join("default-input.css");
    let default_content = r#"@import "tailwindcss";"#;

    tokio::fs::write(&temp_input, default_content).await?;
    Ok(temp_input)
}

pub async fn execute(project_root: &ProjectRoot, development: bool) -> Result<CompileResult> {
    let mut timer = timing::TimingCollector::new();

    // Load configuration
    let config = project_root.load_config().await?;
    let (target_language, _) = config.get_target();

    // Convert target language to CompileLanguage
    let language = match target_language {
        TargetLanguage::Js => CompileLanguage::Js,
        TargetLanguage::Ts => CompileLanguage::Ts,
        TargetLanguage::Python => CompileLanguage::Py,
        TargetLanguage::Go => CompileLanguage::Go,
    };

    // Truncate output file before running Tailwind to prevent scanning old content
    project_root.write_output("").await?;

    // Compile Tailwind CSS if configured (skip in development mode)
    let tailwind_css = if development {
        timer.start_phase("skipping tailwind (development mode)");
        None
    } else {
        timer.start_phase("running tailwind");
        if let Some(p) = project_root.get_tailwind_input_path().await? {
            // Use user-specified Tailwind input
            Some(compile_tailwind(&p).await?)
        } else {
            // Use default Tailwind configuration
            let default_input = create_default_tailwind_input().await?;
            Some(compile_tailwind(&default_input).await?)
        }
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

    // Determine compilation mode
    let compilation_mode = if development {
        CompilationMode::Development
    } else {
        CompilationMode::Production
    };

    timer.start_phase("compiling to IR");
    // Use orchestrate to handle inlining, compilation, and optimization
    let ir_entrypoints = orchestrate(
        program.get_typed_modules().clone(),
        compilation_mode,
        tailwind_css.as_deref(),
    );

    // Generate code based on target language
    let generated_code = match language {
        CompileLanguage::Js => {
            timer.start_phase("transpiling to js");
            let transpiler = JsTranspiler::new(LanguageMode::JavaScript);
            transpiler.transpile_module(&ir_entrypoints)
        }
        CompileLanguage::Ts => {
            timer.start_phase("transpiling to ts");
            let transpiler = JsTranspiler::new(LanguageMode::TypeScript);
            transpiler.transpile_module(&ir_entrypoints)
        }
        CompileLanguage::Go => {
            timer.start_phase("transpiling to go");
            let transpiler = GoTranspiler::new();
            transpiler.transpile_module(&ir_entrypoints)
        }
        CompileLanguage::Py => {
            timer.start_phase("transpiling to python");
            let transpiler = PythonTranspiler::new();
            transpiler.transpile_module(&ir_entrypoints)
        }
    };

    timer.start_phase("writing output");

    project_root.write_output(&generated_code).await?;

    let entry_points: Vec<String> = ir_entrypoints
        .iter()
        .map(|entrypoint| entrypoint.name.replace(['/', '-'], "_"))
        .collect();

    Ok(CompileResult {
        entry_points,
        timer,
    })
}
