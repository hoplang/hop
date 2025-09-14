use crate::CompileLanguage;
use crate::document::DocumentAnnotator;
use crate::filesystem::files::ProjectRoot;
use crate::hop::program::Program;
use crate::ir::{Compiler, JsCompiler, LanguageMode, optimizer::Optimizer};
use crate::tui::timing;
use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

pub struct CompileResult {
    pub output_path: String,
    pub file_size: usize,
    pub entry_points: Vec<String>,
}

pub fn execute(
    projectdir: Option<&str>,
    output_path: &str,
    language: &CompileLanguage,
) -> Result<CompileResult> {
    let mut timer = timing::TimingCollector::new();

    // Find project root
    let project_root = match projectdir {
        Some(dir) => ProjectRoot::find_upwards(Path::new(dir))?,
        None => ProjectRoot::find_upwards(Path::new("."))?,
    };

    // Load configuration
    let config = project_root.load_config()?;

    // Log CSS mode if configured
    if let Some(css_mode) = &config.css.mode {
        eprintln!("Using CSS mode: {}", css_mode);
    }

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

    // Compile to IR
    let mut ir_module = Compiler::compile(program.get_modules());

    timer.start_phase("optimizing");
    // Run optimization passes
    let mut optimizer = Optimizer::default_optimization_pipeline();
    optimizer.run(&mut ir_module);

    // Generate code based on target language
    let generated_code = match language {
        CompileLanguage::Js => {
            timer.start_phase("generating js");
            let mut compiler = JsCompiler::new(LanguageMode::JavaScript);
            compiler.compile_module(&ir_module)
        }
        CompileLanguage::Ts => {
            timer.start_phase("generating ts");
            let mut compiler = JsCompiler::new(LanguageMode::TypeScript);
            compiler.compile_module(&ir_module)
        }
    };

    timer.start_phase("writing output");
    // Write output file
    fs::write(output_path, &generated_code)
        .with_context(|| format!("Failed to write output to {}", output_path))?;

    timer.print();

    let entry_points: Vec<String> = ir_module
        .entry_points
        .keys()
        .map(|name| name.replace(['/', '-'], "_"))
        .collect();

    Ok(CompileResult {
        output_path: output_path.to_string(),
        file_size: generated_code.len(),
        entry_points,
    })
}
