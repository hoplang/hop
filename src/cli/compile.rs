use crate::document::DocumentAnnotator;
use crate::filesystem::files::ProjectRoot;
use crate::hop::program::Program;
use crate::ir::{Compiler, JsCompiler};
use anyhow::{Context, Result};
use std::fs;
use std::path::Path;

pub fn run(projectdir: Option<&str>, output_path: &str) -> Result<()> {
    // Find project root
    let project_root = match projectdir {
        Some(dir) => ProjectRoot::find_upwards(Path::new(dir))?,
        None => ProjectRoot::find_upwards(Path::new("."))?,
    };

    println!("Compiling hop templates to JavaScript...");
    println!("Project root: {}", project_root.get_path().display());

    // Load all .hop files
    let hop_modules = project_root.load_all_hop_modules()?;

    if hop_modules.is_empty() {
        anyhow::bail!("No .hop files found in project");
    }

    println!("Found {} hop files", hop_modules.len());

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

    println!("Type checking passed");

    // Compile to IR
    let ir_module = Compiler::compile(program.get_modules());

    // Count entrypoints
    let entrypoint_count = ir_module.entry_points.len();
    if entrypoint_count == 0 {
        eprintln!("Warning: No entrypoint components found");
        eprintln!("Add 'entrypoint' attribute to components you want to export");
    } else {
        println!("Found {} entrypoint components", entrypoint_count);
    }

    // Compile to JavaScript
    let js_code = JsCompiler::compile_module(&ir_module);

    // Write output file
    fs::write(output_path, js_code)
        .with_context(|| format!("Failed to write output to {}", output_path))?;

    println!("Successfully compiled to {}", output_path);

    // Print exported functions
    if entrypoint_count > 0 {
        println!("\nExported functions:");
        for name in ir_module.entry_points.keys() {
            let func_name = name.replace(['/', '-'], "_");
            println!("  - {}()", func_name);
        }
    }

    Ok(())
}
