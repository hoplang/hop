use crate::config::TargetLanguage;
use crate::document::DocumentAnnotator;
use crate::project::Project;
use crate::hop::program::Program;
use crate::ir::{GoTranspiler, PythonTranspiler, RustTranspiler, Transpiler, TsTranspiler};
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use crate::tui::timing::TimingCollector;
use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;
use tailwind_runner::TailwindRunner;

pub struct CompileResult {
    pub timer: TimingCollector,
    pub output_path: PathBuf,
}

pub async fn execute(project: &Project, skip_optimization: bool) -> Result<CompileResult> {
    let mut timer = TimingCollector::new();

    let config = project.load_config().await?;
    let resolved = config.get_resolved_config()?;

    timer.start_phase("load modules");
    let module_ids = project.find_modules()?;
    let mut hop_modules = HashMap::new();
    for module_id in module_ids {
        let document = project.load_module(&module_id)?;
        hop_modules.insert(module_id, document);
    }

    let hop_sources = hop_modules
        .values()
        .map(|doc| doc.as_str())
        .collect::<Vec<_>>()
        .join("\n");

    timer.start_phase("tailwind");
    let runner = TailwindRunner::new().await?;
    let tailwind_input = project.get_tailwind_input_path().await?;
    let tailwind_css = runner.compile_once(tailwind_input, &hop_sources).await?;

    timer.start_phase("compiling");
    let program = Program::new(hop_modules);

    let mut error_output = Vec::new();
    let annotator = DocumentAnnotator::new()
        .with_label("error")
        .with_lines_before(1)
        .with_location();

    for (module_id, errors) in program.get_parse_errors() {
        if !errors.is_empty() {
            let filename = format!("{}.hop", module_id);
            error_output.push(annotator.annotate(Some(&filename), errors.iter()));
        }
    }

    if error_output.is_empty() {
        for (module_id, errors) in program.get_type_errors() {
            if !errors.is_empty() {
                let filename = format!("{}.hop", module_id);
                error_output.push(annotator.annotate(Some(&filename), errors));
            }
        }
    }

    if !error_output.is_empty() {
        return Err(anyhow::anyhow!(
            "Compilation failed:\n{}",
            error_output.join("\n")
        ));
    }

    timer.start_phase("compiling to IR");

    let ir_module = orchestrate(
        program.get_typed_modules(),
        Some(&tailwind_css),
        OrchestrateOptions {
            skip_optimization,
            ..Default::default()
        },
    );

    let generated_code = match resolved.target {
        TargetLanguage::Typescript => {
            timer.start_phase("transpiling to ts");
            let mut transpiler = TsTranspiler::new();
            transpiler.transpile_module(&ir_module)
        }
        TargetLanguage::Go => {
            timer.start_phase("transpiling to go");
            let package = resolved
                .go_package
                .clone()
                .unwrap_or_else(|| "main".to_string());
            let mut transpiler = GoTranspiler::new(package);
            transpiler.transpile_module(&ir_module)
        }
        TargetLanguage::Python => {
            timer.start_phase("transpiling to python");
            let mut transpiler = PythonTranspiler::new();
            transpiler.transpile_module(&ir_module)
        }
        TargetLanguage::Rust => {
            timer.start_phase("transpiling to rust");
            let mut transpiler = RustTranspiler::new();
            transpiler.transpile_module(&ir_module)
        }
    };

    timer.start_phase("writing output");

    let output_path = project.write_output_path(&generated_code).await?;

    Ok(CompileResult { timer, output_path })
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
            [build]
            target = "go"
            output_path = "components/frontend.go"

            [css]
            mode = "tailwind4"
            -- main.hop --
            entrypoint HelloWorld() {
                Hello, World!
            }
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Execute the compile command
        let result = execute(&project, false).await;
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
            generated_code.contains("package components"),
            "Generated Go code should use 'package components' (derived from output path)"
        );

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }

    #[tokio::test]
    #[ignore]
    async fn deterministic_output_order_across_modules() {
        // Test that output order is deterministic when there are multiple modules.
        // Entrypoints are sorted alphabetically by module name.
        // This test would be flaky if the implementation used HashMap iteration order.
        // With 8 modules, there's only 1/40320 chance of accidental success.
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "output.ts"

            [css]
            mode = "none"
            -- alpha.hop --
            entrypoint AlphaPage() { <div>Alpha</div> }
            -- beta.hop --
            entrypoint BetaPage() { <div>Beta</div> }
            -- gamma.hop --
            entrypoint GammaPage() { <div>Gamma</div> }
            -- delta.hop --
            entrypoint DeltaPage() { <div>Delta</div> }
            -- epsilon.hop --
            entrypoint EpsilonPage() { <div>Epsilon</div> }
            -- zeta.hop --
            entrypoint ZetaPage() { <div>Zeta</div> }
            -- eta.hop --
            entrypoint EtaPage() { <div>Eta</div> }
            -- theta.hop --
            entrypoint ThetaPage() { <div>Theta</div> }
        "#});

        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(&temp_dir).unwrap();

        // Execute the compile command
        let result = execute(&project, false).await;
        assert!(
            result.is_ok(),
            "Compilation should succeed: {:?}",
            result.err()
        );

        // Read the generated output
        let output_path = temp_dir.join("output.ts");
        let generated_code = fs::read_to_string(&output_path).unwrap();

        // Extract the order of exported functions from the TypeScript output.
        // The functions should appear in alphabetical order by module name.
        let expected_order = [
            "AlphaPage",
            "BetaPage",
            "DeltaPage",
            "EpsilonPage",
            "EtaPage",
            "GammaPage",
            "ThetaPage",
            "ZetaPage",
        ];

        // Find positions of each function in the output and sort by position
        let mut positions: Vec<(usize, &str)> = expected_order
            .iter()
            .filter_map(|name| {
                let pattern = format!("export function {}(", name);
                generated_code.find(&pattern).map(|pos| (pos, *name))
            })
            .collect();

        assert_eq!(
            positions.len(),
            expected_order.len(),
            "Not all expected functions were found in the output"
        );

        positions.sort_by_key(|(pos, _)| *pos);
        let actual_order: Vec<&str> = positions.iter().map(|(_, name)| *name).collect();

        assert_eq!(
            actual_order,
            expected_order.to_vec(),
            "Entrypoints should appear in alphabetical order by module name.\n\
             Expected: {:?}\n\
             Actual: {:?}\n\
             TypeScript output:\n{}",
            expected_order,
            actual_order,
            generated_code
        );

        // Clean up
        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
