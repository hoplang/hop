use crate::hop::program::HopMode;
use crate::hop::program::Program;
use crate::tui::source_annotator::SourceAnnotator;
use std::collections::HashMap;

pub fn compile(modules: HashMap<String, String>, hop_mode: HopMode) -> anyhow::Result<Program> {
    let server = Program::from_modules(modules, hop_mode);

    let source_code = server.get_source_code();
    let mut error_output_parts = Vec::new();

    // Check for parse errors
    let parse_errors = server.get_parse_errors();
    for (module_name, errors) in parse_errors {
        if !errors.is_empty() {
            let code = source_code.get(module_name).unwrap();
            let filename = format!("{}.hop", module_name);
            let annotator = SourceAnnotator::new()
                .with_label("error")
                .with_underline('^')
                .with_lines_before(1)
                .with_location()
                .with_filename(filename);
            let formatted_errors = annotator.add_annotations(code, errors);
            error_output_parts.push(formatted_errors);
        }
    }

    if !error_output_parts.is_empty() {
        anyhow::bail!(error_output_parts.join("\n"));
    }

    // Check for type errors
    let type_errors = server.get_type_errors();
    for (module_name, errors) in type_errors {
        if !errors.is_empty() {
            let code = source_code.get(module_name).unwrap();
            let filename = format!("{}.hop", module_name);
            let annotator = SourceAnnotator::new()
                .with_label("error")
                .with_underline('^')
                .with_lines_before(1)
                .with_location()
                .with_filename(filename);
            let formatted_errors = annotator.add_annotations(code, errors);
            error_output_parts.push(formatted_errors);
        }
    }

    if !error_output_parts.is_empty() {
        anyhow::bail!(error_output_parts.join("\n"));
    }

    Ok(server)
}
