use crate::hop::program::HopMode;
use crate::hop::program::Program;
use crate::tui::error_formatter::ErrorFormatter;
use std::collections::HashMap;

pub fn compile(modules: HashMap<String, String>, hop_mode: HopMode) -> anyhow::Result<Program> {
    let server = Program::from_modules(modules, hop_mode);

    let source_code = server.get_source_code();

    let mut error_formatter = ErrorFormatter::new();

    let parse_errors = server.get_parse_errors();
    for (module_name, errors) in parse_errors {
        if !errors.is_empty() {
            let code = source_code.get(module_name).unwrap();
            error_formatter.add_errors(module_name.clone(), code.clone(), errors.clone());
        }
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    let type_errors = server.get_type_errors();
    for (module_name, errors) in type_errors {
        if !errors.is_empty() {
            let code = source_code.get(module_name).unwrap();
            error_formatter.add_errors(module_name.clone(), code.clone(), errors.clone());
        }
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    Ok(server)
}
