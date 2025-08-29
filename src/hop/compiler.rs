use crate::hop::server::HopMode;
use crate::hop::server::Server;
use crate::tui::error_formatter::ErrorFormatter;

pub fn compile(modules: Vec<(String, String)>, hop_mode: HopMode) -> anyhow::Result<Server> {
    let mut server = Server::new(hop_mode);
    for (module_name, source_code) in modules {
        server.update_module(module_name, &source_code);
    }

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
