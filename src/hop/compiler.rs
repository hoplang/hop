use crate::hop::parser::parse;
use crate::hop::runtime::{HopMode, Program};
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::TopoSorter;
use crate::hop::typechecker::{ComponentTypeInformation, typecheck};
use crate::tui::error_formatter::ErrorFormatter;
use std::collections::HashMap;

pub fn compile(modules: Vec<(String, String)>, hop_mode: HopMode) -> anyhow::Result<Program> {
    let source_code_map: HashMap<String, String> = modules.into_iter().collect();
    let mut asts = HashMap::new();
    let mut type_information: HashMap<String, HashMap<String, ComponentTypeInformation>> =
        HashMap::new();
    let mut module_sorter = TopoSorter::new();
    let mut errors = Vec::new();

    // Parse all modules
    let mut error_formatter = ErrorFormatter::new();
    for (module_name, source_code) in &source_code_map {
        let tokenizer = Tokenizer::new(source_code);
        errors.clear();
        let module = parse(module_name.clone(), tokenizer, &mut errors);
        if !errors.is_empty() {
            error_formatter.add_errors(module_name.clone(), source_code.clone(), errors.clone());
        }

        module_sorter.add_node(module_name.clone());
        for import_node in module.get_import_nodes() {
            module_sorter.add_dependency(module_name, &import_node.from_attr.value);
        }

        asts.insert(module_name.to_string(), module);
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    // Sort modules topologically
    let sorted_module_names = match module_sorter.sort() {
        Ok(nodes) => nodes,
        Err(error) => {
            anyhow::bail!(format!(
                "Circular module dependencies: {}",
                error.cycle.join(" -> ")
            ));
        }
    };

    // Typecheck modules in topological order
    for ast in sorted_module_names
        .into_iter()
        .filter_map(|name| asts.get(&name))
    {
        errors.clear();
        let type_info = typecheck(
            ast,
            &type_information,
            &mut errors,
            &mut vec![],
            &mut vec![],
        );
        if !errors.is_empty() {
            let source_code = source_code_map.get(&ast.name).unwrap();
            error_formatter.add_errors(ast.name.clone(), source_code.clone(), errors.clone());
        }
        type_information.insert(ast.name.clone(), type_info);
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    Ok(Program { asts, hop_mode })
}
