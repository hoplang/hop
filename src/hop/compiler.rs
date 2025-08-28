use crate::hop::parser::parse;
use crate::hop::runtime::{HopMode, Program};
use crate::hop::script_collector::ScriptCollector;
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::TopoSorter;
use crate::hop::typechecker::{ComponentTypeInformation, typecheck};
use crate::tui::error_formatter::ErrorFormatter;
use std::collections::HashMap;

pub fn compile(modules: Vec<(String, String)>, hop_mode: HopMode) -> anyhow::Result<Program> {
    let modules_map: HashMap<String, String> = modules.into_iter().collect();
    let mut modules = Vec::new();
    let mut type_results: HashMap<String, HashMap<String, ComponentTypeInformation>> = HashMap::new();
    let mut module_sorter = TopoSorter::new();
    let mut script_collector = ScriptCollector::new();

    // Parse all modules
    let mut error_formatter = ErrorFormatter::new();
    for (module_name, source_code) in &modules_map {
        let mut errors = Vec::new();
        let tokenizer = Tokenizer::new(source_code);
        let module = parse(module_name.clone(), tokenizer, &mut errors);
        if !errors.is_empty() {
            error_formatter.add_errors(module_name.clone(), source_code.clone(), errors);
        }

        module_sorter.add_node(module_name.clone());
        for import_node in &module.import_nodes {
            module_sorter.add_dependency(module_name, &import_node.from_attr.value);
        }

        modules.push(module);
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    // Sort modules topologically
    let sorted_modules = match module_sorter.sort() {
        Ok(nodes) => nodes,
        Err(error) => {
            anyhow::bail!(format!(
                "Circular module dependencies: {}",
                error.cycle.join(" -> ")
            ));
        }
    };

    // Typecheck modules in topological order
    for module_name in sorted_modules {
        let Some(module) = modules.iter().find(|m| m.name == module_name) else {
            continue;
        };
        let mut errors = Vec::new();
        let mut type_annotations = Vec::new();
        let mut component_definition_links = Vec::new();
        let type_info = typecheck(module, &type_results, &mut errors, &mut type_annotations, &mut component_definition_links);
        if !errors.is_empty() {
            let source_code = modules_map.get(&module_name).unwrap();
            error_formatter.add_errors(module_name.clone(), source_code.clone(), errors);
        }

        type_results.insert(module_name.clone(), type_info);

        // Process scripts from this module
        script_collector.process_module(&module_name, &module.component_nodes);
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    Ok(Program::new(modules, script_collector.build(), hop_mode))
}
