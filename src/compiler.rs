use crate::error_formatter::ErrorFormatter;
use crate::hop::parser::parse;
use crate::runtime::{HopMode, Program};
use crate::scriptcollector::ScriptCollector;
use crate::hop::tokenizer::Tokenizer;
use crate::toposorter::TopoSorter;
use crate::typechecker::{TypeResult, typecheck};
use std::collections::HashMap;

pub fn compile(modules: Vec<(String, String)>, hop_mode: HopMode) -> anyhow::Result<Program> {
    let modules_map: HashMap<String, String> = modules.into_iter().collect();
    let mut parsed_modules = HashMap::new();
    let mut module_type_results: HashMap<String, TypeResult> = HashMap::new();
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
        for import_node in &module.imports {
            module_sorter.add_dependency(module_name, &import_node.from_attr.value);
        }

        parsed_modules.insert(module_name.clone(), module);
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
        let Some(module) = parsed_modules.get(&module_name) else {
            continue;
        };
        let mut errors = Vec::new();
        let type_info = typecheck(module, &module_type_results, &mut errors);
        if !errors.is_empty() {
            let source_code = modules_map.get(&module_name).unwrap();
            error_formatter.add_errors(module_name.clone(), source_code.clone(), errors);
        }

        module_type_results.insert(module_name.clone(), type_info);

        // Collect scripts from this module
        script_collector.add_module(module_name.clone(), module.components.clone());
    }

    if error_formatter.has_errors() {
        anyhow::bail!(error_formatter.format_all_errors());
    }

    Ok(Program::new(
        parsed_modules,
        module_type_results,
        script_collector.build(),
        hop_mode,
    ))
}
