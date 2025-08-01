use crate::common::Type;
use crate::error_formatter::ErrorFormatter;
use crate::parser::parse;
use crate::runtime::Program;
use crate::scriptcollector::ScriptCollector;
use crate::tokenizer::tokenize;
use crate::toposorter::TopoSorter;
use crate::typechecker::typecheck;
use std::collections::HashMap;

/// Compiler compiles hop modules into a Program that can execute components
pub struct Compiler {
    modules: HashMap<String, String>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, name: String, module: String) {
        self.modules.insert(name, module);
    }

    pub fn compile(&self) -> Result<Program, String> {
        let mut component_maps = HashMap::new();
        let mut import_maps = HashMap::new();
        let mut render_maps = HashMap::new();
        let mut modules = HashMap::new();
        let mut module_parameter_types: HashMap<String, HashMap<String, Type>> = HashMap::new();
        let mut module_sorter = TopoSorter::new();
        let mut script_collector = ScriptCollector::new();

        // Parse all modules
        for (module_name, source_code) in &self.modules {
            let mut errors = Vec::new();
            let tokens = tokenize(source_code, &mut errors);
            let module = parse(tokens, &mut errors);
            if !errors.is_empty() {
                let formatter =
                    ErrorFormatter::new(source_code.clone(), format!("{}.hop", module_name));
                let mut formatted_errors = String::new();
                for error in &errors {
                    formatted_errors.push_str(&formatter.format_error(error));
                    formatted_errors.push('\n');
                }
                return Err(formatted_errors);
            }

            module_sorter.add_node(module_name.clone());
            for import_node in &module.imports {
                module_sorter.add_dependency(module_name, &import_node.from_attr.value);
            }

            modules.insert(module_name, module);
        }

        // Sort modules topologically
        let sorted_modules = match module_sorter.sort() {
            Ok(nodes) => nodes,
            Err(error) => {
                return Err(format!(
                    "Circular module dependencies: {}",
                    error.cycle.join(" -> ")
                ));
            }
        };

        // Typecheck modules in topological order
        for module_name in sorted_modules {
            let Some(module) = modules.get(&module_name) else {
                continue;
            };
            let mut import_types = HashMap::new();

            for import_node in &module.imports {
                let from_module = &import_node.from_attr.value;
                let component_name = &import_node.component_attr.value;

                if let Some(from_module_types) = module_parameter_types.get(from_module) {
                    if let Some(component_type) = from_module_types.get(component_name) {
                        import_types.insert(component_name.clone(), component_type.clone());
                    } else {
                        return Err(format!(
                            "Component {} not found in module {}",
                            component_name, from_module
                        ));
                    }
                } else {
                    return Err(format!("Module {} not found", from_module));
                }
            }

            let mut errors = Vec::new();
            let type_info = typecheck(module, &import_types, &mut errors);
            if !errors.is_empty() {
                let source_code = self.modules.get(&module_name).unwrap();
                let formatter =
                    ErrorFormatter::new(source_code.clone(), format!("{}.hop", module_name));
                let mut formatted_errors = String::new();
                for error in &errors {
                    formatted_errors.push_str(&formatter.format_error(error));
                    formatted_errors.push('\n');
                }
                return Err(formatted_errors);
            }

            let mut component_map = HashMap::new();
            let mut import_map = HashMap::new();

            for comp_node in &module.components {
                component_map.insert(comp_node.name.clone(), comp_node.clone());
            }

            for import_node in &module.imports {
                import_map.insert(
                    import_node.component_attr.value.clone(),
                    import_node.from_attr.value.clone(),
                );
            }

            // Store build render nodes if any
            if !module.renders.is_empty() {
                render_maps.insert(module_name.clone(), module.renders.clone());
            }

            component_maps.insert(module_name.clone(), component_map);
            import_maps.insert(module_name.clone(), import_map);
            module_parameter_types.insert(module_name.clone(), type_info.parameter_types);

            // Collect scripts from this module
            script_collector.add_module(module_name.clone(), module.components.clone());
        }

        Ok(Program::new(
            component_maps,
            import_maps,
            module_parameter_types,
            render_maps,
            script_collector.build(),
        ))
    }
}
