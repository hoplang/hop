use crate::common::format_range_errors;
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
        let mut modules = HashMap::new();
        let mut module_parameter_types: HashMap<String, HashMap<String, crate::common::Type>> =
            HashMap::new();
        let mut script_collector = ScriptCollector::new();
        let mut module_sorter = TopoSorter::new();

        // Parse all modules
        for (module_name, source_code) in &self.modules {
            let result = parse(tokenize(source_code));
            if !result.errors.is_empty() {
                return Err(format_range_errors(
                    &format!("Parse errors in module {}", module_name),
                    &result.errors,
                ));
            }

            modules.insert(module_name.clone(), result.module.clone());
            script_collector.add_module(module_name.clone(), result.module.components);

            module_sorter.add_node(module_name.clone());
            for import_node in &result.module.imports {
                module_sorter.add_dependency(module_name, &import_node.from_attr.value);
            }
        }

        // Sort modules topologically
        let sort_result = module_sorter.sort();
        if let Some(error) = sort_result.error {
            return Err(format!(
                "Circular module dependencies: {}",
                error.cycle.join(" -> ")
            ));
        }
        let sorted_modules = sort_result.nodes;

        // Typecheck modules in topological order
        for module_name in sorted_modules {
            let module = modules.get(&module_name).unwrap();
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

            let type_info = typecheck(module, &import_types);
            if !type_info.errors.is_empty() {
                return Err(format_range_errors(
                    &format!("Type errors in module {}", module_name),
                    &type_info.errors,
                ));
            }

            let mut component_map = HashMap::new();
            let mut import_map = HashMap::new();

            for comp_node in &module.components {
                component_map.insert(comp_node.name_attr.value.clone(), comp_node.clone());
            }

            for import_node in &module.imports {
                import_map.insert(
                    import_node.component_attr.value.clone(),
                    import_node.from_attr.value.clone(),
                );
            }

            component_maps.insert(module_name.clone(), component_map);
            import_maps.insert(module_name.clone(), import_map);
            module_parameter_types.insert(module_name.clone(), type_info.parameter_types);
        }

        Ok(Program::new(
            component_maps,
            import_maps,
            script_collector.build(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Position, Range, Token, TokenKind};
    use pretty_assertions::assert_eq;
    use simple_txtar::Archive;
    use std::fs;
    use std::path::Path;

    fn normalize_tokens(tokens: Vec<Token>) -> Vec<Token> {
        tokens
            .into_iter()
            .map(|t| {
                if t.kind == TokenKind::Text && t.value.trim().is_empty() {
                    Token::new(
                        t.kind,
                        " ".to_string(),
                        t.attributes,
                        Range::new(Position::new(0, 0), Position::new(0, 0)),
                    )
                } else {
                    Token::new(
                        t.kind,
                        t.value,
                        t.attributes,
                        Range::new(Position::new(0, 0), Position::new(0, 0)),
                    )
                }
            })
            .collect()
    }

    #[test]
    fn test_compiler() {
        let entries = fs::read_dir(Path::new("test_data/compiler")).unwrap();

        for entry in entries {
            let path = entry.unwrap().path();

            let file_name = path.file_name().unwrap().to_string_lossy();

            let archive = Archive::from(fs::read_to_string(&path).unwrap());

            let data_json = archive.get("data.json").unwrap().content.trim();
            let expected_output = archive.get("output.html").unwrap().content.trim();

            let mut compiler = Compiler::new();

            // Add all .hop files as modules
            for file in archive.iter() {
                if file.name.ends_with(".hop") {
                    let module_name = file.name.trim_end_matches(".hop");
                    compiler.add_module(module_name.to_string(), file.content.trim().to_string());
                }
            }

            let program = compiler.compile().unwrap_or_else(|e| {
                panic!("Compilation failed for {}: {}", file_name, e);
            });

            println!("{}", file_name);

            let data: serde_json::Value = serde_json::from_str(data_json).unwrap_or_else(|e| {
                panic!("Failed to parse JSON data for {}: {}", file_name, e);
            });

            let actual_output = program.execute("main", "main", data).unwrap_or_else(|e| {
                panic!("Execution failed for {}: {}", file_name, e);
            });

            // Normalize whitespace by tokenizing both outputs and comparing tokens
            let expected_tokens = normalize_tokens(tokenize(expected_output));
            let actual_tokens = normalize_tokens(tokenize(&actual_output));

            assert_eq!(
                actual_tokens, expected_tokens,
                "Mismatch in file: {}",
                file_name
            );
        }
    }
}
