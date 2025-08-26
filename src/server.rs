use crate::common::{Position, Range, RangeError};
use crate::parser::{Module, parse};
use crate::tokenizer::Tokenizer;
use crate::toposorter::TopoSorter;
use crate::typechecker::{ComponentDefinitionLink, TypeResult, typecheck};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct HoverInfo {
    pub type_str: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionLocation {
    pub module: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub message: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenameLocation {
    pub module: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenameableSymbol {
    pub current_name: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

pub struct Server {
    modules: HashMap<String, Module>,
    type_results: HashMap<String, TypeResult>,
    parse_errors: HashMap<String, Vec<RangeError>>,
    type_errors: HashMap<String, Vec<RangeError>>,
    topo_sorter: TopoSorter,
}

impl Server {
    pub fn new() -> Self {
        Server {
            modules: HashMap::new(),
            type_results: HashMap::new(),
            parse_errors: HashMap::new(),
            type_errors: HashMap::new(),
            topo_sorter: TopoSorter::new(),
        }
    }

    pub fn has_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub fn update_module(&mut self, name: String, source_code: &str) -> Vec<String> {
        let mut parse_errors = Vec::new();
        let tokenizer = Tokenizer::new(source_code);
        let module = parse(name.clone(), tokenizer, &mut parse_errors);

        self.topo_sorter.clear_dependencies(&name);
        self.topo_sorter.add_node(name.clone());

        for import_node in &module.imports {
            self.topo_sorter
                .add_dependency(&name, &import_node.from_attr.value);
        }

        self.modules.insert(name.clone(), module);
        self.parse_errors.insert(name.clone(), parse_errors);

        let dependent_modules = match self.topo_sorter.sort_subgraph(&name) {
            Ok(nodes) => nodes,
            Err(_) => vec![name],
        };

        for module_name in &dependent_modules {
            let module = match self.modules.get(module_name) {
                Some(module) => module,
                None => continue,
            };

            let mut type_errors = Vec::new();
            let type_result = typecheck(module, &self.type_results, &mut type_errors);

            self.type_results
                .insert(module_name.to_string(), type_result);
            self.type_errors
                .insert(module_name.to_string(), type_errors);
        }

        dependent_modules
    }

    pub fn get_hover_info(
        &self,
        module_name: &str,
        line: usize,
        column: usize,
    ) -> Option<HoverInfo> {
        let type_result = self.type_results.get(module_name)?;
        let pos = Position::new(line, column);

        for annotation in &type_result.annotations {
            if annotation.range.contains_position(pos) {
                return Some(HoverInfo {
                    type_str: format!("`{}`: `{}`", annotation.name, annotation.typ),
                    start_line: annotation.range.start.line,
                    start_column: annotation.range.start.column,
                    end_line: annotation.range.end.line,
                    end_column: annotation.range.end.column,
                });
            }
        }

        None
    }

    pub fn get_definition(
        &self,
        module_name: &str,
        line: usize,
        column: usize,
    ) -> Option<DefinitionLocation> {
        let type_result = self.type_results.get(module_name)?;
        let pos = Position::new(line, column);

        for ComponentDefinitionLink {
            reference_opening_name_range,
            reference_closing_name_range,
            definition_module,
            definition_opening_name_range,
            ..
        } in &type_result.definition_links
        {
            // Check if cursor is on the opening name range
            if reference_opening_name_range.contains_position(pos) {
                return Some(DefinitionLocation {
                    module: definition_module.clone(),
                    start_line: definition_opening_name_range.start.line,
                    start_column: definition_opening_name_range.start.column,
                    end_line: definition_opening_name_range.end.line,
                    end_column: definition_opening_name_range.end.column,
                });
            }

            // Check if cursor is on the closing name range (if it exists)
            if let Some(closing_range) = reference_closing_name_range {
                if closing_range.contains_position(pos) {
                    return Some(DefinitionLocation {
                        module: definition_module.clone(),
                        start_line: definition_opening_name_range.start.line,
                        start_column: definition_opening_name_range.start.column,
                        end_line: definition_opening_name_range.end.line,
                        end_column: definition_opening_name_range.end.column,
                    });
                }
            }
        }

        None
    }

    pub fn get_rename_locations(
        &self,
        module_name: &str,
        line: usize,
        column: usize,
    ) -> Option<Vec<RenameLocation>> {
        let type_result = self.type_results.get(module_name)?;
        let pos = Position::new(line, column);

        // First, check if we're on a component reference
        for ComponentDefinitionLink {
            reference_opening_name_range,
            reference_closing_name_range,
            definition_module,
            definition_component_name,
            definition_opening_name_range,
            definition_closing_name_range,
            ..
        } in &type_result.definition_links
        {
            // Check if cursor is on the opening or closing name of a reference
            let is_on_reference = reference_opening_name_range.contains_position(pos)
                || reference_closing_name_range
                    .as_ref()
                    .is_some_and(|range| range.contains_position(pos));

            if is_on_reference {
                // We found the component being renamed
                return Some(self.collect_all_rename_locations(
                    definition_component_name,
                    definition_module,
                    *definition_opening_name_range,
                    *definition_closing_name_range,
                ));
            }
        }

        // Check if we're on a component definition
        for (component_name, component_info) in &type_result.component_info {
            if component_info.definition_module == module_name {
                let is_on_definition = component_info
                    .definition_opening_name_range
                    .contains_position(pos)
                    || component_info
                        .definition_closing_name_range
                        .as_ref()
                        .is_some_and(|range| range.contains_position(pos));

                if is_on_definition {
                    // We're on a definition, collect all rename locations
                    return Some(self.collect_all_rename_locations(
                        component_name,
                        &component_info.definition_module,
                        component_info.definition_opening_name_range,
                        component_info.definition_closing_name_range,
                    ));
                }
            }
        }

        None
    }

    pub fn get_renameable_symbol(
        &self,
        module_name: &str,
        line: usize,
        column: usize,
    ) -> Option<RenameableSymbol> {
        let type_result = self.type_results.get(module_name)?;
        let pos = Position::new(line, column);

        // First, check if we're on a component reference
        for ComponentDefinitionLink {
            reference_opening_name_range,
            reference_closing_name_range,
            definition_component_name,
            ..
        } in &type_result.definition_links
        {
            // Check if cursor is on the opening name of a reference
            if reference_opening_name_range.contains_position(pos) {
                return Some(RenameableSymbol {
                    current_name: definition_component_name.clone(),
                    start_line: reference_opening_name_range.start.line,
                    start_column: reference_opening_name_range.start.column,
                    end_line: reference_opening_name_range.end.line,
                    end_column: reference_opening_name_range.end.column,
                });
            }

            // Check if cursor is on the closing name of a reference
            if let Some(closing_range) = reference_closing_name_range {
                if closing_range.contains_position(pos) {
                    return Some(RenameableSymbol {
                        current_name: definition_component_name.clone(),
                        start_line: closing_range.start.line,
                        start_column: closing_range.start.column,
                        end_line: closing_range.end.line,
                        end_column: closing_range.end.column,
                    });
                }
            }
        }

        // Check if we're on a component definition
        for (component_name, component_info) in &type_result.component_info {
            if component_info.definition_module == module_name {
                // Check if cursor is on the opening name of a definition
                if component_info
                    .definition_opening_name_range
                    .contains_position(pos)
                {
                    return Some(RenameableSymbol {
                        current_name: component_name.clone(),
                        start_line: component_info.definition_opening_name_range.start.line,
                        start_column: component_info.definition_opening_name_range.start.column,
                        end_line: component_info.definition_opening_name_range.end.line,
                        end_column: component_info.definition_opening_name_range.end.column,
                    });
                }

                // Check if cursor is on the closing name of a definition
                if let Some(closing_range) = component_info.definition_closing_name_range {
                    if closing_range.contains_position(pos) {
                        return Some(RenameableSymbol {
                            current_name: component_name.clone(),
                            start_line: closing_range.start.line,
                            start_column: closing_range.start.column,
                            end_line: closing_range.end.line,
                            end_column: closing_range.end.column,
                        });
                    }
                }
            }
        }

        None
    }

    fn collect_all_rename_locations(
        &self,
        component_name: &str,
        definition_module: &str,
        definition_opening_range: Range,
        definition_closing_range: Option<Range>,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        // Add the definition's opening tag name
        locations.push(RenameLocation {
            module: definition_module.to_string(),
            start_line: definition_opening_range.start.line,
            start_column: definition_opening_range.start.column,
            end_line: definition_opening_range.end.line,
            end_column: definition_opening_range.end.column,
        });

        // Add the definition's closing tag name if it exists
        if let Some(closing_range) = definition_closing_range {
            locations.push(RenameLocation {
                module: definition_module.to_string(),
                start_line: closing_range.start.line,
                start_column: closing_range.start.column,
                end_line: closing_range.end.line,
                end_column: closing_range.end.column,
            });
        }

        // Now find all references to this component across all modules
        for (module_name, type_result) in &self.type_results {
            for link in &type_result.definition_links {
                // Check if this link refers to our component definition
                if link.definition_component_name == component_name
                    && link.definition_module == definition_module
                {
                    // Add the reference's opening tag name
                    locations.push(RenameLocation {
                        module: module_name.clone(),
                        start_line: link.reference_opening_name_range.start.line,
                        start_column: link.reference_opening_name_range.start.column,
                        end_line: link.reference_opening_name_range.end.line,
                        end_column: link.reference_opening_name_range.end.column,
                    });

                    // Add the reference's closing tag name if it exists
                    if let Some(ref_closing_range) = link.reference_closing_name_range {
                        locations.push(RenameLocation {
                            module: module_name.clone(),
                            start_line: ref_closing_range.start.line,
                            start_column: ref_closing_range.start.column,
                            end_line: ref_closing_range.end.line,
                            end_column: ref_closing_range.end.column,
                        });
                    }
                }
            }
        }

        // Find all import statements that import this component
        for (module_name, module) in &self.modules {
            for import in &module.imports {
                if import.component_attr.value == component_name
                    && import.from_attr.value == definition_module
                {
                    // This import statement imports our component
                    locations.push(RenameLocation {
                        module: module_name.clone(),
                        start_line: import.component_attr.value_range.start.line,
                        start_column: import.component_attr.value_range.start.column,
                        end_line: import.component_attr.value_range.end.line,
                        end_column: import.component_attr.value_range.end.column,
                    });
                }
            }
        }

        locations
    }

    pub fn get_error_diagnostics(&self, module_name: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        if let Some(parse_errors) = self.parse_errors.get(module_name) {
            for error in parse_errors {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    start_line: error.range.start.line,
                    start_column: error.range.start.column,
                    end_line: error.range.end.line,
                    end_column: error.range.end.column,
                });
            }
        }

        if let Some(typecheck_errors) = self.type_errors.get(module_name) {
            for error in typecheck_errors {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    start_line: error.range.start.line,
                    start_column: error.range.start.column,
                    end_line: error.range.end.line,
                    end_column: error.range.end.column,
                });
            }
        }

        diagnostics
    }
}
