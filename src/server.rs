use crate::common::{Position, RangeError};
use crate::parser::{Module, parse};
use crate::tokenizer::tokenize;
use crate::toposorter::TopoSorter;
use crate::typechecker::{DefinitionLink, TypeAnnotation, TypeResult, typecheck};
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
        let tokens = tokenize(source_code, &mut parse_errors);
        let module = parse(name.clone(), tokens, &mut parse_errors);

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

        for TypeAnnotation(range, val) in &type_result.annotations {
            if range.contains_position(pos) {
                return Some(HoverInfo {
                    type_str: val.to_string(),
                    start_line: range.start.line,
                    start_column: range.start.column,
                    end_line: range.end.line,
                    end_column: range.end.column,
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

        for DefinitionLink {
            reference_range,
            definition_module,
            definition_range,
        } in &type_result.definition_links
        {
            if reference_range.contains_position(pos) {
                return Some(DefinitionLocation {
                    module: definition_module.clone(),
                    start_line: definition_range.start.line,
                    start_column: definition_range.start.column,
                    end_line: definition_range.end.line,
                    end_column: definition_range.end.column,
                });
            }
        }

        None
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
