use crate::common::{Position, RangeError};
use crate::parser::{Module, parse};
use crate::tokenizer::tokenize;
use crate::toposorter::TopoSorter;
use crate::typechecker::{TypeAnnotation, TypeResult, typecheck};
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
pub struct Diagnostic {
    pub message: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseResult {
    pub module: Module,
    pub errors: Vec<RangeError>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypecheckResult {
    pub type_result: TypeResult,
    pub errors: Vec<RangeError>,
}

pub struct Server {
    parse_results: HashMap<String, ParseResult>,
    type_results: HashMap<String, TypecheckResult>,
    topo_sorter: TopoSorter,
}

impl Server {
    pub fn new() -> Self {
        Server {
            parse_results: HashMap::new(),
            type_results: HashMap::new(),
            topo_sorter: TopoSorter::new(),
        }
    }

    pub fn update_module(&mut self, name: String, source_code: &str) {
        let mut parse_errors = Vec::new();
        let tokens = tokenize(source_code, &mut parse_errors);
        let module = parse(tokens, &mut parse_errors);

        let parse_result = ParseResult {
            module,
            errors: parse_errors,
        };

        self.parse_results.insert(name.clone(), parse_result);

        self.topo_sorter.clear_dependencies(&name);
        self.topo_sorter.add_node(name.clone());

        if let Some(parse_result) = self.parse_results.get(&name) {
            for import_node in &parse_result.module.imports {
                self.topo_sorter
                    .add_dependency(&name, &import_node.from_attr.value);
            }
        }

        let sort_result = self.topo_sorter.sort_subgraph(&name);
        let dependent_modules = if sort_result.error.is_some() {
            vec![name]
        } else {
            sort_result.nodes
        };

        for module_name in dependent_modules {
            self.typecheck_module(&module_name);
        }
    }

    fn typecheck_module(&mut self, module_name: &str) {
        let parse_result = match self.parse_results.get(module_name) {
            Some(result) => result,
            None => return,
        };

        let mut import_types = HashMap::new();

        for import_node in &parse_result.module.imports {
            let from_module = &import_node.from_attr.value;
            let component = &import_node.component_attr.value;

            if let Some(type_result) = self.type_results.get(from_module) {
                if let Some(param_type) = type_result.type_result.parameter_types.get(component) {
                    import_types.insert(component.clone(), param_type.clone());
                }
            }
        }

        let mut typecheck_errors = Vec::new();
        let type_result = typecheck(&parse_result.module, &import_types, &mut typecheck_errors);

        let typecheck_result = TypecheckResult {
            type_result,
            errors: typecheck_errors,
        };

        self.type_results
            .insert(module_name.to_string(), typecheck_result);
    }

    pub fn get_hover_info(
        &self,
        module_name: &str,
        line: usize,
        column: usize,
    ) -> Option<HoverInfo> {
        let type_result = self.type_results.get(module_name)?;
        let pos = Position::new(line, column);

        for TypeAnnotation(range, val) in &type_result.type_result.annotations {
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

    pub fn get_error_diagnostics(&self, module_name: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        if let Some(parse_result) = self.parse_results.get(module_name) {
            for error in &parse_result.errors {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    start_line: error.range.start.line,
                    start_column: error.range.start.column,
                    end_line: error.range.end.line,
                    end_column: error.range.end.column,
                });
            }
        }

        if let Some(type_result) = self.type_results.get(module_name) {
            for error in &type_result.errors {
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

    pub fn get_component_at_position(
        &self,
        module_name: &str,
        line: usize,
        column: usize,
    ) -> Option<String> {
        let parse_result = self.parse_results.get(module_name)?;
        let position = Position::new(line, column);

        for component in &parse_result.module.components {
            if component.range.contains_position(position) {
                return Some(component.name_attr.value.clone());
            }
        }

        None
    }
}
