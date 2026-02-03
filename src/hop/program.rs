use crate::document::DocumentPosition;
use crate::document::{Document, DocumentRange, Ranged};
use crate::dop::{ParsedType, Type};
use crate::error_collector::ErrorCollector;
use crate::hop::syntax::format;
use crate::hop::syntax::parser::parse;
use crate::ir;
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use crate::parse_error::ParseError;
use crate::toposorter::TopoSorter;
use crate::type_error::TypeError;
use anyhow::Result;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::semantics::definition_link::DefinitionLink;
use super::semantics::type_annotation::TypeAnnotation;
use super::semantics::type_checker::typecheck;
use super::semantics::typed_ast::TypedAst;
use super::symbols::module_id::ModuleId;
use super::syntax::find_node::find_node_at_position;
use super::syntax::parsed_ast::ParsedAst;
use super::syntax::parsed_node::ParsedNode;
use crate::dop::symbols::type_name::TypeName;

/// HoverInfo is a message that should be displayed when the user hovers
/// a specific range in the source code.
pub struct HoverInfo {
    pub message: String,
    pub range: DocumentRange,
}

/// A DefinitionLocation is the definition of a certain symbol in the source
/// code. This is the response for a go to definition-query.
pub struct DefinitionLocation {
    pub range: DocumentRange,
}

/// A diagnostic is an error, warning or information that should be displayed
/// for a specific range in the document.
pub struct Diagnostic {
    pub message: String,
    pub range: DocumentRange,
}

pub struct RenameLocation {
    pub range: DocumentRange,
}

/// A RenameableSymbol is a range in the document that is renameable.
pub struct RenameableSymbol {
    pub range: DocumentRange,
}

impl RenameableSymbol {
    pub fn current_name(&self) -> &str {
        self.range.as_str()
    }
}

#[derive(Debug, Default)]
pub struct Program {
    topo_sorter: TopoSorter<ModuleId>,
    documents: HashMap<ModuleId, Document>,
    parse_errors: HashMap<ModuleId, ErrorCollector<ParseError>>,
    parsed_asts: HashMap<ModuleId, ParsedAst>,
    typechecker_state: HashMap<ModuleId, HashMap<String, (Arc<Type>, DocumentRange)>>,
    type_errors: HashMap<ModuleId, ErrorCollector<TypeError>>,
    type_annotations: HashMap<ModuleId, Vec<TypeAnnotation>>,
    definition_links: HashMap<ModuleId, Vec<DefinitionLink>>,
    typed_asts: HashMap<ModuleId, TypedAst>,
}

impl Program {
    pub fn new(modules: HashMap<ModuleId, Document>) -> Self {
        let mut program = Self::default();
        for (module_id, document) in modules {
            program.update_module(&module_id, document);
        }
        program
    }

    /// Remove a module from the program.
    ///
    /// This cleans up all state associated with the module and re-typechecks
    /// any modules that depended on it (since their imports are now broken).
    pub fn remove_module(&mut self, module_id: &ModuleId) {
        // Remove document and parsed state
        self.documents.remove(module_id);
        self.parse_errors.remove(module_id);
        self.parsed_asts.remove(module_id);
        self.typechecker_state.remove(module_id);
        self.type_errors.remove(module_id);
        self.type_annotations.remove(module_id);
        self.typed_asts.remove(module_id);

        // Clear the module's dependencies in the topo sorter (but keep the node
        // so that reverse dependencies are preserved). This returns all modules
        // that need to be re-typechecked.
        let grouped_modules = self
            .topo_sorter
            .update_node(module_id.clone(), HashSet::new());

        // Re-typecheck dependent modules (they now have broken imports)
        for names in grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_asts.get(name))
                .collect::<Vec<_>>();
            if !modules.is_empty() {
                typecheck(
                    &modules,
                    &mut self.typechecker_state,
                    &mut self.type_errors,
                    &mut self.type_annotations,
                    &mut self.definition_links,
                    &mut self.typed_asts,
                );
            }
        }
    }

    /// Update or add a module to the program.
    ///
    /// This parses the document, updates the dependency graph, and re-typechecks
    /// the module along with any modules that depend on it (directly or transitively).
    ///
    /// Returns the list of all module IDs that were re-typechecked.
    pub fn update_module(&mut self, module_id: &ModuleId, document: Document) -> Vec<ModuleId> {
        // Store the document
        self.documents.insert(module_id.clone(), document.clone());

        // Parse the module
        let parse_errors = self.parse_errors.entry(module_id.clone()).or_default();
        parse_errors.clear();
        let parsed_ast = parse(module_id.clone(), document, parse_errors);

        // Get all modules that this module depends on
        let module_dependencies = parsed_ast
            .get_import_declarations()
            .map(|import_node| import_node.imported_module().clone())
            .collect::<HashSet<ModuleId>>();

        // Store the AST
        self.parsed_asts.insert(module_id.clone(), parsed_ast);

        // Typecheck the module along with all dependent modules (grouped
        // into strongly connected components).
        let grouped_modules = self
            .topo_sorter
            .update_node(module_id.clone(), module_dependencies);
        for names in &grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_asts.get(name))
                .collect::<Vec<_>>();
            typecheck(
                &modules,
                &mut self.typechecker_state,
                &mut self.type_errors,
                &mut self.type_annotations,
                &mut self.definition_links,
                &mut self.typed_asts,
            );
        }

        // Return all modules that have been re-typechecked
        grouped_modules.into_iter().flatten().collect()
    }

    pub fn get_parse_errors(&self) -> &HashMap<ModuleId, ErrorCollector<ParseError>> {
        &self.parse_errors
    }

    pub fn get_type_errors(&self) -> &HashMap<ModuleId, ErrorCollector<TypeError>> {
        &self.type_errors
    }

    /// Returns the formatted source code for a module.
    ///
    /// Returns an error if the module doesn't exist or has parse errors.
    pub fn get_formatted_module(&self, module_id: &ModuleId) -> Result<String> {
        // Check if module exists
        let ast = self
            .parsed_asts
            .get(module_id)
            .ok_or_else(|| anyhow::anyhow!("Module '{}' not found", module_id))?;

        // Check for parse errors
        if let Some(errors) = self.parse_errors.get(module_id) {
            if !errors.is_empty() {
                return Err(anyhow::anyhow!(
                    "Cannot format module '{}': has parse errors",
                    module_id
                ));
            }
        }

        Ok(format(ast.clone()))
    }

    /// Returns all module sources concatenated into a single string.
    pub fn get_all_sources(&self) -> String {
        self.documents
            .values()
            .map(|doc| doc.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn get_hover_info(
        &self,
        module_id: &ModuleId,
        position: DocumentPosition,
    ) -> Option<HoverInfo> {
        self.type_annotations
            .get(module_id)?
            .iter()
            .find(|a| a.range.contains_position(position))
            .map(|annotation| HoverInfo {
                message: format!("`{}`: `{}`", annotation.name, annotation.typ),
                range: annotation.range.clone(),
            })
    }

    pub fn get_definition_location(
        &self,
        module_id: &ModuleId,
        position: DocumentPosition,
    ) -> Option<DefinitionLocation> {
        if let Some(links) = self.definition_links.get(module_id) {
            for link in links {
                if link.use_range.contains_position(position) {
                    return Some(DefinitionLocation {
                        range: link.definition_range.clone(),
                    });
                }
            }
        }
        None
    }

    pub fn get_rename_locations(
        &self,
        module_id: &ModuleId,
        position: DocumentPosition,
    ) -> Option<Vec<RenameLocation>> {
        let ast = self.parsed_asts.get(module_id)?;

        // Check if cursor is on a record declaration name
        for record in ast.get_record_declarations() {
            if record.name_range.contains_position(position) {
                return Some(self.collect_record_rename_locations(record.name(), module_id));
            }
        }

        for node in ast.get_component_declarations() {
            if node
                .tag_name_ranges()
                .any(|r| r.contains_position(position))
            {
                return Some(
                    self.collect_component_rename_locations(&node.component_name, module_id),
                );
            }
        }

        let node = find_node_at_position(ast, position)?;

        let is_on_tag_name = node.tag_names().any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            ParsedNode::ComponentReference {
                component_name,
                declaring_module: definition_location,
                ..
            } => definition_location.as_ref().map(|target_module| {
                self.collect_component_rename_locations(component_name, target_module)
            }),
            n @ ParsedNode::Html { .. } => Some(
                n.tag_names()
                    .map(|range| RenameLocation {
                        range: range.clone(),
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    /// Returns information about a renameable symbol at the given position.
    ///
    /// Checks if the position is on a component name, record name (reference or definition)
    /// and returns the symbol's current name and range if found.
    pub fn get_renameable_symbol(
        &self,
        module_id: &ModuleId,
        position: DocumentPosition,
    ) -> Option<RenameableSymbol> {
        let ast = self.parsed_asts.get(module_id)?;

        // Check if cursor is on a record declaration name
        for record in ast.get_record_declarations() {
            if record.name_range.contains_position(position) {
                return Some(RenameableSymbol {
                    range: record.name_range.clone(),
                });
            }
        }

        for component_node in ast.get_component_declarations() {
            if let Some(range) = component_node
                .tag_name_ranges()
                .find(|r| r.contains_position(position))
            {
                return Some(RenameableSymbol {
                    range: range.clone(),
                });
            }
        }

        let node = find_node_at_position(ast, position)?;

        node.tag_names()
            .find(|r| r.contains_position(position))
            .map(|range| RenameableSymbol {
                range: range.clone(),
            })
    }

    /// Collects all locations where a component should be renamed, including:
    /// - The component definition (opening and closing tags)
    /// - All references to the component (opening and closing tags)
    /// - All import statements that import the component
    fn collect_component_rename_locations(
        &self,
        component_name: &TypeName,
        definition_module: &ModuleId,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        if let Some(module) = self.parsed_asts.get(definition_module) {
            if let Some(component_node) = module.get_component_declaration(component_name.as_str())
            {
                // Add the definition's opening tag name
                locations.push(RenameLocation {
                    range: component_node.tag_name.clone(),
                });

                // Add the definition's closing tag name if it exists
                if let Some(range) = component_node.closing_tag_name.as_ref() {
                    locations.push(RenameLocation {
                        range: range.clone(),
                    });
                }
            }
        }

        for ast in self.parsed_asts.values() {
            // Find all import statements that import this component
            locations.extend(
                ast.get_import_declarations()
                    .filter(|n| {
                        n.imports_type(component_name.as_str()) && n.imports_from(definition_module)
                    })
                    .map(|n| RenameLocation {
                        range: n.type_name_range().clone(),
                    }),
            );

            // Find all component references that references this component
            locations.extend(
                ast.iter_all_nodes()
                    .filter(|node| match node {
                        ParsedNode::ComponentReference {
                            component_name: ref_component_name,
                            declaring_module: reference_definition_module,
                            ..
                        } => {
                            reference_definition_module
                                .as_ref()
                                .is_some_and(|d| d == definition_module)
                                && ref_component_name == component_name
                        }
                        _ => false,
                    })
                    .flat_map(|node| node.tag_names())
                    .map(|range| RenameLocation {
                        range: range.clone(),
                    }),
            );
        }

        locations
    }

    /// Collects all locations where a record type should be renamed, including:
    /// - The record declaration
    /// - All type annotations that reference the record
    /// - All import statements that import the record
    fn collect_record_rename_locations(
        &self,
        record_name: &str,
        definition_module: &ModuleId,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        // Add the record declaration itself
        if let Some(module) = self.parsed_asts.get(definition_module) {
            if let Some(record) = module.get_record_declaration(record_name) {
                locations.push(RenameLocation {
                    range: record.name_range.clone(),
                });
            }
        }

        // Search all modules for references to this record
        for (module_id, ast) in &self.parsed_asts {
            // Find all import statements that import this record
            locations.extend(
                ast.get_import_declarations()
                    .filter(|n| n.imports_type(record_name) && n.imports_from(definition_module))
                    .map(|n| RenameLocation {
                        range: n.type_name_range().clone(),
                    }),
            );

            // Find all type references in component parameters
            for component in ast.get_component_declarations() {
                if let Some((params, _)) = &component.params {
                    for param in params {
                        locations.extend(self.collect_type_references(
                            &param.var_type,
                            record_name,
                            definition_module,
                            module_id,
                        ));
                    }
                }
            }
        }

        locations
    }

    /// Recursively collects all references to a named type within a ParsedType.
    /// This handles nested types like Array[Icon].
    fn collect_type_references(
        &self,
        parsed_type: &ParsedType,
        record_name: &str,
        definition_module: &ModuleId,
        current_module: &ModuleId,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        match parsed_type {
            ParsedType::Named { name, range } => {
                // Check if this is a reference to the record we're renaming
                // For local references (same module), just match by name
                // For imported references, we need to verify the import points to the right module
                let is_local = current_module == definition_module && name == record_name;
                let is_imported = self.parsed_asts.get(current_module).is_some_and(|ast| {
                    ast.get_import_declarations().any(|import| {
                        import.imports_type(record_name)
                            && import.imports_from(definition_module)
                            && name == record_name
                    })
                });

                if is_local || is_imported {
                    locations.push(RenameLocation {
                        range: range.clone(),
                    });
                }
            }
            ParsedType::Array { element, .. } | ParsedType::Option { element, .. } => {
                // Recursively check the element type
                locations.extend(self.collect_type_references(
                    element,
                    record_name,
                    definition_module,
                    current_module,
                ));
            }
            // Primitive types don't need renaming
            ParsedType::String { .. }
            | ParsedType::Bool { .. }
            | ParsedType::Int { .. }
            | ParsedType::Float { .. }
            | ParsedType::TrustedHTML { .. } => {}
        }

        locations
    }

    pub fn get_error_diagnostics(&self, module_id: ModuleId) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let mut found_parse_errors = false;

        if let Some(errors) = self.parse_errors.get(&module_id) {
            for error in errors {
                diagnostics.push(Diagnostic {
                    message: error.to_string(),
                    range: error.range().clone(),
                });
                found_parse_errors = true;
            }
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            if let Some(errors) = self.type_errors.get(&module_id) {
                for error in errors {
                    diagnostics.push(Diagnostic {
                        message: error.to_string(),
                        range: error.range().clone(),
                    });
                }
            }
        }

        diagnostics
    }

    /// Evaluate an entrypoint given module and entrypoint name.
    pub fn evaluate_entrypoint(
        &self,
        module_id: &ModuleId,
        entrypoint_name: &TypeName,
        args: HashMap<String, serde_json::Value>,
        generated_tailwind_css: Option<&str>,
        skip_optimization: bool,
    ) -> Result<String> {
        let fn_start = std::time::Instant::now();
        crate::log_info!(
            "evaluate_entrypoint",
            step = "enter",
            module = module_id.to_string(),
            entrypoint = entrypoint_name.to_string(),
        );

        // Validate that the module exists
        let module = self.get_typed_modules().get(module_id).ok_or_else(|| {
            anyhow::anyhow!(
                "Module '{}' not found. Available modules: {}",
                module_id,
                self.get_typed_modules()
                    .keys()
                    .map(|m| m.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })?;

        // Check if the entrypoint exists in this module
        let entrypoint_exists = module
            .get_entrypoint_declarations()
            .iter()
            .any(|ep| ep.name.as_str() == entrypoint_name.as_str());

        if !entrypoint_exists {
            let available_entrypoints: Vec<_> = module
                .get_entrypoint_declarations()
                .iter()
                .map(|ep| ep.name.as_str())
                .collect();

            anyhow::bail!(
                "Entrypoint '{}' not found in module '{}'. Available entrypoints: {}",
                entrypoint_name,
                module_id,
                available_entrypoints.join(", ")
            );
        }

        // Use orchestrate to handle inlining and compilation
        // Pass the entrypoint filter to only compile the requested entrypoint
        let orchestrate_start = std::time::Instant::now();
        let ir_module = orchestrate(
            self.get_typed_modules(),
            generated_tailwind_css,
            OrchestrateOptions {
                skip_optimization,
                entrypoint_filter: Some((module_id.clone(), entrypoint_name.clone())),
                ..Default::default()
            },
        );
        crate::log_info!(
            "evaluate_entrypoint",
            step = "orchestrate",
            duration = format!("{:?}", orchestrate_start.elapsed()),
        );

        // The filtered module should contain exactly the requested entrypoint
        let entrypoint = ir_module.entrypoints.first().ok_or_else(|| {
            anyhow::anyhow!(
                "Entrypoint '{}/{}' not found after compilation",
                module_id,
                entrypoint_name
            )
        })?;

        // Convert serde_json::Value args to evaluator::Value
        let converted_args: HashMap<String, ir::semantics::evaluator::Value> = args
            .into_iter()
            .map(|(k, v)| (k, ir::semantics::evaluator::Value::from_json(v)))
            .collect();

        // Evaluate the entrypoint
        let evaluate_start = std::time::Instant::now();
        let result = ir::semantics::evaluator::evaluate_entrypoint(entrypoint, converted_args);
        crate::log_info!(
            "evaluate_entrypoint",
            step = "evaluate",
            duration = format!("{:?}", evaluate_start.elapsed()),
        );
        crate::log_info!(
            "evaluate_entrypoint",
            step = "exit",
            duration = format!("{:?}", fn_start.elapsed()),
        );
        result
    }

    /// Get all typed modules for compilation
    pub fn get_typed_modules(&self) -> &HashMap<ModuleId, TypedAst> {
        &self.typed_asts
    }

    /// Find which module contains a given entrypoint.
    pub fn find_module_for_entrypoint(&self, entrypoint: &str) -> Result<ModuleId, String> {
        let mut all_entrypoints = Vec::new();

        for (module_id, ast) in &self.typed_asts {
            for ep in ast.get_entrypoint_declarations() {
                if ep.name.as_str() == entrypoint {
                    return Ok(module_id.clone());
                }
                all_entrypoints.push(ep.name.to_string());
            }
        }

        all_entrypoints.sort();
        Err(format!(
            "Entrypoint '{}' not found. Available entrypoints: {}",
            entrypoint,
            all_entrypoints.join(", ")
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::{DocumentAnnotator, SimpleAnnotation};
    use crate::test_utils::archive::extract_markers_from_archive;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn program_from_txtar(input: &str) -> Program {
        let archive = Archive::from(input);
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_id =
                ModuleId::new(&file.name.replace(".hop", "").replace('/', "::")).unwrap();
            map.insert(
                module_id.clone(),
                Document::new(module_id, file.content.clone()),
            );
        }
        Program::new(map)
    }

    fn program_from_archive(archive: &Archive) -> Program {
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_id =
                ModuleId::new(&file.name.replace(".hop", "").replace('/', "::")).unwrap();
            map.insert(
                module_id.clone(),
                Document::new(module_id, file.content.clone()),
            );
        }
        Program::new(map)
    }

    fn check_rename_locations(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));
        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module =
            ModuleId::new(&marker.filename.replace(".hop", "").replace('/', "::")).unwrap();

        let locs = program_from_archive(&archive)
            .get_rename_locations(&module, marker.position)
            .expect("Expected locations to be defined");

        let mut output = Vec::new();
        let annotator = DocumentAnnotator::new().with_location();

        for file in archive.iter() {
            let module_id =
                ModuleId::new(&file.name.replace(".hop", "").replace('/', "::")).unwrap();

            let mut annotations: Vec<SimpleAnnotation> = locs
                .iter()
                .filter(|l| l.range.module_id() == &module_id)
                .map(|l| SimpleAnnotation {
                    message: "Rename".to_string(),
                    range: l.range.clone(),
                })
                .collect();

            annotations.sort();

            if !annotations.is_empty() {
                output.push(annotator.annotate(Some(&file.name), &annotations));
            }
        }

        expected.assert_eq(&output.join("\n"));
    }

    fn check_definition_location(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));

        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module =
            ModuleId::new(&marker.filename.replace(".hop", "").replace('/', "::")).unwrap();

        let program = program_from_archive(&archive);

        for (module_id, errors) in program.get_parse_errors() {
            if !errors.is_empty() {
                panic!("Parse errors in module {}: {:?}", module_id, errors);
            }
        }

        for (module_id, errors) in program.get_type_errors() {
            if !errors.is_empty() {
                panic!("Type errors in module {}: {:?}", module_id, errors);
            }
        }

        let loc = program
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let output = DocumentAnnotator::new().with_location().annotate(
            Some(&loc.range.module_id().to_string()),
            [SimpleAnnotation {
                message: "Definition".to_string(),
                range: loc.range,
            }],
        );

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let program = program_from_txtar(input);

        let diagnostics = program.get_error_diagnostics(ModuleId::new(module).unwrap());

        if diagnostics.is_empty() {
            panic!("Expected diagnostics to be non-empty");
        }

        let output = DocumentAnnotator::new().with_location().annotate(
            Some(module),
            diagnostics.into_iter().map(|d| SimpleAnnotation {
                message: d.message,
                range: d.range,
            }),
        );

        expected.assert_eq(&output);
    }

    fn check_type_errors(program: &Program, expected: Expect) {
        let mut output = Vec::new();
        let annotator = DocumentAnnotator::new().with_location();

        // Get all modules that have type errors
        let type_errors = program.get_type_errors();
        let mut modules_with_errors: Vec<_> = type_errors
            .iter()
            .filter(|(_, errors)| !errors.is_empty())
            .collect();

        // Sort by module name for consistent output
        modules_with_errors.sort_by_key(|(module_id, _)| module_id.to_string());

        for (module_id, errors) in modules_with_errors {
            let annotated = annotator.annotate(Some(&module_id.to_string()), errors);

            output.push(annotated);
        }

        expected.assert_eq(&output.join("\n"));
    }

    fn check_renameable_symbol(input: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(input));
        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module =
            ModuleId::new(&marker.filename.replace(".hop", "").replace('/', "::")).unwrap();

        let symbol = program_from_archive(&archive)
            .get_renameable_symbol(&module, marker.position)
            .expect("Expected symbol to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module.to_path())
            .expect("Could not find file in archive");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(Some(&file.name), &[symbol.range]);

        expected.assert_eq(&output);
    }

    fn check_hover_info(archive: &str, expected: Expect) {
        let (archive, markers) = extract_markers_from_archive(&Archive::from(archive));

        if markers.len() != 1 {
            panic!(
                "Expected exactly one position marker, found {}",
                markers.len()
            );
        }

        let marker = &markers[0];
        let module =
            ModuleId::new(&marker.filename.replace(".hop", "").replace('/', "::")).unwrap();

        let program = program_from_archive(&archive);

        for (module_id, errors) in program.get_parse_errors() {
            if !errors.is_empty() {
                panic!("Parse errors in module {}: {:?}", module_id, errors);
            }
        }

        for (module_id, errors) in program.get_type_errors() {
            if !errors.is_empty() {
                panic!("Type errors in module {}: {:?}", module_id, errors);
            }
        }

        let hover_info = program
            .get_hover_info(&module, marker.position)
            .expect("Expected hover info to be defined");

        let file = archive
            .iter()
            .find(|f| ModuleId::new(&f.name.replace(".hop", "")).unwrap() == module)
            .expect("Could not find file in archive");

        let output = DocumentAnnotator::new().with_location().annotate(
            Some(&file.name),
            &[SimpleAnnotation {
                range: hover_info.range,
                message: hover_info.message,
            }],
        );

        expected.assert_eq(&output);
    }

    ///////////////////////////////////////////////////////////////////////////
    // DEFINITION LOCATION                                                   //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_find_definition_from_component_reference_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                -- main.hop --
                import hop::components::HelloWorld

                <Main>
                  <HelloWorld />
                   ^
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> hop::components (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_reference_closing_tag() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                -- main.hop --
                import hop::components::HelloWorld

                <Main>
                  <HelloWorld>
                  </HelloWorld>
                     ^
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> hop::components (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_component_name() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                -- main.hop --
                import hop::components::HelloWorld
                         ^

                <Main>
                  <HelloWorld />
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> hop::components (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_record_name() {
        check_definition_location(
            indoc! {r#"
                -- types.hop --
                record User {name: String, age: Int}
                -- main.hop --
                import types::User
                         ^

                <Main {user: User}>
                  <div>{user.name}</div>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> types (line 1, col 8)
                1 | record User {name: String, age: Int}
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_enum_name() {
        check_definition_location(
            indoc! {r#"
                -- types.hop --
                enum Status { Active, Inactive }
                -- main.hop --
                import types::Status
                         ^

                <Main {status: Status}>
                  <match {status}>
                    <case {Status::Active}><span>Active</span></case>
                    <case {Status::Inactive}><span>Inactive</span></case>
                  </match>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> types (line 1, col 6)
                1 | enum Status { Active, Inactive }
                  |      ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_definition_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                   ^
                  <h1>Hello World</h1>
                </HelloWorld>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_definition_closing_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>
                    ^
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_reference_in_same_module_simple() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                <Main>
                  <HelloWorld />
                   ^
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_reference_inside_match() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                <Main {x: Option[String]}>
                  <match {x}>
                    <case {Some(_)}>
                      <HelloWorld />
                       ^
                    </case>
                    <case {None}></case>
                  </match>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                 1 | <HelloWorld>
                   |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_reference_inside_entrypoint() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                entrypoint Main() {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_variable_reference() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <Main {name: String}>
                  <span>{name}</span>
                         ^
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 8)
                1 | <Main {name: String}>
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_for_loop_variable() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <Main {items: Array[String]}>
                  <ul>
                    <for {item in items}>
                      <li>{item}</li>
                            ^
                    </for>
                  </ul>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 3, col 11)
                3 |     <for {item in items}>
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_let_binding_reference() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {greeting: String = "Hello"}>
                    <span>{greeting}</span>
                            ^
                  </let>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 2, col 9)
                2 |   <let {greeting: String = "Hello"}>
                  |         ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_type_reference_in_parameter() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                record User {name: String}

                <Main {user: User}>
                              ^
                  <span>{user.name}</span>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 8)
                1 | record User {name: String}
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_type_reference_in_array() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                record Item {name: String}

                <Main {items: Array[Item]}>
                                     ^
                  <for {item in items}>
                    <span>{item.name}</span>
                  </for>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 8)
                1 | record Item {name: String}
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_imported_type_reference() {
        check_definition_location(
            indoc! {r#"
                -- types.hop --
                record User {name: String}

                -- main.hop --
                import types::User

                <Main {user: User}>
                              ^
                  <span>{user.name}</span>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> types (line 1, col 8)
                1 | record User {name: String}
                  |        ^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // RENAME LOCATIONS                                                      //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_find_rename_locations_from_component_reference() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                -- main.hop --
                import components::HelloWorld

                <Main>
                  <HelloWorld />
                   ^
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^

                Rename
                  --> components.hop (line 3, col 3)
                3 | </HelloWorld>
                  |   ^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | import components::HelloWorld
                  |                    ^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_component_reference_in_same_module() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                <Main>
                  <HelloWorld />
                   ^
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^

                Rename
                  --> main.hop (line 3, col 3)
                3 | </HelloWorld>
                  |   ^^^^^^^^^^

                Rename
                  --> main.hop (line 6, col 4)
                6 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_component_definition() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <HelloWorld>
                 ^
                  <h1>Hello World</h1>
                </HelloWorld>

                -- main.hop --
                import components::HelloWorld

                <Main>
                  <HelloWorld />
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^

                Rename
                  --> components.hop (line 3, col 3)
                3 | </HelloWorld>
                  |   ^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | import components::HelloWorld
                  |                    ^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }

    // Make sure that when we rename a component in a module that has
    // the same name as a module in some other component, the module in
    // the other component is left unchanged.
    #[test]
    fn should_scope_rename_locations_to_component_definition_module() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <HelloWorld>
                  <h1>Hello World</h1>
                </HelloWorld>

                <Main>
                 ^
                  <HelloWorld />
                </Main>

                -- main.hop --
                import components::HelloWorld

                <Main>
                  <HelloWorld />
                </Main>
            "#},
            // The result here should not contain rename locations in main.hop.
            expect![[r#"
                Rename
                  --> components.hop (line 5, col 2)
                5 | <Main>
                  |  ^^^^

                Rename
                  --> components.hop (line 7, col 3)
                7 | </Main>
                  |   ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_html_opening_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <div>
                     ^
                        <span>Content</span>
                    </div>
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 4, col 7)
                4 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_nested_html_element() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <div>
                    <div>
                     ^
                        <div>Content</div>
                    </div>
                  </div>
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 3, col 6)
                3 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 5, col 7)
                5 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_html_closing_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <div>
                        <span>Content</span>
                    </div>
                       ^
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <div>
                  |      ^^^

                Rename
                  --> main.hop (line 4, col 7)
                4 |     </div>
                  |       ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_self_closing_html_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <br />
                     ^
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 2, col 6)
                2 |     <br />
                  |      ^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_record_type() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                record Icon {
                       ^
                  id: String,
                  title: String,
                  img_src: String,
                  description: String,
                }

                <IconItem {
                  icon: Icon,
                }>
                  <a class="flex flex-col gap-2" href={
                    "/icons/" + icon.id,
                  }>
                    <img class="rounded-lg object-cover aspect-3/2" src={
                      icon.img_src,
                    } />
                    <h2 class="font-semibold text-lg">
                      {icon.title}
                    </h2>
                    {icon.description}
                  </a>
                </IconItem>

                <IconsPage {
                  icons: Array[Icon],
                }>
                  <div class="flex">
                      <for {icon in icons}>
                        <IconItem {
                          icon: icon,
                        }/>
                      </for>
                  </div>
                </IconsPage>

                <IconShowPage {
                  icon: Icon,
                }>
                  <div class="flex">
                    <div class="flex flex-col gap-4 p-8 mx-auto my-8 w-full max-w-4xl">
                      <h1 class="text-xl font-semibold">
                        {icon.title}
                      </h1>
                      <div>
                        {icon.description}
                      </div>
                      <img class="rounded-lg" src={
                        icon.img_src,
                      } />
                    </div>
                  </div>
                </IconShowPage>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 8)
                 1 | record Icon {
                   |        ^^^^

                Rename
                  --> main.hop (line 9, col 9)
                 9 |   icon: Icon,
                   |         ^^^^

                Rename
                  --> main.hop (line 25, col 16)
                25 |   icons: Array[Icon],
                   |                ^^^^

                Rename
                  --> main.hop (line 37, col 9)
                37 |   icon: Icon,
                   |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_even_when_there_is_parse_errors() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <Main>
                  ^
                  <div>
                  <span>
                </Main>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 2)
                1 | <Main>
                  |  ^^^^

                Rename
                  --> main.hop (line 4, col 3)
                4 | </Main>
                  |   ^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // RENAMEABLE SYMBOL                                                     //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_find_renameable_symbol_from_component_definition() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                <HelloWorld>
                 ^
                  <h1>Hello World</h1>
                </HelloWorld>
            "#},
            expect![[r#"
                HelloWorld
                  --> main.hop (line 1, col 2)
                1 | <HelloWorld>
                  |  ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_renameable_symbol_from_html_element() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                <Main>
                    <div>Content</div>
                     ^
                </Main>
            "#},
            expect![[r#"
                div
                  --> main.hop (line 2, col 6)
                2 |     <div>Content</div>
                  |      ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // HOVER INFO                                                            //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_show_hover_info_for_parameter() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main {user: User}>
                       ^
                  <h1>Hello {user.name}</h1>
                </Main>
            "#},
            expect![[r#"
                `user`: `main::User`
                  --> main.hop (line 2, col 8)
                2 | <Main {user: User}>
                  |        ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_component_reference() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String}>
                  <h1>Hello {name}!</h1>
                </Greeting>

                <Main>
                  <Greeting name="World" />
                   ^
                </Main>
            "#},
            expect![[r#"
                `Greeting`: `main::Greeting`
                  --> main.hop (line 6, col 4)
                6 |   <Greeting name="World" />
                  |    ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_component_reference_closing_tag() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String, children: TrustedHTML}>
                  <h1>Hello {name}!</h1>
                  {children}
                </Greeting>

                <Main>
                  <Greeting name="World">
                    content
                  </Greeting>
                     ^
                </Main>
            "#},
            expect![[r#"
                `Greeting`: `main::Greeting`
                  --> main.hop (line 9, col 5)
                 9 |   </Greeting>
                   |     ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_component_definition_opening_tag() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String}>
                  ^
                  <h1>Hello {name}!</h1>
                </Greeting>
            "#},
            expect![[r#"
                `Greeting`: `main::Greeting`
                  --> main.hop (line 1, col 2)
                1 | <Greeting {name: String}>
                  |  ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_component_definition_closing_tag() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String}>
                  <h1>Hello {name}!</h1>
                </Greeting>
                   ^
            "#},
            expect![[r#"
                `Greeting`: `main::Greeting`
                  --> main.hop (line 3, col 3)
                3 | </Greeting>
                  |   ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_variable_in_text_expression() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String}>
                  <div>{name}</div>
                        ^
                </Greeting>
            "#},
            expect![[r#"
                `name`: `String`
                  --> main.hop (line 2, col 9)
                2 |   <div>{name}</div>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_type_in_let_binding() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {name: String = "hello"}>
                              ^
                    {name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                `String`: `String`
                  --> main.hop (line 2, col 15)
                2 |   <let {name: String = "hello"}>
                  |               ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_type_in_component_parameter() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Greeting {name: String}>
                                 ^
                  <h1>Hello {name}!</h1>
                </Greeting>
            "#},
            expect![[r#"
                `String`: `String`
                  --> main.hop (line 1, col 18)
                1 | <Greeting {name: String}>
                  |                  ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_array_type() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {items: Array[String] = ["a", "b"]}>
                               ^
                    <for {item in items}>{item}</for>
                  </let>
                </Main>
            "#},
            expect![[r#"
                `Array[String]`: `Array[String]`
                  --> main.hop (line 2, col 16)
                2 |   <let {items: Array[String] = ["a", "b"]}>
                  |                ^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_option_type() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <let {value: Option[String] = Some("hello")}>
                               ^
                    <match {value}>
                      <case {Some(s)}>{s}</case>
                      <case {None}>none</case>
                    </match>
                  </let>
                </Main>
            "#},
            expect![[r#"
                `Option[String]`: `Option[String]`
                  --> main.hop (line 2, col 16)
                2 |   <let {value: Option[String] = Some("hello")}>
                  |                ^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_named_type_in_let_binding() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main>
                  <let {user: User = User(name: "John")}>
                              ^
                    {user.name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                `main::User`: `main::User`
                  --> main.hop (line 3, col 15)
                3 |   <let {user: User = User(name: "John")}>
                  |               ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_record_literal_type_name() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                <Main>
                  <let {user: User = User(name: "John")}>
                                     ^
                    {user.name}
                  </let>
                </Main>
            "#},
            expect![[r#"
                `User`: `main::User`
                  --> main.hop (line 3, col 22)
                3 |   <let {user: User = User(name: "John")}>
                  |                      ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_enum_literal_constructor() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                enum Color { Red, Green, Blue }
                <Main>
                  <let {color: Color = Color::Red}>
                                       ^
                    <match {color}>
                      <case {Color::Red}>red</case>
                      <case {_}>other</case>
                    </match>
                  </let>
                </Main>
            "#},
            expect![[r#"
                `Color::Red`: `main::Color`
                  --> main.hop (line 3, col 24)
                3 |   <let {color: Color = Color::Red}>
                  |                        ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_enum_literal_constructor_with_fields() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                enum Outcome { Success(value: String), Failure(message: String) }
                <Main>
                  <let {result: Outcome = Outcome::Success(value: "ok")}>
                                          ^
                    <match {result}>
                      <case {Outcome::Success(value: v)}>{v}</case>
                      <case {Outcome::Failure(message: m)}>{m}</case>
                    </match>
                  </let>
                </Main>
            "#},
            expect![[r#"
                `Outcome::Success`: `main::Outcome`
                  --> main.hop (line 3, col 27)
                3 |   <let {result: Outcome = Outcome::Success(value: "ok")}>
                  |                           ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // ERROR DIAGNOSTICS                                                     //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_report_parse_errors_as_diagnostics() {
        check_error_diagnostics(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <div>
                  <span>unclosed span
                </Main>
            "#},
            "main",
            expect![[r#"
                Unclosed <span>
                  --> main (line 3, col 4)
                3 |   <span>unclosed span
                  |    ^^^^

                Unclosed <div>
                  --> main (line 2, col 4)
                2 |   <div>
                  |    ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // IMPORT CYCLES                                                         //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_report_import_cycle_errors() {
        let mut program = program_from_txtar(indoc! {r#"
            -- a.hop --
            import b::BComp
            <AComp>
              <BComp />
            </AComp>

            -- b.hop --
            import a::AComp
            <BComp>
              <AComp />
            </BComp>

            -- c.hop --
            import a::AComp
            <CComp>
              <AComp />
            </CComp>
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → a
                  --> a (line 1, col 8)
                1 | import b::BComp
                  |        ^^^^^^^^

                Import cycle: b imports from a which creates a dependency cycle: a → b → a
                  --> b (line 1, col 8)
                1 | import a::AComp
                  |        ^^^^^^^^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            &ModuleId::new("a").unwrap(),
            Document::new(
                ModuleId::new("a").unwrap(),
                indoc! {r#"
                    <AComp>
                    </AComp>
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
    }

    #[test]
    fn should_report_import_cycle_errors_for_large_cycles() {
        let mut program = program_from_txtar(indoc! {r#"
            -- a.hop --
            import b::BComp
            <AComp>
              <BComp />
            </AComp>

            -- b.hop --
            import c::CComp
            <BComp>
              <CComp />
            </BComp>

            -- c.hop --
            import d::DComp
            <CComp>
              <DComp />
            </CComp>

            -- d.hop --
            import a::AComp
            <DComp>
              <AComp />
            </DComp>
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → c → d → a
                  --> a (line 1, col 8)
                1 | import b::BComp
                  |        ^^^^^^^^

                Import cycle: b imports from c which creates a dependency cycle: a → b → c → d → a
                  --> b (line 1, col 8)
                1 | import c::CComp
                  |        ^^^^^^^^

                Import cycle: c imports from d which creates a dependency cycle: a → b → c → d → a
                  --> c (line 1, col 8)
                1 | import d::DComp
                  |        ^^^^^^^^

                Import cycle: d imports from a which creates a dependency cycle: a → b → c → d → a
                  --> d (line 1, col 8)
                1 | import a::AComp
                  |        ^^^^^^^^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            &ModuleId::new("c").unwrap(),
            Document::new(
                ModuleId::new("c").unwrap(),
                indoc! {r#"
                    <CComp>
                    </CComp>
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
        // Introduce new cycle a → b → a
        program.update_module(
            &ModuleId::new("b").unwrap(),
            Document::new(
                ModuleId::new("b").unwrap(),
                indoc! {r#"
                    import a::AComp
                    <BComp>
                      <AComp />
                    </BComp>
                "#}
                .to_string(),
            ),
        );
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → a
                  --> a (line 1, col 8)
                1 | import b::BComp
                  |        ^^^^^^^^

                Import cycle: b imports from a which creates a dependency cycle: a → b → a
                  --> b (line 1, col 8)
                1 | import a::AComp
                  |        ^^^^^^^^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            &ModuleId::new("b").unwrap(),
            Document::new(
                ModuleId::new("b").unwrap(),
                indoc! {r#"
                    <BComp>
                    </BComp>
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
    }

    ///////////////////////////////////////////////////////////////////////////
    // IR EVALUATION                                                         //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_report_type_error_when_imported_module_is_removed() {
        let mut program = program_from_txtar(indoc! {r#"
            -- components.hop --
            <HelloWorld>
              <h1>Hello World</h1>
            </HelloWorld>

            -- main.hop --
            import components::HelloWorld

            <Main>
              <HelloWorld />
            </Main>
        "#});

        // No type errors initially
        check_type_errors(&program, expect![""]);

        // Remove the components module
        program.remove_module(&ModuleId::new("components").unwrap());

        // Now main should have a type error about the missing import
        check_type_errors(
            &program,
            expect![[r#"
                Module components was not found
                  --> main (line 1, col 8)
                1 | import components::HelloWorld
                  |        ^^^^^^^^^^^^^^^^^^^^^^

                Component HelloWorld is not defined
                  --> main (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );

        // Add the module back
        program.update_module(
            &ModuleId::new("components").unwrap(),
            Document::new(
                ModuleId::new("components").unwrap(),
                indoc! {r#"
                    <HelloWorld>
                      <h1>Hello World</h1>
                    </HelloWorld>
                "#}
                .to_string(),
            ),
        );

        // Type errors should now be resolved
        check_type_errors(&program, expect![""]);
    }

    #[test]
    fn should_evaluate_ir_entrypoint_with_parameters() {
        let program = program_from_txtar(indoc! {r#"
            -- main.hop --
            entrypoint HelloWorld(name: String) {
              <h1>Hello {name}!</h1>
            }

            entrypoint AnotherComp() {
              <p>Static content</p>
            }
        "#});

        // Test evaluating hello-world entrypoint with a name parameter
        let mut args = HashMap::new();
        args.insert("name".to_string(), serde_json::json!("Alice"));

        let main_module = ModuleId::new("main").unwrap();
        let hello_world = TypeName::new("HelloWorld").unwrap();
        let result = program
            .evaluate_entrypoint(&main_module, &hello_world, args, None, false)
            .expect("Should evaluate successfully");

        assert!(result.contains("<h1>Hello Alice!</h1>"));

        // Test evaluating another-comp entrypoint without parameters
        let another_comp = TypeName::new("AnotherComp").unwrap();
        let result = program
            .evaluate_entrypoint(&main_module, &another_comp, HashMap::new(), None, false)
            .expect("Should evaluate successfully");

        assert!(result.contains("<p>Static content</p>"));

        // Test error when entrypoint doesn't exist
        let non_existent = TypeName::new("NonExistent").unwrap();
        let result =
            program.evaluate_entrypoint(&main_module, &non_existent, HashMap::new(), None, false);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Entrypoint 'NonExistent' not found in module 'main'")
        );
    }
}
