use crate::document::DocumentPosition;
use crate::document::{CheapString, Document, DocumentRange, Ranged};
use crate::dop::ParsedType;
use crate::error_collector::ErrorCollector;
use crate::hop::semantics::type_error::TypeError;
use crate::hop::syntax::parse_error::ParseError;
use crate::hop::syntax::parser::parse;
use crate::ir;
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use crate::toposorter::TopoSorter;
use anyhow::Result;
use std::collections::{HashMap, HashSet};

use super::semantics::type_checker::TypeChecker;
use super::semantics::typed_ast::TypedAst;
use super::symbols::component_name::ComponentName;
use super::symbols::module_name::ModuleName;
use super::syntax::find_node::find_node_at_position;
use super::syntax::parsed_ast::ParsedAst;
use super::syntax::parsed_node::ParsedNode;

/// HoverInfo is a message that should be displayed when the user hovers
/// a specific range in the source code.
pub struct HoverInfo {
    pub message: String,
    pub range: DocumentRange,
}

/// A DefinitionLocation is the definition of a certain symbol in the source
/// code. This is the response for a go to definition-query.
pub struct DefinitionLocation {
    pub module: ModuleName,
    pub range: DocumentRange,
}

/// A diagnostic is an error, warning or information that should be displayed
/// for a specific range in the document.
pub struct Diagnostic {
    pub message: String,
    pub range: DocumentRange,
}

pub struct RenameLocation {
    pub module: ModuleName,
    pub range: DocumentRange,
}

/// A RenameableSymbol is a range in the document that is renameable.
pub struct RenameableSymbol {
    pub current_name: CheapString,
    pub range: DocumentRange,
}

#[derive(Debug, Default)]
pub struct Program {
    topo_sorter: TopoSorter<ModuleName>,
    parse_errors: HashMap<ModuleName, ErrorCollector<ParseError>>,
    parsed_asts: HashMap<ModuleName, ParsedAst>,
    type_checker: TypeChecker,
}

impl Program {
    pub fn new(modules: HashMap<ModuleName, Document>) -> Self {
        let mut program = Self::default();
        for (module_name, document) in modules {
            program.update_module(module_name, document);
        }
        program
    }

    /// Remove a module from the program.
    ///
    /// This cleans up all state associated with the module and re-typechecks
    /// any modules that depended on it (since their imports are now broken).
    pub fn remove_module(&mut self, module_name: &ModuleName) {
        // Remove from parse errors and parsed ASTs
        self.parse_errors.remove(module_name);
        self.parsed_asts.remove(module_name);

        // Remove from topo sorter and get modules that depended on this one
        let dependents = self.topo_sorter.remove_node(module_name);

        // Remove from type checker
        self.type_checker.remove_module(module_name);

        // Re-typecheck dependent modules (they now have broken imports)
        for dependent in dependents {
            if let Some(ast) = self.parsed_asts.get(&dependent) {
                self.type_checker.typecheck(&[ast]);
            }
        }
    }

    pub fn update_module(
        &mut self,
        module_name: ModuleName,
        document: Document,
    ) -> Vec<ModuleName> {
        // Parse the module
        let parse_errors = self.parse_errors.entry(module_name.clone()).or_default();
        parse_errors.clear();
        let parsed_ast = parse(module_name.clone(), document, parse_errors);

        // Get all modules that this module depends on
        let module_dependencies = parsed_ast
            .get_import_declarations()
            .map(|import_node| import_node.imported_module().clone())
            .collect::<HashSet<ModuleName>>();

        // Store the AST
        self.parsed_asts.insert(module_name.clone(), parsed_ast);

        // Typecheck the module along with all dependent modules (grouped
        // into strongly connected components).
        let grouped_modules = self
            .topo_sorter
            .update_node(module_name, module_dependencies);
        for names in &grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_asts.get(name))
                .collect::<Vec<_>>();
            self.type_checker.typecheck(&modules);
        }

        // Return all modules that have been re-typechecked
        grouped_modules.into_iter().flatten().collect()
    }

    pub fn get_parse_errors(&self) -> &HashMap<ModuleName, ErrorCollector<ParseError>> {
        &self.parse_errors
    }

    pub fn get_type_errors(&self) -> &HashMap<ModuleName, ErrorCollector<TypeError>> {
        &self.type_checker.type_errors
    }

    pub fn get_parsed_ast(&self, module_name: &ModuleName) -> Option<&ParsedAst> {
        self.parsed_asts.get(module_name)
    }

    pub fn get_hover_info(
        &self,
        module_name: &ModuleName,
        position: DocumentPosition,
    ) -> Option<HoverInfo> {
        self.type_checker
            .type_annotations
            .get(module_name)?
            .iter()
            .find(|a| a.range.contains_position(position))
            .map(|annotation| HoverInfo {
                message: format!("`{}`: `{}`", annotation.name, annotation.typ),
                range: annotation.range.clone(),
            })
    }

    pub fn get_definition_location(
        &self,
        module_name: &ModuleName,
        position: DocumentPosition,
    ) -> Option<DefinitionLocation> {
        let ast = self.parsed_asts.get(module_name)?;

        // First check if we're on an import node's path (module::path::Component)
        for import in ast.get_import_declarations() {
            if import.path.contains_position(position) {
                let target_module = &import.module_name;
                let imported_name = import.type_name.as_str();

                let target_ast = self.parsed_asts.get(target_module)?;

                // Check if it's a component declaration
                if let Some(component_def) = target_ast.get_component_declaration(imported_name) {
                    return Some(DefinitionLocation {
                        module: target_module.clone(),
                        range: component_def.tag_name.clone(),
                    });
                }

                // Check if it's a record declaration
                if let Some(record) = target_ast.get_record_declaration(imported_name) {
                    return Some(DefinitionLocation {
                        module: target_module.clone(),
                        range: record.name_range.clone(),
                    });
                }
            }
        }

        // Check if we're on a component definition's tag name (opening or closing)
        for component_def in ast.get_component_declarations() {
            if component_def
                .tag_name_ranges()
                .any(|r| r.contains_position(position))
            {
                // Navigate to the opening tag of this component definition
                return Some(DefinitionLocation {
                    module: module_name.clone(),
                    range: component_def.tag_name.clone(),
                });
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
                declaring_module: definition_module,
                ..
            } => {
                let module_name = definition_module.as_ref()?;
                let component_def = self
                    .parsed_asts
                    .get(module_name)?
                    .get_component_declaration(component_name.as_str())?;
                Some(DefinitionLocation {
                    module: module_name.clone(),
                    range: component_def.tag_name.clone(),
                })
            }
            ParsedNode::Html { tag_name, .. } => {
                // Navigate to the opening tag of this HTML element
                Some(DefinitionLocation {
                    module: module_name.clone(),
                    range: tag_name.clone(),
                })
            }
            _ => None,
        }
    }

    pub fn get_rename_locations(
        &self,
        module_name: &ModuleName,
        position: DocumentPosition,
    ) -> Option<Vec<RenameLocation>> {
        let ast = self.parsed_asts.get(module_name)?;

        // Check if cursor is on a record declaration name
        for record in ast.get_record_declarations() {
            if record.name_range.contains_position(position) {
                return Some(self.collect_record_rename_locations(record.name(), module_name));
            }
        }

        for node in ast.get_component_declarations() {
            if node
                .tag_name_ranges()
                .any(|r| r.contains_position(position))
            {
                return Some(
                    self.collect_component_rename_locations(&node.component_name, module_name),
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
                        module: module_name.clone(),
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
        module_name: &ModuleName,
        position: DocumentPosition,
    ) -> Option<RenameableSymbol> {
        let ast = self.parsed_asts.get(module_name)?;

        // Check if cursor is on a record declaration name
        for record in ast.get_record_declarations() {
            if record.name_range.contains_position(position) {
                return Some(RenameableSymbol {
                    current_name: record.name_range.to_cheap_string(),
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
                    current_name: range.to_cheap_string(),
                    range: range.clone(),
                });
            }
        }

        let node = find_node_at_position(ast, position)?;

        node.tag_names()
            .find(|r| r.contains_position(position))
            .map(|range| RenameableSymbol {
                current_name: range.to_cheap_string(),
                range: range.clone(),
            })
    }

    /// Collects all locations where a component should be renamed, including:
    /// - The component definition (opening and closing tags)
    /// - All references to the component (opening and closing tags)
    /// - All import statements that import the component
    fn collect_component_rename_locations(
        &self,
        component_name: &ComponentName,
        definition_module: &ModuleName,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        if let Some(module) = self.parsed_asts.get(definition_module) {
            if let Some(component_node) = module.get_component_declaration(component_name.as_str())
            {
                // Add the definition's opening tag name
                locations.push(RenameLocation {
                    module: definition_module.clone(),
                    range: component_node.tag_name.clone(),
                });

                // Add the definition's closing tag name if it exists
                if let Some(range) = component_node.closing_tag_name.as_ref() {
                    locations.push(RenameLocation {
                        module: definition_module.clone(),
                        range: range.clone(),
                    });
                }
            }
        }

        for (module_name, ast) in &self.parsed_asts {
            // Find all import statements that import this component
            locations.extend(
                ast.get_import_declarations()
                    .filter(|n| {
                        n.imports_type(component_name.as_str()) && n.imports_from(definition_module)
                    })
                    .map(|n| RenameLocation {
                        module: module_name.clone(),
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
                        module: module_name.clone(),
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
        definition_module: &ModuleName,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        // Add the record declaration itself
        if let Some(module) = self.parsed_asts.get(definition_module) {
            if let Some(record) = module.get_record_declaration(record_name) {
                locations.push(RenameLocation {
                    module: definition_module.clone(),
                    range: record.name_range.clone(),
                });
            }
        }

        // Search all modules for references to this record
        for (module_name, ast) in &self.parsed_asts {
            // Find all import statements that import this record
            locations.extend(
                ast.get_import_declarations()
                    .filter(|n| n.imports_type(record_name) && n.imports_from(definition_module))
                    .map(|n| RenameLocation {
                        module: module_name.clone(),
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
                            module_name,
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
        definition_module: &ModuleName,
        current_module: &ModuleName,
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
                        module: current_module.clone(),
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

    pub fn get_error_diagnostics(&self, module_name: ModuleName) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let mut found_parse_errors = false;

        for error in self
            .parse_errors
            .get(&module_name)
            .into_iter()
            .flat_map(|c| c.iter())
        {
            diagnostics.push(Diagnostic {
                message: error.to_string(),
                range: error.range().clone(),
            });
            found_parse_errors = true;
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            for error in self
                .type_checker
                .type_errors
                .get(&module_name)
                .into_iter()
                .flatten()
            {
                diagnostics.push(Diagnostic {
                    message: error.to_string(),
                    range: error.range().clone(),
                });
            }
        }

        diagnostics
    }

    /// Evaluate an entrypoint given module and entrypoint name.
    pub fn evaluate_entrypoint(
        &self,
        module_name: &ModuleName,
        entrypoint_name: &ComponentName,
        args: HashMap<String, serde_json::Value>,
        generated_tailwind_css: Option<&str>,
    ) -> Result<String> {
        // Validate that the module exists
        let module = self.get_typed_modules().get(module_name).ok_or_else(|| {
            anyhow::anyhow!(
                "Module '{}' not found. Available modules: {}",
                module_name,
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
                module_name,
                available_entrypoints.join(", ")
            );
        }

        // Use orchestrate to handle inlining and compilation
        let ir_module = orchestrate(
            self.get_typed_modules(),
            generated_tailwind_css,
            OrchestrateOptions::default(),
        );

        // Find the requested entrypoint in the compiled module
        let entrypoint = ir_module
            .entrypoints
            .iter()
            .find(|c| c.name.as_str() == entrypoint_name.as_str())
            .ok_or_else(|| {
                anyhow::anyhow!(
                    "Entrypoint '{}/{}' not found after compilation",
                    module_name,
                    entrypoint_name
                )
            })?;

        // Convert serde_json::Value args to evaluator::Value
        let converted_args: HashMap<String, ir::semantics::evaluator::Value> = args
            .into_iter()
            .map(|(k, v)| (k, ir::semantics::evaluator::Value::from_json(v)))
            .collect();

        // Evaluate the entrypoint
        ir::semantics::evaluator::evaluate_entrypoint(entrypoint, converted_args)
    }

    /// Get all typed modules for compilation
    pub fn get_typed_modules(&self) -> &HashMap<ModuleName, TypedAst> {
        &self.type_checker.typed_asts
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
            let module_name = ModuleName::new(&file.name.replace(".hop", "")).unwrap();
            map.insert(module_name, Document::new(file.content.clone()));
        }
        Program::new(map)
    }

    fn program_from_archive(archive: &Archive) -> Program {
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_name = ModuleName::new(&file.name.replace(".hop", "")).unwrap();
            map.insert(module_name, Document::new(file.content.clone()));
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
        let module = ModuleName::new(&marker.filename.replace(".hop", "")).unwrap();

        let locs = program_from_archive(&archive)
            .get_rename_locations(&module, marker.position)
            .expect("Expected locations to be defined");

        let mut output = Vec::new();
        let annotator = DocumentAnnotator::new().with_location();

        for file in archive.iter() {
            let module_name = ModuleName::new(&file.name.replace(".hop", "")).unwrap();

            let mut annotations: Vec<SimpleAnnotation> = locs
                .iter()
                .filter(|l| l.module == module_name)
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
        let module = ModuleName::new(&marker.filename.replace(".hop", "")).unwrap();

        let program = program_from_archive(&archive);

        let loc = program
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let output = DocumentAnnotator::new().with_location().annotate(
            Some(&loc.module.to_string()),
            [SimpleAnnotation {
                message: "Definition".to_string(),
                range: loc.range,
            }],
        );

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let program = program_from_txtar(input);

        let diagnostics = program.get_error_diagnostics(ModuleName::new(module).unwrap());

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
        modules_with_errors.sort_by_key(|(module_name, _)| module_name.to_string());

        for (module_name, errors) in modules_with_errors {
            let annotated = annotator.annotate(Some(&module_name.to_string()), errors);

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
        let module = ModuleName::new(&marker.filename.replace(".hop", "")).unwrap();

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
        let module = ModuleName::new(&marker.filename.replace(".hop", "")).unwrap();

        let hover_info = program_from_archive(&archive)
            .get_hover_info(&module, marker.position)
            .expect("Expected hover info to be defined");

        let file = archive
            .iter()
            .find(|f| ModuleName::new(&f.name.replace(".hop", "")).unwrap() == module)
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
    fn should_find_definition_from_html_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <div class="container">
                   ^
                    <span>Content</span>
                  </div>
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 2, col 4)
                2 |   <div class="container">
                  |    ^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_html_closing_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <Main>
                  <div class="container">
                    <span>Content</span>
                  </div>
                    ^
                </Main>
            "#},
            expect![[r#"
                Definition
                  --> main (line 2, col 4)
                2 |   <div class="container">
                  |    ^^^
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
                    <case {Some(s)}>
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
                <Greeting {name: String}>
                  <h1>Hello {name}!</h1>
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
                  --> main.hop (line 8, col 5)
                8 |   </Greeting>
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
            ModuleName::new("a").unwrap(),
            Document::new(
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
            ModuleName::new("c").unwrap(),
            Document::new(
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
            ModuleName::new("b").unwrap(),
            Document::new(
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
            ModuleName::new("b").unwrap(),
            Document::new(
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

        let main_module = ModuleName::new("main").unwrap();
        let hello_world = ComponentName::new("HelloWorld".to_string()).unwrap();
        let result = program
            .evaluate_entrypoint(&main_module, &hello_world, args, None)
            .expect("Should evaluate successfully");

        assert!(result.contains("<h1>Hello Alice!</h1>"));

        // Test evaluating another-comp entrypoint without parameters
        let another_comp = ComponentName::new("AnotherComp".to_string()).unwrap();
        let result = program
            .evaluate_entrypoint(&main_module, &another_comp, HashMap::new(), None)
            .expect("Should evaluate successfully");

        assert!(result.contains("<p>Static content</p>"));

        // Test error when entrypoint doesn't exist
        let non_existent = ComponentName::new("NonExistent".to_string()).unwrap();
        let result = program.evaluate_entrypoint(&main_module, &non_existent, HashMap::new(), None);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Entrypoint 'NonExistent' not found in module 'main'")
        );
    }
}
