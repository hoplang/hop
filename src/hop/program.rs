use crate::common::{ParseError, Position, Range, Ranged, TypeError};
use crate::hop::ast::HopAST;
use crate::hop::evaluator;
use crate::hop::parser::parse;
use crate::hop::script_collector::ScriptCollector;
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::{CycleError, TopoSorter};
use crate::hop::typechecker::{DefinitionLink, TypeAnnotation, typecheck};
use crate::tui::source_annotator::Annotated;
use anyhow::Result;
use std::collections::HashMap;

use super::ast::HopNode;
use super::typechecker::TypeCheckerState;

/// HopMode influences the runtime value of the global variable HOP_MODE which
/// will be set to 'build' when running `hop build` and 'dev' when running
/// `hop dev`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HopMode {
    Build,
    Dev,
}

impl HopMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            HopMode::Build => "build",
            HopMode::Dev => "dev",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HoverInfo {
    pub type_str: String,
    pub range: Range,
}

impl Ranged for HoverInfo {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for HoverInfo {
    fn message(&self) -> String {
        self.type_str.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionLocation {
    pub module: String,
    pub range: Range,
}

impl Ranged for DefinitionLocation {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for DefinitionLocation {
    fn message(&self) -> String {
        "Definition".to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub message: String,
    pub range: Range,
}

impl Ranged for Diagnostic {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for Diagnostic {
    fn message(&self) -> String {
        self.message.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenameLocation {
    pub module: String,
    pub range: Range,
}

impl Ranged for RenameLocation {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for RenameLocation {
    fn message(&self) -> String {
        "Rename".to_string()
    }
}

impl PartialOrd for RenameLocation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RenameLocation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // First compare by module name
        match self.module.cmp(&other.module) {
            std::cmp::Ordering::Equal => {
                // Then by range start
                match self.range.start.cmp(&other.range.start) {
                    std::cmp::Ordering::Equal => {
                        // Finally by range end
                        self.range.end.cmp(&other.range.end)
                    }
                    other => other,
                }
            }
            other => other,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenameableSymbol {
    pub current_name: String,
    pub range: Range,
}

impl Ranged for RenameableSymbol {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for RenameableSymbol {
    fn message(&self) -> String {
        format!("Renameable symbol: {}", self.current_name)
    }
}

pub struct Program {
    asts: HashMap<String, HopAST>,
    type_checker_state: HashMap<String, TypeCheckerState>,
    type_annotations: HashMap<String, Vec<TypeAnnotation>>,
    definition_links: HashMap<String, Vec<DefinitionLink>>,
    parse_errors: HashMap<String, Vec<ParseError>>,
    type_errors: HashMap<String, Vec<TypeError>>,
    source_code: HashMap<String, String>,
    topo_sorter: TopoSorter,
    hop_mode: HopMode,
}

impl Program {
    pub fn new(hop_mode: HopMode) -> Self {
        Self {
            asts: HashMap::new(),
            type_checker_state: HashMap::new(),
            type_annotations: HashMap::new(),
            definition_links: HashMap::new(),
            parse_errors: HashMap::new(),
            type_errors: HashMap::new(),
            source_code: HashMap::new(),
            topo_sorter: TopoSorter::default(),
            hop_mode,
        }
    }

    pub fn from_modules(modules: HashMap<String, String>, hop_mode: HopMode) -> Self {
        let mut program = Self::new(hop_mode);

        for (module_name, source_code) in &modules {
            program.parse_module(module_name, source_code);
        }

        for (module_name, ast) in &program.asts {
            program.topo_sorter.add_node(module_name.clone());
            for import_node in ast.get_imports() {
                program
                    .topo_sorter
                    .add_dependency(module_name, &import_node.from_attr.value);
            }
        }

        let sorted_modules = match program.topo_sorter.sort() {
            Ok(modules) => modules,
            Err(cycle_error) => {
                program.handle_cycle_error(&cycle_error);
                return program;
            }
        };

        for module_name in &sorted_modules {
            program.typecheck_module(module_name);
        }

        program
    }

    pub fn get_parse_errors(&self) -> &HashMap<String, Vec<ParseError>> {
        &self.parse_errors
    }

    pub fn get_type_errors(&self) -> &HashMap<String, Vec<TypeError>> {
        &self.type_errors
    }

    pub fn get_source_code(&self) -> &HashMap<String, String> {
        &self.source_code
    }

    pub fn has_module(&self, module_name: &str) -> bool {
        self.asts.contains_key(module_name)
    }

    /// Handles cycle errors by adding type errors for all modules in the cycle
    fn handle_cycle_error(&mut self, cycle_error: &CycleError) {
        for module_name in &cycle_error.cycle {
            let type_errors = self.type_errors.entry(module_name.clone()).or_default();
            type_errors.clear();
            let ast = self
                .asts
                .get(module_name)
                .expect("Failed to retrieve AST in handle_cycle_error");
            for import_node in ast.get_imports() {
                if cycle_error.cycle.contains(&import_node.from_attr.value) {
                    type_errors.push(TypeError::circular_import(
                        module_name,
                        &import_node.from_attr.value,
                        &cycle_error.cycle,
                        import_node.from_attr.value_range,
                    ));
                }
            }
        }
    }

    fn typecheck_module(&mut self, module_name: &str) {
        let ast = match self.asts.get(module_name) {
            Some(module) => module,
            None => return,
        };

        let type_errors = self.type_errors.entry(module_name.to_string()).or_default();
        let type_annotations = self
            .type_annotations
            .entry(module_name.to_string())
            .or_default();
        let definition_links = self
            .definition_links
            .entry(module_name.to_string())
            .or_default();

        type_errors.clear();
        type_annotations.clear();
        definition_links.clear();

        let component_type_info = typecheck(
            ast,
            &self.type_checker_state,
            type_errors,
            type_annotations,
            definition_links,
        );
        self.type_checker_state
            .insert(module_name.to_string(), component_type_info);
    }

    fn parse_module(&mut self, module_name: &str, source_code: &str) {
        let parse_errors = self
            .parse_errors
            .entry(module_name.to_string())
            .or_default();
        parse_errors.clear();

        let tokenizer = Tokenizer::new(source_code);
        let module = parse(module_name.to_string(), tokenizer, parse_errors);

        self.asts.insert(module_name.to_string(), module);
        self.source_code
            .insert(module_name.to_string(), source_code.to_string());
    }

    pub fn update_module(&mut self, module_name: String, source_code: &str) -> Vec<String> {
        self.parse_module(module_name.as_str(), source_code);

        self.topo_sorter.add_node(module_name.clone());

        let ast = match self.asts.get(&module_name) {
            Some(module) => module,
            None => unreachable!(),
        };

        self.topo_sorter.clear_dependencies(&module_name);
        for import_node in ast.get_imports() {
            self.topo_sorter
                .add_dependency(&module_name, &import_node.from_attr.value);
        }

        let dependent_modules = match self.topo_sorter.sort_subgraph(&module_name) {
            Ok(modules) => modules,
            Err(cycle_error) => {
                self.handle_cycle_error(&cycle_error);
                // Return just the updated module for processing
                vec![module_name]
            }
        };

        for module_name in &dependent_modules {
            self.typecheck_module(module_name)
        }

        dependent_modules
    }

    pub fn get_hover_info(&self, module_name: &str, position: Position) -> Option<HoverInfo> {
        self.type_annotations
            .get(module_name)?
            .iter()
            .find(|a| a.contains(position))
            .map(|annotation| HoverInfo {
                type_str: format!("`{}`: `{}`", annotation.name, annotation.typ),
                range: annotation.range,
            })
    }

    pub fn get_definition_location(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<DefinitionLocation> {
        self.definition_links
            .get(module_name)?
            .iter()
            .find(|link| link.contains(position))
            .map(|link| DefinitionLocation {
                module: link.definition_module.clone(),
                range: link.definition_range,
            })
    }

    pub fn get_rename_locations(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<Vec<RenameLocation>> {
        let ast = self.asts.get(module_name)?;
        for component_node in ast.get_component_definitions() {
            let is_on_tag_name = component_node.opening_name_range.contains(position)
                || component_node
                    .closing_name_range
                    .is_some_and(|range| range.contains(position));

            if is_on_tag_name {
                return Some(
                    self.collect_component_rename_locations(&component_node.name, module_name),
                );
            }
        }

        let node = ast.find_node_at_position(position)?;

        let is_on_tag_name = node.name_ranges().any(|r| r.contains(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            HopNode::ComponentReference {
                component,
                definition_module: definition_location,
                ..
            } => definition_location.as_ref().map(|target_module| {
                self.collect_component_rename_locations(component, target_module)
            }),
            n @ HopNode::NativeHTML { .. } => Some(
                n.name_ranges()
                    .map(|range| RenameLocation {
                        module: module_name.to_string(),
                        range,
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    /// Returns information about a renameable symbol at the given position.
    /// Checks if the position is on a component name (reference or definition)
    /// and returns the symbol's current name and range if found.
    pub fn get_renameable_symbol(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<RenameableSymbol> {
        let ast = self.asts.get(module_name)?;

        // Check if we're on a component definition
        for component_node in ast.get_component_definitions() {
            if component_node.opening_name_range.contains(position) {
                return Some(RenameableSymbol {
                    current_name: component_node.name.clone(),
                    range: component_node.opening_name_range,
                });
            }
            if let Some(closing_range) = component_node.closing_name_range {
                if closing_range.contains(position) {
                    return Some(RenameableSymbol {
                        current_name: component_node.name.clone(),
                        range: closing_range,
                    });
                }
            }
        }

        let node = ast.find_node_at_position(position)?;

        let tag_name = node.tag_name()?;

        node.name_ranges()
            .find(|r| r.contains(position))
            .map(|range| RenameableSymbol {
                current_name: tag_name.to_string(),
                range,
            })
    }

    /// Collects all locations where a component should be renamed, including:
    /// - The component definition (opening and closing tags)
    /// - All references to the component (opening and closing tags)
    /// - All import statements that import the component
    fn collect_component_rename_locations(
        &self,
        component_name: &str,
        definition_module: &str,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        if let Some(ast) = self.asts.get(definition_module) {
            if let Some(component_node) = ast
                .get_component_definitions()
                .iter()
                .find(|node| node.name == component_name)
            {
                // Add the definition's opening tag name
                locations.push(RenameLocation {
                    module: definition_module.to_string(),
                    range: component_node.opening_name_range,
                });

                // Add the definition's closing tag name if it exists
                if let Some(range) = component_node.closing_name_range {
                    locations.push(RenameLocation {
                        module: definition_module.to_string(),
                        range,
                    });
                }
            }
        }

        for (module_name, ast) in &self.asts {
            // Find all import statements that import this component
            locations.extend(
                ast.get_imports()
                    .iter()
                    .filter(|n| {
                        n.imports_component(component_name) && n.imports_from(definition_module)
                    })
                    .map(|n| RenameLocation {
                        module: module_name.clone(),
                        range: n.component_attr.value_range,
                    }),
            );

            // Find all component references that references this component
            locations.extend(
                ast.iter_all_nodes()
                    .filter(|node| match node {
                        HopNode::ComponentReference {
                            component,
                            definition_module: reference_definition_module,
                            ..
                        } => {
                            reference_definition_module
                                .as_ref()
                                .is_some_and(|d| d == definition_module)
                                && component == component_name
                        }
                        _ => false,
                    })
                    .flat_map(|node| node.name_ranges())
                    .map(|range| RenameLocation {
                        module: module_name.clone(),
                        range,
                    }),
            );
        }

        locations
    }

    pub fn get_error_diagnostics(&self, module_name: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let mut found_parse_errors = false;

        for error in self.parse_errors.get(module_name).into_iter().flatten() {
            diagnostics.push(Diagnostic {
                message: error.message.clone(),
                range: error.range,
            });
            found_parse_errors = true;
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            for error in self.type_errors.get(module_name).into_iter().flatten() {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    range: error.range,
                });
            }
        }

        diagnostics
    }

    pub fn get_scripts(&self) -> String {
        let mut script_collector = ScriptCollector::new();
        for ast in self.asts.values() {
            script_collector.process_module(ast);
        }
        script_collector.build()
    }

    /// Get all file_attr values from render nodes across all modules
    /// I.e. files specified in <render file="index.html">
    pub fn get_render_file_paths(&self) -> Vec<String> {
        evaluator::get_render_file_paths(&self.asts)
    }

    /// Render the content for a specific file path
    pub fn render_file(&self, file_path: &str) -> Result<String> {
        evaluator::render_file(&self.asts, self.hop_mode, file_path)
    }

    pub fn evaluate_component(
        &self,
        module_name: &str,
        component_name: &str,
        args: HashMap<String, serde_json::Value>,
        slot_content: Option<&str>,
        additional_classes: Option<&str>,
    ) -> Result<String> {
        evaluator::evaluate_component(
            &self.asts,
            self.hop_mode,
            module_name,
            component_name,
            args,
            slot_content,
            additional_classes,
        )
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::archive::extract_markers_from_archive;
    use crate::tui::source_annotator::SourceAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn server_from_archive(archive: &Archive) -> Program {
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            map.insert(module_name, file.content.clone());
        }
        Program::from_modules(map, HopMode::Dev)
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
        let module = marker.filename.replace(".hop", "");

        let mut locs = server_from_archive(&archive)
            .get_rename_locations(&module, marker.position)
            .expect("Expected locations to be defined");

        locs.sort();

        let mut output = Vec::new();
        let annotator = SourceAnnotator::new().with_location();

        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");

            let module_locs: Vec<RenameLocation> = locs
                .iter()
                .filter(|l| l.module == module_name)
                .cloned()
                .collect();

            if !module_locs.is_empty() {
                output.push(annotator.annotate(Some(&file.name), &file.content, &module_locs));
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
        let module = marker.filename.replace(".hop", "");

        let loc = server_from_archive(&archive)
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == loc.module)
            .expect("File not found in archive");

        let output = SourceAnnotator::new().with_location().annotate(
            Some(&file.name),
            &file.content,
            &[loc],
        );

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let archive = Archive::from(input);
        let diagnostics = server_from_archive(&archive).get_error_diagnostics(module);

        if diagnostics.is_empty() {
            panic!("Expected diagnostics to be non-empty");
        }

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module)
            .expect("File not found in archive");

        let output = SourceAnnotator::new().with_location().annotate(
            Some(&file.name),
            &file.content,
            &diagnostics,
        );

        expected.assert_eq(&output);
    }

    fn check_type_errors(input: &str, expected: Expect) {
        let archive = Archive::from(input);
        let program = server_from_archive(&archive);

        let mut output = Vec::new();
        let annotator = SourceAnnotator::new().with_location();

        // Get all modules that have type errors
        let type_errors = program.get_type_errors();
        let mut modules_with_errors: Vec<_> = type_errors
            .iter()
            .filter(|(_, errors)| !errors.is_empty())
            .collect();

        // Sort by module name for consistent output
        modules_with_errors.sort_by_key(|(module_name, _)| module_name.as_str());

        for (module_name, errors) in modules_with_errors {
            let file = archive
                .iter()
                .find(|f| f.name.replace(".hop", "") == *module_name)
                .expect("File not found in archive");

            let annotated = annotator.annotate(Some(&file.name), &file.content, errors);

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
        let module = marker.filename.replace(".hop", "");

        let symbol = server_from_archive(&archive)
            .get_renameable_symbol(&module, marker.position)
            .expect("Expected symbol to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module)
            .expect("Could not find file in archive");

        let output = SourceAnnotator::new().with_location().annotate(
            Some(&file.name),
            &file.content,
            &[symbol],
        );

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
        let module = marker.filename.replace(".hop", "");

        let hover_info = server_from_archive(&archive)
            .get_hover_info(&module, marker.position)
            .expect("Expected hover info to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module)
            .expect("Could not find file in archive");

        let output = SourceAnnotator::new().with_location().annotate(
            Some(&file.name),
            &file.content,
            &[hover_info],
        );

        expected.assert_eq(&output);
    }

    #[test]
    fn test_get_definition_from_component_reference() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="hop/components" />

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_definition_from_component_reference_closing() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="hop/components" />

                <main-comp>
                  <hello-world>

                  </hello-world>
                     ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_component_reference() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="components" />

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^

                Rename
                  --> components.hop (line 3, col 3)
                3 | </hello-world>
                  |   ^^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | <import component="hello-world" from="components" />
                  |                    ^^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <hello-world />
                  |    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_component_reference_same_component() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^

                Rename
                  --> main.hop (line 3, col 3)
                3 | </hello-world>
                  |   ^^^^^^^^^^^

                Rename
                  --> main.hop (line 6, col 4)
                6 |   <hello-world />
                  |    ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_component_definition() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                 ^
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="components" />

                <main-comp>
                  <hello-world />
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^

                Rename
                  --> components.hop (line 3, col 3)
                3 | </hello-world>
                  |   ^^^^^^^^^^^

                Rename
                  --> main.hop (line 1, col 20)
                1 | <import component="hello-world" from="components" />
                  |                    ^^^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 4)
                4 |   <hello-world />
                  |    ^^^^^^^^^^^
            "#]],
        );
    }

    // Make sure that when we rename a component in a module that has
    // the same name as a module in some other component, the module in
    // the other component is left unchanged.
    #[test]
    fn test_get_rename_locations_main_comp_scoped() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                <main-comp>
                 ^
                  <hello-world />
                </main-comp>

                -- main.hop --
                <import component="hello-world" from="components" />

                <main-comp>
                  <hello-world />
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 5, col 2)
                5 | <main-comp>
                  |  ^^^^^^^^^

                Rename
                  --> components.hop (line 7, col 3)
                7 | </main-comp>
                  |   ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_renameable_symbol() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                <hello-world>
                 ^
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            expect![[r#"
                Renameable symbol: hello-world
                  --> main.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_hover_info() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                <main-comp {user: {name: string}}>
                              ^
                  <h1>Hello {user.name}</h1>
                </main-comp>
            "#},
            expect![[r#"
                `user`: `{name: string}`
                  --> main.hop (line 1, col 13)
                1 | <main-comp {user: {name: string}}>
                  |             ^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_rename_locations_from_native_html_opening_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <div>
                     ^
                        <span>Content</span>
                    </div>
                </main-comp>
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
    fn test_get_rename_locations_from_native_html_closing_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <div>
                        <span>Content</span>
                    </div>
                       ^
                </main-comp>
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
    fn test_get_rename_locations_from_self_closing_html_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <br />
                     ^
                </main-comp>
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
    fn test_get_renameable_symbol_native_html() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                    <div>Content</div>
                     ^
                </main-comp>
            "#},
            expect![[r#"
                Renameable symbol: div
                  --> main.hop (line 2, col 6)
                2 |     <div>Content</div>
                  |      ^^^
            "#]],
        );
    }

    #[test]
    fn test_get_error_diagnostics_parse_errors() {
        check_error_diagnostics(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div>
                  <span>unclosed span
                </main-comp>
            "#},
            "main",
            expect![[r#"
                Unclosed <span>
                  --> main.hop (line 3, col 3)
                3 |   <span>unclosed span
                  |   ^^^^^^

                Unclosed <div>
                  --> main.hop (line 2, col 3)
                2 |   <div>
                  |   ^^^^^
            "#]],
        );
    }

    // Even when there's parse errors we should be able to rename.
    #[test]
    fn test_rename_with_parse_errors() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  ^
                  <div>
                  <span>unclosed span
                </main-comp>
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 2)
                1 | <main-comp>
                  |  ^^^^^^^^^

                Rename
                  --> main.hop (line 4, col 3)
                4 | </main-comp>
                  |   ^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_cycle_error_reporting() {
        check_type_errors(
            indoc! {r#"
                -- a.hop --
                <import component="b-comp" from="b" />
                <a-comp>
                  <b-comp />
                </a-comp>

                -- b.hop --
                <import component="a-comp" from="a" />
                <b-comp>
                  <a-comp />
                </b-comp>

                -- c.hop --
                <import component="a-comp" from="a" />
                <c-comp>
                  <a-comp />
                </c-comp>
            "#},
            expect![[r#"
                Circular import detected: a imports b which creates a dependency cycle: a → b → a
                  --> a.hop (line 1, col 34)
                1 | <import component="b-comp" from="b" />
                  |                                  ^

                Circular import detected: b imports a which creates a dependency cycle: a → b → a
                  --> b.hop (line 1, col 34)
                1 | <import component="a-comp" from="a" />
                  |                                  ^
            "#]],
        );
    }
}
