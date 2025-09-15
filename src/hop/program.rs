use crate::document::DocumentPosition;
use crate::document::document_cursor::{DocumentRange, Ranged, StringSpan};
use crate::error_collector::ErrorCollector;
use crate::hop::ast::Ast;
use crate::hop::parse_error::ParseError;
use crate::hop::parser::parse;
use crate::hop::script_collector::ScriptCollector;
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::TopoSorter;
use crate::hop::type_error::TypeError;
use crate::ir;
use anyhow::Result;
use std::collections::{HashMap, HashSet};

use super::ast::Node;
use super::module_name::ModuleName;
use super::typechecker::TypeChecker;

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
    pub current_name: StringSpan,
    pub range: DocumentRange,
}

#[derive(Debug, Default)]
pub struct Program {
    topo_sorter: TopoSorter<ModuleName>,
    parse_errors: HashMap<ModuleName, ErrorCollector<ParseError>>,
    modules: HashMap<ModuleName, Ast>,
    type_checker: TypeChecker,
}

impl Program {
    pub fn new(modules: HashMap<ModuleName, String>) -> Self {
        let mut program = Self::default();
        for (module_name, source_code) in modules {
            program.update_module(module_name, source_code);
        }
        program
    }

    pub fn update_module(
        &mut self,
        module_name: ModuleName,
        source_code: String,
    ) -> Vec<ModuleName> {
        // Parse the module
        let parse_errors = self.parse_errors.entry(module_name.clone()).or_default();
        parse_errors.clear();
        let module = parse(
            module_name.clone(),
            Tokenizer::new(source_code),
            parse_errors,
        );

        // Get all modules that this module depends on
        let module_dependencies = module
            .get_imports()
            .iter()
            .map(|import_node| import_node.imported_module().clone())
            .collect::<HashSet<ModuleName>>();

        // Store the AST
        self.modules.insert(module_name.clone(), module);

        // Typecheck the module along with all dependent modules (grouped
        // into strongly connected components).
        let grouped_modules = self
            .topo_sorter
            .update_node(module_name, module_dependencies);
        for names in &grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.modules.get(name))
                .collect::<Vec<_>>();
            self.type_checker.typecheck(&modules);
        }

        // Return all modules that have been re-typechecked
        grouped_modules.into_iter().flatten().collect()
    }

    pub fn get_parse_errors(&self) -> &HashMap<ModuleName, ErrorCollector<ParseError>> {
        &self.parse_errors
    }

    pub fn get_type_errors(&self) -> &HashMap<ModuleName, Vec<TypeError>> {
        &self.type_checker.type_errors
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
        let ast = self.modules.get(module_name)?;

        // First check if we're on an import node's component attribute
        for import in ast.get_imports() {
            if import.component_attr.value.contains_position(position) {
                // The import specifies the component name and module
                let target_module = &import.module_name;
                let component_name = import.component_attr.value.as_str();

                // Find the component definition in the target module
                let target_ast = self.modules.get(target_module)?;
                let component_def = target_ast.get_component_definition(component_name)?;

                return Some(DefinitionLocation {
                    module: target_module.clone(),
                    range: component_def.tag_name.clone(),
                });
            }
        }

        // Check if we're on a component definition's tag name (opening or closing)
        for component_def in ast.get_component_definitions() {
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

        let node = ast.find_node_at_position(position)?;

        let is_on_tag_name = node.tag_names().any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            Node::ComponentReference {
                tag_name,
                definition_module,
                ..
            } => {
                let module_name = definition_module.as_ref()?;
                let component_def = self
                    .modules
                    .get(module_name)?
                    .get_component_definition(tag_name.as_str())?;
                Some(DefinitionLocation {
                    module: module_name.clone(),
                    range: component_def.tag_name.clone(),
                })
            }
            Node::Html { tag_name, .. } => {
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
        let ast = self.modules.get(module_name)?;
        for node in ast.get_component_definitions() {
            if node
                .tag_name_ranges()
                .any(|r| r.contains_position(position))
            {
                return Some(
                    self.collect_component_rename_locations(node.tag_name.as_str(), module_name),
                );
            }
        }

        let node = ast.find_node_at_position(position)?;

        let is_on_tag_name = node.tag_names().any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            Node::ComponentReference {
                tag_name,
                definition_module: definition_location,
                ..
            } => definition_location.as_ref().map(|target_module| {
                self.collect_component_rename_locations(tag_name.as_str(), target_module)
            }),
            n @ Node::Html { .. } => Some(
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
    /// Checks if the position is on a component name (reference or definition)
    /// and returns the symbol's current name and range if found.
    pub fn get_renameable_symbol(
        &self,
        module_name: &ModuleName,
        position: DocumentPosition,
    ) -> Option<RenameableSymbol> {
        let ast = self.modules.get(module_name)?;

        for component_node in ast.get_component_definitions() {
            if let Some(range) = component_node
                .tag_name_ranges()
                .find(|r| r.contains_position(position))
            {
                return Some(RenameableSymbol {
                    current_name: range.to_string_span(),
                    range: range.clone(),
                });
            }
        }

        let node = ast.find_node_at_position(position)?;

        node.tag_names()
            .find(|r| r.contains_position(position))
            .map(|range| RenameableSymbol {
                current_name: range.to_string_span(),
                range: range.clone(),
            })
    }

    /// Collects all locations where a component should be renamed, including:
    /// - The component definition (opening and closing tags)
    /// - All references to the component (opening and closing tags)
    /// - All import statements that import the component
    fn collect_component_rename_locations(
        &self,
        component_name: &str,
        definition_module: &ModuleName,
    ) -> Vec<RenameLocation> {
        let mut locations = Vec::new();

        if let Some(module) = self.modules.get(definition_module) {
            if let Some(component_node) = module.get_component_definition(component_name) {
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

        for (module_name, ast) in &self.modules {
            // Find all import statements that import this component
            locations.extend(
                ast.get_imports()
                    .iter()
                    .filter(|n| {
                        n.imports_component(component_name) && n.imports_from(definition_module)
                    })
                    .map(|n| RenameLocation {
                        module: module_name.clone(),
                        range: n.imported_component().clone(),
                    }),
            );

            // Find all component references that references this component
            locations.extend(
                ast.iter_all_nodes()
                    .filter(|node| match node {
                        Node::ComponentReference {
                            tag_name,
                            definition_module: reference_definition_module,
                            ..
                        } => {
                            reference_definition_module
                                .as_ref()
                                .is_some_and(|d| d == definition_module)
                                && tag_name.as_str() == component_name
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

    pub fn get_scripts(&self) -> String {
        let mut script_collector = ScriptCollector::new();
        for ast in self.modules.values() {
            script_collector.process_module(ast);
        }
        script_collector.build()
    }

    /// Evaluate an IR entrypoint by name
    pub fn evaluate_ir_entrypoint(
        &self,
        entrypoint_name: &str,
        args: HashMap<String, serde_json::Value>,
        hop_mode: &str,
    ) -> Result<String> {
        // Compile to IR
        let ir_module = ir::Compiler::compile(&self.modules);

        // Get the entrypoint
        let entrypoint = ir_module
            .entry_points
            .get(entrypoint_name)
            .ok_or_else(|| anyhow::anyhow!("Entrypoint '{}' not found", entrypoint_name))?;

        // Evaluate the entrypoint
        ir::evaluator::evaluate_entrypoint(entrypoint, args, hop_mode)
    }

    /// Get all modules for compilation
    pub fn get_modules(&self) -> &HashMap<ModuleName, Ast> {
        &self.modules
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
            let module_name = ModuleName::new(file.name.replace(".hop", "")).unwrap();
            map.insert(module_name, file.content.clone());
        }
        Program::new(map)
    }

    fn program_from_archive(archive: &Archive) -> Program {
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_name = ModuleName::new(file.name.replace(".hop", "")).unwrap();
            map.insert(module_name, file.content.clone());
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
        let module = ModuleName::new(marker.filename.replace(".hop", "")).unwrap();

        let locs = program_from_archive(&archive)
            .get_rename_locations(&module, marker.position)
            .expect("Expected locations to be defined");

        let mut output = Vec::new();
        let annotator = DocumentAnnotator::new().with_location();

        for file in archive.iter() {
            let module_name = ModuleName::new(file.name.replace(".hop", "")).unwrap();

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
        let module = ModuleName::new(marker.filename.replace(".hop", "")).unwrap();

        let program = program_from_archive(&archive);

        let loc = program
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let output = DocumentAnnotator::new().with_location().annotate(
            Some(loc.module.as_str()),
            [SimpleAnnotation {
                message: "Definition".to_string(),
                range: loc.range,
            }],
        );

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let program = program_from_txtar(input);

        let diagnostics =
            program.get_error_diagnostics(ModuleName::new(module.to_string()).unwrap());

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
        modules_with_errors.sort_by_key(|(module_name, _)| module_name.as_str());

        for (module_name, errors) in modules_with_errors {
            let annotated = annotator.annotate(Some(module_name.as_str()), errors);

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
        let module = ModuleName::new(marker.filename.replace(".hop", "")).unwrap();

        let symbol = program_from_archive(&archive)
            .get_renameable_symbol(&module, marker.position)
            .expect("Expected symbol to be defined");

        let file = archive
            .iter()
            .find(|f| f.name.replace(".hop", "") == module.as_str())
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
        let module = ModuleName::new(marker.filename.replace(".hop", "")).unwrap();

        let hover_info = program_from_archive(&archive)
            .get_hover_info(&module, marker.position)
            .expect("Expected hover info to be defined");

        let file = archive
            .iter()
            .find(|f| ModuleName::new(f.name.replace(".hop", "")).unwrap() == module)
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
    /// DEFINITION LOCATION                                                 ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_get_definition_from_component_reference() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="@/hop/components" />

                <main-comp>
                  <hello-world />
                   ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components (line 1, col 2)
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
                <import component="hello-world" from="@/hop/components" />

                <main-comp>
                  <hello-world>
                  </hello-world>
                     ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_definition_from_import_component_attribute() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="@/hop/components" />
                                     ^

                <main-comp>
                  <hello-world />
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> hop/components (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_definition_from_component_definition_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <hello-world>
                   ^
                  <h1>Hello World</h1>
                </hello-world>
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_definition_from_component_definition_closing_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>
                    ^
            "#},
            expect![[r#"
                Definition
                  --> main (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn test_get_definition_from_html_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div class="container">
                   ^
                    <span>Content</span>
                  </div>
                </main-comp>
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
    fn test_get_definition_from_html_closing_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div class="container">
                    <span>Content</span>
                  </div>
                    ^
                </main-comp>
            "#},
            expect![[r#"
                Definition
                  --> main (line 2, col 4)
                2 |   <div class="container">
                  |    ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// RENAME LOCATIONS                                                    ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_get_rename_locations_from_component_reference() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                <hello-world>
                  <h1>Hello World</h1>
                </hello-world>

                -- main.hop --
                <import component="hello-world" from="@/components" />

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
                1 | <import component="hello-world" from="@/components" />
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
                <import component="hello-world" from="@/components" />

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
                1 | <import component="hello-world" from="@/components" />
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
                <import component="hello-world" from="@/components" />

                <main-comp>
                  <hello-world />
                </main-comp>
            "#},
            // The result here should not contain rename locations in main.hop.
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
    fn test_get_rename_locations_from_native_html_nested() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  <div>
                    <div>
                     ^
                        <div>Content</div>
                    </div>
                  </div>
                </main-comp>
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

    ///////////////////////////////////////////////////////////////////////////
    /// RENAMEABLE SYMBOL                                                   ///
    ///////////////////////////////////////////////////////////////////////////

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
                hello-world
                  --> main.hop (line 1, col 2)
                1 | <hello-world>
                  |  ^^^^^^^^^^^
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
                div
                  --> main.hop (line 2, col 6)
                2 |     <div>Content</div>
                  |      ^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    /// HOVER INFO                                                          ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_get_hover_info_parameter() {
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

    ///////////////////////////////////////////////////////////////////////////
    /// ERROR DIAGNOSTICS                                                   ///
    ///////////////////////////////////////////////////////////////////////////

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
    /// RENAME LOCATIONS                                                    ///
    ///////////////////////////////////////////////////////////////////////////

    // Even when there's parse errors we should be able to rename.
    #[test]
    fn test_rename_with_parse_errors() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                <main-comp>
                  ^
                  <div>
                  <span>
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

    ///////////////////////////////////////////////////////////////////////////
    /// IMPORT CYCLES                                                       ///
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn test_cycle_error_reporting() {
        let mut program = program_from_txtar(indoc! {r#"
            -- a.hop --
            <import component="b-comp" from="@/b" />
            <a-comp>
              <b-comp />
            </a-comp>

            -- b.hop --
            <import component="a-comp" from="@/a" />
            <b-comp>
              <a-comp />
            </b-comp>

            -- c.hop --
            <import component="a-comp" from="@/a" />
            <c-comp>
              <a-comp />
            </c-comp>
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → a
                  --> a (line 1, col 34)
                1 | <import component="b-comp" from="@/b" />
                  |                                  ^^^

                Import cycle: b imports from a which creates a dependency cycle: a → b → a
                  --> b (line 1, col 34)
                1 | <import component="a-comp" from="@/a" />
                  |                                  ^^^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            ModuleName::new("a".to_string()).unwrap(),
            indoc! {r#"
                <a-comp>
                </a-comp>
            "#}
            .to_string(),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
    }

    #[test]
    fn test_cycle_error_reporting_large() {
        let mut program = program_from_txtar(indoc! {r#"
            -- a.hop --
            <import component="b-comp" from="@/b" />
            <a-comp>
              <b-comp />
            </a-comp>

            -- b.hop --
            <import component="c-comp" from="@/c" />
            <b-comp>
              <c-comp />
            </b-comp>

            -- c.hop --
            <import component="d-comp" from="@/d" />
            <c-comp>
              <d-comp />
            </c-comp>

            -- d.hop --
            <import component="a-comp" from="@/a" />
            <d-comp>
              <a-comp />
            </d-comp>
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → c → d → a
                  --> a (line 1, col 34)
                1 | <import component="b-comp" from="@/b" />
                  |                                  ^^^

                Import cycle: b imports from c which creates a dependency cycle: a → b → c → d → a
                  --> b (line 1, col 34)
                1 | <import component="c-comp" from="@/c" />
                  |                                  ^^^

                Import cycle: c imports from d which creates a dependency cycle: a → b → c → d → a
                  --> c (line 1, col 34)
                1 | <import component="d-comp" from="@/d" />
                  |                                  ^^^

                Import cycle: d imports from a which creates a dependency cycle: a → b → c → d → a
                  --> d (line 1, col 34)
                1 | <import component="a-comp" from="@/a" />
                  |                                  ^^^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            ModuleName::new("c".to_string()).unwrap(),
            indoc! {r#"
                <c-comp>
                </c-comp>
            "#}
            .to_string(),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
        // Introduce new cycle a → b → a
        program.update_module(
            ModuleName::new("b".to_string()).unwrap(),
            indoc! {r#"
                <import component="a-comp" from="@/a" />
                <b-comp>
                  <a-comp />
                </b-comp>
            "#}
            .to_string(),
        );
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → a
                  --> a (line 1, col 34)
                1 | <import component="b-comp" from="@/b" />
                  |                                  ^^^

                Import cycle: b imports from a which creates a dependency cycle: a → b → a
                  --> b (line 1, col 34)
                1 | <import component="a-comp" from="@/a" />
                  |                                  ^^^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            ModuleName::new("b".to_string()).unwrap(),
            indoc! {r#"
                <b-comp>
                </b-comp>
            "#}
            .to_string(),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
    }

    #[test]
    fn test_evaluate_ir_entrypoint() {
        let program = program_from_txtar(indoc! {r#"
            -- main.hop --
            <hello-world entrypoint {name: string}>
              <h1>Hello {name}!</h1>
            </hello-world>

            <another-comp entrypoint>
              <p>Static content</p>
            </another-comp>
        "#});

        // Test evaluating hello-world entrypoint with a name parameter
        let mut args = HashMap::new();
        args.insert("name".to_string(), serde_json::json!("Alice"));

        let result = program
            .evaluate_ir_entrypoint("hello-world", args, "dev")
            .expect("Should evaluate successfully");

        assert!(result.contains("<h1>Hello Alice!</h1>"));

        // Test evaluating another-comp entrypoint without parameters
        let result = program
            .evaluate_ir_entrypoint("another-comp", HashMap::new(), "dev")
            .expect("Should evaluate successfully");

        assert!(result.contains("<p>Static content</p>"));

        // Test error when entrypoint doesn't exist
        let result = program.evaluate_ir_entrypoint("non-existent", HashMap::new(), "dev");
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Entrypoint 'non-existent' not found")
        );
    }
}
