use crate::common::{Position, Range, RangeError};
use crate::hop::parser::{Module, parse};
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::TopoSorter;
use crate::hop::typechecker::{TypeResult, typecheck};
use crate::tui::source_annotator::Annotation;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct HoverInfo {
    pub type_str: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefinitionLocation {
    pub module: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub message: String,
    pub range: Range,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenameLocation {
    pub module: String,
    pub range: Range,
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

impl Annotation for RenameLocation {
    fn range(&self) -> Range {
        self.range
    }

    fn message(&self) -> String {
        "Rename".to_string()
    }
}

impl Annotation for DefinitionLocation {
    fn range(&self) -> Range {
        self.range
    }

    fn message(&self) -> String {
        "Definition".to_string()
    }
}

impl Annotation for Diagnostic {
    fn range(&self) -> Range {
        self.range
    }

    fn message(&self) -> String {
        self.message.clone()
    }
}

impl Annotation for RenameableSymbol {
    fn range(&self) -> Range {
        self.range
    }

    fn message(&self) -> String {
        format!("Renameable symbol: {}", self.current_name)
    }
}

impl Annotation for HoverInfo {
    fn range(&self) -> Range {
        self.range
    }

    fn message(&self) -> String {
        self.type_str.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenameableSymbol {
    pub current_name: String,
    pub range: Range,
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

        for import_node in &module.import_nodes {
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

    pub fn get_hover_info(&self, module_name: &str, position: Position) -> Option<HoverInfo> {
        self.type_results
            .get(module_name)?
            .type_annotations
            .iter()
            .find(|a| a.range.contains_position(position))
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
        self.type_results
            .get(module_name)?
            .component_definition_links
            .iter()
            .find(|link| link.reference_name_contains_position(position))
            .map(|link| DefinitionLocation {
                module: link.definition_module.clone(),
                range: link.definition_opening_name_range,
            })
    }

    pub fn get_rename_locations(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<Vec<RenameLocation>> {
        let type_result = self.type_results.get(module_name)?;

        // Check if we're on a component reference
        for link in &type_result.component_definition_links {
            if link.reference_name_contains_position(position) {
                return Some(self.collect_component_rename_locations(
                    &link.definition_component_name,
                    &link.definition_module,
                ));
            }
        }

        // Check if we're on a component definition
        for (component_name, component_info) in &type_result.component_info {
            let is_on_definition = component_info
                .definition_opening_name_range
                .contains_position(position)
                || component_info
                    .definition_closing_name_range
                    .is_some_and(|range| range.contains_position(position));

            if is_on_definition {
                // We're on a definition, collect all rename locations
                return Some(self.collect_component_rename_locations(component_name, module_name));
            }
        }

        None
    }

    /// Returns information about a renameable symbol at the given position.
    /// Checks if the position is on a component name (reference or definition)
    /// and returns the symbol's current name and range if found.
    pub fn get_renameable_symbol(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<RenameableSymbol> {
        let type_result = self.type_results.get(module_name)?;

        // Check if we're on a component reference
        for link in &type_result.component_definition_links {
            if link
                .reference_opening_name_range
                .contains_position(position)
            {
                return Some(RenameableSymbol {
                    current_name: link.definition_component_name.clone(),
                    range: link.reference_opening_name_range,
                });
            }
            if link
                .reference_closing_name_range
                .is_some_and(|r| r.contains_position(position))
            {
                return Some(RenameableSymbol {
                    current_name: link.definition_component_name.clone(),
                    range: link.reference_closing_name_range.unwrap(),
                });
            }
        }

        // Check if we're on a component definition
        for (component_name, component_info) in &type_result.component_info {
            if component_info
                .definition_opening_name_range
                .contains_position(position)
            {
                return Some(RenameableSymbol {
                    current_name: component_name.clone(),
                    range: component_info.definition_opening_name_range,
                });
            }
            if let Some(closing_range) = component_info.definition_closing_name_range {
                if closing_range.contains_position(position) {
                    return Some(RenameableSymbol {
                        current_name: component_name.clone(),
                        range: closing_range,
                    });
                }
            }
        }

        None
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

        if let Some(component_info) = self
            .type_results
            .get(definition_module)
            .and_then(|t| t.component_info.get(component_name))
        {
            // Add the definition's opening tag name
            locations.push(RenameLocation {
                module: definition_module.to_string(),
                range: component_info.definition_opening_name_range,
            });

            // Add the definition's closing tag name if it exists
            if let Some(closing_range) = component_info.definition_closing_name_range {
                locations.push(RenameLocation {
                    module: definition_module.to_string(),
                    range: closing_range,
                });
            }
        }

        for (module_name, type_result) in &self.type_results {
            for link in &type_result.component_definition_links {
                if link.definition_component_name == component_name
                    && link.definition_module == definition_module
                {
                    // Add the reference's opening tag name
                    locations.push(RenameLocation {
                        module: module_name.clone(),
                        range: link.reference_opening_name_range,
                    });

                    // Add the reference's closing tag name if it exists
                    if let Some(name_range) = link.reference_closing_name_range {
                        locations.push(RenameLocation {
                            module: module_name.clone(),
                            range: name_range,
                        });
                    }
                }
            }
        }

        // Find all import statements that import this component
        for (module_name, module) in &self.modules {
            for import in &module.import_nodes {
                if import.component_attr.value == component_name
                    && import.from_attr.value == definition_module
                {
                    locations.push(RenameLocation {
                        module: module_name.clone(),
                        range: import.component_attr.value_range,
                    });
                }
            }
        }

        locations
    }

    pub fn get_error_diagnostics(&self, module_name: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let mut found_parse_errors = false;

        if let Some(parse_errors) = self.parse_errors.get(module_name) {
            for error in parse_errors {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    range: error.range,
                });
                found_parse_errors = true;
            }
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            if let Some(typecheck_errors) = self.type_errors.get(module_name) {
                for error in typecheck_errors {
                    diagnostics.push(Diagnostic {
                        message: error.message.clone(),
                        range: error.range,
                    });
                }
            }
        }

        diagnostics
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

    fn server_from_archive(archive: &Archive) -> Server {
        let mut server = Server::new();
        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            server.update_module(module_name, &file.content);
        }
        server
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

        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");

            let module_locs: Vec<RenameLocation> = locs
                .iter()
                .filter(|l| l.module == module_name)
                .cloned()
                .collect();

            if !module_locs.is_empty() {
                output.push(
                    SourceAnnotator::new()
                        .with_filename(&file.name)
                        .with_location()
                        .annotate(&file.content, &module_locs),
                );
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

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .annotate(&file.content, &[loc]);

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

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .annotate(&file.content, &diagnostics);

        expected.assert_eq(&output);
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

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .annotate(&file.content, &[symbol]);

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

        let output = SourceAnnotator::new()
            .with_filename(&file.name)
            .with_location()
            .annotate(&file.content, &[hover_info]);

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
}
