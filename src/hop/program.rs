use crate::common::{ParseError, TypeError};
use crate::hop::ast::HopAst;
use crate::hop::evaluator;
use crate::hop::parser::parse;
use crate::hop::script_collector::ScriptCollector;
use crate::hop::tokenizer::Tokenizer;
use crate::hop::toposorter::TopoSorter;
use crate::range::Position;
use crate::range::string_cursor::{Spanned, StringSpan};
use anyhow::Result;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display};

use super::ast::HopNode;
use super::evaluator::HopMode;
use super::typechecker::TypeChecker;

#[derive(Debug, Clone)]
pub struct HoverInfo {
    pub type_str: String,
    pub span: StringSpan,
}

impl Spanned for HoverInfo {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}

impl Display for HoverInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_str)
    }
}

#[derive(Debug, Clone)]
pub struct DefinitionLocation {
    pub module: String,
    pub span: StringSpan,
}

impl Spanned for DefinitionLocation {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}

impl Display for DefinitionLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Definition")
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub span: StringSpan,
}

impl Spanned for Diagnostic {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug, Clone)]
pub struct RenameLocation {
    pub module: String,
    pub span: StringSpan,
}

impl Spanned for RenameLocation {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}

impl Display for RenameLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Rename")
    }
}

impl PartialEq for RenameLocation {
    fn eq(&self, other: &Self) -> bool {
        self.module == other.module
            && self.span.start() == other.span.start()
            && self.span.end() == other.span.end()
    }
}

impl Eq for RenameLocation {}

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
                match self.span.start().cmp(&other.span.start()) {
                    std::cmp::Ordering::Equal => {
                        // Finally by range end
                        self.span.end().cmp(&other.span.end())
                    }
                    other => other,
                }
            }
            other => other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RenameableSymbol {
    pub current_name: String,
    pub span: StringSpan,
}

impl Spanned for RenameableSymbol {
    fn span(&self) -> &StringSpan {
        &self.span
    }
}

impl Display for RenameableSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Renameable symbol: {}", self.current_name)
    }
}

#[derive(Default, Debug)]
pub struct Program {
    topo_sorter: TopoSorter,
    source_code: HashMap<String, String>,
    parse_errors: HashMap<String, Vec<ParseError>>,
    asts: HashMap<String, HopAst>,
    type_checker: TypeChecker,
}

impl From<HashMap<String, String>> for Program {
    fn from(modules: HashMap<String, String>) -> Self {
        let mut program = Self::new();
        for (module_name, source_code) in modules {
            program.update_module(&module_name, source_code);
        }
        program
    }
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn update_module(&mut self, module_name: &str, source_code: String) -> Vec<String> {
        // Parse the module
        let parse_errors = self
            .parse_errors
            .entry(module_name.to_string())
            .or_default();
        parse_errors.clear();
        let ast = parse(
            module_name.to_string(),
            Tokenizer::new(&source_code),
            parse_errors,
        );

        // Get all dependencies from the module
        let dependencies = ast
            .get_imports()
            .iter()
            .map(|import_node| import_node.from_attr.value.to_string())
            .collect::<HashSet<String>>();

        // Store the AST and source code
        self.asts.insert(module_name.to_string(), ast);
        self.source_code
            .insert(module_name.to_string(), source_code);

        // Typecheck the module along with all dependent modules (grouped
        // into strongly connected components).
        let dependent_modules = self.topo_sorter.update_node(module_name, dependencies);
        for c in &dependent_modules {
            let asts = c
                .iter()
                .filter_map(|n| self.asts.get(n))
                .collect::<Vec<_>>();
            self.type_checker.typecheck(&asts);
        }

        // Return all modules that have been re-typechecked
        dependent_modules.into_iter().flatten().collect()
    }

    pub fn get_parse_errors(&self) -> &HashMap<String, Vec<ParseError>> {
        &self.parse_errors
    }

    pub fn get_type_errors(&self) -> &HashMap<String, Vec<TypeError>> {
        &self.type_checker.type_errors
    }

    pub fn get_source_code(&self) -> &HashMap<String, String> {
        &self.source_code
    }

    pub fn get_hover_info(&self, module_name: &str, position: Position) -> Option<HoverInfo> {
        self.type_checker
            .type_annotations
            .get(module_name)?
            .iter()
            .find(|a| a.span.contains_position(position))
            .map(|annotation| HoverInfo {
                type_str: format!("`{}`: `{}`", annotation.name, annotation.typ),
                span: annotation.span.clone(),
            })
    }

    pub fn get_definition_location(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<DefinitionLocation> {
        let ast = self.asts.get(module_name)?;

        let node = ast.find_node_at_position(position)?;

        let is_on_tag_name = node
            .tag_name_ranges()
            .any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            HopNode::ComponentReference {
                tag_name,
                definition_module,
                ..
            } => {
                let m = match definition_module {
                    Some(m) => m,
                    None => return None,
                };
                let module = self.asts.get(m)?;
                let component_def = module.get_component_definition(tag_name.as_str())?;
                Some(DefinitionLocation {
                    module: m.to_string(),
                    span: component_def.tag_name.clone(),
                })
            }
            HopNode::Html { .. } => {
                // TODO
                None
            }
            _ => None,
        }
    }

    pub fn get_rename_locations(
        &self,
        module_name: &str,
        position: Position,
    ) -> Option<Vec<RenameLocation>> {
        let ast = self.asts.get(module_name)?;
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

        let is_on_tag_name = node
            .tag_name_ranges()
            .any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            HopNode::ComponentReference {
                tag_name,
                definition_module: definition_location,
                ..
            } => definition_location.as_ref().map(|target_module| {
                self.collect_component_rename_locations(tag_name.as_str(), target_module)
            }),
            n @ HopNode::Html { .. } => Some(
                n.tag_name_ranges()
                    .map(|range| RenameLocation {
                        module: module_name.to_string(),
                        span: range,
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
        module_name: &str,
        position: Position,
    ) -> Option<RenameableSymbol> {
        let ast = self.asts.get(module_name)?;

        for component_node in ast.get_component_definitions() {
            if let Some(range) = component_node
                .tag_name_ranges()
                .find(|r| r.contains_position(position))
            {
                return Some(RenameableSymbol {
                    current_name: component_node.tag_name.to_string(),
                    span: range,
                });
            }
        }

        let node = ast.find_node_at_position(position)?;

        let tag_name = node.tag_name()?;

        node.tag_name_ranges()
            .find(|r| r.contains_position(position))
            .map(|range| RenameableSymbol {
                current_name: tag_name.to_string(),
                span: range,
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
                .find(|node| node.tag_name.as_str() == component_name)
            {
                // Add the definition's opening tag name
                locations.push(RenameLocation {
                    module: definition_module.to_string(),
                    span: component_node.tag_name.clone(),
                });

                // Add the definition's closing tag name if it exists
                if let Some(span) = component_node.closing_tag_name.as_ref() {
                    locations.push(RenameLocation {
                        module: definition_module.to_string(),
                        span: span.clone(),
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
                        span: n.component_attr.value.clone(),
                    }),
            );

            // Find all component references that references this component
            locations.extend(
                ast.iter_all_nodes()
                    .filter(|node| match node {
                        HopNode::ComponentReference {
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
                    .flat_map(|node| node.tag_name_ranges())
                    .map(|range| RenameLocation {
                        module: module_name.clone(),
                        span: range,
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
                span: error.span.clone(),
            });
            found_parse_errors = true;
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            for error in self
                .type_checker
                .type_errors
                .get(module_name)
                .into_iter()
                .flatten()
            {
                diagnostics.push(Diagnostic {
                    message: error.message.clone(),
                    span: error.span.clone(),
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
        let mut result = Vec::new();
        for ast in self.asts.values() {
            for node in ast.get_renders() {
                result.push(node.file_attr.value.to_string())
            }
        }
        result
    }

    /// Render the content for a specific file path
    pub fn render_file(
        &self,
        file_path: &str,
        hop_mode: HopMode,
        output: &mut String,
    ) -> Result<()> {
        evaluator::render_file(&self.asts, hop_mode, file_path, output)
    }

    pub fn evaluate_component(
        &self,
        module_name: &str,
        component_name: &str,
        args: HashMap<String, serde_json::Value>,
        hop_mode: HopMode,
        output: &mut String,
    ) -> Result<()> {
        evaluator::evaluate_component(
            &self.asts,
            hop_mode,
            module_name,
            component_name,
            args,
            None,
            None,
            output,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::range::SourceAnnotator;
    use crate::test_utils::archive::extract_markers_from_archive;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;

    fn program_from_txtar(input: &str) -> Program {
        let archive = Archive::from(input);
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            map.insert(module_name, file.content.clone());
        }
        Program::from(map)
    }

    fn program_from_archive(archive: &Archive) -> Program {
        let mut map = HashMap::new();
        for file in archive.iter() {
            let module_name = file.name.replace(".hop", "");
            map.insert(module_name, file.content.clone());
        }
        Program::from(map)
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

        let mut locs = program_from_archive(&archive)
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

        let program = program_from_archive(&archive);

        let loc = program
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let source_code = program
            .source_code
            .get(&loc.module)
            .expect("Could not get source code");

        let output = SourceAnnotator::new().with_location().annotate(
            Some(&loc.module.clone()),
            source_code,
            [loc],
        );

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let program = program_from_txtar(input);

        let diagnostics = program.get_error_diagnostics(module);

        if diagnostics.is_empty() {
            panic!("Expected diagnostics to be non-empty");
        }

        let source_code = program
            .source_code
            .get(module)
            .expect("Source code not found");

        let output = SourceAnnotator::new().with_location().annotate(
            Some(module),
            source_code,
            &diagnostics,
        );

        expected.assert_eq(&output);
    }

    fn check_type_errors(program: &Program, expected: Expect) {
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
            let source_code = program.source_code.get(module_name).unwrap();

            let annotated = annotator.annotate(Some(module_name), source_code, errors);

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

        let symbol = program_from_archive(&archive)
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

        let hover_info = program_from_archive(&archive)
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
                <import component="hello-world" from="hop/components" />

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
                <import component="hello-world" from="hop/components" />

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
                Renameable symbol: hello-world
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
                Renameable symbol: div
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
                  --> main (line 3, col 3)
                3 |   <span>unclosed span
                  |   ^^^^^^

                Unclosed <div>
                  --> main (line 2, col 3)
                2 |   <div>
                  |   ^^^^^
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
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → a
                  --> a (line 1, col 34)
                1 | <import component="b-comp" from="b" />
                  |                                  ^

                Import cycle: b imports from a which creates a dependency cycle: a → b → a
                  --> b (line 1, col 34)
                1 | <import component="a-comp" from="a" />
                  |                                  ^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            "a",
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
            <import component="b-comp" from="b" />
            <a-comp>
              <b-comp />
            </a-comp>

            -- b.hop --
            <import component="c-comp" from="c" />
            <b-comp>
              <c-comp />
            </b-comp>

            -- c.hop --
            <import component="d-comp" from="d" />
            <c-comp>
              <d-comp />
            </c-comp>

            -- d.hop --
            <import component="a-comp" from="a" />
            <d-comp>
              <a-comp />
            </d-comp>
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Import cycle: a imports from b which creates a dependency cycle: a → b → c → d → a
                  --> a (line 1, col 34)
                1 | <import component="b-comp" from="b" />
                  |                                  ^

                Import cycle: b imports from c which creates a dependency cycle: a → b → c → d → a
                  --> b (line 1, col 34)
                1 | <import component="c-comp" from="c" />
                  |                                  ^

                Import cycle: c imports from d which creates a dependency cycle: a → b → c → d → a
                  --> c (line 1, col 34)
                1 | <import component="d-comp" from="d" />
                  |                                  ^

                Import cycle: d imports from a which creates a dependency cycle: a → b → c → d → a
                  --> d (line 1, col 34)
                1 | <import component="a-comp" from="a" />
                  |                                  ^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            "c",
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
            "b",
            indoc! {r#"
                <import component="a-comp" from="a" />
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
                1 | <import component="b-comp" from="b" />
                  |                                  ^

                Import cycle: b imports from a which creates a dependency cycle: a → b → a
                  --> b (line 1, col 34)
                1 | <import component="a-comp" from="a" />
                  |                                  ^
            "#]],
        );
        // Resolve cycle
        program.update_module(
            "b",
            indoc! {r#"
                <b-comp>
                </b-comp>
            "#}
            .to_string(),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
    }
}
