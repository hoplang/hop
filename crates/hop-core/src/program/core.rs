use super::format_error::FormatError;
use crate::asset_path_rewriter::AssetPathRewriter;
use crate::asset_reference::AssetReference;
use crate::config::TargetLanguage;
use crate::css;
use crate::css_error::CssError;
use crate::definition_link::DefinitionLink;
use crate::dependency_graph::DependencyGraph;
use crate::document::{CheapString, Document, DocumentPosition, PositionEncoding};
use crate::hop::format;
use crate::hop::parsing::{ParseError, ParsedModule, parse};
use crate::hop::typing::{Export, TypeError, TypeRegistry, TypedModule, typecheck};
use crate::hover_annotation::HoverAnnotation;
use crate::ir;
use crate::ir::{DocumentShell, TailwindInjection, Transpiler};
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use crate::root_contained_file_path::RootContainedFilePath;
use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;

#[derive(Debug, Default)]
pub struct Program {
    pub(super) dependency_graph: DependencyGraph<RootContainedFilePath>,
    pub(super) documents: HashMap<RootContainedFilePath, Document>,
    pub(super) css_documents: HashMap<RootContainedFilePath, Document>,
    pub(super) css_errors: HashMap<RootContainedFilePath, Vec<CssError>>,
    pub(super) parse_errors: HashMap<RootContainedFilePath, Vec<ParseError>>,
    pub(super) parsed_modules: HashMap<RootContainedFilePath, ParsedModule>,
    pub(super) exports: HashMap<RootContainedFilePath, HashMap<CheapString, Export>>,
    pub(super) type_registry: TypeRegistry,
    pub(super) type_errors: HashMap<RootContainedFilePath, Vec<TypeError>>,
    pub(super) hover_annotations: HashMap<RootContainedFilePath, Vec<HoverAnnotation>>,
    pub(super) definition_links: HashMap<RootContainedFilePath, Vec<DefinitionLink>>,
    pub(super) asset_references: HashMap<RootContainedFilePath, Vec<AssetReference>>,
    pub(super) typed_modules: HashMap<RootContainedFilePath, TypedModule>,
}

impl Program {
    pub fn new() -> Self {
        Self::default()
    }

    /// Update or add a hop document to the program.
    ///
    /// This parses the document, updates the dependency graph, and re-typechecks
    /// the module along with any modules that depend on it (directly or transitively).
    ///
    /// Returns the ids of all documents that were re-typechecked.
    pub fn update_hop_document(
        &mut self,
        document_id: &RootContainedFilePath,
        document: Document,
    ) -> Vec<RootContainedFilePath> {
        // Store the document
        self.documents.insert(document_id.clone(), document.clone());

        // Parse the document
        let parse_errors = self.parse_errors.entry(document_id.clone()).or_default();
        parse_errors.clear();
        let parsed_module = parse(document, parse_errors);

        // Get all modules that this module depends on
        let module_dependencies = parsed_module
            .import_declarations()
            .map(|import_node| import_node.module_name.to_file_path())
            .collect::<BTreeSet<RootContainedFilePath>>();

        // Store the parsed module
        self.parsed_modules
            .insert(document_id.clone(), parsed_module);

        // Typecheck the module along with all dependent modules (grouped
        // into strongly connected components).
        self.dependency_graph
            .set_dependencies(document_id.clone(), module_dependencies);
        let grouped_modules = self.dependency_graph.dependent_sccs(document_id);
        for names in &grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_modules.get_key_value(name))
                .collect::<Vec<_>>();
            typecheck(
                &modules,
                &mut self.exports,
                &mut self.type_registry,
                &mut self.typed_modules,
                &mut self.type_errors,
                &mut self.hover_annotations,
                &mut self.definition_links,
                &mut self.asset_references,
            );
        }

        // Return the ids of all documents that were re-typechecked
        grouped_modules.into_iter().flatten().collect()
    }

    /// Remove a hop document from the program.
    ///
    /// This cleans up all state associated with the document and re-typechecks
    /// any modules that depended on it (since their imports are now broken).
    pub fn remove_hop_document(&mut self, document_id: &RootContainedFilePath) {
        // Remove document and parsed state
        self.documents.remove(document_id);
        self.parse_errors.remove(document_id);
        self.parsed_modules.remove(document_id);
        self.exports.remove(document_id);
        self.type_registry.remove_module(document_id);
        self.type_errors.remove(document_id);
        self.hover_annotations.remove(document_id);
        self.asset_references.remove(document_id);
        self.typed_modules.remove(document_id);

        // Clear the module's dependencies but keep the node so that its
        // dependents are still found and re-typechecked.
        self.dependency_graph
            .set_dependencies(document_id.clone(), BTreeSet::new());
        let grouped_modules = self.dependency_graph.dependent_sccs(document_id);

        // Re-typecheck dependent modules (they now have broken imports)
        for names in grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_modules.get_key_value(name))
                .collect::<Vec<_>>();
            if !modules.is_empty() {
                typecheck(
                    &modules,
                    &mut self.exports,
                    &mut self.type_registry,
                    &mut self.typed_modules,
                    &mut self.type_errors,
                    &mut self.hover_annotations,
                    &mut self.definition_links,
                    &mut self.asset_references,
                );
            }
        }
    }

    /// Remove a CSS document from the program.
    pub fn remove_css_document(&mut self, document_id: &RootContainedFilePath) {
        self.css_documents.remove(document_id);
        self.css_errors.remove(document_id);
        self.asset_references.remove(document_id);
    }

    /// Update or add a CSS document to the program.
    pub fn update_css_document(&mut self, document_id: &RootContainedFilePath, document: Document) {
        let css_errors = self.css_errors.entry(document_id.clone()).or_default();
        css_errors.clear();
        let asset_references = self
            .asset_references
            .entry(document_id.clone())
            .or_default();
        asset_references.clear();
        css::scan_for_asset_references(&document, asset_references, css_errors);
        self.css_documents.insert(document_id.clone(), document);
    }

    pub fn asset_references(&self) -> &HashMap<RootContainedFilePath, Vec<AssetReference>> {
        &self.asset_references
    }

    /// Returns the CSS document with asset paths rewritten, or `None` if
    /// there is no CSS document with the given id.
    pub fn compile_css_document(
        &self,
        document_id: &RootContainedFilePath,
        asset_path_rewriter: Arc<dyn AssetPathRewriter>,
    ) -> Option<String> {
        let css = self.css_documents.get(document_id)?;
        Some(css::rewrite_asset_paths(css, asset_path_rewriter))
    }

    /// Returns the formatted source code for a hop document.
    ///
    /// Returns an error if the document doesn't exist or has parse errors.
    pub fn format_hop_document(
        &self,
        document_id: &RootContainedFilePath,
    ) -> Result<String, FormatError> {
        let module = self
            .parsed_modules
            .get(document_id)
            .ok_or_else(|| FormatError::DocumentNotFound(document_id.clone()))?;

        if self
            .parse_errors
            .get(document_id)
            .is_some_and(|errors| !errors.is_empty())
        {
            return Err(FormatError::HasParseErrors(document_id.clone()));
        }

        Ok(format(module))
    }

    /// Returns the text of every hop document concatenated into a single string.
    pub fn sources(&self) -> String {
        self.documents
            .values()
            .map(|doc| doc.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Resolve an editor's line and column in a document to a position.
    /// None if the document is unknown or the position is outside its text.
    pub fn position(
        &self,
        document_id: &RootContainedFilePath,
        encoding: PositionEncoding,
        line: usize,
        column: usize,
    ) -> Option<DocumentPosition> {
        self.documents
            .get(document_id)?
            .position(encoding, line, column)
    }

    /// Compile all typed modules to source code for the given target language.
    pub fn transpile(
        &self,
        target: TargetLanguage,
        css_link_href: &str,
        js_script_src: Option<&str>,
        skip_optimization: bool,
        asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    ) -> String {
        let shell = DocumentShell::new(
            Some(TailwindInjection::Link {
                href: css_link_href,
            }),
            js_script_src,
        );
        let ir_module = orchestrate(
            self.typed_modules(),
            OrchestrateOptions {
                skip_optimization,
                asset_path_rewriter,
                ..Default::default()
            },
            &shell,
        );

        match target {
            TargetLanguage::Typescript => {
                ir::TsTranspiler::new().transpile_module(&ir_module, &self.type_registry)
            }
            TargetLanguage::Rust => {
                ir::RustTranspiler::new().transpile_module(&ir_module, &self.type_registry)
            }
        }
    }

    /// Get all typed modules for compilation
    pub(crate) fn typed_modules(&self) -> &HashMap<RootContainedFilePath, TypedModule> {
        &self.typed_modules
    }

    #[cfg(test)]
    pub(crate) fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    /// Return the names of every page declared across all modules, sorted.
    pub fn page_names(&self) -> Vec<String> {
        let mut names: Vec<String> = self
            .typed_modules
            .values()
            .flat_map(|module| module.page_declarations())
            .map(|page| page.name.to_string())
            .collect();
        names.sort();
        names
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_support::program_from_archive;
    use super::*;
    use crate::document_annotator::DocumentAnnotator;
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::Archive;

    fn check_diagnostics(program: &Program, expected: Expect) {
        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(program.diagnostics())
            .render();

        expected.assert_eq(&output);
    }

    #[test]
    fn should_report_import_cycle_errors() {
        let mut program = program_from_archive(&Archive::from(indoc! {r#"
            -- a.hop --
            import b::BComp
            pub fn AComp() -> Html {
              <BComp />
            }

            -- b.hop --
            import a::AComp
            pub fn BComp() -> Html {
              <AComp />
            }

            -- c.hop --
            import a::AComp
            fn CComp() -> Html {
              <AComp />
            }
        "#}));
        check_diagnostics(
            &program,
            expect![[r#"
                Import cycle: a.hop imports from b which creates a dependency cycle: a.hop → b.hop → a.hop
                  --> a.hop (line 1, col 8)
                1 | import b::BComp
                  |        ^^^^^^^^

                Import cycle: b.hop imports from a which creates a dependency cycle: a.hop → b.hop → a.hop
                  --> b.hop (line 1, col 8)
                1 | import a::AComp
                  |        ^^^^^^^^
            "#]],
        );
        // Resolve cycle
        program.update_hop_document(
            &RootContainedFilePath::new("a.hop").unwrap(),
            Document::new(
                RootContainedFilePath::new("a.hop").unwrap(),
                indoc! {r#"
                    pub fn AComp() -> Html {
                      <></>
                    }
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_diagnostics(&program, expect![""]);
    }

    #[test]
    fn should_report_import_cycle_errors_for_large_cycles() {
        let mut program = program_from_archive(&Archive::from(indoc! {r#"
            -- a.hop --
            import b::BComp
            pub fn AComp() -> Html {
              <BComp />
            }

            -- b.hop --
            import c::CComp
            pub fn BComp() -> Html {
              <CComp />
            }

            -- c.hop --
            import d::DComp
            pub fn CComp() -> Html {
              <DComp />
            }

            -- d.hop --
            import a::AComp
            pub fn DComp() -> Html {
              <AComp />
            }
        "#}));
        check_diagnostics(
            &program,
            expect![[r#"
                Import cycle: a.hop imports from b which creates a dependency cycle: a.hop → b.hop → c.hop → d.hop → a.hop
                  --> a.hop (line 1, col 8)
                1 | import b::BComp
                  |        ^^^^^^^^

                Import cycle: b.hop imports from c which creates a dependency cycle: a.hop → b.hop → c.hop → d.hop → a.hop
                  --> b.hop (line 1, col 8)
                1 | import c::CComp
                  |        ^^^^^^^^

                Import cycle: c.hop imports from d which creates a dependency cycle: a.hop → b.hop → c.hop → d.hop → a.hop
                  --> c.hop (line 1, col 8)
                1 | import d::DComp
                  |        ^^^^^^^^

                Import cycle: d.hop imports from a which creates a dependency cycle: a.hop → b.hop → c.hop → d.hop → a.hop
                  --> d.hop (line 1, col 8)
                1 | import a::AComp
                  |        ^^^^^^^^
            "#]],
        );
        // Resolve cycle
        program.update_hop_document(
            &RootContainedFilePath::new("c.hop").unwrap(),
            Document::new(
                RootContainedFilePath::new("c.hop").unwrap(),
                indoc! {r#"
                    pub fn CComp() -> Html {
                      <></>
                    }
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_diagnostics(&program, expect![""]);
        // Introduce new cycle a → b → a
        program.update_hop_document(
            &RootContainedFilePath::new("b.hop").unwrap(),
            Document::new(
                RootContainedFilePath::new("b.hop").unwrap(),
                indoc! {r#"
                    import a::AComp
                    pub fn BComp() -> Html {
                      <AComp />
                    }
                "#}
                .to_string(),
            ),
        );
        check_diagnostics(
            &program,
            expect![[r#"
                Import cycle: a.hop imports from b which creates a dependency cycle: a.hop → b.hop → a.hop
                  --> a.hop (line 1, col 8)
                1 | import b::BComp
                  |        ^^^^^^^^

                Import cycle: b.hop imports from a which creates a dependency cycle: a.hop → b.hop → a.hop
                  --> b.hop (line 1, col 8)
                1 | import a::AComp
                  |        ^^^^^^^^
            "#]],
        );
        // Resolve cycle
        program.update_hop_document(
            &RootContainedFilePath::new("b.hop").unwrap(),
            Document::new(
                RootContainedFilePath::new("b.hop").unwrap(),
                indoc! {r#"
                    pub fn BComp() -> Html {
                      <></>
                    }
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_diagnostics(&program, expect![""]);
    }

    #[test]
    fn should_report_type_error_when_imported_module_is_removed() {
        let mut program = program_from_archive(&Archive::from(indoc! {r#"
            -- components.hop --
            pub fn HelloWorld() -> Html {
              <h1>Hello World</h1>
            }

            -- main.hop --
            import components::HelloWorld

            fn Main() -> Html {
              <HelloWorld />
            }
        "#}));

        // No type errors initially
        check_diagnostics(&program, expect![""]);

        // Remove the components module
        program.remove_hop_document(&RootContainedFilePath::new("components.hop").unwrap());

        // Now main should have a type error about the missing import
        check_diagnostics(
            &program,
            expect![[r#"
                Module components was not found
                  --> main.hop (line 1, col 8)
                1 | import components::HelloWorld
                  |        ^^^^^^^^^^^^^^^^^^^^^^

                Function HelloWorld is not defined
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );

        // Add the module back
        program.update_hop_document(
            &RootContainedFilePath::new("components.hop").unwrap(),
            Document::new(
                RootContainedFilePath::new("components.hop").unwrap(),
                indoc! {r#"
                    fn HelloWorld() -> Html {
                      <h1>Hello World</h1>
                    }
                "#}
                .to_string(),
            ),
        );

        // Type errors should now be resolved
        check_diagnostics(
            &program,
            expect![[r#"
                HelloWorld from module components is not public
                  --> main.hop (line 1, col 20)
                1 | import components::HelloWorld
                  |                    ^^^^^^^^^^

                Function HelloWorld is not defined
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );
    }
}
