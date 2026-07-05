use crate::annotation::Annotation;
use crate::asset_reference::AssetReference;
use crate::asset_rewriter::AssetRewriter;
use crate::config::{ResolvedConfig, TargetLanguage};
use crate::css;
use crate::css_error::CssError;
use crate::dependency_graph::DependencyGraph;
use crate::document::{Document, DocumentRange};
use crate::document_id::DocumentId;
use crate::document_position::DocumentPosition;
use crate::expr::fake::random_value;
use crate::expr::typing::type_export::TypeExport;
use crate::expr::typing::type_registry::TypeRegistry;
use crate::hop::inlining::transform::TailwindInjection;
use crate::hop::parsing::find_node::find_node_at_position;
use crate::hop::parsing::format;
use crate::hop::parsing::parsed_ast::ParsedAst;
use crate::hop::parsing::parsed_node::ParsedNode;
use crate::hop::parsing::parser::parse;
use crate::hop::typing::definition_link::DefinitionLink;
use crate::hop::typing::type_annotation::TypeAnnotation;
use crate::hop::typing::type_checker::typecheck;
use crate::hop::typing::typed_ast::TypedAst;
use crate::ir;
use crate::ir::Transpiler;
use crate::orchestrator::{OrchestrateOptions, orchestrate};
use crate::parse_error::ParseError;
use crate::symbols::type_name::TypeName;
use crate::type_error::TypeError;
use anyhow::Result;
use rand::Rng;
use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

/// A diagnostic is an error, warning or information that should be displayed
/// for a specific range in the document.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub range: DocumentRange,
    pub severity: Severity,
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
    dependency_graph: DependencyGraph<DocumentId>,
    documents: HashMap<DocumentId, Document>,
    css_documents: HashMap<DocumentId, Document>,
    css_errors: HashMap<DocumentId, Vec<CssError>>,
    parse_errors: HashMap<DocumentId, Vec<ParseError>>,
    parsed_asts: HashMap<DocumentId, ParsedAst>,
    type_exports: HashMap<DocumentId, HashMap<TypeName, TypeExport>>,
    type_registry: TypeRegistry,
    type_errors: HashMap<DocumentId, Vec<TypeError>>,
    type_annotations: HashMap<DocumentId, Vec<TypeAnnotation>>,
    definition_links: HashMap<DocumentId, Vec<DefinitionLink>>,
    asset_references: HashMap<DocumentId, Vec<AssetReference>>,
    typed_asts: HashMap<DocumentId, TypedAst>,
}

impl Program {
    /// Update or add a hop module to the program.
    ///
    /// This parses the document, updates the dependency graph, and re-typechecks
    /// the module along with any modules that depend on it (directly or transitively).
    ///
    /// Returns the list of all module IDs that were re-typechecked.
    pub fn update_module(
        &mut self,
        document_id: &DocumentId,
        document: Document,
    ) -> Vec<DocumentId> {
        // Store the document
        self.documents.insert(document_id.clone(), document.clone());

        // Parse the module
        let parse_errors = self.parse_errors.entry(document_id.clone()).or_default();
        parse_errors.clear();
        let parsed_ast = parse(document_id.clone(), document, parse_errors);

        // Get all modules that this module depends on
        let module_dependencies = parsed_ast
            .get_import_declarations()
            .map(|import_node| import_node.imported_module().to_document_id())
            .collect::<BTreeSet<DocumentId>>();

        // Store the AST
        self.parsed_asts.insert(document_id.clone(), parsed_ast);

        // Typecheck the module along with all dependent modules (grouped
        // into strongly connected components).
        self.dependency_graph
            .set_dependencies(document_id.clone(), module_dependencies);
        let grouped_modules = self.dependency_graph.dependent_sccs(document_id);
        for names in &grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_asts.get(name))
                .collect::<Vec<_>>();
            typecheck(
                &modules,
                &mut self.type_exports,
                &mut self.type_registry,
                &mut self.typed_asts,
                &mut self.type_errors,
                &mut self.type_annotations,
                &mut self.definition_links,
                &mut self.asset_references,
            );
        }

        // Return all modules that have been re-typechecked
        grouped_modules.into_iter().flatten().collect()
    }

    /// Remove a hop module from the program.
    ///
    /// This cleans up all state associated with the module and re-typechecks
    /// any modules that depended on it (since their imports are now broken).
    pub fn remove_module(&mut self, document_id: &DocumentId) {
        // Remove document and parsed state
        self.documents.remove(document_id);
        self.parse_errors.remove(document_id);
        self.parsed_asts.remove(document_id);
        self.type_exports.remove(document_id);
        self.type_registry.remove_module(document_id);
        self.type_errors.remove(document_id);
        self.type_annotations.remove(document_id);
        self.asset_references.remove(document_id);
        self.typed_asts.remove(document_id);

        // Clear the module's dependencies but keep the node so that its
        // dependents are still found and re-typechecked.
        self.dependency_graph
            .set_dependencies(document_id.clone(), BTreeSet::new());
        let grouped_modules = self.dependency_graph.dependent_sccs(document_id);

        // Re-typecheck dependent modules (they now have broken imports)
        for names in grouped_modules {
            let modules = names
                .iter()
                .filter_map(|name| self.parsed_asts.get(name))
                .collect::<Vec<_>>();
            if !modules.is_empty() {
                typecheck(
                    &modules,
                    &mut self.type_exports,
                    &mut self.type_registry,
                    &mut self.typed_asts,
                    &mut self.type_errors,
                    &mut self.type_annotations,
                    &mut self.definition_links,
                    &mut self.asset_references,
                );
            }
        }
    }

    /// Update or add a CSS document to the program.
    pub fn remove_css_document(&mut self, document_id: &DocumentId) {
        self.css_documents.remove(document_id);
        self.css_errors.remove(document_id);
        self.asset_references.remove(document_id);
    }

    /// Update or add a CSS document to the program.
    pub fn update_css_document(&mut self, document_id: &DocumentId, document: Document) {
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

    pub fn get_parse_errors(&self) -> &HashMap<DocumentId, Vec<ParseError>> {
        &self.parse_errors
    }

    pub fn get_type_errors(&self) -> &HashMap<DocumentId, Vec<TypeError>> {
        &self.type_errors
    }

    pub fn get_css_errors(&self) -> &HashMap<DocumentId, Vec<CssError>> {
        &self.css_errors
    }

    pub fn get_asset_references(&self) -> &HashMap<DocumentId, Vec<AssetReference>> {
        &self.asset_references
    }

    pub fn get_compiled_css_document(
        &self,
        document_id: &DocumentId,
        asset_rewriter: Arc<dyn AssetRewriter>,
    ) -> Result<String> {
        let css = self
            .css_documents
            .get(document_id)
            .ok_or_else(|| anyhow::anyhow!("CSS document '{}' not found", document_id))?;
        Ok(css::rewrite_asset_paths(css, asset_rewriter))
    }

    /// Returns the formatted source code for a module.
    ///
    /// Returns an error if the module doesn't exist or has parse errors.
    pub fn get_formatted_module(&self, document_id: &DocumentId) -> Result<String> {
        // Check if module exists
        let ast = self
            .parsed_asts
            .get(document_id)
            .ok_or_else(|| anyhow::anyhow!("Module '{}' not found", document_id))?;

        // Check for parse errors
        if let Some(errors) = self.parse_errors.get(document_id) {
            if !errors.is_empty() {
                return Err(anyhow::anyhow!(
                    "Cannot format module '{}': has parse errors",
                    document_id
                ));
            }
        }

        Ok(format(ast))
    }

    /// Returns all hop module sources concatenated into a single string.
    pub fn get_all_hop_sources(&self) -> String {
        self.documents
            .values()
            .map(|doc| doc.as_str())
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn get_hover_info(
        &self,
        document_id: &DocumentId,
        position: DocumentPosition,
    ) -> Option<HoverInfo> {
        self.type_annotations
            .get(document_id)?
            .iter()
            .find(|a| a.range().contains_position(position))
            .map(|annotation| {
                let message = annotation.to_string();
                HoverInfo {
                    message,
                    range: annotation.range().clone(),
                }
            })
    }

    pub fn get_definition_location(
        &self,
        document_id: &DocumentId,
        position: DocumentPosition,
    ) -> Option<DefinitionLocation> {
        if let Some(links) = self.definition_links.get(document_id) {
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
        document_id: &DocumentId,
        position: DocumentPosition,
    ) -> Option<Vec<RenameLocation>> {
        let ast = self.parsed_asts.get(document_id)?;

        // Check if cursor is on a record declaration name
        for record in ast.get_record_declarations() {
            if record.name_range.contains_position(position) {
                return Some(self.collect_record_rename_locations(&record.name, document_id));
            }
        }

        // Check if cursor is on an enum declaration name
        for enum_decl in ast.get_enum_declarations() {
            if enum_decl.name_range.contains_position(position) {
                return Some(self.collect_enum_rename_locations(&enum_decl.name, document_id));
            }
        }

        for node in ast.get_component_declarations() {
            if node
                .tag_name_ranges()
                .any(|r| r.contains_position(position))
            {
                return Some(
                    self.collect_component_rename_locations(&node.component_name, document_id),
                );
            }
        }

        let node = find_node_at_position(ast, position)?;

        let is_on_tag_name = node.tag_names().any(|r| r.contains_position(position));

        if !is_on_tag_name {
            return None;
        }

        match node {
            ParsedNode::ComponentInvocation {
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
        document_id: &DocumentId,
        position: DocumentPosition,
    ) -> Option<RenameableSymbol> {
        let ast = self.parsed_asts.get(document_id)?;

        // Check if cursor is on a record declaration name
        for record in ast.get_record_declarations() {
            if record.name_range.contains_position(position) {
                return Some(RenameableSymbol {
                    range: record.name_range.clone(),
                });
            }
        }

        // Check if cursor is on an enum declaration name
        for enum_decl in ast.get_enum_declarations() {
            if enum_decl.name_range.contains_position(position) {
                return Some(RenameableSymbol {
                    range: enum_decl.name_range.clone(),
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
    /// - All invocations of the component (opening and closing tags)
    /// - All import statements that import the component
    fn collect_component_rename_locations(
        &self,
        component_name: &TypeName,
        definition_module: &DocumentId,
    ) -> Vec<RenameLocation> {
        // Find the definition range (the opening tag_name of the component declaration)
        let definition_range = self
            .parsed_asts
            .get(definition_module)
            .and_then(|module| module.get_component_declaration(component_name.as_str()))
            .map(|decl| &decl.tag_name);

        let Some(definition_range) = definition_range else {
            return Vec::new();
        };

        // Collect all use_ranges across all modules whose definition_range
        // matches the component's definition
        self.definition_links
            .values()
            .flatten()
            .filter(|link| link.definition_range == *definition_range)
            .map(|link| RenameLocation {
                range: link.use_range.clone(),
            })
            .collect()
    }

    /// Collects all locations where a record type should be renamed, including:
    /// - The record declaration
    /// - All type annotations that reference the record
    /// - All import statements that import the record
    fn collect_record_rename_locations(
        &self,
        record_name: &TypeName,
        definition_module: &DocumentId,
    ) -> Vec<RenameLocation> {
        // Find the definition range (the name_range of the record declaration)
        let definition_range = self
            .parsed_asts
            .get(definition_module)
            .and_then(|module| module.get_record_declaration(record_name.as_str()))
            .map(|decl| &decl.name_range);

        let Some(definition_range) = definition_range else {
            return Vec::new();
        };

        // Collect all use_ranges across all modules whose definition_range
        // matches the record's definition
        self.definition_links
            .values()
            .flatten()
            .filter(|link| link.definition_range == *definition_range)
            .map(|link| RenameLocation {
                range: link.use_range.clone(),
            })
            .collect()
    }

    /// Collects all locations where an enum type should be renamed, including:
    /// - The enum declaration
    /// - All type annotations that reference the enum
    /// - All import statements that import the enum
    fn collect_enum_rename_locations(
        &self,
        enum_name: &TypeName,
        definition_module: &DocumentId,
    ) -> Vec<RenameLocation> {
        // Find the definition range (the name_range of the enum declaration)
        let definition_range = self
            .parsed_asts
            .get(definition_module)
            .and_then(|module| module.get_enum_declaration(enum_name.as_str()))
            .map(|decl| &decl.name_range);

        let Some(definition_range) = definition_range else {
            return Vec::new();
        };

        // Collect all use_ranges across all modules whose definition_range
        // matches the enum's definition
        self.definition_links
            .values()
            .flatten()
            .filter(|link| link.definition_range == *definition_range)
            .map(|link| RenameLocation {
                range: link.use_range.clone(),
            })
            .collect()
    }

    pub fn get_error_diagnostics(&self, document_id: DocumentId) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        let mut found_parse_errors = false;

        if let Some(errors) = self.parse_errors.get(&document_id) {
            for error in errors {
                diagnostics.push(Diagnostic {
                    message: error.message(),
                    range: error.range().clone(),
                    severity: Severity::Error,
                });
                found_parse_errors = true;
            }
        }

        // If there's parse errors for the file we do not emit the type errors since they may be
        // non-sensical if parsing fails.
        if !found_parse_errors {
            if let Some(errors) = self.type_errors.get(&document_id) {
                for error in errors {
                    diagnostics.push(Diagnostic {
                        message: error.message(),
                        range: error.range().clone(),
                        severity: error.severity(),
                    });
                }
            }
        }

        diagnostics
    }

    /// Evaluate an view given module and view name.
    fn evaluate_view_with_values(
        &self,
        document_id: &DocumentId,
        view_name: &TypeName,
        args: HashMap<String, ir::semantics::evaluator::Value>,
        generated_tailwind_css: Option<&str>,
        skip_optimization: bool,
        disable_links: bool,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    ) -> Result<String> {
        // Refuse to evaluate if there are errors in any module
        if self.parse_errors.values().any(|errors| !errors.is_empty()) {
            anyhow::bail!("Cannot evaluate view: program has parse errors");
        }
        if self.type_errors.values().any(|errors| !errors.is_empty()) {
            anyhow::bail!("Cannot evaluate view: program has type errors");
        }
        // Validate that the module exists
        let module = self.get_typed_modules().get(document_id).ok_or_else(|| {
            anyhow::anyhow!(
                "Module '{}' not found. Available modules: {}",
                document_id,
                self.get_typed_modules()
                    .keys()
                    .map(|m| m.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })?;

        // Check if the view exists in this module
        let view_exists = module
            .get_view_declarations()
            .iter()
            .any(|ep| ep.name.as_str() == view_name.as_str());

        if !view_exists {
            let available_views: Vec<_> = module
                .get_view_declarations()
                .iter()
                .map(|ep| ep.name.as_str())
                .collect();

            anyhow::bail!(
                "View '{}' not found in module '{}'. Available views: {}",
                view_name,
                document_id,
                available_views.join(", ")
            );
        }

        // Use orchestrate to handle inlining and compilation
        // Pass the view filter to only compile the requested view
        let ir_module = orchestrate(
            self.get_typed_modules(),
            &self.type_registry,
            OrchestrateOptions {
                skip_optimization,
                disable_links,
                view_filter: Some((document_id.clone(), view_name.clone())),
                asset_rewriter,
                tailwind_injection: generated_tailwind_css.map(TailwindInjection::Inline),
                ..Default::default()
            },
        );

        // The filtered module should contain exactly the requested view
        let view = ir_module.views.first().ok_or_else(|| {
            anyhow::anyhow!(
                "View '{}/{}' not found after compilation",
                document_id,
                view_name
            )
        })?;

        // Evaluate the view
        ir::semantics::evaluator::evaluate_view(view, args, &ir_module.components)
    }

    /// Evaluate a view with randomly generated parameter values using the given RNG.
    pub fn evaluate_view_with_random_values(
        &self,
        view: &str,
        rng: &mut impl Rng,
        generated_tailwind_css: Option<&str>,
        skip_optimization: bool,
        disable_links: bool,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    ) -> Result<String> {
        let document_id = self
            .find_module_for_view(view)
            .map_err(anyhow::Error::msg)?;
        let view_name =
            TypeName::new(view).map_err(|e| anyhow::anyhow!("Invalid view name: {}", e))?;

        let typed_ast = self.get_typed_modules().get(&document_id).ok_or_else(|| {
            anyhow::anyhow!("Module '{}' not found in typed modules", document_id)
        })?;

        let view_decl = typed_ast
            .get_view_declarations()
            .iter()
            .find(|ep| ep.name.as_str() == view)
            .ok_or_else(|| {
                anyhow::anyhow!("View '{}' not found in module '{}'", view, document_id)
            })?;

        let params = view_decl
            .params
            .iter()
            .map(|param| {
                (
                    param.var_name.as_str().to_string(),
                    random_value(
                        rng,
                        &param.var_type,
                        param.examples.as_ref(),
                        &self.type_registry,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();

        self.evaluate_view_with_values(
            &document_id,
            &view_name,
            params,
            generated_tailwind_css,
            skip_optimization,
            disable_links,
            asset_rewriter,
        )
    }

    /// Compile all typed modules to source code for the given target language.
    pub fn transpile(
        &self,
        resolved_config: &ResolvedConfig,
        css_link_href: &str,
        js_script_src: Option<&str>,
        skip_optimization: bool,
        asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    ) -> String {
        let ir_module = orchestrate(
            self.get_typed_modules(),
            &self.type_registry,
            OrchestrateOptions {
                skip_optimization,
                asset_rewriter,
                tailwind_injection: Some(TailwindInjection::Link {
                    href: css_link_href,
                }),
                script_src: js_script_src,
                ..Default::default()
            },
        );

        match resolved_config.target {
            TargetLanguage::Typescript => {
                ir::TsTranspiler::new().transpile_module(&ir_module, &self.type_registry)
            }
            TargetLanguage::Rust => {
                ir::RustTranspiler::new().transpile_module(&ir_module, &self.type_registry)
            }
        }
    }

    /// Get all typed modules for compilation
    pub(crate) fn get_typed_modules(&self) -> &HashMap<DocumentId, TypedAst> {
        &self.typed_asts
    }

    #[cfg(test)]
    pub(crate) fn type_registry(&self) -> &TypeRegistry {
        &self.type_registry
    }

    /// Find which module contains a given view.
    pub fn find_module_for_view(&self, view: &str) -> Result<DocumentId, String> {
        let mut all_views = Vec::new();

        for (document_id, ast) in &self.typed_asts {
            for ep in ast.get_view_declarations() {
                if ep.name.as_str() == view {
                    return Ok(document_id.clone());
                }
                all_views.push(ep.name.to_string());
            }
        }

        all_views.sort();
        Err(format!(
            "View '{}' not found. Available views: {}",
            view,
            all_views.join(", ")
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        document_annotator::DocumentAnnotator, extract_position::extract_position,
        simple_annotation::SimpleAnnotation,
    };
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use txtar::{Archive, Builder, File};

    #[derive(Debug, Clone, PartialEq)]
    pub struct MarkerInfo {
        pub filename: String,
        pub position: DocumentPosition,
    }

    /// Extracts all position markers from an archive and returns the cleaned archive
    /// along with information about each marker found.
    ///
    /// # Returns
    /// - The cleaned archive (with all markers removed)
    /// - A vector of MarkerInfo containing filenames and positions for each marker
    pub fn extract_markers_from_archive(archive: &Archive) -> (Archive, Vec<MarkerInfo>) {
        let mut markers = Vec::new();
        let mut builder = Builder::new();

        for file in archive.iter() {
            if let Some((clean_content, pos)) = extract_position(&file.content) {
                markers.push(MarkerInfo {
                    filename: file.name.clone(),
                    position: pos,
                });
                builder.file(File::new(file.name.clone(), clean_content));
            } else {
                builder.file(file.clone());
            }
        }

        (builder.build(), markers)
    }

    fn program_from_txtar(input: &str) -> Program {
        let archive = Archive::from(input);
        let mut program = Program::default();
        for file in archive.iter() {
            let document_id = DocumentId::new(&file.name).unwrap();
            let document = Document::new(document_id.clone(), file.content.clone());
            program.update_module(&document_id, document);
        }
        program
    }

    fn program_from_archive(archive: &Archive) -> Program {
        let mut program = Program::default();
        for file in archive.iter() {
            let document_id = DocumentId::new(&file.name).unwrap();
            let document = Document::new(document_id.clone(), file.content.clone());
            program.update_module(&document_id, document);
        }
        program
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
        let module = DocumentId::new(&marker.filename).unwrap();

        let locs = program_from_archive(&archive)
            .get_rename_locations(&module, marker.position)
            .expect("Expected locations to be defined");

        let mut annotator = DocumentAnnotator::new().with_location();

        for file in archive.iter() {
            let document_id = DocumentId::new(&file.name).unwrap();

            let annotations = locs
                .iter()
                .filter(|l| l.range.document_id() == &document_id)
                .map(|l| SimpleAnnotation {
                    message: "Rename".to_string(),
                    range: l.range.clone(),
                })
                .collect::<Vec<_>>();

            if !annotations.is_empty() {
                annotator.annotate(&document_id, &annotations);
            }
        }

        expected.assert_eq(&annotator.render());
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
        let module = DocumentId::new(&marker.filename).unwrap();

        let program = program_from_archive(&archive);

        for (document_id, errors) in program.get_parse_errors() {
            if !errors.is_empty() {
                panic!("Parse errors in module {}: {:?}", document_id, errors);
            }
        }

        for (document_id, errors) in program.get_type_errors() {
            if !errors.is_empty() {
                panic!("Type errors in module {}: {:?}", document_id, errors);
            }
        }

        let loc = program
            .get_definition_location(&module, marker.position)
            .expect("Expected definition location to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(
                loc.range.clone().document_id(),
                [SimpleAnnotation {
                    message: "Definition".to_string(),
                    range: loc.range,
                }],
            )
            .render();

        expected.assert_eq(&output);
    }

    fn check_error_diagnostics(input: &str, module: &str, expected: Expect) {
        let program = program_from_txtar(input);

        let diagnostics = program.get_error_diagnostics(DocumentId::new(module).unwrap());

        if diagnostics.is_empty() {
            panic!("Expected diagnostics to be non-empty");
        }

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(
                &DocumentId::new(module).unwrap(),
                diagnostics.into_iter().map(|d| SimpleAnnotation {
                    message: d.message,
                    range: d.range,
                }),
            )
            .render();

        expected.assert_eq(&output);
    }

    fn check_type_errors(program: &Program, expected: Expect) {
        let mut annotator = DocumentAnnotator::new().with_location();

        // Get all modules that have type errors
        let type_errors = program.get_type_errors();
        let mut modules_with_errors: Vec<_> = type_errors
            .iter()
            .filter(|(_, errors)| !errors.is_empty())
            .collect();

        // Sort by module name for consistent output
        modules_with_errors.sort_by_key(|(document_id, _)| document_id.to_string());

        for (document_id, errors) in modules_with_errors {
            annotator.annotate(document_id, errors);
        }

        expected.assert_eq(&annotator.render());
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
        let module = DocumentId::new(&marker.filename).unwrap();

        let symbol = program_from_archive(&archive)
            .get_renameable_symbol(&module, marker.position)
            .expect("Expected symbol to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(
                &module,
                &[SimpleAnnotation {
                    message: symbol.range.as_str().to_string(),
                    range: symbol.range,
                }],
            )
            .render();

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
        let module = DocumentId::new(&marker.filename).unwrap();

        let program = program_from_archive(&archive);

        for (document_id, errors) in program.get_parse_errors() {
            if !errors.is_empty() {
                panic!("Parse errors in module {}: {:?}", document_id, errors);
            }
        }

        for (document_id, errors) in program.get_type_errors() {
            if !errors.is_empty() {
                panic!("Type errors in module {}: {:?}", document_id, errors);
            }
        }

        let hover_info = program
            .get_hover_info(&module, marker.position)
            .expect("Expected hover info to be defined");

        let output = DocumentAnnotator::new()
            .with_location()
            .annotate(
                &module,
                &[SimpleAnnotation {
                    range: hover_info.range,
                    message: hover_info.message,
                }],
            )
            .render();

        expected.assert_eq(&output);
    }

    ///////////////////////////////////////////////////////////////////////////
    // DEFINITION LOCATION                                                   //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_find_definition_from_component_invocation_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                pub component HelloWorld {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import hop::components::HelloWorld

                component Main {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 15)
                1 | pub component HelloWorld {
                  |               ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_invocation_closing_tag() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                pub component HelloWorld(children: Fragment) {
                  <h1>Hello World {children}</h1>
                }

                -- main.hop --
                import hop::components::HelloWorld

                component Main {
                  <HelloWorld>
                  </HelloWorld>
                     ^
                }
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 15)
                1 | pub component HelloWorld(children: Fragment) {
                  |               ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_component_name() {
        check_definition_location(
            indoc! {r#"
                -- hop/components.hop --
                pub component HelloWorld {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import hop::components::HelloWorld
                                        ^

                component Main {
                  <HelloWorld />
                }
            "#},
            expect![[r#"
                Definition
                  --> hop/components.hop (line 1, col 15)
                1 | pub component HelloWorld {
                  |               ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_record_name() {
        check_definition_location(
            indoc! {r#"
                -- types.hop --
                pub record User {name: String, age: Int}
                -- main.hop --
                import types::User
                              ^

                component Main(user: User) {
                  <div>{user.name}</div>
                }
            "#},
            expect![[r#"
                Definition
                  --> types.hop (line 1, col 12)
                1 | pub record User {name: String, age: Int}
                  |            ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_import_enum_name() {
        check_definition_location(
            indoc! {r#"
                -- types.hop --
                pub enum Status { Active, Inactive }
                -- main.hop --
                import types::Status
                              ^

                component Main(status: Status) {
                  <match {status}>
                    <case {Status::Active}><span>Active</span></case>
                    <case {Status::Inactive}><span>Inactive</span></case>
                  </match>
                }
            "#},
            expect![[r#"
                Definition
                  --> types.hop (line 1, col 10)
                1 | pub enum Status { Active, Inactive }
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_definition_opening_tag() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                component HelloWorld {
                            ^
                  <h1>Hello World</h1>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 11)
                1 | component HelloWorld {
                  |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_invocation_in_same_module_simple() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                component HelloWorld {
                  <h1>Hello World</h1>
                }

                component Main {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 11)
                1 | component HelloWorld {
                  |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_invocation_inside_match() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                component HelloWorld {
                  <h1>Hello World</h1>
                }

                component Main(x: Option[String]) {
                  <match {x}>
                    <case {Some(_)}>
                      <HelloWorld />
                       ^
                    </case>
                    <case {None}></case>
                  </match>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 11)
                 1 | component HelloWorld {
                   |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_component_invocation_inside_view() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                component HelloWorld {
                  <h1>Hello World</h1>
                }

                view Main() {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 11)
                1 | component HelloWorld {
                  |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_variable_reference() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                component Main(name: String) {
                  <span>{name}</span>
                         ^
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 16)
                1 | component Main(name: String) {
                  |                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_definition_from_for_loop_variable() {
        check_definition_location(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[String]) {
                  <ul>
                    <for {item in items}>
                      <li>{item}</li>
                            ^
                    </for>
                  </ul>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 3, col 11)
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
                component Main {
                  <let {greeting: String = "Hello"}>
                    <span>{greeting}</span>
                            ^
                  </let>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 2, col 9)
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

                component Main(user: User) {
                                     ^
                  <span>{user.name}</span>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 8)
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

                component Main(items: Array[Item]) {
                                            ^
                  <for {item in items}>
                    <span>{item.name}</span>
                  </for>
                }
            "#},
            expect![[r#"
                Definition
                  --> main.hop (line 1, col 8)
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
                pub record User {name: String}

                -- main.hop --
                import types::User

                component Main(user: User) {
                                     ^
                  <span>{user.name}</span>
                }
            "#},
            expect![[r#"
                Definition
                  --> types.hop (line 1, col 12)
                1 | pub record User {name: String}
                  |            ^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // RENAME LOCATIONS                                                      //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_find_rename_locations_from_component_invocation() {
        check_rename_locations(
            indoc! {r#"
                -- components.hop --
                pub component HelloWorld {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import components::HelloWorld

                component Main {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 15)
                1 | pub component HelloWorld {
                  |               ^^^^^^^^^^

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
    fn should_find_rename_locations_from_component_invocation_in_same_module() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                component HelloWorld {
                  <h1>Hello World</h1>
                }

                component Main {
                  <HelloWorld />
                   ^
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 11)
                1 | component HelloWorld {
                  |           ^^^^^^^^^^

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
                pub component HelloWorld {
                               ^
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import components::HelloWorld

                component Main {
                  <HelloWorld />
                }
            "#},
            expect![[r#"
                Rename
                  --> components.hop (line 1, col 15)
                1 | pub component HelloWorld {
                  |               ^^^^^^^^^^

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
                component HelloWorld {
                  <h1>Hello World</h1>
                }

                component Main {
                          ^
                  <HelloWorld />
                }

                -- main.hop --
                import components::HelloWorld

                component Main {
                  <HelloWorld />
                }
            "#},
            // The result here should not contain rename locations in main.hop.
            expect![[r#"
                Rename
                  --> components.hop (line 5, col 11)
                5 | component Main {
                  |           ^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_from_html_opening_tag() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                component Main {
                    <div>
                     ^
                        <span>Content</span>
                    </div>
                }
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
                component Main {
                  <div>
                    <div>
                     ^
                        <div>Content</div>
                    </div>
                  </div>
                }
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
                component Main {
                    <div>
                        <span>Content</span>
                    </div>
                       ^
                }
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
                component Main {
                    <br />
                     ^
                }
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

                component IconItem(
                  icon: Icon,
                ) {
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
                }

                component IconsPage(
                  icons: Array[Icon],
                ) {
                  <div class="flex">
                      <for {icon in icons}>
                        <IconItem {
                          icon: icon,
                        }/>
                      </for>
                  </div>
                }

                component IconShowPage(
                  icon: Icon,
                ) {
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
                }
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
    fn should_find_rename_locations_for_enum_type() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                enum Status {
                     ^
                  Active,
                  Inactive,
                }

                component UserBadge(status: Status) {
                  <match {status}>
                    <case {Status::Active}><span>Active</span></case>
                    <case {Status::Inactive}><span>Inactive</span></case>
                  </match>
                }

                component UsersPage(statuses: Array[Status]) {
                  <for {status in statuses}>
                    <UserBadge {status: status} />
                  </for>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 6)
                 1 | enum Status {
                   |      ^^^^^^

                Rename
                  --> main.hop (line 6, col 29)
                 6 | component UserBadge(status: Status) {
                   |                             ^^^^^^

                Rename
                  --> main.hop (line 13, col 37)
                13 | component UsersPage(statuses: Array[Status]) {
                   |                                     ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_imported_enum_type() {
        check_rename_locations(
            indoc! {r#"
                -- types.hop --
                pub enum Status {
                         ^
                  Active,
                  Inactive,
                }

                -- main.hop --
                import types::Status

                component Main(status: Status) {
                  <match {status}>
                    <case {Status::Active}><span>Active</span></case>
                    <case {Status::Inactive}><span>Inactive</span></case>
                  </match>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 15)
                1 | import types::Status
                  |               ^^^^^^

                Rename
                  --> main.hop (line 3, col 24)
                3 | component Main(status: Status) {
                  |                        ^^^^^^

                Rename
                  --> types.hop (line 1, col 10)
                1 | pub enum Status {
                  |          ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_for_enum_type_in_view() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                enum Device {
                     ^
                  Desktop,
                  Mobile,
                }

                view Preview(
                  iframe_src: String,
                  device: Device,
                ) {
                  <div class={
                    join!(
                      "bg-white",
                      "h-full",
                      "border",
                      "border-neutral-300",
                      "rounded",
                      "overflow-hidden",
                      match device {
                        Device::Mobile => "w-md",
                        _ => "w-full",
                      },
                    )
                  }>
                    <iframe src={iframe_src} class="w-full h-full">
                    </iframe>
                  </div>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 6)
                 1 | enum Device {
                   |      ^^^^^^

                Rename
                  --> main.hop (line 8, col 11)
                 8 |   device: Device,
                   |           ^^^^^^

                Rename
                  --> main.hop (line 19, col 9)
                19 |         Device::Mobile => "w-md",
                   |         ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_rename_locations_even_when_there_is_parse_errors() {
        check_rename_locations(
            indoc! {r#"
                -- main.hop --
                component Main {
                           ^
                  <div>
                  <span>
                }
            "#},
            expect![[r#"
                Rename
                  --> main.hop (line 1, col 11)
                1 | component Main {
                  |           ^^^^
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
                component HelloWorld {
                          ^
                  <h1>Hello World</h1>
                }
            "#},
            expect![[r#"
                HelloWorld
                  --> main.hop (line 1, col 11)
                1 | component HelloWorld {
                  |           ^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_renameable_symbol_from_enum_declaration() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                enum Status { Active, Inactive }
                     ^
                component Main(status: Status) {
                  <div>{status}</div>
                }
            "#},
            expect![[r#"
                Status
                  --> main.hop (line 1, col 6)
                1 | enum Status { Active, Inactive }
                  |      ^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_find_renameable_symbol_from_html_element() {
        check_renameable_symbol(
            indoc! {r#"
                -- main.hop --
                component Main {
                    <div>Content</div>
                     ^
                }
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
                component Main(user: User) {
                               ^
                  <h1>Hello {user.name}</h1>
                }
            "#},
            expect![[r#"
                ```
                user : main::User
                ```
                  --> main.hop (line 2, col 16)
                2 | component Main(user: User) {
                  |                ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_variable_in_text_expression() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                component Greeting(name: String) {
                  <div>{name}</div>
                        ^
                }
            "#},
            expect![[r#"
                ```
                name : String
                ```
                  --> main.hop (line 2, col 9)
                2 |   <div>{name}</div>
                  |         ^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_record_literal_type_name() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                record User {name: String}
                component Main {
                  <let {user: User = User{name: "John"}}>
                                     ^
                    {user.name}
                  </let>
                }
            "#},
            expect![[r#"
                ```
                User : main::User
                ```
                  --> main.hop (line 3, col 22)
                3 |   <let {user: User = User{name: "John"}}>
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
                component Main {
                  <let {color: Color = Color::Red}>
                                       ^
                    <match {color}>
                      <case {Color::Red}>red</case>
                      <case {_}>other</case>
                    </match>
                  </let>
                }
            "#},
            expect![[r#"
                ```
                Color : main::Color
                ```
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
                enum Outcome { Success{value: String}, Failure{message: String} }
                component Main {
                  <let {result: Outcome = Outcome::Success{value: "ok"}}>
                                          ^
                    <match {result}>
                      <case {Outcome::Success{value: v}}>{v}</case>
                      <case {Outcome::Failure{message: m}}>{m}</case>
                    </match>
                  </let>
                }
            "#},
            expect![[r#"
                ```
                Outcome : main::Outcome
                ```
                  --> main.hop (line 3, col 27)
                3 |   <let {result: Outcome = Outcome::Success{value: "ok"}}>
                  |                           ^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_len_on_array() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[String]) {
                  <if {items.len() == 0}>
                             ^
                    Empty
                  </if>
                }
            "#},
            expect![[r#"
                ```
                Array::len() -> Int
                ```

                Returns the number of elements in the array.
                  --> main.hop (line 2, col 14)
                2 |   <if {items.len() == 0}>
                  |              ^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_is_empty_on_array() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                component Main(items: Array[String]) {
                  <if {items.is_empty()}>
                              ^
                    Empty
                  </if>
                }
            "#},
            expect![[r#"
                ```
                Array::is_empty() -> Bool
                ```

                Returns `true` if the array is empty.
                  --> main.hop (line 2, col 14)
                2 |   <if {items.is_empty()}>
                  |              ^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_join_macro() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                component Main(a: String, b: String) {
                  <div class={
                    join!(a, b)
                    ^
                  }>
                  </div>
                }
            "#},
            expect![[r#"
                ```
                join!(String, ...) -> String
                ```

                Joins strings with spaces.
                  --> main.hop (line 3, col 5)
                3 |     join!(a, b)
                  |     ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_show_hover_info_for_asset_macro() {
        check_hover_info(
            indoc! {r#"
                -- main.hop --
                component Main {
                  <img src={
                    asset!("/logo.svg")
                    ^
                  } />
                }
            "#},
            expect![[r#"
                ```
                asset!(literal: String) -> String
                ```

                Resolves to a path served by the dev server in dev mode and prefixed by `assets.production_prefix` in production builds.
                  --> main.hop (line 3, col 5)
                3 |     asset!("/logo.svg")
                  |     ^^^^^^
            "#]],
        );
    }

    ///////////////////////////////////////////////////////////////////////////
    // WARNING DIAGNOSTICS                                                    //
    ///////////////////////////////////////////////////////////////////////////

    #[test]
    fn should_warn_on_unused_import() {
        check_error_diagnostics(
            indoc! {r#"
                -- components.hop --
                pub component HelloWorld {
                  <h1>Hello World</h1>
                }

                -- main.hop --
                import components::HelloWorld

                component Main {
                  <span>No usage of HelloWorld</span>
                }
            "#},
            "main.hop",
            expect![[r#"
                Unused import 'HelloWorld'
                  --> main.hop (line 1, col 1)
                1 | import components::HelloWorld
                  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
            "#]],
        );
    }

    #[test]
    fn should_not_warn_on_used_import() {
        let program = program_from_txtar(indoc! {r#"
            -- components.hop --
            pub component HelloWorld {
              <h1>Hello World</h1>
            }

            -- main.hop --
            import components::HelloWorld

            component Main {
              <HelloWorld />
            }
        "#});

        let diagnostics = program.get_error_diagnostics(DocumentId::new("main.hop").unwrap());
        let warnings: Vec<_> = diagnostics
            .into_iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert!(
            warnings.is_empty(),
            "Expected no warnings for used import, got: {:?}",
            warnings
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
                component Main {
                  <div>
                  <span>unclosed span
                }
            "#},
            "main.hop",
            expect![[r#"
                Unclosed <div>
                  --> main.hop (line 2, col 4)
                2 |   <div>
                  |    ^^^

                Unclosed <span>
                  --> main.hop (line 3, col 4)
                3 |   <span>unclosed span
                  |    ^^^^
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
            pub component AComp {
              <BComp />
            }

            -- b.hop --
            import a::AComp
            pub component BComp {
              <AComp />
            }

            -- c.hop --
            import a::AComp
            component CComp {
              <AComp />
            }
        "#});
        check_type_errors(
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
        program.update_module(
            &DocumentId::new("a.hop").unwrap(),
            Document::new(
                DocumentId::new("a.hop").unwrap(),
                indoc! {r#"
                    pub component AComp {
                    }
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
            pub component AComp {
              <BComp />
            }

            -- b.hop --
            import c::CComp
            pub component BComp {
              <CComp />
            }

            -- c.hop --
            import d::DComp
            pub component CComp {
              <DComp />
            }

            -- d.hop --
            import a::AComp
            pub component DComp {
              <AComp />
            }
        "#});
        check_type_errors(
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
        program.update_module(
            &DocumentId::new("c.hop").unwrap(),
            Document::new(
                DocumentId::new("c.hop").unwrap(),
                indoc! {r#"
                    pub component CComp {
                    }
                "#}
                .to_string(),
            ),
        );
        // Type errors should now be empty
        check_type_errors(&program, expect![""]);
        // Introduce new cycle a → b → a
        program.update_module(
            &DocumentId::new("b.hop").unwrap(),
            Document::new(
                DocumentId::new("b.hop").unwrap(),
                indoc! {r#"
                    import a::AComp
                    pub component BComp {
                      <AComp />
                    }
                "#}
                .to_string(),
            ),
        );
        check_type_errors(
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
        program.update_module(
            &DocumentId::new("b.hop").unwrap(),
            Document::new(
                DocumentId::new("b.hop").unwrap(),
                indoc! {r#"
                    pub component BComp {
                    }
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
            pub component HelloWorld {
              <h1>Hello World</h1>
            }

            -- main.hop --
            import components::HelloWorld

            component Main {
              <HelloWorld />
            }
        "#});

        // No type errors initially
        check_type_errors(&program, expect![""]);

        // Remove the components module
        program.remove_module(&DocumentId::new("components.hop").unwrap());

        // Now main should have a type error about the missing import
        check_type_errors(
            &program,
            expect![[r#"
                Module components was not found
                  --> main.hop (line 1, col 8)
                1 | import components::HelloWorld
                  |        ^^^^^^^^^^^^^^^^^^^^^^

                Component HelloWorld is not defined
                  --> main.hop (line 4, col 4)
                4 |   <HelloWorld />
                  |    ^^^^^^^^^^
            "#]],
        );

        // Add the module back
        program.update_module(
            &DocumentId::new("components.hop").unwrap(),
            Document::new(
                DocumentId::new("components.hop").unwrap(),
                indoc! {r#"
                    component HelloWorld {
                      <h1>Hello World</h1>
                    }
                "#}
                .to_string(),
            ),
        );

        // Type errors should now be resolved
        check_type_errors(
            &program,
            expect![[r#"
            Type HelloWorld from module components is not public
              --> main.hop (line 1, col 20)
            1 | import components::HelloWorld
              |                    ^^^^^^^^^^

            Component HelloWorld is not defined
              --> main.hop (line 4, col 4)
            4 |   <HelloWorld />
              |    ^^^^^^^^^^
        "#]],
        );
    }

    #[test]
    fn should_evaluate_ir_view_with_parameters() {
        let program = program_from_txtar(indoc! {r#"
            -- main.hop --
            view HelloWorld(name: String) {
              <h1>Hello {name}!</h1>
            }

            view AnotherComp() {
              <p>Static content</p>
            }
        "#});

        // Test evaluating hello-world view with a name parameter
        let mut args = HashMap::new();
        args.insert(
            "name".to_string(),
            ir::semantics::evaluator::Value::String("Alice".to_string()),
        );

        let main_module = DocumentId::new("main.hop").unwrap();
        let hello_world = TypeName::new("HelloWorld").unwrap();
        let result = program
            .evaluate_view_with_values(&main_module, &hello_world, args, None, false, false, None)
            .expect("Should evaluate successfully");

        assert!(result.contains("<h1>Hello Alice!</h1>"));

        // Test evaluating another-comp view without parameters
        let another_comp = TypeName::new("AnotherComp").unwrap();
        let result = program
            .evaluate_view_with_values(
                &main_module,
                &another_comp,
                HashMap::new(),
                None,
                false,
                false,
                None,
            )
            .expect("Should evaluate successfully");

        assert!(result.contains("<p>Static content</p>"));

        // Test error when view doesn't exist
        let non_existent = TypeName::new("NonExistent").unwrap();
        let result = program.evaluate_view_with_values(
            &main_module,
            &non_existent,
            HashMap::new(),
            None,
            false,
            false,
            None,
        );
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("View 'NonExistent' not found in module 'main.hop'")
        );
    }

    #[test]
    fn should_report_enum_equality_as_type_error() {
        let program = program_from_txtar(indoc! {r#"
            -- main.hop --
            enum Color {
              Red,
              Green,
              Blue,
            }

            view Test {
              <let {color: Color = Color::Red}>
                <if {color == Color::Red}>
                  equal
                </if>
              </let>
            }
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Type main::Color is not comparable
                  --> main.hop (line 9, col 10)
                 9 |     <if {color == Color::Red}>
                   |          ^^^^^
            "#]],
        );
    }

    #[test]
    fn should_report_enum_not_equals_as_type_error() {
        let program = program_from_txtar(indoc! {r#"
            -- main.hop --
            enum Color {
              Red,
              Green,
              Blue,
            }

            view Test {
              <let {color: Color = Color::Red}>
                <if {color != Color::Red}>
                  not equal
                </if>
              </let>
            }
        "#});
        check_type_errors(
            &program,
            expect![[r#"
                Type main::Color is not comparable
                  --> main.hop (line 9, col 10)
                 9 |     <if {color != Color::Red}>
                   |          ^^^^^
            "#]],
        );
    }
}
