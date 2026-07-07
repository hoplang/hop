use crate::asset_rewriter::AssetRewriter;
use crate::document_id::DocumentId;
use crate::expr::typing::type_registry::TypeRegistry;
use crate::hop::inlining::Inliner;
use crate::hop::inlining::transform::{
    DoctypeInjector, HtmlStructureInjector, LinkRewriter, MetaInjector, ScriptInjector,
    TailwindInjection, TailwindInjector,
};
use crate::hop::typing::typed_ast::TypedAst;
use crate::ir::syntax::variable_renaming::VariableRenamingPass;
use crate::ir::{Compiler, IrEnumDeclaration, IrModule, IrRecordDeclaration, optimize};
use crate::symbols::type_name::TypeName;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default)]
pub struct OrchestrateOptions<'a> {
    pub skip_html_structure: bool,
    pub skip_optimization: bool,
    /// When set, rewrite all `<a href="...">` to `<a href="#">` to disable navigation.
    pub disable_links: bool,
    /// When set, only compile the specified view instead of all views.
    pub view_filter: Option<(DocumentId, TypeName)>,
    /// Controls how `asset!()` macro invocations are resolved.
    pub asset_rewriter: Option<Arc<dyn AssetRewriter>>,
    /// When set, inject the given Tailwind CSS into the `<head>` of each view.
    pub tailwind_injection: Option<TailwindInjection<'a>>,
    /// When set, inject a `<script type="module" src=...>` into the `<head>` of each view.
    pub script_src: Option<&'a str>,
}

pub fn orchestrate(
    typed_asts: &HashMap<DocumentId, TypedAst>,
    registry: &TypeRegistry,
    options: OrchestrateOptions<'_>,
) -> IrModule {
    // Collect record declarations from all modules
    let mut records: Vec<IrRecordDeclaration> = typed_asts
        .values()
        .flat_map(|module| module.get_records())
        .map(|record| IrRecordDeclaration {
            name: record.name.clone(),
            fields: record.fields.clone(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    // Collect enum declarations from all modules
    let mut enums: Vec<IrEnumDeclaration> = typed_asts
        .values()
        .flat_map(|module| module.get_enums())
        .map(|enum_decl| IrEnumDeclaration {
            name: enum_decl.name.clone(),
            variants: enum_decl.variants.clone(),
        })
        .collect();
    enums.sort_by(|a, b| a.name.cmp(&b.name));

    // Take views from all modules (sorted by module ID for deterministic order)
    let mut document_ids: Vec<_> = typed_asts.keys().cloned().collect();
    document_ids.sort();
    let typed_views: Vec<_> = document_ids
        .iter()
        .flat_map(|id| {
            typed_asts[id]
                .get_view_declarations()
                .iter()
                .filter(|ep| match &options.view_filter {
                    Some((_, view_name)) => ep.name.as_str() == view_name.as_str(),
                    None => true,
                })
                .cloned()
        })
        .collect();

    // Inline component invocations into the views
    let (inlined_views, component_declarations) =
        Inliner::inline_ast_views(typed_asts, &typed_views);

    // Transform and compile each view
    let mut views = Vec::with_capacity(inlined_views.len());
    for mut e in inlined_views {
        if !options.skip_html_structure {
            DoctypeInjector::run(&mut e);
            HtmlStructureInjector::run(&mut e);
            MetaInjector::run(&mut e);
            TailwindInjector::run(&mut e, options.tailwind_injection);
            ScriptInjector::run(&mut e, options.script_src);
        }
        if options.disable_links {
            LinkRewriter::run(&mut e);
        }

        // Compile to IR
        let mut e = Compiler::compile(e, options.asset_rewriter.clone());
        VariableRenamingPass::run(&mut e);

        views.push(e);
    }

    // Compile component decls
    let mut components = Vec::with_capacity(component_declarations.len());
    for mut decl in component_declarations {
        if options.disable_links {
            LinkRewriter::run_component(&mut decl);
        }
        let mut comp = Compiler::compile_component_decl(decl, options.asset_rewriter.clone());
        VariableRenamingPass::run_component(&mut comp);
        components.push(comp);
    }

    let module = IrModule {
        views,
        components,
        records,
        enums,
    };

    if options.skip_optimization {
        module
    } else {
        optimize(module, registry)
    }
}
