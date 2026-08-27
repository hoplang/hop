use crate::asset_rewriter::AssetRewriter;
use crate::document_id::DocumentId;
use crate::hop::inlining::Inliner;
use crate::hop::inlining::transform::{
    DoctypeInjector, HtmlStructureInjector, LinkRewriter, MetaInjector, ScriptInjector,
    TailwindInjection, TailwindInjector,
};
use crate::hop::typing::typed_ast::TypedAst;
use crate::ir::pure_module::PureModule;
use crate::ir::{WriterModule, compile, lower_pure, optimize};
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
    options: OrchestrateOptions<'_>,
) -> WriterModule {
    lower_pure(orchestrate_pure(typed_asts, options))
}

pub fn orchestrate_pure(
    typed_asts: &HashMap<DocumentId, TypedAst>,
    options: OrchestrateOptions<'_>,
) -> PureModule {
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
    let (mut inlined_views, mut inlined_component_declarations) =
        Inliner::inline_ast_views(typed_asts, &typed_views);

    // Transform each view
    for view in &mut inlined_views {
        if !options.skip_html_structure {
            DoctypeInjector::run(view);
            HtmlStructureInjector::run(view);
            MetaInjector::run(view);
            TailwindInjector::run(view, options.tailwind_injection);
            ScriptInjector::run(view, options.script_src);
        }
        if options.disable_links {
            LinkRewriter::run(view);
        }
    }

    // Transform component decls
    if options.disable_links {
        for decl in &mut inlined_component_declarations {
            LinkRewriter::run_component(decl);
        }
    }

    let records: Vec<_> = typed_asts
        .values()
        .flat_map(|module| module.get_records())
        .collect();
    let enums: Vec<_> = typed_asts
        .values()
        .flat_map(|module| module.get_enums())
        .collect();

    let pure_module = compile(
        inlined_views,
        inlined_component_declarations,
        &records,
        &enums,
        options.asset_rewriter,
    );
    if options.skip_optimization {
        pure_module
    } else {
        optimize(pure_module)
    }
}
