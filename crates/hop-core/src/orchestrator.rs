use crate::asset_path_rewriter::AssetPathRewriter;
use crate::hop::assembly::{self, AssembledPageDeclaration, TailwindInjection};
use crate::hop::typing::typed_ast::TypedAst;
use crate::ir::pure_module::PureModule;
use crate::ir::{WriterModule, compile, lower_pure, optimize, retain_reachable};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::type_name::TypeName;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default)]
pub struct OrchestrateOptions<'a> {
    pub skip_html_structure: bool,
    pub skip_optimization: bool,
    /// When set, only compile the specified page instead of all pages.
    pub page_filter: Option<(RootContainedFilePath, TypeName)>,
    /// Controls how `asset!()` macro invocations are resolved.
    pub asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
    /// When set, inject the given Tailwind CSS into the `<head>` of each page.
    pub tailwind_injection: Option<TailwindInjection<'a>>,
    /// When set, inject a `<script type="module" src=...>` into the `<head>` of each page.
    pub script_src: Option<&'a str>,
}

pub fn orchestrate(
    typed_asts: &HashMap<RootContainedFilePath, TypedAst>,
    options: OrchestrateOptions<'_>,
) -> WriterModule {
    lower_pure(orchestrate_pure(typed_asts, options))
}

pub fn orchestrate_pure(
    typed_asts: &HashMap<RootContainedFilePath, TypedAst>,
    options: OrchestrateOptions<'_>,
) -> PureModule {
    // Take pages from all modules (sorted by module ID for deterministic order)
    let mut document_ids: Vec<_> = typed_asts.keys().cloned().collect();
    document_ids.sort();
    let typed_pages: Vec<_> = document_ids
        .iter()
        .flat_map(|id| {
            typed_asts[id]
                .page_declarations()
                .iter()
                .filter(|ep| match &options.page_filter {
                    Some((_, page_name)) => ep.name.as_str() == page_name.as_str(),
                    None => true,
                })
                .cloned()
        })
        .collect();

    // Merge each page's head and body into a single document tree
    let assembled_pages: Vec<AssembledPageDeclaration> = typed_pages
        .into_iter()
        .map(|page| {
            if options.skip_html_structure {
                AssembledPageDeclaration::from_body_only(page)
            } else {
                assembly::assemble_page(page, options.tailwind_injection, options.script_src)
            }
        })
        .collect();

    let functions: Vec<_> = document_ids
        .iter()
        .flat_map(|id| {
            typed_asts[id]
                .function_declarations()
                .iter()
                .map(move |decl| (id, decl))
        })
        .collect();

    let pure_module = compile(assembled_pages, &functions, options.asset_path_rewriter);
    // Every function in the project is compiled, whether or not
    // the selected pages reach it. Dropping the unreachable ones keeps a
    // page_filter build to what that page actually needs.
    if options.skip_optimization {
        retain_reachable(pure_module)
    } else {
        optimize(pure_module)
    }
}
