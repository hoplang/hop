use crate::asset_path_rewriter::AssetPathRewriter;
use crate::hop::typing::TypedModule;
use crate::ir::pure_module::PureModule;
use crate::ir::typed_to_pure;
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::type_name::TypeName;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Default)]
pub struct OrchestrateOptions {
    /// When set, only compile the specified page instead of all pages.
    pub page_filter: Option<(RootContainedFilePath, TypeName)>,
    /// Controls how `asset!()` macro invocations are resolved.
    pub asset_path_rewriter: Option<Arc<dyn AssetPathRewriter>>,
}

pub fn orchestrate_pure(
    typed_modules: &HashMap<RootContainedFilePath, TypedModule>,
    options: OrchestrateOptions,
) -> PureModule {
    // Take pages from all modules (sorted by module ID for deterministic order)
    let mut document_ids: Vec<_> = typed_modules.keys().cloned().collect();
    document_ids.sort();
    let pages: Vec<_> = document_ids
        .iter()
        .flat_map(|id| {
            typed_modules[id]
                .page_declarations()
                .iter()
                .filter(|ep| match &options.page_filter {
                    Some((_, page_name)) => ep.name.as_str() == page_name.as_str(),
                    None => true,
                })
                .cloned()
        })
        .collect();

    let functions: Vec<_> = document_ids
        .iter()
        .flat_map(|id| {
            typed_modules[id]
                .function_declarations()
                .iter()
                .map(move |decl| (id, decl))
        })
        .collect();

    // Only the functions the selected pages reach are compiled, so a
    // page_filter build holds what that page actually needs.
    typed_to_pure(pages, &functions, options.asset_path_rewriter)
}
