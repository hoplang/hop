use crate::hop::semantics::typed_ast::TypedAst;
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::symbols::module_id::ModuleId;
use crate::inlined::{
    DoctypeInjector, HtmlStructureInjector, Inliner, MetaInjector, TailwindInjector,
};
use crate::ir::syntax::transform::{Pass, VariableRenamingPass};
use crate::ir::{Compiler, IrEnumDeclaration, IrModule, IrRecordDeclaration, optimize};
use std::collections::HashMap;

#[derive(Default)]
pub struct OrchestrateOptions {
    pub skip_html_structure: bool,
    pub skip_optimization: bool,
    /// When set, only compile the specified entrypoint instead of all entrypoints.
    pub entrypoint_filter: Option<(ModuleId, ComponentName)>,
}

pub fn orchestrate(
    typed_asts: &HashMap<ModuleId, TypedAst>,
    generated_tailwind_css: Option<&str>,
    options: OrchestrateOptions,
) -> IrModule {
    // Collect record declarations from all modules
    let mut records: Vec<IrRecordDeclaration> = typed_asts
        .values()
        .flat_map(|module| module.get_records())
        .map(|record| IrRecordDeclaration {
            name: record.name.to_string(),
            fields: record.fields.clone(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    // Collect enum declarations from all modules
    let mut enums: Vec<IrEnumDeclaration> = typed_asts
        .values()
        .flat_map(|module| module.get_enums())
        .map(|enum_decl| IrEnumDeclaration {
            name: enum_decl.name.as_str().to_string(),
            variants: enum_decl.variants.clone(),
        })
        .collect();
    enums.sort_by(|a, b| a.name.cmp(&b.name));

    // Get entrypoints - either all of them or just the filtered one
    let inlined_entrypoints =
        if let Some((ref module_name, ref component_name)) = options.entrypoint_filter {
            // Only inline the specific entrypoint requested
            let module = typed_asts
                .get(module_name)
                .expect("Filtered module should exist");
            let entrypoint = module
                .get_entrypoint_declarations()
                .iter()
                .find(|ep| ep.name.as_str() == component_name.as_str())
                .expect("Filtered entrypoint should exist");
            vec![Inliner::inline_single_entrypoint(
                module_name,
                entrypoint,
                typed_asts,
            )]
        } else {
            // Inline all entrypoints from all modules
            Inliner::inline_ast_entrypoints(typed_asts)
        };

    // Transform and compile each entrypoint
    let mut entrypoints = Vec::with_capacity(inlined_entrypoints.len());
    for e in inlined_entrypoints {
        // Transform ASTs (skip if options.skip_html_structure is set)
        let e = if options.skip_html_structure {
            e
        } else {
            DoctypeInjector::run(e)
        };
        let e = if options.skip_html_structure {
            e
        } else {
            HtmlStructureInjector::run(e)
        };
        let e = if options.skip_html_structure {
            e
        } else {
            MetaInjector::run(e)
        };
        let e = if options.skip_html_structure {
            e
        } else {
            TailwindInjector::run(e, generated_tailwind_css)
        };

        // Compile to IR
        let mut e = Compiler::compile(e);
        VariableRenamingPass::run(&mut e);

        entrypoints.push(e);
    }

    let module = IrModule {
        entrypoints,
        records,
        enums,
    };

    if options.skip_optimization {
        module
    } else {
        optimize(module)
    }
}
