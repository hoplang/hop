use crate::hop::ast::TypedAst;
use crate::hop::component_name::ComponentName;
use crate::hop::inliner::Inliner;
use crate::hop::module_name::ModuleName;
use crate::ir::optimize::{
    AlphaRenamingPass, ConstantPropagationPass, Pass, UnusedIfEliminationPass,
    UnusedLetEliminationPass, WriteExprSimplificationPass,
};
use crate::ir::transform::{DoctypeInjector, HtmlStructureInjector, TailwindInjector};
use crate::ir::{Compiler, IrModule, IrRecord};
use anyhow::Result;
use std::collections::HashMap;

pub fn orchestrate(
    typed_asts: HashMap<ModuleName, TypedAst>,
    generated_tailwind_css: Option<&str>,
    pages: &[(ModuleName, ComponentName)],
) -> Result<IrModule> {
    // Collect record declarations from all modules
    let mut records: Vec<IrRecord> = typed_asts
        .values()
        .flat_map(|module| module.get_records())
        .map(|record| IrRecord {
            name: record.name().to_string(),
            fields: record
                .declaration
                .fields
                .iter()
                .map(|f| (f.name.clone(), f.field_type.clone()))
                .collect(),
        })
        .collect();
    records.sort_by(|a, b| a.name.cmp(&b.name));

    let entrypoints = Inliner::inline_entrypoints(typed_asts, pages)?
        .into_iter()
        // transform ASTs
        .map(DoctypeInjector::run)
        .map(HtmlStructureInjector::run)
        .map(|entrypoint| TailwindInjector::run(entrypoint, generated_tailwind_css))
        // compile to IR
        .map(Compiler::compile)
        // optimize IR
        .map(AlphaRenamingPass::run)
        .map(ConstantPropagationPass::run)
        .map(UnusedLetEliminationPass::run)
        .map(UnusedIfEliminationPass::run)
        .map(WriteExprSimplificationPass::run)
        .collect();

    Ok(IrModule {
        entrypoints,
        records,
    })
}
