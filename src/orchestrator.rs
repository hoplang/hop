use crate::hop::ast::TypedAst;
use crate::hop::component_name::ComponentName;
use crate::hop::module_name::ModuleName;
use crate::ir::{Compiler, IrEntrypoint};
use crate::ir::inliner::Inliner;
use crate::ir::passes::{
    AlphaRenamingPass, ConstantPropagationPass, UnusedIfEliminationPass, Pass,
    UnusedLetEliminationPass, WriteExprSimplificationPass,
};
use crate::ir::transforms::{DoctypeInjector, HtmlStructureInjector, TailwindInjector};
use anyhow::Result;
use std::collections::HashMap;

pub fn orchestrate(
    typed_asts: HashMap<ModuleName, TypedAst>,
    generated_tailwind_css: Option<&str>,
    pages: &[(ModuleName, ComponentName)],
) -> Result<Vec<IrEntrypoint>> {
    Ok(Inliner::inline_entrypoints(typed_asts, pages)?
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
        .collect())
}
