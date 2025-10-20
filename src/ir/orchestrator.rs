use crate::dop::SimpleTypedExpr;
use crate::hop::ast::Ast;
use crate::hop::module_name::ModuleName;
use crate::ir::Compiler;
use crate::ir::ast::IrEntrypoint;
use crate::ir::inliner::Inliner;
use crate::ir::passes::{
    AlphaRenamingPass, ConstantPropagationPass, DeadCodeEliminationPass, Pass,
    UnusedLetEliminationPass, WriteExprSimplificationPass,
};
use crate::ir::transforms::{DoctypeInjector, HtmlStructureInjector, TailwindInjector};
use std::collections::HashMap;

pub fn orchestrate(
    typed_asts: HashMap<ModuleName, Ast<SimpleTypedExpr>>,
    generated_tailwind_css: Option<&str>,
) -> Vec<IrEntrypoint> {
    Inliner::inline_entrypoints(typed_asts)
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
        .map(DeadCodeEliminationPass::run)
        .map(WriteExprSimplificationPass::run)
        .collect()
}
