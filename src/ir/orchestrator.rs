use crate::dop::TypedExpr;
use crate::hop::ast::Ast;
use crate::hop::module_name::ModuleName;
use crate::ir::ast::IrEntrypoint;
use crate::ir::inliner::Inliner;
use crate::ir::passes::{
    AlphaRenamingPass, ConstantPropagationPass, DeadCodeEliminationPass, Pass,
    UnusedLetEliminationPass, WriteExprSimplificationPass,
};
use crate::ir::transforms::DoctypeInjector;
use crate::ir::{CompilationMode, Compiler};
use std::collections::HashMap;

pub fn orchestrate(
    typed_asts: HashMap<ModuleName, Ast<TypedExpr>>,
    mode: CompilationMode,
) -> Vec<IrEntrypoint> {
    Inliner::inline_entrypoints(typed_asts)
        .into_iter()
        // transform ASTs
        .map(DoctypeInjector::run)
        // compile to IR
        .map(|entrypoint| Compiler::compile(entrypoint, mode))
        // optimize IR
        .map(AlphaRenamingPass::run)
        .map(ConstantPropagationPass::run)
        .map(UnusedLetEliminationPass::run)
        .map(DeadCodeEliminationPass::run)
        .map(WriteExprSimplificationPass::run)
        .collect()
}
