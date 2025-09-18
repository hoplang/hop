use crate::hop::ast::Ast;
use crate::hop::module_name::ModuleName;
use crate::dop::expr::TypedExpr;
use crate::ir::ast::IrEntrypoint;
use crate::ir::inliner::Inliner;
use crate::ir::passes::{
    Pass, AlphaRenamingPass, ConstantPropagationPass, DeadCodeEliminationPass,
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
        // Apply transforms (only in production mode)
        .map(|entrypoint| {
            if mode == CompilationMode::Production {
                DoctypeInjector::transform(entrypoint)
            } else {
                entrypoint
            }
        })
        // Compile to IR
        .map(|entrypoint| Compiler::compile(entrypoint, mode))
        // Run optimization passes (only in production mode)
        .map(|ir_entrypoint| {
            if mode == CompilationMode::Production {
                // Create optimization passes as function pointers
                let passes: Vec<fn(IrEntrypoint) -> IrEntrypoint> = vec![
                    AlphaRenamingPass::run,
                    ConstantPropagationPass::run,
                    UnusedLetEliminationPass::run,
                    DeadCodeEliminationPass::run,
                    WriteExprSimplificationPass::run,
                    // Skip WriteCoalescingPass for now, since typescript-language-server can't handle long strings very well
                ];

                // Run passes on this entrypoint
                passes.into_iter().fold(ir_entrypoint, |acc, pass| pass(acc))
            } else {
                ir_entrypoint
            }
        })
        .collect()
}