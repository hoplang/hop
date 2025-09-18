use crate::hop::ast::Ast;
use crate::hop::module_name::ModuleName;
use crate::dop::expr::TypedExpr;
use crate::ir::ast::{IrModule, IrEntrypoint};
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
) -> IrModule {
    // Step 1: Inline components
    let mut inlined_entrypoints = Inliner::inline_entrypoints(typed_asts);

    // Step 2: Apply transforms (only in production mode)
    if mode == CompilationMode::Production {
        for component in &mut inlined_entrypoints {
            DoctypeInjector::transform(component);
        }
    }

    // Step 3: Compile to IR and build module
    let mut ir_module = IrModule::new();

    for entrypoint in inlined_entrypoints {
        let name = entrypoint.tag_name.as_str().to_string();
        let mut ir_entrypoint = Compiler::compile(&entrypoint, mode);

        // Step 4: Run optimization passes (only in production mode)
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
            for pass in passes {
                ir_entrypoint = pass(ir_entrypoint);
            }
        }

        ir_module.entry_points.insert(name, ir_entrypoint);
    }

    ir_module
}