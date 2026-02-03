mod constant_propagation;
mod unused_if_elimination;
mod unused_let_elimination;
mod variable_renaming;
mod write_coalescing;
mod write_expr_simplification;

pub use constant_propagation::ConstantPropagationPass;
pub use unused_if_elimination::UnusedIfEliminationPass;
pub use unused_let_elimination::UnusedLetEliminationPass;
pub use variable_renaming::VariableRenamingPass;
pub use write_coalescing::WriteCoalescingPass;
pub use write_expr_simplification::WriteExprSimplificationPass;

use super::ast::IrEntrypointDeclaration;

pub trait Pass {
    /// Run the pass on a single IR entrypoint, mutating it in place
    fn run(entrypoint: &mut IrEntrypointDeclaration);
}
