mod alpha_renaming;
mod constant_propagation;
mod unused_if_elimination;
mod unused_let_elimination;
mod write_coalescing;
mod write_expr_simplification;

pub use alpha_renaming::AlphaRenamingPass;
pub use constant_propagation::ConstantPropagationPass;
pub use unused_if_elimination::UnusedIfEliminationPass;
pub use unused_let_elimination::UnusedLetEliminationPass;
pub use write_coalescing::WriteCoalescingPass;
pub use write_expr_simplification::WriteExprSimplificationPass;

use super::ast::IrComponentDeclaration;

/// Trait for IR optimization passes that operate on individual entrypoints
///
/// Each pass is responsible for managing its own environment/state while
/// traversing the entrypoint. Since entrypoints are independent, there's
/// no need for cross-entrypoint analysis.
pub trait Pass {
    /// Run the pass on a single IR entrypoint, transforming it into a new entrypoint
    fn run(entrypoint: IrComponentDeclaration) -> IrComponentDeclaration;
}
