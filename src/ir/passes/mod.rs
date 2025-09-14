mod constant_folding;
mod constant_propagation;
mod dead_code_elimination;
mod write_coalescing;

pub use constant_folding::ConstantFoldingPass;
pub use constant_propagation::ConstantPropagationPass;
pub use dead_code_elimination::DeadCodeEliminationPass;
pub use write_coalescing::WriteCoalescingPass;

use super::ast::{IrEntrypoint, IrModule};

/// Trait for IR optimization passes that operate on individual entrypoints
///
/// Each pass is responsible for managing its own environment/state while
/// traversing the entrypoint. Since entrypoints are independent, there's
/// no need for cross-entrypoint analysis.
pub trait Pass {
    /// Run the pass on a single IR entrypoint, transforming it into a new entrypoint
    fn run(&mut self, entrypoint: IrEntrypoint) -> IrEntrypoint;
}

/// A pass manager that runs optimization passes on IR modules
pub struct PassManager {
    passes: Vec<Box<dyn Pass>>,
}

impl PassManager {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Add a pass to the manager
    pub fn add_pass(&mut self, pass: Box<dyn Pass>) {
        self.passes.push(pass);
    }

    /// Run all passes on the module
    pub fn run(&mut self, module: &mut IrModule) {
        // Iterate over each entrypoint in the module
        for entrypoint in module.entry_points.values_mut() {
            // Take ownership of the entrypoint, run passes, then put it back
            let mut current = std::mem::take(entrypoint);
            for pass in &mut self.passes {
                current = pass.run(current);
            }
            *entrypoint = current;
        }
    }

    /// Create a default optimization pipeline
    pub fn default_optimization_pipeline() -> Self {
        let mut manager = Self::new();
        manager.add_pass(Box::new(ConstantFoldingPass::new()));
        manager.add_pass(Box::new(DeadCodeEliminationPass::new()));
        manager.add_pass(Box::new(WriteCoalescingPass::new()));
        manager
    }
}
