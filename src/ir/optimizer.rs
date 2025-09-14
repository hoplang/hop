use super::ast::IrModule;
use super::passes::Pass;

/// An optimizer that runs optimization passes on IR modules
pub struct Optimizer {
    passes: Vec<Box<dyn Pass>>,
}

impl Optimizer {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    /// Add a pass to the optimizer
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
        use super::passes::{
            ConstantPropagationPass, DeadCodeEliminationPass, UnusedLetEliminationPass,
            WriteCoalescingPass, WriteExprSimplificationPass,
        };

        let mut optimizer = Self::new();
        optimizer.add_pass(Box::new(ConstantPropagationPass::new()));
        optimizer.add_pass(Box::new(UnusedLetEliminationPass::new()));
        optimizer.add_pass(Box::new(DeadCodeEliminationPass::new()));
        optimizer.add_pass(Box::new(WriteExprSimplificationPass::new()));
        optimizer.add_pass(Box::new(WriteCoalescingPass::new()));
        optimizer
    }
}
