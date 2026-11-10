use std::collections::HashMap;

use crate::ir::runtime::state::ElementIdentity;

/// The labels of the elements a run shows, by the order in which it first
/// shows them.
#[derive(Default)]
pub struct Labels {
    identities: Vec<ElementIdentity>,
    labels: HashMap<ElementIdentity, usize>,
}

impl Labels {
    /// The label of the identity, which takes the next label if the run has not
    /// shown it before. Should be called on each element a render shows, in
    /// document order.
    pub fn label(&mut self, node: ElementIdentity) -> usize {
        *self.labels.entry(node).or_insert_with(|| {
            self.identities.push(node);
            self.identities.len()
        })
    }

    /// The identity of the label. Panics if the run has not shown it.
    pub fn identity(&self, label: usize) -> ElementIdentity {
        self.identities[label - 1]
    }
}
