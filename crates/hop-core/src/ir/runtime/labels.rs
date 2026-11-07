use std::collections::HashMap;

use crate::ir::runtime::state::NodeId;

/// The labels of the elements a run shows, by the order in which it first
/// shows them.
#[derive(Default)]
pub struct Labels {
    nodes: Vec<NodeId>,
    labels: HashMap<NodeId, usize>,
}

impl Labels {
    /// The label of the node, which takes the next label if the run has not
    /// shown it before. Should be called on each element a render shows, in
    /// document order.
    pub fn label(&mut self, node: NodeId) -> usize {
        *self.labels.entry(node).or_insert_with(|| {
            self.nodes.push(node);
            self.nodes.len()
        })
    }

    /// The node of the label. Panics if the run has not shown it.
    pub fn node(&self, label: usize) -> NodeId {
        self.nodes[label - 1]
    }
}
