use std::collections::{BTreeMap, BTreeSet};

/// A dependency graph between nodes, answering strongly connected
/// component queries.
#[derive(Debug, Clone)]
pub struct DependencyGraph<N> {
    dependencies: BTreeMap<N, BTreeSet<N>>,
}

impl<N: Ord + Clone> Default for DependencyGraph<N> {
    fn default() -> Self {
        Self {
            dependencies: BTreeMap::new(),
        }
    }
}

impl<N: Ord + Clone> DependencyGraph<N> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the dependencies of a node, adding the node to the graph if
    /// it does not exist. Any previous dependencies are replaced.
    pub fn set_dependencies(&mut self, node: N, dependencies: BTreeSet<N>) {
        self.dependencies.insert(node, dependencies);
    }

    /// Check whether a node directly depends on the given node. Passing
    /// the same node twice checks for a self-cycle.
    pub fn depends_on(&self, node: &N, dependency: &N) -> bool {
        self.dependencies
            .get(node)
            .is_some_and(|deps| deps.contains(dependency))
    }

    /// Compute the strongly connected components of the given node and
    /// its transitive dependents, topologically sorted with the node's
    /// component first.
    ///
    /// This answers: when this node changes, what has to be reprocessed
    /// and in what order?
    pub fn dependent_sccs(&self, node: &N) -> Vec<Vec<N>> {
        let mut dependents: BTreeMap<&N, BTreeSet<&N>> = BTreeMap::new();
        for (dependent, dependencies) in &self.dependencies {
            for dependency in dependencies {
                dependents.entry(dependency).or_default().insert(dependent);
            }
        }
        let neighbors = |n: &N| -> Vec<N> {
            dependents
                .get(n)
                .into_iter()
                .flatten()
                .map(|d| (*d).clone())
                .collect()
        };

        let mut state = TarjanState::new();
        visit(node, &neighbors, &mut state);
        state.sccs.reverse();
        state.sccs
    }

    /// Compute all strongly connected components of the graph,
    /// topologically sorted with dependencies before dependents.
    pub fn sorted_sccs(&self) -> Vec<Vec<N>> {
        let neighbors = |n: &N| -> Vec<N> {
            self.dependencies
                .get(n)
                .into_iter()
                .flatten()
                .cloned()
                .collect()
        };

        let mut state = TarjanState::new();
        for node in self.dependencies.keys() {
            if !state.indices.contains_key(node) {
                visit(node, &neighbors, &mut state);
            }
        }
        state.sccs
    }
}

struct TarjanState<N> {
    index_counter: usize,
    stack: Vec<N>,
    indices: BTreeMap<N, usize>,
    lowlinks: BTreeMap<N, usize>,
    on_stack: BTreeSet<N>,
    sccs: Vec<Vec<N>>,
}

impl<N: Ord + Clone> TarjanState<N> {
    fn new() -> Self {
        Self {
            index_counter: 0,
            stack: Vec::new(),
            indices: BTreeMap::new(),
            lowlinks: BTreeMap::new(),
            on_stack: BTreeSet::new(),
            sccs: Vec::new(),
        }
    }
}

/// Tarjan's algorithm. Components are emitted when their root finishes,
/// so successors along the traversal edges come out before their
/// predecessors.
fn visit<N: Ord + Clone>(node: &N, neighbors: &impl Fn(&N) -> Vec<N>, state: &mut TarjanState<N>) {
    state.indices.insert(node.clone(), state.index_counter);
    state.lowlinks.insert(node.clone(), state.index_counter);
    state.index_counter += 1;
    state.stack.push(node.clone());
    state.on_stack.insert(node.clone());

    for neighbor in neighbors(node) {
        if !state.indices.contains_key(&neighbor) {
            visit(&neighbor, neighbors, state);
            let neighbor_lowlink = state.lowlinks[&neighbor];
            let node_lowlink = state.lowlinks[node];
            state
                .lowlinks
                .insert(node.clone(), node_lowlink.min(neighbor_lowlink));
        } else if state.on_stack.contains(&neighbor) {
            let neighbor_index = state.indices[&neighbor];
            let node_lowlink = state.lowlinks[node];
            state
                .lowlinks
                .insert(node.clone(), node_lowlink.min(neighbor_index));
        }
    }

    if state.lowlinks[node] == state.indices[node] {
        let mut scc = Vec::new();
        loop {
            let w = state.stack.pop().unwrap();
            state.on_stack.remove(&w);
            scc.push(w.clone());
            if &w == node {
                break;
            }
        }
        scc.sort();
        state.sccs.push(scc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn set(input: &[&str]) -> BTreeSet<String> {
        input.iter().map(|s| s.to_string()).collect()
    }

    fn graph(edges: &[(&str, &[&str])]) -> DependencyGraph<String> {
        let mut g = DependencyGraph::new();
        for (node, deps) in edges {
            g.set_dependencies(node.to_string(), set(deps));
        }
        g
    }

    fn sccs(input: &[&[&str]]) -> Vec<Vec<String>> {
        input
            .iter()
            .map(|scc| scc.iter().map(|s| s.to_string()).collect())
            .collect()
    }

    #[test]
    fn dependent_sccs_returns_node_first_then_dependents() {
        // a -> b -> c
        let g = graph(&[("a", &["b"]), ("b", &["c"]), ("c", &[])]);
        assert_eq!(
            g.dependent_sccs(&"c".to_string()),
            sccs(&[&["c"], &["b"], &["a"]])
        );
        assert_eq!(g.dependent_sccs(&"b".to_string()), sccs(&[&["b"], &["a"]]));
        assert_eq!(g.dependent_sccs(&"a".to_string()), sccs(&[&["a"]]));
    }

    #[test]
    fn dependent_sccs_groups_cycles() {
        // a -> b
        //  ^   |
        //   \  v
        //     c
        let g = graph(&[("a", &["b"]), ("b", &["c"]), ("c", &["a"])]);
        assert_eq!(
            g.dependent_sccs(&"c".to_string()),
            sccs(&[&["a", "b", "c"]])
        );
    }

    #[test]
    fn dependent_sccs_of_unknown_node_is_the_node_itself() {
        let g = graph(&[("a", &["b"])]);
        assert_eq!(g.dependent_sccs(&"x".to_string()), sccs(&[&["x"]]));
    }

    #[test]
    fn replacing_dependencies_drops_stale_edges() {
        // a -> b, then a -> c
        let mut g = graph(&[("a", &["b"]), ("b", &[]), ("c", &[])]);
        g.set_dependencies("a".to_string(), set(&["c"]));
        assert_eq!(g.dependent_sccs(&"b".to_string()), sccs(&[&["b"]]));
        assert_eq!(g.dependent_sccs(&"c".to_string()), sccs(&[&["c"], &["a"]]));
    }

    #[test]
    fn sorted_sccs_yields_dependencies_first() {
        // a -> b -> c
        let g = graph(&[("a", &["b"]), ("b", &["c"]), ("c", &[])]);
        assert_eq!(g.sorted_sccs(), sccs(&[&["c"], &["b"], &["a"]]));
    }

    #[test]
    fn sorted_sccs_groups_cycles() {
        // a <-> b, both -> c
        let g = graph(&[("a", &["b", "c"]), ("b", &["a", "c"]), ("c", &[])]);
        assert_eq!(g.sorted_sccs(), sccs(&[&["c"], &["a", "b"]]));
    }

    #[test]
    fn sorted_sccs_keeps_self_loop_as_singleton() {
        let g = graph(&[("a", &["a"])]);
        assert_eq!(g.sorted_sccs(), sccs(&[&["a"]]));
    }

    #[test]
    fn sorted_sccs_orders_disconnected_nodes_deterministically() {
        let g = graph(&[("c", &[]), ("a", &[]), ("b", &[])]);
        assert_eq!(g.sorted_sccs(), sccs(&[&["a"], &["b"], &["c"]]));
    }

    #[test]
    fn depends_on_checks_direct_dependencies_only() {
        let g = graph(&[("a", &["b"]), ("b", &["c"]), ("c", &["c"])]);
        assert!(g.depends_on(&"a".to_string(), &"b".to_string()));
        assert!(!g.depends_on(&"a".to_string(), &"c".to_string()));
        assert!(!g.depends_on(&"b".to_string(), &"b".to_string()));
        assert!(g.depends_on(&"c".to_string(), &"c".to_string()));
        assert!(!g.depends_on(&"x".to_string(), &"x".to_string()));
    }

    #[test]
    fn sorted_sccs_of_empty_graph_is_empty() {
        let g: DependencyGraph<String> = DependencyGraph::new();
        assert_eq!(g.sorted_sccs(), Vec::<Vec<String>>::new());
    }

    #[test]
    fn sorted_sccs_includes_nodes_only_known_as_dependencies() {
        // b never had set_dependencies called
        let g = graph(&[("a", &["b"])]);
        assert_eq!(g.sorted_sccs(), sccs(&[&["b"], &["a"]]));
    }
}
