use std::collections::{HashMap, HashSet};

/// The TopoSorter performs topological sorting of directed graphs.
#[derive(Default, Debug, Clone)]
pub struct TopoSorter {
    nodes: HashSet<String>,
    dependencies: HashMap<String, HashSet<String>>, // a -> set of nodes that a depends on
    dependents: HashMap<String, HashSet<String>>,   // a -> set of nodes that depend on a
    sccs: Vec<Vec<String>>,
}

impl TopoSorter {
    /// Update the state of a given node in the graph.
    ///
    /// Time complexity: O(D + V + E) where D is the number of old dependencies to clear
    /// and V and E is the number of nodes and edges in the graph after the update.
    pub fn update_node(&mut self, node: &str, dependencies: HashSet<String>) {
        self.nodes.insert(node.to_string());

        // Remove old reverse dependencies
        if let Some(old_deps) = self.dependencies.get(node) {
            for dep in old_deps {
                if let Some(dependents) = self.dependents.get_mut(dep) {
                    dependents.remove(node);
                }
            }
        }

        // Set new dependencies
        self.dependencies
            .insert(node.to_string(), dependencies.clone());

        // Add new reverse dependencies
        for dep in &dependencies {
            self.nodes.insert(dep.to_string());
            self.dependents
                .entry(dep.to_string())
                .or_default()
                .insert(node.to_string());
        }

        // Calculate strongly connected components
        self.sccs = self.strongly_connected_components().into_iter().collect();
    }

    /// Get the strongly connected component for a given node.
    pub fn get_component(&self, node: &str) -> &[String] {
        for scc in &self.sccs {
            if scc.contains(&node.to_string()) {
                return scc;
            }
        }
        panic!()
    }

    /// Get all nodes that have a direct or transitive dependency on a given
    /// node including the node itself (the transitive closure).
    ///
    /// This function performs a depth-first search and the result will
    /// be a topologically sorted list suitable for work scheduling (i.e.
    /// dependencies first) given that the dependencies are not part of a cycles.
    ///
    /// Time complexity: O(V + E) where V is the number of reachable nodes
    /// and E is the number of edges in the reachable subgraph.
    pub fn get_transitive_dependents(&self, node: &str) -> Vec<String> {
        if !self.nodes.contains(node) {
            return Vec::new();
        }

        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = vec![node.to_string()];

        while let Some(current) = stack.pop() {
            if visited.contains(&current) {
                continue;
            }

            visited.insert(current.clone());
            result.push(current.clone());

            if let Some(deps) = self.dependents.get(&current) {
                for dep in deps {
                    if !visited.contains(dep) {
                        stack.push(dep.clone());
                    }
                }
            }
        }

        result
    }

    /// Compute strongly connected components using Tarjan's algorithm.
    /// Returns a vector of SCCs, where each SCC is a vector of node names.
    /// The SCCs are returned in reverse topological order.
    ///
    /// Time complexity: O(V + E) where V is the number of nodes
    /// and E is the number of edges in the graph.
    fn strongly_connected_components(&self) -> Vec<Vec<String>> {
        let mut index_counter = 0;
        let mut stack = Vec::new();
        let mut lowlinks = HashMap::new();
        let mut index = HashMap::new();
        let mut on_stack = HashSet::new();
        let mut sccs = Vec::new();

        for node in &self.nodes {
            if !index.contains_key(node) {
                self.tarjan_scc(
                    node,
                    &mut index_counter,
                    &mut stack,
                    &mut lowlinks,
                    &mut index,
                    &mut on_stack,
                    &mut sccs,
                );
            }
        }

        sccs
    }

    fn tarjan_scc(
        &self,
        node: &str,
        index_counter: &mut usize,
        stack: &mut Vec<String>,
        lowlinks: &mut HashMap<String, usize>,
        index: &mut HashMap<String, usize>,
        on_stack: &mut HashSet<String>,
        sccs: &mut Vec<Vec<String>>,
    ) {
        index.insert(node.to_string(), *index_counter);
        lowlinks.insert(node.to_string(), *index_counter);
        *index_counter += 1;
        stack.push(node.to_string());
        on_stack.insert(node.to_string());

        if let Some(deps) = self.dependencies.get(node) {
            for dep in deps {
                if !index.contains_key(dep) {
                    self.tarjan_scc(dep, index_counter, stack, lowlinks, index, on_stack, sccs);
                    let dep_lowlink = *lowlinks.get(dep).unwrap();
                    let node_lowlink = *lowlinks.get(node).unwrap();
                    lowlinks.insert(node.to_string(), node_lowlink.min(dep_lowlink));
                } else if on_stack.contains(dep) {
                    let dep_index = *index.get(dep).unwrap();
                    let node_lowlink = *lowlinks.get(node).unwrap();
                    lowlinks.insert(node.to_string(), node_lowlink.min(dep_index));
                }
            }
        }

        if lowlinks.get(node).unwrap() == index.get(node).unwrap() {
            let mut scc = Vec::new();
            loop {
                let w = stack.pop().unwrap();
                on_stack.remove(&w);
                scc.push(w.clone());
                if w == node {
                    break;
                }
            }
            scc.sort();
            sccs.push(scc);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn set(input: &[&str]) -> HashSet<String> {
        let mut set = HashSet::new();
        for s in input {
            set.insert(s.to_string());
        }
        set
    }

    #[test]
    fn test_scc_simple() {
        let mut ts = TopoSorter::default();
        // a → b → c
        ts.update_node("a", set(&["b"]));
        ts.update_node("b", set(&["c"]));

        assert_eq!(ts.get_component("a"), vec!["a"]);
        assert_eq!(ts.get_component("b"), vec!["b"]);
        assert_eq!(ts.get_component("c"), vec!["c"]);
        assert_eq!(ts.get_transitive_dependents("c"), vec!["c", "b", "a"]);
        assert_eq!(ts.get_transitive_dependents("b"), vec!["b", "a"]);
        assert_eq!(ts.get_transitive_dependents("a"), vec!["a"]);
    }

    #[test]
    fn test_scc_with_cycle() {
        let mut ts = TopoSorter::default();
        // a → b
        //  ↖ ↙
        //   c
        ts.update_node("a", set(&["b"]));
        ts.update_node("b", set(&["c"]));
        ts.update_node("c", set(&["a"]));

        assert_eq!(ts.get_component("a"), vec!["a", "b", "c"]);
        assert_eq!(ts.get_component("b"), vec!["a", "b", "c"]);
        assert_eq!(ts.get_component("c"), vec!["a", "b", "c"]);
        assert_eq!(ts.get_transitive_dependents("b"), vec!["b", "a", "c"]);
        assert_eq!(ts.get_transitive_dependents("c"), vec!["c", "b", "a"]);
        assert_eq!(ts.get_transitive_dependents("a"), vec!["a", "c", "b"]);
    }

    #[test]
    fn test_scc_multiple_components() {
        let mut ts = TopoSorter::default();
        // a ⇄ b
        //     ↓
        // d ⇄ c
        ts.update_node("a", set(&["b"]));
        ts.update_node("b", set(&["a", "c"]));
        ts.update_node("c", set(&["d"]));
        ts.update_node("d", set(&["c"]));

        assert_eq!(ts.get_component("a"), vec!["a", "b"]);
        assert_eq!(ts.get_component("c"), vec!["c", "d"]);
        assert_eq!(ts.get_transitive_dependents("b"), vec!["b", "a"]);
        assert_eq!(ts.get_transitive_dependents("d"), vec!["d", "c", "b", "a"]);
    }

    #[test]
    fn test_scc_disconnected_graph() {
        let mut ts = TopoSorter::default();
        // a   b   c
        ts.update_node("a", set(&[]));
        ts.update_node("b", set(&[]));
        ts.update_node("c", set(&[]));

        assert_eq!(ts.get_component("a"), vec!["a"]);
        assert_eq!(ts.get_component("b"), vec!["b"]);
        assert_eq!(ts.get_component("c"), vec!["c"]);
    }

    #[test]
    fn test_scc_complex_graph() {
        let mut ts = TopoSorter::default();
        ts.update_node("1", set(&["2"]));
        ts.update_node("2", set(&["3"]));
        ts.update_node("3", set(&["1"]));
        ts.update_node("4", set(&["2", "3", "5"]));
        ts.update_node("5", set(&["4", "6"]));
        ts.update_node("6", set(&["3", "7"]));
        ts.update_node("7", set(&["6"]));
        ts.update_node("8", set(&["5", "7", "8"]));

        assert_eq!(ts.get_component("1"), vec!["1", "2", "3"]);
        assert_eq!(ts.get_component("4"), vec!["4", "5"]);
        assert_eq!(ts.get_component("6"), vec!["6", "7"]);
        assert_eq!(ts.get_component("8"), vec!["8"]);
    }
}
