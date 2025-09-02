use std::collections::{HashMap, HashSet};

/// The TopoSorter performs topological sorting of directed graphs.
#[derive(Default, Debug, Clone)]
pub struct TopoSorter {
    nodes: HashSet<String>,
    dependencies: HashMap<String, HashSet<String>>, // a -> set of nodes that a depends on
    dependents: HashMap<String, HashSet<String>>,   // a -> set of nodes that depend on a
    pub cycles: Vec<Vec<String>>,
    pub sccs: Vec<Vec<String>>,
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
            self.dependents
                .entry(dep.to_string())
                .or_default()
                .insert(node.to_string());
        }

        // Calculate cycles
        self.sccs = self.strongly_connected_components().into_iter().collect();
        self.cycles = self
            .sccs
            .clone()
            .into_iter()
            .filter(|v| v.len() > 1)
            .collect();
    }

    /// Get the strongly connected component for a given node.
    /// Returns None if the node is not in the graph.
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

    #[test]
    fn test_scc_simple() {
        let mut toposorter = TopoSorter::default();
        toposorter.update_node("a", HashSet::from(["b".to_string()]));
        toposorter.update_node("b", HashSet::from(["c".to_string()]));

        assert_eq!(toposorter.get_component("a"), vec!["a"]);
        assert_eq!(toposorter.get_component("b"), vec!["b"]);
        assert_eq!(toposorter.get_component("c"), vec!["c"]);
    }

    #[test]
    fn test_scc_with_cycle() {
        let mut toposorter = TopoSorter::default();
        toposorter.update_node("a", HashSet::from(["b".to_string()]));
        toposorter.update_node("b", HashSet::from(["c".to_string()]));
        toposorter.update_node("c", HashSet::from(["a".to_string()]));

        assert_eq!(toposorter.get_component("a"), vec!["a", "b", "c"]);
        assert_eq!(toposorter.get_component("b"), vec!["a", "b", "c"]);
        assert_eq!(toposorter.get_component("c"), vec!["a", "b", "c"]);
    }

    #[test]
    fn test_scc_multiple_components() {
        let mut toposorter = TopoSorter::default();
        toposorter.update_node("a", HashSet::from(["b".to_string()]));
        toposorter.update_node("b", HashSet::from(["a".to_string()]));
        toposorter.update_node("c", HashSet::from(["d".to_string()]));
        toposorter.update_node("d", HashSet::from(["c".to_string()]));
        toposorter.update_node("b", HashSet::from(["a".to_string(), "c".to_string()]));

        assert_eq!(toposorter.get_component("a"), vec!["a", "b"]);
        assert_eq!(toposorter.get_component("c"), vec!["c", "d"]);
    }

    #[test]
    fn test_scc_disconnected_graph() {
        let mut toposorter = TopoSorter::default();
        toposorter.update_node("a", HashSet::new());
        toposorter.update_node("b", HashSet::new());
        toposorter.update_node("c", HashSet::new());

        assert_eq!(toposorter.get_component("a"), vec!["a"]);
        assert_eq!(toposorter.get_component("b"), vec!["b"]);
        assert_eq!(toposorter.get_component("c"), vec!["c"]);
    }

    #[test]
    fn test_scc_complex_graph() {
        let mut toposorter = TopoSorter::default();
        toposorter.update_node("1", HashSet::from(["2".to_string()]));
        toposorter.update_node("2", HashSet::from(["3".to_string()]));
        toposorter.update_node("3", HashSet::from(["1".to_string()]));
        toposorter.update_node(
            "4",
            HashSet::from(["2".to_string(), "3".to_string(), "5".to_string()]),
        );
        toposorter.update_node("5", HashSet::from(["4".to_string(), "6".to_string()]));
        toposorter.update_node("6", HashSet::from(["3".to_string(), "7".to_string()]));
        toposorter.update_node("7", HashSet::from(["6".to_string()]));
        toposorter.update_node(
            "8",
            HashSet::from(["5".to_string(), "7".to_string(), "8".to_string()]),
        );

        assert_eq!(toposorter.get_component("1"), vec!["1", "2", "3"]);
        assert_eq!(toposorter.get_component("4"), vec!["4", "5"]);
        assert_eq!(toposorter.get_component("6"), vec!["6", "7"]);
        assert_eq!(toposorter.get_component("8"), vec!["8"]);
    }
}
