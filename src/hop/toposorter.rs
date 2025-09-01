use std::collections::{HashMap, HashSet};

/// The TopoSorter performs topological sorting of directed graphs.
#[derive(Debug, Clone)]
pub struct TopoSorter {
    nodes: HashSet<String>,
    dependencies: HashMap<String, HashSet<String>>, // a -> set of nodes that a depends on
    dependents: HashMap<String, HashSet<String>>,   // a -> set of nodes that depend on a
    pub cycles: Vec<Vec<String>>,
}

impl TopoSorter {
    pub fn new() -> Self {
        TopoSorter {
            nodes: HashSet::new(),
            dependencies: HashMap::new(),
            dependents: HashMap::new(),
            cycles: Vec::new(),
        }
    }

    /// Add a node to the graph.
    pub fn update_node(&mut self, node: &str, dependencies: HashSet<String>) {
        self.nodes.insert(node.to_string());
        let old_dependencies = self.dependencies.entry(node.to_string()).or_default();
        // Clear
        for dep in old_dependencies.iter() {
            self.dependents
                .entry(dep.to_string())
                .or_default()
                .remove(node);
        }
        old_dependencies.clear();
        // Add
        for dep in &dependencies {
            old_dependencies.insert(dep.to_string());
            self.dependents
                .entry(dep.to_string())
                .or_default()
                .insert(node.to_string());
        }
        // Calculate cycles
        self.cycles = self
            .strongly_connected_components()
            .into_iter()
            .filter(|v| v.len() > 1)
            .collect();
    }

    pub fn get_dependents(&self, node: &str) -> Vec<String> {
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
    pub fn strongly_connected_components(&self) -> Vec<Vec<String>> {
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

impl Default for TopoSorter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scc_simple() {
        let mut toposorter = TopoSorter::new();
        toposorter.update_node("a", HashSet::from(["b".to_string()]));
        toposorter.update_node("b", HashSet::from(["c".to_string()]));

        let sccs = toposorter.strongly_connected_components();
        assert_eq!(sccs.len(), 3);
        assert_eq!(sccs[0], vec!["c"]);
        assert_eq!(sccs[1], vec!["b"]);
        assert_eq!(sccs[2], vec!["a"]);
    }

    #[test]
    fn test_scc_with_cycle() {
        let mut toposorter = TopoSorter::new();
        toposorter.update_node("a", HashSet::from(["b".to_string()]));
        toposorter.update_node("b", HashSet::from(["c".to_string()]));
        toposorter.update_node("c", HashSet::from(["a".to_string()]));

        let sccs = toposorter.strongly_connected_components();
        assert_eq!(sccs.len(), 1);
        assert_eq!(sccs[0], vec!["a", "b", "c"]);
    }

    #[test]
    fn test_scc_multiple_components() {
        let mut toposorter = TopoSorter::new();
        toposorter.update_node("a", HashSet::from(["b".to_string()]));
        toposorter.update_node("b", HashSet::from(["a".to_string()]));
        toposorter.update_node("c", HashSet::from(["d".to_string()]));
        toposorter.update_node("d", HashSet::from(["c".to_string()]));
        toposorter.update_node("b", HashSet::from(["a".to_string(), "c".to_string()]));

        let mut sccs = toposorter.strongly_connected_components();
        sccs.sort();
        assert_eq!(sccs.len(), 2);
        assert_eq!(sccs[0], vec!["a", "b"]);
        assert_eq!(sccs[1], vec!["c", "d"]);
    }

    #[test]
    fn test_scc_disconnected_graph() {
        let mut toposorter = TopoSorter::new();
        toposorter.update_node("a", HashSet::new());
        toposorter.update_node("b", HashSet::new());
        toposorter.update_node("c", HashSet::new());

        let mut sccs = toposorter.strongly_connected_components();
        sccs.sort();
        assert_eq!(sccs.len(), 3);
        assert_eq!(sccs[0], vec!["a"]);
        assert_eq!(sccs[1], vec!["b"]);
        assert_eq!(sccs[2], vec!["c"]);
    }

    #[test]
    fn test_scc_complex_graph() {
        let mut toposorter = TopoSorter::new();
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

        let mut sccs = toposorter.strongly_connected_components();
        sccs.sort();
        assert_eq!(sccs[0], vec!["1", "2", "3"]);
        assert_eq!(sccs[1], vec!["4", "5"]);
        assert_eq!(sccs[2], vec!["6", "7"]);
        assert_eq!(sccs[3], vec!["8"]);
    }
}
