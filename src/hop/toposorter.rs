use std::collections::{HashMap, HashSet};

/// CycleError represents a cycle in the graph.
/// Each node in the cycle occurs exactly once in the `cycle` vector.
#[derive(Debug, Clone, PartialEq)]
pub struct CycleError {
    pub cycle: Vec<String>,
}

impl CycleError {
    pub fn new(cycle: Vec<String>) -> Self {
        CycleError { cycle }
    }
}

/// The TopoSorter module is responsible for performing topological sorting of directed graphs.
#[derive(Debug, Clone)]
pub struct TopoSorter {
    nodes: HashSet<String>,
    dependencies: HashMap<String, HashSet<String>>, // a -> set of nodes that a depends on
    dependents: HashMap<String, HashSet<String>>,   // a -> set of nodes that depend on a
}

impl TopoSorter {
    pub fn new() -> Self {
        TopoSorter {
            nodes: HashSet::new(),
            dependencies: HashMap::new(),
            dependents: HashMap::new(),
        }
    }

    /// Add a node to the graph.
    pub fn add_node(&mut self, node: String) {
        self.nodes.insert(node.clone());
        if !self.dependencies.contains_key(&node) {
            self.dependencies.insert(node.clone(), HashSet::new());
        }
        if !self.dependents.contains_key(&node) {
            self.dependents.insert(node.clone(), HashSet::new());
        }
    }

    /// Add a dependency (a -> b).
    ///
    /// This dependency implies that the node b should come before the node a when sorting.
    /// Each node of the dependency is added to the graph if they do not already exist.
    pub fn add_dependency(&mut self, a: &str, b: &str) {
        self.add_node(a.to_string());
        self.add_node(b.to_string());

        self.dependencies.get_mut(a).unwrap().insert(b.to_string());
        self.dependents.get_mut(b).unwrap().insert(a.to_string());
    }

    /// Sort the nodes of the graph and return a Result where the nodes are in topological order
    /// or an error if the graph contains a cycle.
    pub fn sort(&self) -> Result<Vec<String>, CycleError> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut path = Vec::new();
        let mut in_path = HashSet::new();

        for node in &self.nodes {
            if !visited.contains(node) {
                if let Some(error) = self.dfs(
                    node,
                    &mut result,
                    &mut visited,
                    &mut path,
                    &mut in_path,
                    &self.nodes,
                ) {
                    return Err(error);
                }
            }
        }

        Ok(result)
    }

    /// Do a topological sort of the subgraph containing all nodes that depend on the given node.
    ///
    /// The result includes the node `root` (which will be the first node of the array) and all
    /// nodes that depend on `root` (directly or transitively).
    pub fn sort_subgraph(&self, root: &str) -> Result<Vec<String>, CycleError> {
        if !self.nodes.contains(root) {
            return Ok(Vec::new());
        }

        // Find all nodes that depend on root (transitively)
        let mut subgraph_nodes = HashSet::new();
        let mut queue = vec![root.to_string()];

        while let Some(current) = queue.pop() {
            if subgraph_nodes.contains(&current) {
                continue;
            }

            subgraph_nodes.insert(current.clone());

            if let Some(deps) = self.dependents.get(&current) {
                for dep in deps {
                    if !subgraph_nodes.contains(dep) {
                        queue.push(dep.clone());
                    }
                }
            }
        }

        // Now do topological sort on this subgraph
        let mut result = Vec::new();
        let mut visited = HashSet::new();
        let mut path = Vec::new();
        let mut in_path = HashSet::new();

        for node in &subgraph_nodes {
            if !visited.contains(node) {
                if let Some(error) = self.dfs(
                    node,
                    &mut result,
                    &mut visited,
                    &mut path,
                    &mut in_path,
                    &subgraph_nodes,
                ) {
                    return Err(error);
                }
            }
        }

        Ok(result)
    }

    /// Clear all dependencies that matches (node -> _).
    pub fn clear_dependencies(&mut self, node: &str) {
        if let Some(deps) = self.dependencies.get(node) {
            let deps_clone = deps.clone();
            for dep in deps_clone {
                if let Some(dep_dependents) = self.dependents.get_mut(&dep) {
                    dep_dependents.remove(node);
                }
            }
            self.dependencies.get_mut(node).unwrap().clear();
        }
    }

    fn dfs(
        &self,
        node: &str,
        result: &mut Vec<String>,
        visited: &mut HashSet<String>,
        path: &mut Vec<String>,
        in_path: &mut HashSet<String>,
        subgraph_nodes: &HashSet<String>,
    ) -> Option<CycleError> {
        if !subgraph_nodes.contains(node) {
            return None;
        }

        if in_path.contains(node) {
            // Found a cycle - extract the cycle from path
            let cycle_start = path.iter().position(|x| x == node).unwrap();
            let mut cycle = path[cycle_start..].to_vec();
            cycle.sort();
            return Some(CycleError::new(cycle));
        }

        if visited.contains(node) {
            return None;
        }

        path.push(node.to_string());
        in_path.insert(node.to_string());

        if let Some(deps) = self.dependencies.get(node) {
            for dep in deps {
                if subgraph_nodes.contains(dep) {
                    if let Some(error) =
                        self.dfs(dep, result, visited, path, in_path, subgraph_nodes)
                    {
                        return Some(error);
                    }
                }
            }
        }

        path.pop();
        in_path.remove(node);
        visited.insert(node.to_string());
        result.push(node.to_string());

        None
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
    fn test_basic_sorting_and_cycle_detection() {
        let mut toposorter = TopoSorter::new();
        toposorter.add_dependency("a", "b");
        toposorter.add_dependency("b", "c");

        // Should sort successfully
        assert_eq!(toposorter.sort().unwrap(), vec!["c", "b", "a"]);

        // Should still sort the same way
        assert_eq!(toposorter.sort().unwrap(), vec!["c", "b", "a"]);

        // Add a cycle
        toposorter.add_dependency("c", "a");

        // Should detect cycle
        let result = toposorter.sort();
        assert!(result.is_err());
        let cycle_error = result.unwrap_err();
        assert_eq!(cycle_error.cycle, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_cycle_detection() {
        let mut toposorter = TopoSorter::new();
        toposorter.add_dependency("a", "b");
        toposorter.add_dependency("b", "c");
        toposorter.add_dependency("c", "a");

        let result = toposorter.sort();
        assert!(result.is_err());
        let cycle_error = result.unwrap_err();
        assert_eq!(cycle_error.cycle, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_self_cycle() {
        let mut toposorter = TopoSorter::new();
        toposorter.add_dependency("a", "a");

        let result = toposorter.sort();
        assert!(result.is_err());
        let cycle_error = result.unwrap_err();
        assert_eq!(cycle_error.cycle, vec!["a"]);
    }

    #[test]
    fn test_subgraph_sorting_and_dependencies() {
        let mut toposorter = TopoSorter::new();
        toposorter.add_dependency("a", "b");
        toposorter.add_dependency("b", "c");
        toposorter.add_dependency("c", "d");
        toposorter.add_dependency("d", "e");

        // Test subgraph sorting
        assert_eq!(toposorter.sort_subgraph("b").unwrap(), vec!["b", "a"]);
        assert_eq!(toposorter.sort_subgraph("c").unwrap(), vec!["c", "b", "a"]);
        assert_eq!(
            toposorter.sort_subgraph("d").unwrap(),
            vec!["d", "c", "b", "a"]
        );
        assert_eq!(
            toposorter.sort_subgraph("e").unwrap(),
            vec!["e", "d", "c", "b", "a"]
        );

        // Full sort
        assert_eq!(toposorter.sort().unwrap(), vec!["e", "d", "c", "b", "a"]);

        // Clear dependencies and modify
        toposorter.clear_dependencies("d");
        toposorter.add_dependency("c", "e");
        toposorter.add_dependency("e", "d");

        // Test after modification
        assert_eq!(toposorter.sort().unwrap(), vec!["d", "e", "c", "b", "a"]);
        assert_eq!(
            toposorter.sort_subgraph("e").unwrap(),
            vec!["e", "c", "b", "a"]
        );
    }
}
