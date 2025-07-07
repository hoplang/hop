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

/// SortResult represents the result of a topological sort operation.
#[derive(Debug, Clone, PartialEq)]
pub struct SortResult {
    pub nodes: Vec<String>,
    pub error: Option<CycleError>,
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
    pub fn add_dependency(&mut self, a: String, b: String) {
        self.add_node(a.clone());
        self.add_node(b.clone());

        self.dependencies.get_mut(&a).unwrap().insert(b.clone());
        self.dependents.get_mut(&b).unwrap().insert(a);
    }

    /// Sort the nodes of the graph and return a SortResult where the nodes are in topological order
    /// or an empty vector and an error if the graph contains a cycle.
    pub fn sort(&self) -> SortResult {
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
                    return SortResult {
                        nodes: Vec::new(),
                        error: Some(error),
                    };
                }
            }
        }

        SortResult {
            nodes: result,
            error: None,
        }
    }

    /// Do a topological sort of the subgraph containing all nodes that depend on the given node.
    ///
    /// The result includes the node `root` (which will be the first node of the array) and all
    /// nodes that depend on `root` (directly or transitively).
    pub fn sort_subgraph(&self, root: &str) -> SortResult {
        if !self.nodes.contains(root) {
            return SortResult {
                nodes: Vec::new(),
                error: None,
            };
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
                    return SortResult {
                        nodes: Vec::new(),
                        error: Some(error),
                    };
                }
            }
        }

        SortResult {
            nodes: result,
            error: None,
        }
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
            let cycle = path[cycle_start..].to_vec();
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
