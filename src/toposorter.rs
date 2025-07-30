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
    use simple_txtar::Archive;
    use std::{env, fs, path::PathBuf};

    #[test]
    fn test_toposorter() {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("test_data/toposorter.cases");
        
        let content = fs::read_to_string(&d).unwrap();
        let test_cases = parse_test_cases(&content);

        for (case_num, (txtar_content, line_number)) in test_cases.iter().enumerate() {
            let archive = Archive::from(txtar_content.clone());

            let input = archive.get("in").expect("Missing 'in' section in test case").content.trim();
            let expected = archive.get("out").expect("Missing 'out' section in test case").content.trim();
            
            let mut toposorter = TopoSorter::new();
            let mut lines: Vec<String> = Vec::new();
            
            println!("Test case {} (line {})", case_num + 1, line_number);

            for line in input.split('\n') {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }

                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.is_empty() {
                    continue;
                }

                match parts[0] {
                    "add_node" => {
                        assert_eq!(parts.len(), 2, "add_node expects 1 argument");
                        toposorter.add_node(parts[1].to_string());
                    }
                    "add_dependency" => {
                        assert_eq!(parts.len(), 3, "add_dependency expects 2 arguments");
                        toposorter.add_dependency(parts[1], parts[2]);
                    }
                    "sort" => {
                        assert_eq!(parts.len(), 1, "sort expects no arguments");
                        match toposorter.sort() {
                            Ok(sorted) => {
                                lines.push(format!("sorted: {}", sorted.join(" ")));
                            }
                            Err(cycle_error) => {
                                lines.push(format!("cycle: {}", cycle_error.cycle.join(" ")));
                            }
                        }
                    }
                    "sort_subgraph" => {
                        assert_eq!(parts.len(), 2, "sort_subgraph expects 1 argument");
                        match toposorter.sort_subgraph(parts[1]) {
                            Ok(sorted) => {
                                lines.push(format!("subgraph: {}", sorted.join(" ")));
                            }
                            Err(cycle_error) => {
                                lines.push(format!("cycle: {}", cycle_error.cycle.join(" ")));
                            }
                        }
                    }
                    "clear_dependencies" => {
                        assert_eq!(parts.len(), 2, "clear_dependencies expects 1 argument");
                        toposorter.clear_dependencies(parts[1]);
                    }
                    _ => panic!("Unknown command: {}", parts[0]),
                }
            }

            let output = lines.join("\n");
            assert_eq!(output, expected, "Mismatch in test case {} (line {})", case_num + 1, line_number);
        }
    }

    fn parse_test_cases(content: &str) -> Vec<(String, usize)> {
        let mut test_cases = Vec::new();
        let mut current_case = String::new();
        let mut in_case = false;
        let mut case_start_line = 0;

        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;
            
            if line == "## BEGIN" {
                assert!(!in_case, "Found '## BEGIN' at line {} while already inside a test case", line_number);
                in_case = true;
                case_start_line = line_number;
                current_case.clear();
            } else if line == "## END" {
                assert!(in_case, "Found '## END' at line {} without matching '## BEGIN'", line_number);
                test_cases.push((current_case.clone(), case_start_line));
                in_case = false;
            } else if in_case {
                if !current_case.is_empty() {
                    current_case.push('\n');
                }
                current_case.push_str(line);
            }
        }

        assert!(!in_case, "Reached end of file while inside a test case (missing '## END')");

        test_cases
    }
}
