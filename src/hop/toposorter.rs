use std::collections::{HashMap, HashSet};

/// The TopoSorter performs topological sorting of directed graphs.
#[derive(Default, Debug, Clone)]
pub struct TopoSorter {
    reverse_dependencies: HashMap<String, HashSet<String>>,
}

impl TopoSorter {
    /// Update the state of a given node in the graph by adding it to the
    /// graph if it doesn't exist and updating its dependencies.
    ///
    /// Returns the strongly connected components of the transitive closure
    /// of the dependents of the given node, topologically sorted.
    ///
    /// Time complexity: O(V + E)
    pub fn update_node(&mut self, node: &str, dependencies: HashSet<String>) -> Vec<Vec<String>> {
        for reverse_deps in self.reverse_dependencies.values_mut() {
            reverse_deps.remove(node);
        }

        for dep in &dependencies {
            self.reverse_dependencies
                .entry(dep.to_string())
                .or_default()
                .insert(node.to_string());
        }

        compute_scc(node, &self.reverse_dependencies)
    }
}

/// Compute strongly connected components using Tarjan's algorithm starting
/// from a given node.
///
/// Returns a vector of SCCs, where each SCC is a vector of node names.
/// The SCCs are returned in topological order.
///
/// Time complexity: O(V + E) where V is the number of nodes
/// and E is the number of edges in the graph.
fn compute_scc(node: &str, links: &HashMap<String, HashSet<String>>) -> Vec<Vec<String>> {
    let mut index_counter = 0;
    let mut stack = Vec::new();
    let mut lowlinks = HashMap::new();
    let mut index = HashMap::new();
    let mut on_stack = HashSet::new();
    let mut sccs = Vec::new();

    tarjan_scc(
        node,
        links,
        &mut index_counter,
        &mut stack,
        &mut lowlinks,
        &mut index,
        &mut on_stack,
        &mut sccs,
    );

    sccs.reverse();

    sccs
}

fn tarjan_scc(
    node: &str,
    links: &HashMap<String, HashSet<String>>,
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

    if let Some(deps) = links.get(node) {
        for dep in deps {
            if !index.contains_key(dep) {
                tarjan_scc(
                    dep,
                    links,
                    index_counter,
                    stack,
                    lowlinks,
                    index,
                    on_stack,
                    sccs,
                );
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
    fn test_simple() {
        let mut ts = TopoSorter::default();
        // a → b → c
        assert_eq!(ts.update_node("a", set(&["b"])), vec![vec!["a"]]);
        assert_eq!(ts.update_node("b", set(&["c"])), vec![vec!["b"], vec!["a"]]);
    }

    #[test]
    fn test_with_cycle() {
        let mut ts = TopoSorter::default();
        // a → b
        //  ↖ ↙
        //   c
        assert_eq!(ts.update_node("a", set(&["b"])), vec![vec!["a"]]);
        assert_eq!(ts.update_node("b", set(&["c"])), vec![vec!["b"], vec!["a"]]);
        assert_eq!(ts.update_node("c", set(&["a"])), vec![vec!["a", "b", "c"]]);
    }

    #[test]
    fn test_multiple_components() {
        let mut ts = TopoSorter::default();
        // a ⇄ b
        //     ↓
        // d ⇄ c
        assert_eq!(ts.update_node("a", set(&["b"])), vec![vec!["a"]]);
        assert_eq!(ts.update_node("b", set(&["a", "c"])), vec![vec!["a", "b"]]);
        assert_eq!(
            ts.update_node("c", set(&["d"])),
            vec![vec!["c"], vec!["a", "b"]]
        );
        assert_eq!(
            ts.update_node("d", set(&["c"])),
            vec![vec!["c", "d"], vec!["a", "b"]]
        );
    }

    #[test]
    fn test_scc_disconnected_graph() {
        let mut ts = TopoSorter::default();
        // a   b   c
        assert_eq!(ts.update_node("a", set(&[])), vec![vec!["a"]]);
        assert_eq!(ts.update_node("b", set(&[])), vec![vec!["b"]]);
        assert_eq!(ts.update_node("c", set(&[])), vec![vec!["c"]]);
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
    }
}
