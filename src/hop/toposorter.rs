use std::collections::{HashMap, HashSet};

/// The TopoSorter performs topological sorting of directed graphs.
#[derive(Debug, Clone)]
pub struct TopoSorter<N> {
    reverse_dependencies: HashMap<N, HashSet<N>>,
}

impl<N> Default for TopoSorter<N>
where
    N: Eq + std::hash::Hash + Clone,
{
    fn default() -> Self {
        Self {
            reverse_dependencies: HashMap::new(),
        }
    }
}

impl<N> TopoSorter<N>
where
    N: Eq + std::hash::Hash + Clone + std::cmp::Ord,
{
    /// Update the state of a given node in the graph by adding it to the
    /// graph if it doesn't exist and updating its dependencies.
    ///
    /// Returns the strongly connected components of the transitive closure
    /// of the dependents of the given node, topologically sorted.
    ///
    /// Time complexity: O(V + E)
    pub fn update_node(&mut self, node: N, dependencies: HashSet<N>) -> Vec<Vec<N>> {
        for reverse_deps in self.reverse_dependencies.values_mut() {
            reverse_deps.remove(&node);
        }

        for dep in &dependencies {
            self.reverse_dependencies
                .entry(dep.clone())
                .or_insert_with(HashSet::new)
                .insert(node.clone());
        }

        compute_scc(&node, &self.reverse_dependencies)
    }
}

/// Compute strongly connected components using Tarjan's algorithm starting
/// from a given node.
///
/// Returns a vector of SCCs, where each SCC is a vector of nodes.
/// The SCCs are returned in topological order.
///
/// Time complexity: O(V + E) where V is the number of nodes
/// and E is the number of edges in the graph.
fn compute_scc<N>(node: &N, links: &HashMap<N, HashSet<N>>) -> Vec<Vec<N>>
where
    N: Eq + std::hash::Hash + Clone + std::cmp::Ord,
{
    let mut index_counter = 0;
    let mut stack: Vec<N> = Vec::new();
    let mut lowlinks: HashMap<N, usize> = HashMap::new();
    let mut index: HashMap<N, usize> = HashMap::new();
    let mut on_stack: HashSet<N> = HashSet::new();
    let mut sccs: Vec<Vec<N>> = Vec::new();

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

fn tarjan_scc<N>(
    node: &N,
    links: &HashMap<N, HashSet<N>>,
    index_counter: &mut usize,
    stack: &mut Vec<N>,
    lowlinks: &mut HashMap<N, usize>,
    index: &mut HashMap<N, usize>,
    on_stack: &mut HashSet<N>,
    sccs: &mut Vec<Vec<N>>,
) where
    N: Eq + std::hash::Hash + Clone + std::cmp::Ord,
{
    index.insert(node.clone(), *index_counter);
    lowlinks.insert(node.clone(), *index_counter);
    *index_counter += 1;
    stack.push(node.clone());
    on_stack.insert(node.clone());

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
                lowlinks.insert(node.clone(), node_lowlink.min(dep_lowlink));
            } else if on_stack.contains(dep) {
                let dep_index = *index.get(dep).unwrap();
                let node_lowlink = *lowlinks.get(node).unwrap();
                lowlinks.insert(node.clone(), node_lowlink.min(dep_index));
            }
        }
    }

    if lowlinks.get(node).unwrap() == index.get(node).unwrap() {
        let mut scc = Vec::new();
        loop {
            let w = stack.pop().unwrap();
            on_stack.remove(&w);
            scc.push(w.clone());
            if &w == node {
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
        let mut ts: TopoSorter<String> = TopoSorter::default();
        // a → b → c
        assert_eq!(
            ts.update_node("a".to_string(), set(&["b"])),
            vec![vec!["a".to_string()]]
        );
        assert_eq!(
            ts.update_node("b".to_string(), set(&["c"])),
            vec![vec!["b".to_string()], vec!["a".to_string()]]
        );
    }

    #[test]
    fn test_with_cycle() {
        let mut ts: TopoSorter<String> = TopoSorter::default();
        // a → b
        //  ↖ ↙
        //   c
        assert_eq!(
            ts.update_node("a".to_string(), set(&["b"])),
            vec![vec!["a".to_string()]]
        );
        assert_eq!(
            ts.update_node("b".to_string(), set(&["c"])),
            vec![vec!["b".to_string()], vec!["a".to_string()]]
        );
        assert_eq!(
            ts.update_node("c".to_string(), set(&["a"])),
            vec![vec!["a".to_string(), "b".to_string(), "c".to_string()]]
        );
    }

    #[test]
    fn test_multiple_components() {
        let mut ts: TopoSorter<String> = TopoSorter::default();
        // a ⇄ b
        //     ↓
        // d ⇄ c
        assert_eq!(
            ts.update_node("a".to_string(), set(&["b"])),
            vec![vec!["a".to_string()]]
        );
        assert_eq!(
            ts.update_node("b".to_string(), set(&["a", "c"])),
            vec![vec!["a".to_string(), "b".to_string()]]
        );
        assert_eq!(
            ts.update_node("c".to_string(), set(&["d"])),
            vec![
                vec!["c".to_string()],
                vec!["a".to_string(), "b".to_string()]
            ]
        );
        assert_eq!(
            ts.update_node("d".to_string(), set(&["c"])),
            vec![
                vec!["c".to_string(), "d".to_string()],
                vec!["a".to_string(), "b".to_string()]
            ]
        );
    }

    #[test]
    fn test_scc_disconnected_graph() {
        let mut ts: TopoSorter<String> = TopoSorter::default();
        // a   b   c
        assert_eq!(
            ts.update_node("a".to_string(), set(&[])),
            vec![vec!["a".to_string()]]
        );
        assert_eq!(
            ts.update_node("b".to_string(), set(&[])),
            vec![vec!["b".to_string()]]
        );
        assert_eq!(
            ts.update_node("c".to_string(), set(&[])),
            vec![vec!["c".to_string()]]
        );
    }

    #[test]
    fn test_scc_complex_graph() {
        let mut ts: TopoSorter<String> = TopoSorter::default();
        ts.update_node("1".to_string(), set(&["2"]));
        ts.update_node("2".to_string(), set(&["3"]));
        ts.update_node("3".to_string(), set(&["1"]));
        ts.update_node("4".to_string(), set(&["2", "3", "5"]));
        ts.update_node("5".to_string(), set(&["4", "6"]));
        ts.update_node("6".to_string(), set(&["3", "7"]));
        ts.update_node("7".to_string(), set(&["6"]));
        ts.update_node("8".to_string(), set(&["5", "7", "8"]));
    }
}
