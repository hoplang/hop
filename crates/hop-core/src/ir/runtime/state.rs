use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;

#[derive(Debug, Clone)]
pub struct State<'a> {
    region_tree: Rc<BTreeMap<(RegionId, Step), RegionId>>,
    cells: Rc<BTreeMap<CellId, Value<'a>>>,
    next_region: RegionId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Step {
    Call(ValueId),
    Arm(ValueId, usize),
    Iteration(ValueId, IterationKey, usize),
    Apply(ValueId),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IterationKey {
    Int(i32),
    String(Rc<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellId {
    region: RegionId,
    var: ValueId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElementId {
    region: RegionId,
    var: ValueId,
}

pub struct MutationTransaction<'a> {
    old: State<'a>,
    writes: BTreeMap<CellId, Value<'a>>,
}

pub struct RenderTransaction<'a> {
    old: State<'a>,
    stack: Vec<RegionId>,
    region_tree: BTreeMap<(RegionId, Step), RegionId>,
    cells: BTreeMap<CellId, Value<'a>>,
    next_region: RegionId,
}

impl RegionId {
    const ROOT: RegionId = RegionId(0);
}

impl<'a> State<'a> {
    #[cfg_attr(not(test), allow(dead_code))]
    pub fn begin_mutations(&self) -> MutationTransaction<'a> {
        MutationTransaction {
            old: self.clone(),
            writes: BTreeMap::new(),
        }
    }

    pub fn begin_render(&self) -> RenderTransaction<'a> {
        RenderTransaction {
            old: self.clone(),
            stack: vec![RegionId::ROOT],
            region_tree: BTreeMap::new(),
            cells: BTreeMap::new(),
            next_region: self.next_region,
        }
    }
}

impl<'a> Default for State<'a> {
    fn default() -> State<'a> {
        State {
            region_tree: Rc::new(BTreeMap::new()),
            cells: Rc::new(BTreeMap::new()),
            next_region: RegionId(RegionId::ROOT.0 + 1),
        }
    }
}

impl<'a> MutationTransaction<'a> {
    pub fn read(&self, cell: CellId) -> &Value<'a> {
        match self.writes.get(&cell) {
            Some(value) => value,
            None => (self.old.cells.get(&cell)).expect("a read of a cell that is not bound"),
        }
    }

    pub fn write(&mut self, cell: CellId, value: Value<'a>) {
        assert!(
            self.old.cells.contains_key(&cell),
            "a write to a cell that is not bound"
        );
        self.writes.insert(cell, value);
    }

    #[cfg_attr(not(test), allow(dead_code))]
    pub fn commit(self) -> State<'a> {
        let mut state = self.old;
        if !self.writes.is_empty() {
            Rc::make_mut(&mut state.cells).extend(self.writes);
        }
        state
    }
}

impl<'a> RenderTransaction<'a> {
    fn current(&self) -> RegionId {
        *self.stack.last().expect("a render is inside the root")
    }

    pub fn enter(&mut self, step: Step) {
        let key = (self.current(), step);
        let region = match self.old.region_tree.get(&key) {
            Some(region) => *region,
            None => {
                let region = self.next_region;
                self.next_region = RegionId(region.0 + 1);
                region
            }
        };
        let previous = self.region_tree.insert(key, region);
        assert!(previous.is_none(), "two regions at the same step");
        self.stack.push(region);
    }

    pub fn exit(&mut self) {
        self.stack.pop();
        assert!(!self.stack.is_empty(), "an exit of the root");
    }

    pub fn bind_cell<E>(
        &mut self,
        var: ValueId,
        initial: impl FnOnce(&Self) -> Result<Value<'a>, E>,
    ) -> Result<CellId, E> {
        let cell = CellId {
            region: self.current(),
            var,
        };
        let value = match self.old.cells.get(&cell) {
            Some(value) => value.clone(),
            None => initial(self)?,
        };
        let previous = self.cells.insert(cell, value);
        assert!(previous.is_none(), "two cells of the same binding");
        Ok(cell)
    }

    pub fn bind_element(&self, var: ValueId) -> ElementId {
        ElementId {
            region: self.current(),
            var,
        }
    }

    pub fn read(&self, cell: CellId) -> &Value<'a> {
        (self.cells.get(&cell)).expect("a render reads only cells it has bound")
    }

    pub fn commit(self) -> State<'a> {
        assert!(self.stack.len() == 1, "a render commits inside a region");
        State {
            region_tree: Rc::new(self.region_tree),
            cells: Rc::new(self.cells),
            next_region: self.next_region,
        }
    }
}

impl IterationKey {
    pub(super) fn from_value(value: Value<'_>) -> IterationKey {
        match value {
            Value::Int(i) => IterationKey::Int(i),
            Value::String(s) => IterationKey::String(s),
            _ => panic!("expected an Int or a String value, found {value:?}"),
        }
    }
}

#[cfg(test)]
mod test_support {
    use super::*;

    impl<'a> State<'a> {
        fn paths(&self) -> BTreeMap<RegionId, Vec<Step>> {
            let mut ordered: Vec<_> = (self.region_tree.iter())
                .map(|((parent, step), region)| (*region, *parent, step))
                .collect();
            ordered.sort_by_key(|(region, _, _)| *region);
            let mut paths = BTreeMap::from([(RegionId::ROOT, Vec::new())]);
            for (region, parent, step) in ordered {
                let mut path = paths[&parent].clone();
                path.push(step.clone());
                paths.insert(region, path);
            }
            paths
        }

        pub fn regions(&self) -> impl Iterator<Item = Vec<Step>> {
            let mut paths = self.paths();
            paths.remove(&RegionId::ROOT);
            paths.into_values()
        }

        pub fn cell_values(&self) -> impl Iterator<Item = (Vec<Step>, ValueId, &Value<'a>)> {
            let paths = self.paths();
            (self.cells.iter())
                .map(move |(cell, value)| (paths[&cell.region].clone(), cell.var, value))
        }
    }

    impl PartialEq for State<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.region_tree == other.region_tree
                && self.cells.len() == other.cells.len()
                && (self.cells.iter().zip(other.cells.iter()))
                    .all(|((a_cell, a), (b_cell, b))| a_cell == b_cell && a.total_eq(b))
                && self.next_region == other.next_region
        }
    }

    impl Eq for State<'_> {}
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::convert::Infallible;

    use arbitrary::Unstructured;

    use super::*;
    use crate::ir::value_id::ValueIdCounter;

    struct Block {
        cells: Vec<ValueId>,
        elements: Vec<ValueId>,
        held: Vec<(Step, Block)>,
        matches: Vec<(ValueId, Vec<Block>)>,
        loops: Vec<(ValueId, Block)>,
    }

    type Path = (Vec<Step>, ValueId);

    #[derive(Default)]
    struct Bound {
        cells: Vec<(Path, CellId, i32)>,
        elements: Vec<(Path, ElementId)>,
    }

    fn random_block(
        u: &mut Unstructured<'_>,
        names: &mut ValueIdCounter,
        depth: usize,
    ) -> arbitrary::Result<Block> {
        let mut block = Block {
            cells: Vec::new(),
            elements: Vec::new(),
            held: Vec::new(),
            matches: Vec::new(),
            loops: Vec::new(),
        };
        for _ in 0..u.int_in_range(0..=2)? {
            block.cells.push(names.next());
        }
        for _ in 0..u.int_in_range(0..=2)? {
            block.elements.push(names.next());
        }
        if depth == 0 {
            return Ok(block);
        }
        if u.arbitrary()? {
            let step = Step::Call(names.next());
            (block.held).push((step, random_block(u, names, depth - 1)?));
        }
        if u.arbitrary()? {
            let step = Step::Apply(names.next());
            (block.held).push((step, random_block(u, names, depth - 1)?));
        }
        if u.arbitrary()? {
            let arms = vec![
                random_block(u, names, depth - 1)?,
                random_block(u, names, depth - 1)?,
            ];
            block.matches.push((names.next(), arms));
        }
        if u.arbitrary()? {
            (block.loops).push((names.next(), random_block(u, names, depth - 1)?));
        }
        Ok(block)
    }

    fn render_block(
        block: &Block,
        u: &mut Unstructured<'_>,
        render: &mut RenderTransaction<'_>,
        path: &mut Vec<Step>,
        bound: &mut Bound,
    ) -> arbitrary::Result<()> {
        for var in &block.cells {
            let initial = u.int_in_range(0..=9)?;
            let Ok(cell) = render.bind_cell(*var, |_| Ok::<_, Infallible>(Value::Int(initial)));
            bound.cells.push(((path.clone(), *var), cell, initial));
        }
        for var in &block.elements {
            let element = render.bind_element(*var);
            bound.elements.push(((path.clone(), *var), element));
        }
        let mut held: Vec<(Step, &Block)> = (block.held.iter())
            .map(|(step, block)| (step.clone(), block))
            .collect();
        for (var, arms) in &block.matches {
            let arm = u.choose_index(arms.len())?;
            held.push((Step::Arm(*var, arm), &arms[arm]));
        }
        for (var, body) in &block.loops {
            let mut occurrences = [0; 3];
            for _ in 0..u.int_in_range(0..=3)? {
                let key = u.int_in_range(0..=2)?;
                let occurrence = &mut occurrences[key as usize];
                held.push((
                    Step::Iteration(*var, IterationKey::Int(key), *occurrence),
                    body,
                ));
                *occurrence += 1;
            }
        }
        for (step, block) in held {
            render.enter(step.clone());
            path.push(step);
            render_block(block, u, render, path, bound)?;
            path.pop();
            render.exit();
        }
        Ok(())
    }

    #[test]
    fn fuzz_cells_and_elements_live_as_long_as_their_paths_are_bound() {
        arbtest::arbtest(|u| {
            let block = random_block(u, &mut ValueIdCounter::new(), 3)?;
            let mut state = State::default();
            let mut values: BTreeMap<Path, i32> = BTreeMap::new();
            let mut cells: BTreeMap<Path, CellId> = BTreeMap::new();
            let mut elements: BTreeMap<Path, ElementId> = BTreeMap::new();
            let mut seen_cells = BTreeSet::new();
            let mut seen_elements = BTreeSet::new();
            for _ in 0..u.int_in_range(1..=8)? {
                let mut render = state.begin_render();
                let mut bound = Bound::default();
                render_block(&block, u, &mut render, &mut Vec::new(), &mut bound)?;

                let mut next_values = BTreeMap::new();
                let mut next_cells = BTreeMap::new();
                for (path, cell, initial) in bound.cells {
                    match cells.get(&path) {
                        Some(before) => assert_eq!(cell, *before),
                        None => assert!(seen_cells.insert(cell), "{cell:?} is used again"),
                    }
                    let value = values.get(&path).copied().unwrap_or(initial);
                    assert!(matches!(render.read(cell), Value::Int(read) if *read == value));
                    next_values.insert(path.clone(), value);
                    next_cells.insert(path, cell);
                }
                let mut next_elements = BTreeMap::new();
                for (path, element) in bound.elements {
                    match elements.get(&path) {
                        Some(before) => assert_eq!(element, *before),
                        None => {
                            assert!(seen_elements.insert(element), "{element:?} is used again");
                        }
                    }
                    next_elements.insert(path, element);
                }
                state = render.commit();
                (values, cells, elements) = (next_values, next_cells, next_elements);

                let mut mutations = state.begin_mutations();
                for (path, cell) in &cells {
                    if u.arbitrary()? {
                        let value = u.int_in_range(10..=19)?;
                        mutations.write(*cell, Value::Int(value));
                        values.insert(path.clone(), value);
                    }
                }
                for (path, cell) in &cells {
                    let value = values[path];
                    assert!(matches!(mutations.read(*cell), Value::Int(read) if *read == value));
                }
                state = mutations.commit();
            }
            Ok(())
        });
    }
}
