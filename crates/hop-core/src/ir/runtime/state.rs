use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;

#[derive(Debug, Clone)]
pub struct State<'a> {
    cells: Rc<BTreeMap<Vec<Step>, CellId>>,
    store: Rc<BTreeMap<CellId, Value<'a>>>,
    elements: Rc<BTreeMap<Vec<Step>, ElementId>>,
    next_cell: CellId,
    next_element: ElementId,
}

/// The identity of a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CellId(usize);

/// The identity of an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElementId(usize);

pub struct EffectTransaction<'a> {
    old: State<'a>,
    writes: BTreeMap<CellId, Value<'a>>,
}

pub struct RenderTransaction<'a> {
    old: State<'a>,
    path: Vec<Step>,
    cells: BTreeMap<Vec<Step>, CellId>,
    store: BTreeMap<CellId, Value<'a>>,
    elements: BTreeMap<Vec<Step>, ElementId>,
    next_cell: CellId,
    next_element: ElementId,
}

/// A step of the path of a cell or an element: the site of a binding the
/// render evaluates inside of, by its name, or an iteration of a loop.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Step {
    Site(ValueId),
    Iteration(IterationKey, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IterationKey {
    Int(i32),
    String(Rc<str>),
}

impl<'a> State<'a> {
    #[cfg_attr(not(test), allow(dead_code))]
    pub fn begin_effects(&self) -> EffectTransaction<'a> {
        EffectTransaction {
            old: self.clone(),
            writes: BTreeMap::new(),
        }
    }

    pub fn begin_render(&self) -> RenderTransaction<'a> {
        RenderTransaction {
            old: self.clone(),
            path: Vec::new(),
            cells: BTreeMap::new(),
            store: BTreeMap::new(),
            next_cell: self.next_cell,
            elements: BTreeMap::new(),
            next_element: self.next_element,
        }
    }
}

impl<'a> Default for State<'a> {
    fn default() -> State<'a> {
        State {
            cells: Rc::new(BTreeMap::new()),
            store: Rc::new(BTreeMap::new()),
            next_cell: CellId(0),
            elements: Rc::new(BTreeMap::new()),
            next_element: ElementId(0),
        }
    }
}

impl<'a> EffectTransaction<'a> {
    pub fn read(&self, cell: CellId) -> &Value<'a> {
        match self.writes.get(&cell) {
            Some(value) => value,
            None => (self.old.store.get(&cell)).expect("a read of a cell that is not allocated"),
        }
    }

    pub fn write(&mut self, cell: CellId, value: Value<'a>) {
        assert!(
            self.old.store.contains_key(&cell),
            "a write to a cell that is not allocated"
        );
        self.writes.insert(cell, value);
    }

    #[cfg_attr(not(test), allow(dead_code))]
    pub fn commit(self) -> State<'a> {
        let mut state = self.old;
        if !self.writes.is_empty() {
            Rc::make_mut(&mut state.store).extend(self.writes);
        }
        state
    }
}

impl<'a> RenderTransaction<'a> {
    /// Enter the step from the path the render has reached.
    pub fn enter(&mut self, step: Step) {
        self.path.push(step);
    }

    /// Exit the step the render entered last.
    pub fn exit(&mut self) {
        self.path.pop().expect("an exit of a step that was entered");
    }

    pub fn allocate_cell(&mut self, initial: &Value<'a>) -> CellId {
        let cell = match self.old.cells.get(&self.path) {
            Some(cell) => *cell,
            None => {
                let cell = self.next_cell;
                self.next_cell = CellId(cell.0 + 1);
                cell
            }
        };
        let previous = self.cells.insert(self.path.clone(), cell);
        assert!(previous.is_none(), "two cells at the same path");
        let value = self.old.store.get(&cell).unwrap_or(initial).clone();
        self.store.insert(cell, value);
        cell
    }

    pub fn allocate_element(&mut self) -> ElementId {
        let element = match self.old.elements.get(&self.path) {
            Some(element) => *element,
            None => {
                let element = self.next_element;
                self.next_element = ElementId(element.0 + 1);
                element
            }
        };
        let previous = self.elements.insert(self.path.clone(), element);
        assert!(previous.is_none(), "two elements at the same path");
        element
    }

    pub fn read(&self, cell: CellId) -> &Value<'a> {
        (self.store.get(&cell)).expect("a render reads only cells it has allocated")
    }

    pub fn commit(self) -> State<'a> {
        assert!(self.path.is_empty(), "a render commits inside a step");
        State {
            cells: Rc::new(self.cells),
            store: Rc::new(self.store),
            next_cell: self.next_cell,
            elements: Rc::new(self.elements),
            next_element: self.next_element,
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
        /// The path of each cell of the state, with the value it holds,
        /// then the path of each element, with no value.
        pub fn entries(&self) -> impl Iterator<Item = (&[Step], Option<&Value<'a>>)> {
            let cells =
                (self.cells.iter()).map(|(path, cell)| (path.as_slice(), Some(&self.store[cell])));
            let elements = (self.elements.keys()).map(|path| (path.as_slice(), None));
            cells.chain(elements)
        }
    }

    impl PartialEq for State<'_> {
        fn eq(&self, other: &Self) -> bool {
            self.cells == other.cells
                && self.store.len() == other.store.len()
                && (self.store.iter().zip(other.store.iter()))
                    .all(|((a_cell, a), (b_cell, b))| a_cell == b_cell && a.total_eq(b))
                && self.next_cell == other.next_cell
                && self.elements == other.elements
                && self.next_element == other.next_element
        }
    }

    impl Eq for State<'_> {}
}
