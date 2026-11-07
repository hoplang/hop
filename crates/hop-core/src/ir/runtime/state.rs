use std::collections::BTreeMap;
use std::rc::Rc;

use rpds::Vector;

use crate::ir::runtime::value::Value;
use crate::ir::value_id::ValueId;

#[derive(Debug, Clone, Default)]
pub struct State<'a> {
    pub cells: BTreeMap<Place, Value<'a>>,
    pub elements: BTreeMap<Place, NodeId>,
    pub node_counter: NodeId,
}

#[cfg(test)]
impl PartialEq for State<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.cells.len() == other.cells.len()
            && (self.cells.iter().zip(&other.cells))
                .all(|((a_place, a), (b_place, b))| a_place == b_place && a.total_eq(b))
            && self.elements == other.elements
            && self.node_counter == other.node_counter
    }
}

#[cfg(test)]
impl Eq for State<'_> {}

impl<'a> State<'a> {
    pub fn bind_cell(&mut self, old: &State<'a>, place: &Place, initial: &Value<'a>) -> Value<'a> {
        let value = old.cells.get(place).unwrap_or(initial).clone();
        let previous = self.cells.insert(place.clone(), value.clone());
        assert!(previous.is_none(), "two cells are bound at the same place");
        value
    }

    pub fn bind_element(&mut self, old: &State<'a>, place: &Place) -> NodeId {
        let node = match old.elements.get(place) {
            Some(node) => *node,
            None => {
                self.node_counter += 1;
                self.node_counter
            }
        };
        let previous = self.elements.insert(place.clone(), node);
        assert!(
            previous.is_none(),
            "two elements are bound at the same place"
        );
        node
    }

    #[cfg(test)]
    pub fn apply_writes(&mut self, writes: &[(Place, Value<'a>)]) {
        for (place, value) in writes {
            *self.cells.get_mut(place).unwrap() = value.clone();
        }
    }
}

pub type NodeId = usize;

pub type Place = Vector<Step>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Step {
    Name(ValueId),
    Iteration(IterationKey, usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum IterationKey {
    Int(i32),
    String(Rc<str>),
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
