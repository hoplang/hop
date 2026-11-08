use std::collections::BTreeMap;
use std::rc::Rc;

use rpds::Vector;

#[cfg(test)]
use crate::ir::flat_program::FlatFunction;
#[cfg(test)]
use crate::ir::runtime::eval_error::EvalError;
#[cfg(test)]
use crate::ir::runtime::flat_evaluator::run_effect;
#[cfg(test)]
use crate::ir::runtime::value::Effect;
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
    pub fn bind_cell(&mut self, old: &State<'a>, place: &Place, initial: &Value<'a>) {
        let value = old.cells.get(place).unwrap_or(initial).clone();
        let previous = self.cells.insert(place.clone(), value);
        assert!(previous.is_none(), "two cells are bound at the same place");
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

    /// Runs the effect, and writes the cells only if it succeeds.
    #[cfg(test)]
    pub fn run(
        &mut self,
        effect: &Effect<'a>,
        functions: &'a [FlatFunction],
    ) -> Result<(), EvalError> {
        let mut cells = self.cells.clone();
        run_effect(effect, &mut cells, functions, &mut 0)?;
        self.cells = cells;
        Ok(())
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
