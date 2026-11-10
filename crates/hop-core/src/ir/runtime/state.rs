use std::collections::BTreeMap;
use std::rc::Rc;

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
    pub elements: BTreeMap<Place, ElementIdentity>,
    pub node_counter: ElementIdentity,
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
    fn retain<T: Clone>(
        kept: &mut BTreeMap<Place, T>,
        old: &BTreeMap<Place, T>,
        place: &Place,
        fresh: impl FnOnce() -> T,
    ) -> T {
        let value = old.get(place).cloned().unwrap_or_else(fresh);
        let previous = kept.insert(place.clone(), value.clone());
        assert!(previous.is_none(), "two bindings at the same place");
        value
    }

    pub fn bind_cell(&mut self, old: &State<'a>, place: &Place, initial: &Value<'a>) {
        Self::retain(&mut self.cells, &old.cells, place, || initial.clone());
    }

    pub fn bind_element(&mut self, old: &State<'a>, place: &Place) -> ElementIdentity {
        let node_counter = &mut self.node_counter;
        Self::retain(&mut self.elements, &old.elements, place, || {
            *node_counter += 1;
            *node_counter
        })
    }

    #[cfg(test)]
    pub fn run(
        &mut self,
        effect: &Effect<'a>,
        functions: &'a [FlatFunction],
    ) -> Result<(), EvalError> {
        let (_, cells) = run_effect(effect, &self.cells, functions, &mut 0)?;
        self.cells = cells;
        Ok(())
    }
}

pub type ElementIdentity = usize;

pub type Place = Rc<[Step]>;

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
