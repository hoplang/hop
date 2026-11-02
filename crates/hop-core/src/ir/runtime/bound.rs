use std::rc::Rc;

use crate::ir::runtime::value::Value;

#[derive(Debug)]
pub enum Bound<C> {
    Value(Value),
    Closure(Rc<C>),
}

impl<C> Clone for Bound<C> {
    fn clone(&self) -> Self {
        match self {
            Bound::Value(value) => Bound::Value(value.clone()),
            Bound::Closure(closure) => Bound::Closure(closure.clone()),
        }
    }
}

impl<C> Bound<C> {
    /// Panics if the name is bound to a closure.
    pub fn as_value(&self) -> &Value {
        match self {
            Bound::Value(value) => value,
            Bound::Closure(_) => panic!("Expected a value, found a closure"),
        }
    }

    /// Panics if the name is bound to a value.
    pub fn as_closure(&self) -> &C {
        match self {
            Bound::Closure(closure) => closure,
            Bound::Value(value) => panic!("Expected a closure, found {value:?}"),
        }
    }
}
