use std::collections::HashMap;

use crate::ir::{binder_id::BinderId, runtime::value::Value};

/// Variable environment for the evaluator.
pub struct VariableEnv {
    map: HashMap<BinderId, Value>,
}

impl VariableEnv {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    /// Insert a binding into the environment.
    ///
    /// Panics if the binding already exists in the environment.
    pub fn insert(&mut self, key: BinderId, value: Value) {
        assert_eq!(self.map.insert(key, value), None);
    }

    /// Remove a binding from the environment.
    ///
    /// Panics if the binding does not exist in the environment.
    pub fn remove(&mut self, key: &BinderId) {
        assert!(self.map.remove(key).is_some());
    }

    /// Get a value from the environment.
    ///
    /// Panics if the binding does not exist in the environment.
    pub fn get(&self, key: &BinderId) -> &Value {
        self.map.get(key).unwrap()
    }
}
