use std::collections::HashMap;

use crate::ir::{runtime::value::Value, var_id::VarId};

/// Variable environment for the evaluator.
pub struct VariableEnv {
    map: HashMap<VarId, Value>,
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
    pub fn insert(&mut self, key: VarId, value: Value) {
        assert_eq!(self.map.insert(key, value), None);
    }

    /// Remove a binding from the environment.
    ///
    /// Panics if the binding does not exist in the environment.
    pub fn remove(&mut self, key: &VarId) {
        assert!(self.map.remove(key).is_some());
    }

    /// Get a value from the environment.
    ///
    /// Panics if the binding does not exist in the environment.
    pub fn get(&self, key: &VarId) -> &Value {
        self.map.get(key).unwrap()
    }
}
