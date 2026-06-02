//! A scoped variable tracker for tracking variable bindings during compilation.
//!
//! [`VariableScope`] provides push/pop semantics for nested scopes, tracks whether
//! variables are accessed, and generates fresh variable names.

use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use crate::symbols::var_name::VarName;

/// Counter for generating fresh variable names like "v_0", "v_1", etc.
#[derive(Debug, Clone)]
pub struct FreshVarCounter {
    counter: usize,
}

impl FreshVarCounter {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    /// Generate a fresh variable name.
    /// Returns names like "v_0", "v_1", "v_2", etc.
    pub fn fresh_var(&mut self) -> VarName {
        let name = format!("v_{}", self.counter);
        self.counter += 1;
        VarName::new(&name).unwrap()
    }
}

impl Default for FreshVarCounter {
    fn default() -> Self {
        Self::new()
    }
}

/// The VariableScope tracks variables in scope.
#[derive(Debug, Clone)]
pub struct VariableScope<K, V> {
    entries: HashMap<K, VariableScopeEntry<V>>,
    operations: Vec<K>,
    fresh_vars: FreshVarCounter,
}

/// VariableScope entry that holds both value and a boolean indicating
/// whether the variable has been accessed.
#[derive(Debug, Clone)]
struct VariableScopeEntry<V> {
    value: V,
    accessed: bool,
}

impl<K: Hash + Eq + Clone, V> VariableScope<K, V> {
    pub fn new() -> Self {
        VariableScope {
            entries: HashMap::new(),
            operations: Vec::new(),
            fresh_vars: FreshVarCounter::new(),
        }
    }

    /// Generate a fresh variable name.
    /// Returns names like "v_0", "v_1", "v_2", etc.
    pub fn fresh_var(&mut self) -> VarName {
        self.fresh_vars.fresh_var()
    }

    /// Returns a mutable reference to the fresh variable counter.
    pub fn fresh_var_counter(&mut self) -> &mut FreshVarCounter {
        &mut self.fresh_vars
    }

    /// Bind the key to the given value in the environment.
    ///
    /// Returns an error if the variable is already defined.
    pub fn push(&mut self, key: K, value: V) -> Result<(), ()> {
        if self.entries.contains_key(&key) {
            return Err(());
        }
        self.entries.insert(
            key.clone(),
            VariableScopeEntry {
                value,
                accessed: false,
            },
        );
        self.operations.push(key);
        Ok(())
    }

    /// Undo the latest push operation.
    ///
    /// Returns a bool indicating whether the variable has been accessed.
    pub fn pop(&mut self) -> (K, V, bool) {
        let key = self
            .operations
            .pop()
            .expect("Tried to pop from empty variable scope");
        self.entries
            .remove(&key)
            .map(|entry| (key, entry.value, entry.accessed))
            .unwrap()
    }

    /// Check whether a key has been accessed via lookup.
    pub fn has_been_accessed<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.entries.get(key).is_some_and(|entry| entry.accessed)
    }

    /// Access the value behind a key in the environment.
    pub fn lookup<Q>(&mut self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        if let Some(entry) = self.entries.get_mut(key) {
            entry.accessed = true;
            Some(&entry.value)
        } else {
            None
        }
    }
}

impl<K: Hash + Eq + Clone, V> Default for VariableScope<K, V> {
    fn default() -> Self {
        Self::new()
    }
}
