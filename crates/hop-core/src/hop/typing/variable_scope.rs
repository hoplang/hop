//! A scoped variable tracker for tracking variable bindings during compilation.
//!
//! [`VariableScope`] provides push/pop semantics for nested scopes, tracks whether
//! variables are accessed, and generates fresh variable names.

use std::collections::HashMap;

use super::r#type::Type;
use crate::document::DocumentRange;
use crate::symbols::var_name::VarName;

/// Counter for generating fresh variable names like "v__0", "v__1", etc.
#[derive(Debug, Clone)]
pub struct FreshVarCounter {
    counter: usize,
}

impl FreshVarCounter {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    /// Generate a fresh variable name.
    /// Returns names like "v__0", "v__1", "v__2", etc.
    pub fn fresh_var(&mut self) -> VarName {
        let name = VarName::internal("v", self.counter);
        self.counter += 1;
        name
    }
}

impl Default for FreshVarCounter {
    fn default() -> Self {
        Self::new()
    }
}

/// The VariableScope tracks variables in scope.
#[derive(Debug, Clone)]
pub struct VariableScope {
    entries: HashMap<VarName, VariableScopeEntry>,
    operations: Vec<VarName>,
    fresh_vars: FreshVarCounter,
}

/// A variable in scope: its type, the range where it was bound, and
/// whether it has been accessed.
#[derive(Debug, Clone)]
pub struct VariableScopeEntry {
    pub typ: Type,
    pub range: DocumentRange,
    pub accessed: bool,
}

impl VariableScope {
    pub fn new() -> Self {
        VariableScope {
            entries: HashMap::new(),
            operations: Vec::new(),
            fresh_vars: FreshVarCounter::new(),
        }
    }

    /// Returns a mutable reference to the fresh variable counter.
    pub fn fresh_var_counter(&mut self) -> &mut FreshVarCounter {
        &mut self.fresh_vars
    }

    /// Bind the name to the given type in the environment.
    ///
    /// Returns an error if the variable is already defined.
    pub fn push(&mut self, name: VarName, typ: Type, range: DocumentRange) -> Result<(), ()> {
        if self.entries.contains_key(&name) {
            return Err(());
        }
        self.entries.insert(
            name.clone(),
            VariableScopeEntry {
                typ,
                range,
                accessed: false,
            },
        );
        self.operations.push(name);
        Ok(())
    }

    /// Undo the latest push operation.
    pub fn pop(&mut self) -> (VarName, VariableScopeEntry) {
        let name = self
            .operations
            .pop()
            .expect("Tried to pop from empty variable scope");
        self.entries
            .remove(&name)
            .map(|entry| (name, entry))
            .unwrap()
    }

    /// Access the entry behind a name in the environment.
    pub fn lookup(&mut self, name: &VarName) -> Option<&VariableScopeEntry> {
        if let Some(entry) = self.entries.get_mut(name) {
            entry.accessed = true;
            Some(entry)
        } else {
            None
        }
    }
}

impl Default for VariableScope {
    fn default() -> Self {
        Self::new()
    }
}
