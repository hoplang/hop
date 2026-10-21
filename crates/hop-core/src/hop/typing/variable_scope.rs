//! A scoped variable tracker for tracking variable bindings during compilation.
//!
//! [`VariableScope`] provides push/pop semantics for nested scopes and tracks whether
//! variables are accessed. It also tracks the uses of Html values, which are affine:
//! an Html value is used at most once on every path, and never in a loop it is bound
//! outside of.

use std::collections::HashMap;

use super::r#type::Type;
use crate::document::DocumentRange;
use crate::symbols::var_name::VarName;

/// The VariableScope tracks variables in scope.
#[derive(Debug, Clone)]
pub struct VariableScope {
    entries: HashMap<VarName, VariableScopeEntry>,
    operations: Vec<VarName>,
    /// The number of loop bodies around the code being checked.
    loops: usize,
}

/// A variable in scope: its type, the range where it was bound, and
/// whether it has been accessed.
#[derive(Debug, Clone)]
pub struct VariableScopeEntry {
    pub typ: Type,
    pub range: DocumentRange,
    pub accessed: bool,
    /// The number of loop bodies around the binding.
    loops: usize,
    /// Whether the code checked so far uses the Html value on the path
    /// being checked.
    used: bool,
}

/// How a use of an Html value breaks the rule that it is used at most once
/// on every path, and never in a loop it is bound outside of.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HtmlMisuse {
    /// The path already uses the value.
    Again,
    /// The use is in a loop body the value is bound outside of, so it would
    /// be used in every iteration.
    InLoop,
}

impl VariableScope {
    pub fn new() -> Self {
        VariableScope {
            entries: HashMap::new(),
            operations: Vec::new(),
            loops: 0,
        }
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
                loops: self.loops,
                used: false,
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

    /// Check the code from here up to the matching `exit_loop` as a loop
    /// body, which runs any number of times.
    pub fn enter_loop(&mut self) {
        self.loops += 1;
    }

    pub fn exit_loop(&mut self) {
        self.loops -= 1;
    }

    /// Record a use of the Html value bound to `name` on the path being
    /// checked, or say how the use breaks the rule.
    pub fn use_html(&mut self, name: &VarName) -> Result<(), HtmlMisuse> {
        let loops = self.loops;
        let entry = self.entries.get_mut(name).unwrap();
        if entry.loops < loops {
            return Err(HtmlMisuse::InLoop);
        }
        if entry.used {
            return Err(HtmlMisuse::Again);
        }
        entry.used = true;
        Ok(())
    }

    /// The names whose Html values the path being checked uses.
    pub fn html_uses(&self) -> Vec<VarName> {
        self.entries
            .iter()
            .filter(|(_, entry)| entry.used)
            .map(|(name, _)| name.clone())
            .collect()
    }

    /// Make `uses` the names whose Html values the path being checked uses,
    /// as when checking an arm of a match from the uses before it.
    pub fn set_html_uses(&mut self, uses: &[VarName]) {
        for (name, entry) in &mut self.entries {
            entry.used = uses.contains(name);
        }
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
