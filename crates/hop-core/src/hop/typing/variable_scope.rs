use std::collections::HashMap;
use std::fmt;

use super::r#type::Type;
use super::type_env::FunctionSignature;
use crate::document::{CheapString, DocumentRange};
use crate::symbols::var_name::VarName;

/// The VariableScope tracks the names in scope.
#[derive(Debug, Clone)]
pub struct VariableScope {
    entries: HashMap<CheapString, ScopeEntry>,
    operations: Vec<VarName>,
    /// The loop and lambda bodies around the code being checked, outermost
    /// first.
    bodies: Vec<RepeatedBody>,
}

/// A body that runs any number of times for each run of the code around
/// it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepeatedBody {
    Loop,
    Lambda,
}

impl fmt::Display for RepeatedBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RepeatedBody::Loop => write!(f, "loop"),
            RepeatedBody::Lambda => write!(f, "lambda"),
        }
    }
}

/// What a name in scope refers to.
#[derive(Debug, Clone)]
pub enum ScopeEntry {
    Variable(VariableScopeEntry),
    Function(FunctionScopeEntry),
}

/// A variable in scope: its type, the range where it was bound, and
/// whether it has been accessed.
#[derive(Debug, Clone)]
pub struct VariableScopeEntry {
    pub typ: Type,
    pub range: DocumentRange,
    pub accessed: bool,
    /// The number of loop and lambda bodies around the binding.
    bodies: usize,
    /// Whether the code checked so far uses the Html value on the path
    /// being checked.
    used: bool,
}

/// A function the module declares or imports, and the range of its
/// declaration.
#[derive(Debug, Clone)]
pub struct FunctionScopeEntry {
    /// None when the signature did not resolve, which was reported at the
    /// declaration.
    pub signature: Option<FunctionSignature>,
    pub range: DocumentRange,
}

/// How a use of an Html value breaks the rule that it is used at most once
/// on every path, and never in a loop or lambda body it is bound outside of.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HtmlMisuse {
    /// The path already uses the value.
    Again,
    /// The use is in a body the value is bound outside of, so it would be
    /// used in every run of the body.
    Outside(RepeatedBody),
}

impl VariableScope {
    pub fn new() -> Self {
        VariableScope {
            entries: HashMap::new(),
            operations: Vec::new(),
            bodies: Vec::new(),
        }
    }

    /// Bring a function into scope for every body of the module. Functions
    /// are declared before any variable is pushed, and never popped.
    ///
    /// Returns an error if the name is already defined.
    pub fn declare_function(
        &mut self,
        name: CheapString,
        signature: Option<FunctionSignature>,
        range: DocumentRange,
    ) -> Result<(), ()> {
        assert!(
            self.operations.is_empty(),
            "a function is declared beneath variables"
        );
        if self.entries.contains_key(&name) {
            return Err(());
        }
        self.entries.insert(
            name,
            ScopeEntry::Function(FunctionScopeEntry { signature, range }),
        );
        Ok(())
    }

    /// Bind the name to the given type in the environment.
    ///
    /// Returns an error if the name is already defined, as a variable or as
    /// a function.
    pub fn push(&mut self, name: VarName, typ: Type, range: DocumentRange) -> Result<(), ()> {
        if self.entries.contains_key(name.as_str()) {
            return Err(());
        }
        self.entries.insert(
            name.to_cheap_string(),
            ScopeEntry::Variable(VariableScopeEntry {
                typ,
                range,
                accessed: false,
                bodies: self.bodies.len(),
                used: false,
            }),
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
        match self.entries.remove(name.as_str()) {
            Some(ScopeEntry::Variable(entry)) => (name, entry),
            Some(ScopeEntry::Function(_)) | None => unreachable!("{name} was pushed as a variable"),
        }
    }

    /// Check the code from here up to the matching `exit` as a body that
    /// runs any number of times.
    pub fn enter(&mut self, body: RepeatedBody) {
        self.bodies.push(body);
    }

    pub fn exit(&mut self) {
        self.bodies.pop().expect("exit without a matching enter");
    }

    /// The outermost loop or lambda body around the code being checked, in
    /// which a rest spread would pass on the Html of the parameters it adds
    /// in every run.
    pub fn outermost_body(&self) -> Option<RepeatedBody> {
        self.bodies.first().copied()
    }

    /// Record a use of the Html value bound to `name` on the path being
    /// checked, or say how the use breaks the rule.
    pub fn use_html(&mut self, name: &VarName) -> Result<(), HtmlMisuse> {
        let Some(ScopeEntry::Variable(entry)) = self.entries.get_mut(name.as_str()) else {
            unreachable!("{name} is not a variable");
        };
        if let Some(body) = self.bodies.get(entry.bodies) {
            return Err(HtmlMisuse::Outside(*body));
        }
        if entry.used {
            return Err(HtmlMisuse::Again);
        }
        entry.used = true;
        Ok(())
    }

    /// The names whose Html values the path being checked uses.
    pub fn html_uses(&self) -> Vec<VarName> {
        self.operations
            .iter()
            .filter(|name| {
                matches!(&self.entries[name.as_str()], ScopeEntry::Variable(entry) if entry.used)
            })
            .cloned()
            .collect()
    }

    /// Make `uses` the names whose Html values the path being checked uses,
    /// as when checking an arm of a match from the uses before it.
    pub fn set_html_uses(&mut self, uses: &[VarName]) {
        for name in &self.operations {
            if let Some(ScopeEntry::Variable(entry)) = self.entries.get_mut(name.as_str()) {
                entry.used = uses.contains(name);
            }
        }
    }

    /// Access the entry behind a name in the environment.
    pub fn lookup(&mut self, name: &str) -> Option<&ScopeEntry> {
        let entry = self.entries.get_mut(name)?;
        if let ScopeEntry::Variable(variable) = entry {
            variable.accessed = true;
        }
        Some(entry)
    }
}

impl Default for VariableScope {
    fn default() -> Self {
        Self::new()
    }
}
