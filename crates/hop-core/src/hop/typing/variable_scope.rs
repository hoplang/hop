use std::collections::HashMap;
use std::fmt;

use super::r#type::Type;
use super::type_env::FunctionSignature;
use super::type_error::{TypeError, TypeErrorKind};
use crate::document::{CheapString, DocumentRange};
use crate::symbols::var_name::VarName;

/// The VariableScope tracks the names in scope.
#[derive(Debug, Clone)]
pub struct VariableScope {
    entries: HashMap<CheapString, ScopeEntry>,
    operations: Vec<VarName>,
    /// The loop and lambda bodies around the code being checked, outermost
    /// first.
    bodies: Vec<Body>,
    context: Context,
}

/// A loop or lambda body around the code being checked.
#[derive(Debug, Clone)]
enum Body {
    Loop,
    Lambda(Lambda),
}

/// What the modifier of a lambda lets its body do with the cells bound
/// outside it.
#[derive(Debug, Clone)]
enum Lambda {
    /// A render or a mut lambda, which may read them.
    Reads,
    /// A plain lambda, which may not: it would read the value the render
    /// gave, even when a mut fn applies it after a write.
    Plain,
    /// A lambda whose modifier waits on the type of its body. It renders if
    /// its body renders Html, and is plain otherwise. It holds the errors of
    /// what needs it to render, and the reads of cells, each with the number
    /// of bodies around the binding of the cell it reads.
    Undecided {
        needs_render: Vec<TypeError>,
        reads: Vec<(TypeError, usize)>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Context {
    Render,
    Outside(Position),
    /// The body of an undecided lambda, which holds what needs it to render.
    Undecided,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Position {
    PureFn,
    ReadFn,
    MutFn,
    InitialValue,
    Key,
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

/// A variable in scope: its type, the range where it was bound, whether
/// it has been accessed, and whether it is a cell.
#[derive(Debug, Clone)]
pub struct VariableScopeEntry {
    pub typ: Type,
    pub range: DocumentRange,
    pub accessed: bool,
    pub cell: bool,
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
            context: Context::Outside(Position::PureFn),
        }
    }

    pub fn mutates(&self) -> bool {
        matches!(self.context, Context::Outside(Position::MutFn))
    }

    pub fn set_context(&mut self, context: Context) -> Context {
        std::mem::replace(&mut self.context, context)
    }

    pub fn require(
        &mut self,
        allowed: impl FnOnce(Position) -> bool,
        error: impl FnOnce(Position) -> TypeErrorKind,
        range: &DocumentRange,
        errors: &mut Vec<TypeError>,
    ) {
        match self.context {
            Context::Render => {}
            Context::Outside(position) if allowed(position) => {}
            Context::Outside(position) => {
                errors.push(TypeError::new(error(position), range.clone()));
            }
            Context::Undecided => {
                // A loop body may lie between the code and its lambda.
                let Some(Body::Lambda(Lambda::Undecided { needs_render, .. })) = self
                    .bodies
                    .iter_mut()
                    .rev()
                    .find(|body| matches!(body, Body::Lambda(_)))
                else {
                    unreachable!("an undecided context is the body of an undecided lambda");
                };
                needs_render.push(TypeError::new(error(Position::PureFn), range.clone()));
            }
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
        self.bind(name, typ, range, false)
    }

    pub fn bind(
        &mut self,
        name: VarName,
        typ: Type,
        range: DocumentRange,
        cell: bool,
    ) -> Result<(), ()> {
        if self.entries.contains_key(name.as_str()) {
            return Err(());
        }
        self.entries.insert(
            name.to_cheap_string(),
            ScopeEntry::Variable(VariableScopeEntry {
                typ,
                range,
                accessed: false,
                cell,
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

    /// Check the code from here up to the matching `exit_loop` as a loop
    /// body, which runs any number of times.
    pub fn enter_loop(&mut self) {
        self.bodies.push(Body::Loop);
    }

    pub fn exit_loop(&mut self) {
        let Some(Body::Loop) = self.bodies.pop() else {
            panic!("exit_loop without a matching enter_loop");
        };
    }

    /// Check the code from here up to the matching `exit_lambda` as the body
    /// of a lambda, which runs any number of times, in `context`. Returns
    /// the context around the lambda.
    pub fn enter_lambda(&mut self, context: Context) -> Context {
        self.bodies.push(Body::Lambda(match context {
            Context::Render | Context::Outside(Position::ReadFn | Position::MutFn) => Lambda::Reads,
            Context::Outside(_) => Lambda::Plain,
            Context::Undecided => Lambda::Undecided {
                needs_render: Vec::new(),
                reads: Vec::new(),
            },
        }));
        self.set_context(context)
    }

    /// Leave the body of a lambda, back to the context `outer` around it.
    /// `renders_html` is whether the body renders Html, or None if it did
    /// not typecheck. An undecided lambda whose body does not render Html is
    /// plain, which makes the errors it holds errors.
    pub fn exit_lambda(
        &mut self,
        outer: Context,
        renders_html: Option<bool>,
        errors: &mut Vec<TypeError>,
    ) {
        self.context = outer;
        let Some(Body::Lambda(lambda)) = self.bodies.pop() else {
            panic!("exit_lambda without a matching enter_lambda");
        };
        let Lambda::Undecided {
            needs_render,
            reads,
        } = lambda
        else {
            return;
        };
        match renders_html {
            // A read may still cross a lambda further out that does not
            // render.
            Some(true) => {
                for (error, bodies) in reads {
                    self.read_cell_from(bodies, error, errors);
                }
            }
            Some(false) => {
                errors.extend(needs_render);
                errors.extend(reads.into_iter().map(|(error, _)| error));
            }
            None => {}
        }
    }

    /// Record a read of the cell bound to `name` at `range`. Only a render
    /// or a mut lambda may read a cell bound outside it, so the read is an
    /// error if a plain lambda lies between it and the binding.
    pub fn read_cell(
        &mut self,
        name: &VarName,
        range: &DocumentRange,
        errors: &mut Vec<TypeError>,
    ) {
        let Some(ScopeEntry::Variable(entry)) = self.entries.get(name.as_str()) else {
            unreachable!("{name} is not a variable");
        };
        assert!(entry.cell, "{name} is not a cell");
        let bodies = entry.bodies;
        let error = TypeError::new(TypeErrorKind::MutableCaptureInPlainFn, range.clone());
        self.read_cell_from(bodies, error, errors);
    }

    /// Check a read of a cell bound inside `bodies` loop and lambda bodies
    /// against the lambdas between it and the binding, innermost first.
    /// An undecided lambda holds the read, and checks it against the lambdas
    /// further out once it is known to render.
    fn read_cell_from(&mut self, bodies: usize, error: TypeError, errors: &mut Vec<TypeError>) {
        for body in self.bodies[bodies..].iter_mut().rev() {
            match body {
                Body::Loop | Body::Lambda(Lambda::Reads) => {}
                Body::Lambda(Lambda::Plain) => {
                    errors.push(error);
                    return;
                }
                Body::Lambda(Lambda::Undecided { reads, .. }) => {
                    reads.push((error, bodies));
                    return;
                }
            }
        }
    }

    /// The outermost loop or lambda body around the code being checked, in
    /// which a rest spread would pass on the Html of the parameters it adds
    /// in every run.
    pub fn outermost_body(&self) -> Option<RepeatedBody> {
        self.bodies.first().map(|body| match body {
            Body::Loop => RepeatedBody::Loop,
            Body::Lambda(_) => RepeatedBody::Lambda,
        })
    }

    /// Record a use of the Html value bound to `name` on the path being
    /// checked, or say how the use breaks the rule.
    pub fn use_html(&mut self, name: &VarName) -> Result<(), HtmlMisuse> {
        let Some(ScopeEntry::Variable(entry)) = self.entries.get_mut(name.as_str()) else {
            unreachable!("{name} is not a variable");
        };
        if let Some(body) = self.bodies.get(entry.bodies) {
            return Err(HtmlMisuse::Outside(match body {
                Body::Loop => RepeatedBody::Loop,
                Body::Lambda(_) => RepeatedBody::Lambda,
            }));
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
