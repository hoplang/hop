//! The match of the Flat IR, which keeps the arms of the source.

use std::fmt::{self, Write};

use crate::display::{Indent, join, write_tuple};
use crate::hop::typing::{Constructor, Pattern, PatternField};
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_type::IrDataType;
use crate::ir::value_id::ValueId;

/// A pattern whose bindings bind names.
pub type IrPattern = Pattern<IrBinder<IrDataType>>;

/// A match as written in the source. The first arm whose pattern matches
/// the subject is taken, and its pattern binds the parts of the subject it
/// names. Every value of the subject's type matches some arm.
#[derive(Debug, Clone)]
pub struct IrMatch<Body> {
    pub subject: ValueId,
    pub arms: Vec<IrMatchArm<Body>>,
}

#[derive(Debug, Clone)]
pub struct IrMatchArm<Body> {
    pub pattern: IrPattern,
    pub body: Body,
}

impl<Body> IrMatch<Body> {
    /// The match head, then each arm as its pattern and its body on lines
    /// of their own. The body starts with a line break and is indented.
    pub fn write(
        &self,
        w: &mut dyn Write,
        write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
    ) -> fmt::Result {
        write!(w, "match {} {{", self.subject)?;
        {
            let mut arms = Indent::new(w);
            for arm in &self.arms {
                write!(arms, "\n{} => {{", arm.pattern)?;
                write_body(&arm.body, &mut Indent::new(&mut arms))?;
                arms.write_str("\n}")?;
            }
        }
        w.write_str("\n}")
    }
}

impl fmt::Display for PatternField<IrBinder<IrDataType>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.pattern {
            Pattern::Binding(binder) => write!(f, "{}@{binder}", self.name.as_str()),
            pattern => write!(f, "{}: {pattern}", self.name.as_str()),
        }
    }
}

/// The pattern with each binding as its name and type, and a field that
/// binds as the field name, an at sign and the binding.
impl fmt::Display for IrPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Wildcard => f.write_str("_"),
            Pattern::Binding(binder) => write!(f, "{binder}"),
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => match constructor {
                Constructor::Tuple => write_tuple(f, args),
                Constructor::OptionSome => write!(f, "Some({})", join(args, ", ")),
                Constructor::Record { .. } => {
                    write!(f, "{constructor} {{{}}}", join(fields, ", "))
                }
                Constructor::EnumVariant { .. } if !fields.is_empty() => {
                    write!(f, "{constructor} {{{}}}", join(fields, ", "))
                }
                _ => write!(f, "{constructor}"),
            },
        }
    }
}

impl IrPattern {
    /// Apply `f` to the binder of each binding of the pattern.
    pub fn for_each_binding(&self, f: &mut impl FnMut(&IrBinder<IrDataType>)) {
        match self {
            Pattern::Wildcard => {}
            Pattern::Binding(binder) => f(binder),
            Pattern::Constructor { args, fields, .. } => {
                for arg in args {
                    arg.for_each_binding(f);
                }
                for field in fields {
                    field.pattern.for_each_binding(f);
                }
            }
        }
    }

    /// Apply `f` to each binding of the pattern, to rewrite it, for example
    /// into a wildcard.
    pub fn for_each_binding_mut(&mut self, f: &mut impl FnMut(&mut IrPattern)) {
        match self {
            Pattern::Wildcard => {}
            Pattern::Binding(_) => f(self),
            Pattern::Constructor { args, fields, .. } => {
                for arg in args {
                    arg.for_each_binding_mut(f);
                }
                for field in fields {
                    field.pattern.for_each_binding_mut(f);
                }
            }
        }
    }
}
