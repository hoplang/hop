use crate::hop::typing::Pattern;
use crate::ir::flat_type::FlatDataType;
use crate::ir::ir_binder::IrBinder;
use crate::ir::value_id::ValueId;

pub type IrPattern = Pattern<IrBinder<FlatDataType>>;

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

impl IrPattern {
    pub fn for_each_binding(&self, f: &mut impl FnMut(&IrBinder<FlatDataType>)) {
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

#[cfg(test)]
mod test_support {
    use super::IrMatch;
    use super::IrPattern;
    use super::Pattern;
    use crate::display::Indent;
    use crate::display::{join, write_tuple};
    use crate::hop::typing::{Constructor, PatternField};
    use crate::ir::flat_type::FlatDataType;
    use crate::ir::ir_binder::IrBinder;
    use std::fmt::{self, Write};

    impl<Body> IrMatch<Body> {
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

    impl fmt::Display for PatternField<IrBinder<FlatDataType>> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.pattern {
                Pattern::Binding(binder) => write!(f, "{}@{binder}", self.name.as_str()),
                pattern => write!(f, "{}: {pattern}", self.name.as_str()),
            }
        }
    }

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
}
