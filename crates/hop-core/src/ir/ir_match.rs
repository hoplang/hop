//! The match of the Flat IR, which keeps the arms of the source.

use pretty::BoxDoc;

use crate::hop::typing::{Constructor, DataType, Pattern};
use crate::ir::ir_binder::IrBinder;
use crate::ir::value_id::ValueId;

/// A pattern whose bindings bind names.
pub type IrPattern = Pattern<IrBinder<DataType>>;

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
    /// of their own. The body doc starts with a line break and the arm
    /// nests it.
    pub fn to_doc<'a>(&'a self, body_to_doc: impl Fn(&'a Body) -> BoxDoc<'a>) -> BoxDoc<'a> {
        BoxDoc::text(format!("match {} {{", self.subject))
            .append(
                BoxDoc::concat(self.arms.iter().map(|arm| {
                    BoxDoc::line()
                        .append(arm.pattern.to_doc())
                        .append(BoxDoc::text(" => {"))
                        .append(body_to_doc(&arm.body).nest(2))
                        .append(BoxDoc::line())
                        .append(BoxDoc::text("}"))
                }))
                .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}

impl IrPattern {
    /// The pattern with each binding as its name and type, and a field
    /// that binds as the field name, an at sign and the binding.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Pattern::Wildcard => BoxDoc::text("_"),
            Pattern::Binding(binder) => BoxDoc::text(binder.to_string()),
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                let fields_doc = BoxDoc::intersperse(
                    fields.iter().map(|field| match &field.pattern {
                        Pattern::Binding(binder) => {
                            BoxDoc::text(format!("{}@{binder}", field.name.as_str()))
                        }
                        pattern => BoxDoc::text(format!("{}: ", field.name.as_str()))
                            .append(pattern.to_doc()),
                    }),
                    BoxDoc::text(", "),
                );
                let args_doc =
                    BoxDoc::intersperse(args.iter().map(IrPattern::to_doc), BoxDoc::text(", "));
                match constructor {
                    Constructor::Tuple => BoxDoc::text("(")
                        .append(args_doc)
                        .append(if args.len() == 1 {
                            BoxDoc::text(",")
                        } else {
                            BoxDoc::nil()
                        })
                        .append(BoxDoc::text(")")),
                    Constructor::OptionSome => BoxDoc::text("Some(")
                        .append(args_doc)
                        .append(BoxDoc::text(")")),
                    Constructor::Record { .. } => constructor
                        .to_doc()
                        .append(BoxDoc::text(" {"))
                        .append(fields_doc)
                        .append(BoxDoc::text("}")),
                    Constructor::EnumVariant { .. } if !fields.is_empty() => constructor
                        .to_doc()
                        .append(BoxDoc::text(" {"))
                        .append(fields_doc)
                        .append(BoxDoc::text("}")),
                    _ => constructor.to_doc(),
                }
            }
        }
    }

    /// Apply `f` to the binder of each binding of the pattern.
    pub fn for_each_binding(&self, f: &mut impl FnMut(&IrBinder<DataType>)) {
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
