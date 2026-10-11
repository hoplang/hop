//! The match of the Pure and Flat IR, which keeps the arms of the source.

use pretty::BoxDoc;

use crate::hop::typing::{Constructor, Pattern};
use crate::ir::binder_id::BinderId;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_switch::{EnumMatchArm, EnumPattern, IrSwitch};

/// A pattern whose bindings bind binders.
pub type IrPattern = Pattern<BinderId>;

/// A match as written in the source. The first arm whose pattern matches
/// the subject is taken, and its pattern binds the parts of the subject it
/// names. Every value of the subject's type matches some arm.
#[derive(Debug, Clone)]
pub struct IrMatch<Subj, Body> {
    pub subject: Subj,
    pub arms: Vec<IrMatchArm<Body>>,
}

#[derive(Debug, Clone)]
pub struct IrMatchArm<Body> {
    pub pattern: IrPattern,
    pub body: Body,
}

impl<Subj, Body> IrMatch<Subj, Body> {
    /// The match head, then each arm as its pattern and its body on lines
    /// of their own. The body doc starts with a line break and the arm
    /// nests it.
    pub fn to_doc<'a>(
        &'a self,
        subject_to_doc: impl Fn(&'a Subj) -> BoxDoc<'a>,
        body_to_doc: impl Fn(&'a Body) -> BoxDoc<'a>,
    ) -> BoxDoc<'a> {
        BoxDoc::text("match ")
            .append(subject_to_doc(&self.subject))
            .append(BoxDoc::text(" {"))
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

    /// The match as a switch on the constructor of its subject. Every arm
    /// must test a bool, option or enum constructor of the subject and bind
    /// or ignore what that constructor holds, as in the matches that
    /// `lower_matches` leaves.
    pub fn into_switch(self) -> IrSwitch<Subj, Body> {
        let mut true_body = None;
        let mut false_body = None;
        let mut some_arm = None;
        let mut none_body = None;
        let mut enum_arms = Vec::new();
        for arm in self.arms {
            let Pattern::Constructor {
                constructor,
                args,
                fields,
            } = arm.pattern
            else {
                panic!(
                    "an arm of a switch tests a constructor, not {:?}",
                    arm.pattern
                )
            };
            match constructor {
                Constructor::BoolTrue => true_body = Some(arm.body),
                Constructor::BoolFalse => false_body = Some(arm.body),
                Constructor::OptionSome => {
                    let binding = match args.into_iter().next() {
                        Some(Pattern::Binding { name, typ }) => Some(IrBinder { var: name, typ }),
                        Some(Pattern::Wildcard) => None,
                        arg => panic!("a switch binds or ignores what Some holds, not {arg:?}"),
                    };
                    some_arm = Some((binding, arm.body));
                }
                Constructor::OptionNone => none_body = Some(arm.body),
                Constructor::EnumVariant {
                    type_name,
                    variant_name,
                } => {
                    let bindings = fields
                        .into_iter()
                        .filter_map(|field| match field.pattern {
                            Pattern::Binding { name, typ } => {
                                Some((field.name, IrBinder { var: name, typ }))
                            }
                            Pattern::Wildcard => None,
                            pattern => panic!(
                                "a switch binds or ignores the fields of a variant, not {pattern:?}"
                            ),
                        })
                        .collect();
                    enum_arms.push(EnumMatchArm {
                        pattern: EnumPattern::Variant {
                            type_name,
                            variant_name,
                        },
                        bindings,
                        body: arm.body,
                    });
                }
                Constructor::Record { .. } | Constructor::Tuple => {
                    panic!("a record or tuple is destructured, not switched on")
                }
            }
        }
        let subject = self.subject;
        if !enum_arms.is_empty() {
            return IrSwitch::Enum {
                subject,
                arms: enum_arms,
            };
        }
        match (true_body, false_body, some_arm, none_body) {
            (Some(true_body), Some(false_body), None, None) => IrSwitch::Bool {
                subject,
                true_body,
                false_body,
            },
            (None, None, Some((some_arm_binding, some_arm_body)), Some(none_arm_body)) => {
                IrSwitch::Option {
                    subject,
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                }
            }
            _ => panic!("a switch has an arm for each constructor of its subject"),
        }
    }
}

impl IrPattern {
    /// The pattern with each binding as its binder and type, and a field
    /// that binds as the field name, an at sign and the binding.
    pub fn to_doc(&self) -> BoxDoc<'_> {
        match self {
            Pattern::Wildcard => BoxDoc::text("_"),
            Pattern::Binding { name, typ } => BoxDoc::text(format!("{name}: {typ}")),
            Pattern::Constructor {
                constructor,
                args,
                fields,
            } => {
                let fields_doc = BoxDoc::intersperse(
                    fields.iter().map(|field| match &field.pattern {
                        Pattern::Binding { name, typ } => {
                            BoxDoc::text(format!("{}@{name}: {typ}", field.name.as_str()))
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

    /// Apply `f` to each binding of the pattern, to rewrite it, for example
    /// into a wildcard.
    pub fn for_each_binding_mut(&mut self, f: &mut impl FnMut(&mut IrPattern)) {
        match self {
            Pattern::Wildcard => {}
            Pattern::Binding { .. } => f(self),
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
