//! The match of the Writer IR, a switch on the constructor of its subject.

use std::fmt::{self, Write};

use crate::display::Indent;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_type::IrDataType;
use crate::ir::value_id::ValueId;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// An enum variant pattern, e.g. `Color::Red`
#[derive(Debug)]
pub enum EnumPattern {
    Variant {
        type_name: TypeName,
        variant_name: TypeName,
    },
}

/// A single arm in an enum match, e.g. `Color::Red => "red"`
#[derive(Debug)]
pub struct EnumMatchArm<Body> {
    pub pattern: EnumPattern,
    /// Field bindings for this arm, e.g. `Result::Ok(value: v)` binds field "value" to variable "v"
    pub bindings: Vec<(FieldName, IrBinder<IrDataType>)>,
    pub body: Body,
}

/// A match that can be used for different expression and statement types.
#[derive(Debug)]
pub enum IrSwitch<Body> {
    /// An enum match, e.g. `match color { Color::Red => "red", ... }`
    Enum {
        subject: ValueId,
        arms: Vec<EnumMatchArm<Body>>,
    },

    /// A boolean match, e.g. `match flag { true => "yes", false => "no" }`
    Bool {
        subject: ValueId,
        true_body: Body,
        false_body: Body,
    },

    /// An option match, e.g. `match opt { Some(x) => x, None => "empty" }`
    Option {
        subject: ValueId,
        some_arm_binding: Option<IrBinder<IrDataType>>,
        some_arm_body: Body,
        none_arm_body: Body,
    },
}

impl<Body> IrSwitch<Body> {
    /// The match head, then each arm as its pattern and its body on lines
    /// of their own. The body starts with a line break and is indented.
    pub fn write(
        &self,
        w: &mut dyn Write,
        write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
    ) -> fmt::Result {
        let (IrSwitch::Bool { subject, .. }
        | IrSwitch::Option { subject, .. }
        | IrSwitch::Enum { subject, .. }) = self;
        if let IrSwitch::Enum { arms, .. } = self
            && arms.is_empty()
        {
            return write!(w, "match {subject} {{}}");
        }
        write!(w, "match {subject} {{")?;
        {
            let mut arms = Indent::new(w);
            // Each arm writes its pattern on a new line, then this.
            let arm_body = |arms: &mut dyn Write, body: &Body| -> fmt::Result {
                arms.write_str(" => {")?;
                write_body(body, &mut Indent::new(arms))?;
                arms.write_str("\n}")
            };
            match self {
                IrSwitch::Bool {
                    true_body,
                    false_body,
                    ..
                } => {
                    arms.write_str("\ntrue")?;
                    arm_body(&mut arms, true_body)?;
                    arms.write_str("\nfalse")?;
                    arm_body(&mut arms, false_body)?;
                }
                IrSwitch::Option {
                    some_arm_binding,
                    some_arm_body,
                    none_arm_body,
                    ..
                } => {
                    match some_arm_binding {
                        Some(binder) => write!(arms, "\nSome({binder})")?,
                        None => arms.write_str("\nSome(_)")?,
                    }
                    arm_body(&mut arms, some_arm_body)?;
                    arms.write_str("\nNone")?;
                    arm_body(&mut arms, none_arm_body)?;
                }
                IrSwitch::Enum {
                    arms: enum_arms, ..
                } => {
                    for enum_arm in enum_arms {
                        let EnumPattern::Variant {
                            type_name,
                            variant_name,
                        } = &enum_arm.pattern;
                        write!(arms, "\n{}::{}", type_name.as_str(), variant_name.as_str())?;
                        if !enum_arm.bindings.is_empty() {
                            arms.write_str(" {")?;
                            for (i, (field, binder)) in enum_arm.bindings.iter().enumerate() {
                                let sep = if i > 0 { ", " } else { "" };
                                write!(arms, "{sep}{}@{binder}", field.as_str())?;
                            }
                            arms.write_str("}")?;
                        }
                        arm_body(&mut arms, &enum_arm.body)?;
                    }
                }
            }
        }
        w.write_str("\n}")
    }
}
