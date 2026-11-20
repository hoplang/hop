use crate::ir::ir_binder::IrBinder;
use crate::ir::value_id::ValueId;
use crate::ir::writer_type::WriterDataType;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug)]
pub enum EnumPattern {
    Variant {
        type_name: TypeName,
        variant_name: TypeName,
    },
}

#[derive(Debug)]
pub struct EnumMatchArm<Body> {
    pub pattern: EnumPattern,
    pub bindings: Vec<(FieldName, IrBinder<WriterDataType>)>,
    pub body: Body,
}

#[derive(Debug)]
pub enum IrSwitch<Body> {
    Enum {
        subject: ValueId,
        arms: Vec<EnumMatchArm<Body>>,
    },
    Bool {
        subject: ValueId,
        true_body: Body,
        false_body: Body,
    },
    Option {
        subject: ValueId,
        some_arm_binding: Option<IrBinder<WriterDataType>>,
        some_arm_body: Body,
        none_arm_body: Body,
    },
}

#[cfg(test)]
mod test_support {
    use super::EnumPattern;
    use super::IrSwitch;
    use crate::display::Indent;
    use std::fmt::{self, Write};

    impl<Body> IrSwitch<Body> {
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
}
