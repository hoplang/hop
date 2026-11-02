use std::fmt::{self, Write};

use crate::display::Indent;
use crate::ir::ir_binder::IrBinder;
use crate::ir::ir_type::IrDataType;
use crate::ir::value_id::ValueId;

/// A loop that runs the body once per element of the array, in order. When
/// var is None, the loop binds no variable, but still iterates.
#[derive(Debug, Clone)]
pub struct IrFor<Body> {
    pub var: Option<IrBinder<IrDataType>>,
    pub array: ValueId,
    pub body: Body,
}

impl<Body> IrFor<Body> {
    /// The loop head, then the body, which starts with a line break and is
    /// indented.
    pub fn write(
        &self,
        w: &mut dyn Write,
        write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
    ) -> fmt::Result {
        match &self.var {
            Some(binder) => write!(w, "for {binder} in {} {{", self.array)?,
            None => write!(w, "for _ in {} {{", self.array)?,
        }
        write_body(&self.body, &mut Indent::new(w))?;
        w.write_str("\n}")
    }
}
