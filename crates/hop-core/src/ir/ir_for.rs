use std::fmt::{self, Write};

use crate::display::Indent;
use crate::ir::ir_binder::IrBinder;
use crate::ir::value_id::ValueId;
use crate::ir::writer_type::WriterDataType;

#[derive(Debug, Clone)]
pub struct IrFor<Body> {
    pub var: Option<IrBinder<WriterDataType>>,
    pub array: ValueId,
    pub body: Body,
}

impl<Body> IrFor<Body> {
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
