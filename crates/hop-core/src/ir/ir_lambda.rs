use std::fmt::{self, Write};

use crate::display::{Indent, join};
use crate::ir::ir_binder::IrBinder;

#[derive(Debug, Clone)]
pub struct IrLambda<Param, Body> {
    pub parameters: Vec<IrBinder<Param>>,
    pub body: Body,
}

impl<Param: fmt::Display, Body> IrLambda<Param, Body> {
    /// The parameters, then the body, which starts with a line break and is
    /// indented.
    pub fn write(
        &self,
        w: &mut dyn Write,
        write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
    ) -> fmt::Result {
        write!(w, "fn({}) {{", join(&self.parameters, ", "))?;
        write_body(&self.body, &mut Indent::new(w))?;
        w.write_str("\n}")
    }
}
