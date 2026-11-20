use std::fmt::{self, Write};

use crate::display::Indent;
use crate::ir::ir_binder::IrBinder;

#[derive(Debug, Clone)]
pub struct IrLambda<Param, Body> {
    pub parameters: Vec<IrBinder<Param>>,
    pub body: Body,
}

impl<Param: fmt::Display, Body> IrLambda<Param, Body> {
    pub fn write(
        &self,
        w: &mut dyn Write,
        write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
    ) -> fmt::Result {
        w.write_str("fn(")?;
        for parameter in &self.parameters {
            write!(Indent::new(w), "\n{parameter},")?;
        }
        if !self.parameters.is_empty() {
            w.write_str("\n")?;
        }
        w.write_str(") {")?;
        write_body(&self.body, &mut Indent::new(w))?;
        w.write_str("\n}")
    }
}
