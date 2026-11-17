use std::fmt::{self, Write};

use crate::display::Indent;
use crate::ir::ir_function::IrFunction;
use crate::ir::ir_parameter::IrParameter;

#[derive(Debug, Clone)]
pub struct IrFunctionDeclaration<Param, Return, Body> {
    pub function: IrFunction,
    pub parameters: Vec<IrParameter<Param>>,
    pub return_type: Return,
    pub body: Body,
}

impl<Param: fmt::Display, Return: fmt::Display, Body> IrFunctionDeclaration<Param, Return, Body> {
    /// The signature, then the body, which starts with a line break and is
    /// indented.
    pub fn write(
        &self,
        w: &mut dyn Write,
        write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
    ) -> fmt::Result {
        write_function(
            w,
            &self.function,
            &self.parameters,
            &self.return_type,
            &self.body,
            write_body,
        )
    }
}

pub fn write_function<Param: fmt::Display, Return: fmt::Display, Body>(
    w: &mut dyn Write,
    function: &IrFunction,
    parameters: &[IrParameter<Param>],
    return_type: &Return,
    body: &Body,
    write_body: impl Fn(&Body, &mut dyn Write) -> fmt::Result,
) -> fmt::Result {
    write!(w, "fn {function}(")?;
    for parameter in parameters {
        write!(Indent::new(w), "\n{parameter},")?;
    }
    if !parameters.is_empty() {
        w.write_str("\n")?;
    }
    write!(w, ") -> {return_type} {{")?;
    write_body(body, &mut Indent::new(w))?;
    w.write_str("\n}")
}
