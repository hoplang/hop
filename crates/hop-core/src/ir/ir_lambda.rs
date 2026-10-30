use std::fmt;

use pretty::BoxDoc;

use crate::ir::ir_binder::IrBinder;

// The body runs once for each application, so it reads no Html bound
// outside it.
#[derive(Debug, Clone)]
pub struct IrLambda<T, Body> {
    pub parameters: Vec<IrBinder<T>>,
    pub body: Body,
}

impl<T: fmt::Display, Body> IrLambda<T, Body> {
    pub fn to_doc<'a>(&'a self, body_to_doc: impl Fn(&'a Body) -> BoxDoc<'a>) -> BoxDoc<'a> {
        let parameters = self
            .parameters
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        BoxDoc::text(format!("fn({parameters}) {{"))
            .append(body_to_doc(&self.body).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}
