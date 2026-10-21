use pretty::BoxDoc;

use crate::ir::ir_binder::IrBinder;
use crate::ir::value_id::ValueId;

/// A loop that runs the body once per element of the array, in order. When
/// var is None, the loop binds no variable, but still iterates.
#[derive(Debug, Clone)]
pub struct IrFor<Body> {
    pub var: Option<IrBinder>,
    pub array: ValueId,
    pub body: Body,
}

impl<Body> IrFor<Body> {
    pub fn to_doc<'a>(&'a self, body_to_doc: impl Fn(&'a Body) -> BoxDoc<'a>) -> BoxDoc<'a> {
        let var = match &self.var {
            Some(binder) => binder.to_string(),
            None => "_".to_string(),
        };
        BoxDoc::text(format!("for {var} in {} {{", self.array))
            .append(body_to_doc(&self.body).nest(2))
            .append(BoxDoc::line())
            .append(BoxDoc::text("}"))
    }
}
