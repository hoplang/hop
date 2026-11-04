use std::fmt;

use crate::ir::entrypoint_id::EntrypointId;
use crate::symbols::type_name::TypeName;

/// A page, which Flat knows nothing of. A page compiles to entrypoints,
/// one for its head, if it has one, and one for its body. Each declares
/// the page's parameters in order. The Writer assembles the page from them
/// again.
#[derive(Debug, Clone)]
pub struct IrPage {
    pub name: TypeName,
    /// The entrypoint that renders the page head, if the page has one.
    pub head: Option<EntrypointId>,
    /// The entrypoint that renders the page body.
    pub body: EntrypointId,
}

impl IrPage {
    /// A page for each entrypoint of a test program, rendered by that
    /// entrypoint alone and named after it.
    #[cfg(test)]
    pub fn for_entrypoints(program: &crate::ir::flat_program::FlatProgram) -> Vec<IrPage> {
        program
            .entrypoints
            .iter_enumerated()
            .map(|(id, decl)| IrPage {
                name: TypeName::parse(decl.name.as_str())
                    .expect("a test entrypoint is named like a page"),
                head: None,
                body: id,
            })
            .collect()
    }
}

impl fmt::Display for IrPage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "page {}: ", self.name)?;
        if let Some(head) = &self.head {
            write!(f, "e{head}, ")?;
        }
        write!(f, "e{}", self.body)
    }
}
