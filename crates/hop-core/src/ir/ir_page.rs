use std::fmt;

use crate::ir::ir_function::IrFunction;
use crate::symbols::type_name::TypeName;

/// A page, which Pure and Flat know nothing of. A page compiles to entry
/// functions, one for its head, if it has one, and one for its body, each
/// declaring the page's parameters in order. The Writer assembles the page
/// from them again.
#[derive(Debug, Clone)]
pub struct IrPage {
    pub name: TypeName,
    /// The entry function that renders the page head, if the page has one.
    pub head: Option<IrFunction>,
    /// The entry function that renders the page body.
    pub body: IrFunction,
}

impl IrPage {
    /// A page for each entry function of a test module, rendered by that
    /// function alone and named after it.
    #[cfg(test)]
    pub fn for_entries(module: &crate::ir::pure_module::PureModule) -> Vec<IrPage> {
        module
            .functions
            .iter()
            .filter(|decl| decl.entry)
            .map(|decl| IrPage {
                name: TypeName::parse(decl.function.name.as_str())
                    .expect("a test entry function is named like a page"),
                head: None,
                body: decl.function.clone(),
            })
            .collect()
    }
}

impl fmt::Display for IrPage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "page {}: ", self.name)?;
        if let Some(head) = &self.head {
            write!(f, "{head}, ")?;
        }
        write!(f, "{}", self.body)
    }
}
