use crate::ir::entrypoint_id::EntrypointId;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone)]
pub struct IrPage {
    pub name: TypeName,
    pub head: Option<EntrypointId>,
    pub body: EntrypointId,
}

#[cfg(test)]
mod test_support {
    use super::IrPage;
    use crate::ir::flat_program::FlatProgram;
    use crate::symbols::type_name::TypeName;
    use std::fmt;

    impl IrPage {
        pub fn for_entrypoints(program: &FlatProgram) -> Vec<IrPage> {
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
}
