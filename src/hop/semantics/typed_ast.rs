use crate::dop::{Expr, Type};
use crate::hop::syntax::ast::{Attribute, ComponentDefinition, Enum, Record};

pub type TypedAttribute = Attribute<Expr>;
pub type TypedRecord = Record<Type>;
pub type TypedComponentDefinition = ComponentDefinition<Expr, Type>;

#[derive(Debug, Clone)]
pub struct TypedAst {
    records: Vec<TypedRecord>,
    enums: Vec<Enum>,
    component_definitions: Vec<TypedComponentDefinition>,
}

impl TypedAst {
    pub fn new(
        component_definitions: Vec<TypedComponentDefinition>,
        records: Vec<TypedRecord>,
        enums: Vec<Enum>,
    ) -> Self {
        Self {
            component_definitions,
            records,
            enums,
        }
    }

    pub fn get_component_definition(&self, name: &str) -> Option<&TypedComponentDefinition> {
        self.component_definitions
            .iter()
            .find(|&n| n.tag_name.as_str() == name)
    }

    /// Returns a reference to all component definition nodes in the AST.
    pub fn get_component_definitions(&self) -> &[TypedComponentDefinition] {
        &self.component_definitions
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[TypedRecord] {
        &self.records
    }

    /// Returns a reference to all enum declarations in the AST.
    pub fn get_enums(&self) -> &[Enum] {
        &self.enums
    }
}
