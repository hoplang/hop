use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::{Expr, Type, VarName};
use crate::hop::symbols::component_name::ComponentName;
use crate::hop::syntax::ast::{Attribute, Enum};
use crate::hop::semantics::typed_node::TypedNode;

pub type TypedAttribute = Attribute<Expr>;

#[derive(Debug, Clone)]
pub struct TypedRecordField {
    pub name: FieldName,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedRecord {
    pub name: TypeName,
    pub fields: Vec<TypedRecordField>,
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub var_name: VarName,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedComponentDefinition {
    pub component_name: ComponentName,
    pub children: Vec<TypedNode>,
    pub params: Option<Vec<TypedParameter>>,
}

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
            .find(|&n| n.component_name.as_str() == name)
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
