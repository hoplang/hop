use crate::dop::symbols::field_name::FieldName;
use crate::dop::symbols::type_name::TypeName;
use crate::dop::{Type, TypedExpr, VarName};
use crate::hop::semantics::typed_node::TypedNode;
use crate::hop::symbols::component_name::ComponentName;

#[derive(Debug, Clone)]
pub struct TypedRecordDeclaration {
    pub name: TypeName,
    pub fields: Vec<(FieldName, Type)>,
}

#[derive(Debug, Clone)]
pub struct TypedEnumDeclaration {
    pub name: TypeName,
    pub variants: Vec<TypeName>,
}

#[derive(Debug, Clone)]
pub struct TypedComponentDeclaration {
    pub component_name: ComponentName,
    pub children: Vec<TypedNode>,
    pub params: Vec<(VarName, Type, Option<TypedExpr>)>,
}

#[derive(Debug, Clone)]
pub struct TypedAst {
    record_declarations: Vec<TypedRecordDeclaration>,
    enum_declarations: Vec<TypedEnumDeclaration>,
    component_declarations: Vec<TypedComponentDeclaration>,
}

impl TypedAst {
    pub fn new(
        component_declarations: Vec<TypedComponentDeclaration>,
        record_declarations: Vec<TypedRecordDeclaration>,
        enum_declarations: Vec<TypedEnumDeclaration>,
    ) -> Self {
        Self {
            component_declarations,
            record_declarations,
            enum_declarations,
        }
    }

    pub fn get_component_declaration(&self, name: &str) -> Option<&TypedComponentDeclaration> {
        self.component_declarations
            .iter()
            .find(|&n| n.component_name.as_str() == name)
    }

    /// Returns a reference to all component declarations in the AST.
    pub fn get_component_declarations(&self) -> &[TypedComponentDeclaration] {
        &self.component_declarations
    }

    /// Returns a reference to all record declarations in the AST.
    pub fn get_records(&self) -> &[TypedRecordDeclaration] {
        &self.record_declarations
    }

    /// Returns a reference to all enum declarations in the AST.
    pub fn get_enums(&self) -> &[TypedEnumDeclaration] {
        &self.enum_declarations
    }
}
