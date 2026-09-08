use std::collections::HashMap;

use super::r#type::Type;
use crate::document_id::DocumentId;
use crate::examples_annotation::ExamplesAnnotation;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: FieldName,
    pub typ: Type,
    pub examples: Option<ExamplesAnnotation>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: TypeName,
    pub fields: Vec<RecordField>,
}

#[derive(Debug, Clone)]
pub enum TypeDef {
    Record { fields: Vec<RecordField> },
    Enum { variants: Vec<EnumVariant> },
}

/// A `Type` with named types resolved to their registry definition.
/// Resolution is exactly one level deep, fields and variants carry
/// unresolved types.
#[derive(Debug, Clone, Copy)]
pub enum ResolvedType<'a> {
    String,
    Bool,
    Int,
    Float,
    Fragment,
    Array(&'a Type),
    Option(&'a Type),
    Record {
        name: &'a TypeName,
        fields: &'a [RecordField],
    },
    Enum {
        name: &'a TypeName,
        variants: &'a [EnumVariant],
    },
}

#[derive(Debug, Clone, Default)]
pub struct TypeRegistry {
    defs: HashMap<DocumentId, HashMap<TypeName, TypeDef>>,
}

impl TypeRegistry {
    pub fn remove_module(&mut self, module: &DocumentId) {
        self.defs.remove(module);
    }

    pub fn insert(&mut self, module: DocumentId, name: TypeName, def: TypeDef) {
        self.defs.entry(module).or_default().insert(name, def);
    }

    pub fn resolve<'a>(&'a self, typ: &'a Type) -> Option<ResolvedType<'a>> {
        match typ {
            Type::String => Some(ResolvedType::String),
            Type::Bool => Some(ResolvedType::Bool),
            Type::Int => Some(ResolvedType::Int),
            Type::Float => Some(ResolvedType::Float),
            Type::Fragment => Some(ResolvedType::Fragment),
            Type::Attrs => None,
            Type::Array(inner) => Some(ResolvedType::Array(inner)),
            Type::Option(inner) => Some(ResolvedType::Option(inner)),
            Type::Named { module, name } => match self.defs.get(module)?.get(name)? {
                TypeDef::Record { fields } => Some(ResolvedType::Record { name, fields }),
                TypeDef::Enum { variants } => Some(ResolvedType::Enum { name, variants }),
            },
        }
    }
}
