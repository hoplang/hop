use std::collections::HashMap;
use std::sync::Arc;

use super::r#type::{EnumVariant, Type};
use crate::document_id::DocumentId;
use crate::expr::ExamplesAnnotation;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

type RecordFields = Vec<(FieldName, Arc<Type>, Option<ExamplesAnnotation>)>;

#[derive(Debug, Clone)]
pub enum TypeDef {
    Record { fields: RecordFields },
    Enum { variants: Vec<EnumVariant> },
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

    pub fn record_fields(
        &self,
        module: &DocumentId,
        name: &TypeName,
    ) -> Option<&[(FieldName, Arc<Type>, Option<ExamplesAnnotation>)]> {
        match self.defs.get(module)?.get(name)? {
            TypeDef::Record { fields } => Some(fields),
            TypeDef::Enum { .. } => None,
        }
    }

    pub fn enum_variants(&self, module: &DocumentId, name: &TypeName) -> Option<&[EnumVariant]> {
        match self.defs.get(module)?.get(name)? {
            TypeDef::Enum { variants } => Some(variants),
            TypeDef::Record { .. } => None,
        }
    }

    pub fn variant_fields(
        &self,
        module: &DocumentId,
        name: &TypeName,
        variant: &str,
    ) -> Option<&[(FieldName, Arc<Type>, Option<ExamplesAnnotation>)]> {
        let variants = self.enum_variants(module, name)?;
        variants
            .iter()
            .find(|v| v.name.as_str() == variant)
            .map(|v| v.fields.as_slice())
    }
}
