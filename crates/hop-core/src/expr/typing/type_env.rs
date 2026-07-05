use std::collections::HashMap;
use std::sync::Arc;

use super::r#type::{ComponentSignature, Type};
use crate::document::DocumentRange;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone)]
pub enum TypeBinding {
    Type(Arc<Type>),
    Component(ComponentSignature),
    View,
}

#[derive(Debug, Clone, Default)]
pub struct TypeEnv {
    entries: HashMap<TypeName, Entry>,
}

#[derive(Debug, Clone)]
struct Entry {
    binding: TypeBinding,
    definition_range: DocumentRange,
    accessed: bool,
    origin: Origin,
}

#[derive(Debug, Clone)]
enum Origin {
    Local,
    Imported { import_range: DocumentRange },
}

impl TypeEnv {
    pub fn new() -> Self {
        Self::default()
    }

    /// Bind a locally declared name.
    ///
    /// Errors if the name is already taken.
    pub fn insert_local(
        &mut self,
        name: TypeName,
        binding: TypeBinding,
        definition_range: DocumentRange,
    ) -> Result<(), ()> {
        self.insert(name, binding, definition_range, Origin::Local)
    }

    /// Bind an imported name. The import range is the range of the
    /// import statement in the importing module.
    ///
    /// Errors if the name is already taken.
    pub fn insert_import(
        &mut self,
        name: TypeName,
        binding: TypeBinding,
        definition_range: DocumentRange,
        import_range: DocumentRange,
    ) -> Result<(), ()> {
        self.insert(
            name,
            binding,
            definition_range,
            Origin::Imported { import_range },
        )
    }

    fn insert(
        &mut self,
        name: TypeName,
        binding: TypeBinding,
        definition_range: DocumentRange,
        origin: Origin,
    ) -> Result<(), ()> {
        if self.entries.contains_key(&name) {
            return Err(());
        }
        self.entries.insert(
            name,
            Entry {
                binding,
                definition_range,
                accessed: false,
                origin,
            },
        );
        Ok(())
    }

    /// Access the binding behind a name.
    pub fn lookup(&mut self, name: &TypeName) -> Option<(&TypeBinding, &DocumentRange)> {
        let entry = self.entries.get_mut(name)?;
        entry.accessed = true;
        Some((&entry.binding, &entry.definition_range))
    }

    /// Imported names that were never accessed via lookup, with their
    /// import statement ranges.
    pub fn unused_imports(&self) -> Vec<(&TypeName, &DocumentRange)> {
        let unused: Vec<_> = self
            .entries
            .iter()
            .filter_map(|(name, entry)| match &entry.origin {
                Origin::Imported { import_range } if !entry.accessed => Some((name, import_range)),
                _ => None,
            })
            .collect();
        unused
    }

    /// Replace the binding behind a name, keeping its definition range.
    ///
    /// Panics if the name is not bound.
    pub fn replace_binding(&mut self, name: &TypeName, binding: TypeBinding) {
        self.entries
            .get_mut(name)
            .unwrap_or_else(|| panic!("no binding registered for {name}"))
            .binding = binding;
    }
}
