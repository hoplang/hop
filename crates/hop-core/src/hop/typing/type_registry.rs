use std::collections::HashMap;
use std::fmt::{self, Display};

use pretty::BoxDoc;

use super::r#type::Type;
use crate::examples_annotation::ExamplesAnnotation;
use crate::root_contained_file_path::RootContainedFilePath;
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
    Html,
    Array(&'a Type),
    Option(&'a Type),
    Tuple(&'a [Type]),
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
    defs: HashMap<RootContainedFilePath, HashMap<TypeName, TypeDef>>,
}

impl TypeRegistry {
    pub fn remove_module(&mut self, module: &RootContainedFilePath) {
        self.defs.remove(module);
    }

    pub fn insert(&mut self, module: RootContainedFilePath, name: TypeName, def: TypeDef) {
        self.defs.entry(module).or_default().insert(name, def);
    }

    /// Every definition, sorted by module and then by name so that callers
    /// iterating the unordered maps get a deterministic order.
    pub fn iter(&self) -> impl Iterator<Item = (&RootContainedFilePath, &TypeName, &TypeDef)> {
        let mut defs: Vec<_> = self
            .defs
            .iter()
            .flat_map(|(module, defs)| defs.iter().map(move |(name, def)| (module, name, def)))
            .collect();
        defs.sort_by(|(m1, n1, _), (m2, n2, _)| m1.cmp(m2).then_with(|| n1.cmp(n2)));
        defs.into_iter()
    }

    /// Record definitions across all modules, sorted by name.
    pub fn records(&self) -> impl Iterator<Item = (&TypeName, &[RecordField])> {
        let mut records: Vec<_> = self
            .iter()
            .filter_map(|(_, name, def)| match def {
                TypeDef::Record { fields } => Some((name, fields.as_slice())),
                TypeDef::Enum { .. } => None,
            })
            .collect();
        records.sort_by_key(|(name, _)| *name);
        records.into_iter()
    }

    /// Enum definitions across all modules, sorted by name.
    pub fn enums(&self) -> impl Iterator<Item = (&TypeName, &[EnumVariant])> {
        let mut enums: Vec<_> = self
            .iter()
            .filter_map(|(_, name, def)| match def {
                TypeDef::Enum { variants } => Some((name, variants.as_slice())),
                TypeDef::Record { .. } => None,
            })
            .collect();
        enums.sort_by_key(|(name, _)| *name);
        enums.into_iter()
    }

    pub fn resolve<'a>(&'a self, typ: &'a Type) -> Option<ResolvedType<'a>> {
        match typ {
            Type::String => Some(ResolvedType::String),
            Type::Bool => Some(ResolvedType::Bool),
            Type::Int => Some(ResolvedType::Int),
            Type::Float => Some(ResolvedType::Float),
            Type::Html => Some(ResolvedType::Html),
            Type::Attrs => None,
            Type::Array(inner) => Some(ResolvedType::Array(inner)),
            Type::Option(inner) => Some(ResolvedType::Option(inner)),
            Type::Tuple(elements) => Some(ResolvedType::Tuple(elements)),
            Type::Named { module, name } => match self.defs.get(module)?.get(name)? {
                TypeDef::Record { fields } => Some(ResolvedType::Record { name, fields }),
                TypeDef::Enum { variants } => Some(ResolvedType::Enum { name, variants }),
            },
        }
    }
}

impl RecordField {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(self.name.as_str())
            .append(BoxDoc::text(": "))
            .append(self.typ.to_doc())
    }
}

impl TypeDef {
    pub fn to_doc<'a>(&'a self, name: &'a TypeName) -> BoxDoc<'a> {
        let (keyword, members): (&str, Vec<BoxDoc<'a>>) = match self {
            TypeDef::Record { fields } => {
                ("record", fields.iter().map(RecordField::to_doc).collect())
            }
            TypeDef::Enum { variants } => (
                "enum",
                variants
                    .iter()
                    .map(|variant| {
                        if variant.fields.is_empty() {
                            BoxDoc::text(variant.name.as_str())
                        } else {
                            BoxDoc::text(variant.name.as_str())
                                .append(BoxDoc::text(" { "))
                                .append(BoxDoc::intersperse(
                                    variant.fields.iter().map(RecordField::to_doc),
                                    BoxDoc::text(", "),
                                ))
                                .append(BoxDoc::text(" }"))
                        }
                    })
                    .collect(),
            ),
        };
        BoxDoc::text(keyword)
            .append(BoxDoc::space())
            .append(BoxDoc::text(name.as_str()))
            .append(BoxDoc::space())
            .append(BoxDoc::text("{"))
            .append(if members.is_empty() {
                BoxDoc::nil()
            } else {
                BoxDoc::line()
                    .append(BoxDoc::intersperse(
                        members,
                        BoxDoc::text(",").append(BoxDoc::line()),
                    ))
                    .append(BoxDoc::text(","))
                    .nest(2)
                    .append(BoxDoc::line())
            })
            .append(BoxDoc::text("}"))
    }
}

impl TypeRegistry {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let docs: Vec<_> = self.iter().map(|(_, name, def)| def.to_doc(name)).collect();
        if docs.is_empty() {
            BoxDoc::nil()
        } else {
            BoxDoc::intersperse(docs, BoxDoc::line().append(BoxDoc::line())).append(BoxDoc::line())
        }
    }
}

impl Display for TypeRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
    }
}
