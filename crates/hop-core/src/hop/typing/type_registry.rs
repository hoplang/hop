use std::collections::HashMap;
use std::fmt::{self, Display};

use super::match_types::{MatchTypes, TypeShape};
use super::r#type::DataType;
use crate::display::{write_lines, write_variant};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone)]
pub struct RecordField {
    pub name: FieldName,
    pub typ: DataType,
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

/// A `DataType` with named types resolved to their registry definition.
/// Resolution is exactly one level deep, fields and variants carry
/// unresolved types.
#[derive(Debug, Clone, Copy)]
pub enum ResolvedType<'a> {
    String,
    Bool,
    Int,
    Float,
    Array,
    Option(&'a DataType),
    Tuple(&'a [DataType]),
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

    pub fn resolve<'a>(&'a self, typ: &'a DataType) -> Option<ResolvedType<'a>> {
        match typ {
            DataType::String => Some(ResolvedType::String),
            DataType::Bool => Some(ResolvedType::Bool),
            DataType::Int => Some(ResolvedType::Int),
            DataType::Float => Some(ResolvedType::Float),
            DataType::Array(_) => Some(ResolvedType::Array),
            DataType::Option(inner) => Some(ResolvedType::Option(inner)),
            DataType::Tuple(elements) => Some(ResolvedType::Tuple(elements)),
            DataType::Named { module, name } => match self.defs.get(module)?.get(name)? {
                TypeDef::Record { fields } => Some(ResolvedType::Record { name, fields }),
                TypeDef::Enum { variants } => Some(ResolvedType::Enum { name, variants }),
            },
        }
    }
}

impl MatchTypes for TypeRegistry {
    type Type = DataType;

    fn shape<'a>(&'a self, typ: &'a DataType) -> TypeShape<'a, DataType> {
        let resolved = self.resolve(typ).expect("named type must be registered");
        match resolved {
            ResolvedType::Bool => TypeShape::Bool,
            ResolvedType::Option(inner) => TypeShape::Option(inner),
            ResolvedType::Tuple(elements) => TypeShape::Tuple(elements),
            ResolvedType::Record { name, fields } => TypeShape::Record {
                name,
                fields: fields
                    .iter()
                    .map(|field| (&field.name, &field.typ))
                    .collect(),
            },
            ResolvedType::Enum { name, variants } => TypeShape::Enum {
                name,
                variants: variants
                    .iter()
                    .map(|variant| {
                        let fields = variant
                            .fields
                            .iter()
                            .map(|field| (&field.name, &field.typ))
                            .collect();
                        (&variant.name, fields)
                    })
                    .collect(),
            },
            ResolvedType::String
            | ResolvedType::Int
            | ResolvedType::Float
            | ResolvedType::Array => TypeShape::Unmatchable,
        }
    }
}

impl Display for RecordField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.as_str(), self.typ)
    }
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_variant(f, self.name.as_str(), &self.fields)
    }
}

/// The definitions in order, separated by blank lines.
impl Display for TypeRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, (_, name, def)) in self.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            match def {
                TypeDef::Record { fields } => {
                    write_lines(f, format_args!("record {} {{", name.as_str()), fields, "}")?;
                }
                TypeDef::Enum { variants } => {
                    write_lines(f, format_args!("enum {} {{", name.as_str()), variants, "}")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
