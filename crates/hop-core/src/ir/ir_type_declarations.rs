use std::fmt;

use crate::display::{write_lines, write_variant};
use crate::hop::typing::{MatchTypes, TypeShape};
use crate::ir::ir_type::IrDataType;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, Default)]
pub struct IrTypeDeclarations {
    pub records: Vec<IrRecord>,
    pub enums: Vec<IrEnum>,
}

#[derive(Debug, Clone)]
pub struct IrRecord {
    pub name: TypeName,
    pub fields: Vec<IrField>,
}

#[derive(Debug, Clone)]
pub struct IrEnum {
    pub name: TypeName,
    pub variants: Vec<IrVariant>,
}

#[derive(Debug, Clone)]
pub struct IrVariant {
    pub name: TypeName,
    pub fields: Vec<IrField>,
}

#[derive(Debug, Clone)]
pub struct IrField {
    pub name: FieldName,
    pub typ: IrDataType,
}

impl IrTypeDeclarations {
    pub fn record(&self, name: &TypeName) -> Option<&IrRecord> {
        self.records.iter().find(|record| record.name == *name)
    }

    pub fn enumeration(&self, name: &TypeName) -> Option<&IrEnum> {
        self.enums
            .iter()
            .find(|enumeration| enumeration.name == *name)
    }
}

/// The records and then the enums, separated by blank lines.
impl fmt::Display for IrTypeDeclarations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sep = "";
        for record in &self.records {
            writeln!(f, "{sep}{record}")?;
            sep = "\n";
        }
        for enumeration in &self.enums {
            writeln!(f, "{sep}{enumeration}")?;
            sep = "\n";
        }
        Ok(())
    }
}

impl fmt::Display for IrRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_lines(
            f,
            format_args!("record {} {{", self.name.as_str()),
            &self.fields,
            "}",
        )
    }
}

impl fmt::Display for IrEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_lines(
            f,
            format_args!("enum {} {{", self.name.as_str()),
            &self.variants,
            "}",
        )
    }
}

impl fmt::Display for IrVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_variant(f, self.name.as_str(), &self.fields)
    }
}

impl fmt::Display for IrField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name.as_str(), self.typ)
    }
}

fn field_shapes(fields: &[IrField]) -> Vec<(&FieldName, &IrDataType)> {
    fields
        .iter()
        .map(|field| (&field.name, &field.typ))
        .collect()
}

impl MatchTypes for IrTypeDeclarations {
    type Type = IrDataType;

    fn shape<'a>(&'a self, typ: &'a IrDataType) -> TypeShape<'a, IrDataType> {
        match typ {
            IrDataType::Bool => TypeShape::Bool,
            IrDataType::Option(inner) => TypeShape::Option(inner),
            IrDataType::Tuple(elements) => TypeShape::Tuple(elements),
            IrDataType::Record(name) => {
                let record = self.record(name).expect("record must be declared");
                TypeShape::Record {
                    name: &record.name,
                    fields: field_shapes(&record.fields),
                }
            }
            IrDataType::Enum(name) => {
                let enumeration = self.enumeration(name).expect("enum must be declared");
                TypeShape::Enum {
                    name: &enumeration.name,
                    variants: enumeration
                        .variants
                        .iter()
                        .map(|variant| (&variant.name, field_shapes(&variant.fields)))
                        .collect(),
                }
            }
            IrDataType::String | IrDataType::Int | IrDataType::Float | IrDataType::Array(_) => {
                TypeShape::Unmatchable
            }
        }
    }
}
