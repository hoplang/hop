use std::fmt;

use pretty::BoxDoc;

use crate::examples_annotation::ExamplesAnnotation;
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
    pub examples: Option<ExamplesAnnotation>,
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

impl IrTypeDeclarations {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        let mut declarations: Vec<(&TypeName, BoxDoc<'_>)> = self
            .records
            .iter()
            .map(|record| {
                let fields = record.fields.iter().map(IrField::to_doc).collect();
                (
                    &record.name,
                    declaration_doc("record", &record.name, fields),
                )
            })
            .chain(self.enums.iter().map(|enumeration| {
                let variants = enumeration.variants.iter().map(IrVariant::to_doc).collect();
                (
                    &enumeration.name,
                    declaration_doc("enum", &enumeration.name, variants),
                )
            }))
            .collect();
        declarations.sort_by_key(|(name, _)| *name);
        if declarations.is_empty() {
            return BoxDoc::nil();
        }
        BoxDoc::intersperse(
            declarations.into_iter().map(|(_, doc)| doc),
            BoxDoc::line().append(BoxDoc::line()),
        )
        .append(BoxDoc::line())
    }
}

fn declaration_doc<'a>(
    keyword: &'a str,
    name: &'a TypeName,
    members: Vec<BoxDoc<'a>>,
) -> BoxDoc<'a> {
    let members = if members.is_empty() {
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
    };
    BoxDoc::text(format!("{keyword} {} {{", name.as_str()))
        .append(members)
        .append(BoxDoc::text("}"))
}

impl IrVariant {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        if self.fields.is_empty() {
            return BoxDoc::text(self.name.as_str());
        }
        BoxDoc::text(self.name.as_str())
            .append(BoxDoc::text(" { "))
            .append(BoxDoc::intersperse(
                self.fields.iter().map(IrField::to_doc),
                BoxDoc::text(", "),
            ))
            .append(BoxDoc::text(" }"))
    }
}

impl IrField {
    pub fn to_doc(&self) -> BoxDoc<'_> {
        BoxDoc::text(format!("{}: ", self.name.as_str())).append(self.typ.to_doc())
    }
}

impl fmt::Display for IrTypeDeclarations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_doc().pretty(80))
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
