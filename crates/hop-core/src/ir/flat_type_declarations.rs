use crate::hop::typing::{MatchTypes, TypeShape};
use crate::ir::flat_type::FlatDataType;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

#[derive(Debug, Clone, Default)]
pub struct FlatTypeDeclarations {
    pub records: Vec<FlatRecord>,
    pub enums: Vec<FlatEnum>,
}

#[derive(Debug, Clone)]
pub struct FlatRecord {
    pub name: TypeName,
    pub fields: Vec<FlatField>,
}

#[derive(Debug, Clone)]
pub struct FlatEnum {
    pub name: TypeName,
    pub variants: Vec<FlatVariant>,
}

#[derive(Debug, Clone)]
pub struct FlatVariant {
    pub name: TypeName,
    pub fields: Vec<FlatField>,
}

#[derive(Debug, Clone)]
pub struct FlatField {
    pub name: FieldName,
    pub typ: FlatDataType,
}

impl FlatTypeDeclarations {
    pub fn record(&self, name: &TypeName) -> Option<&FlatRecord> {
        self.records.iter().find(|record| record.name == *name)
    }

    pub fn enumeration(&self, name: &TypeName) -> Option<&FlatEnum> {
        self.enums
            .iter()
            .find(|enumeration| enumeration.name == *name)
    }
}

impl FlatTypeDeclarations {
    pub fn holds_closure(&self, typ: &FlatDataType) -> bool {
        fn holds<'a>(
            declarations: &'a FlatTypeDeclarations,
            typ: &'a FlatDataType,
            visiting: &mut Vec<&'a TypeName>,
        ) -> bool {
            match typ {
                FlatDataType::String
                | FlatDataType::Bool
                | FlatDataType::Int
                | FlatDataType::Float => false,
                FlatDataType::Array(element) | FlatDataType::Option(element) => {
                    holds(declarations, element, visiting)
                }
                FlatDataType::Tuple(elements) => {
                    (elements.iter()).any(|element| holds(declarations, element, visiting))
                }
                FlatDataType::Record(name) | FlatDataType::Enum(name) => {
                    if visiting.contains(&name) {
                        return false;
                    }
                    visiting.push(name);
                    let record = declarations.record(name).into_iter();
                    let enumeration = declarations.enumeration(name).into_iter();
                    let held = (record.flat_map(|record| &record.fields))
                        .chain(
                            enumeration
                                .flat_map(|enumeration| &enumeration.variants)
                                .flat_map(|variant| &variant.fields),
                        )
                        .any(|field| holds(declarations, &field.typ, visiting));
                    visiting.pop();
                    held
                }
                FlatDataType::Function(_) => true,
            }
        }
        holds(self, typ, &mut Vec::new())
    }
}

fn field_shapes(fields: &[FlatField]) -> Vec<(&FieldName, &FlatDataType)> {
    fields
        .iter()
        .map(|field| (&field.name, &field.typ))
        .collect()
}

impl MatchTypes for FlatTypeDeclarations {
    type Type = FlatDataType;

    fn shape<'a>(&'a self, typ: &'a FlatDataType) -> TypeShape<'a, FlatDataType> {
        match typ {
            FlatDataType::Bool => TypeShape::Bool,
            FlatDataType::Option(inner) => TypeShape::Option(inner),
            FlatDataType::Tuple(elements) => TypeShape::Tuple(elements),
            FlatDataType::Record(name) => {
                let record = self.record(name).expect("record must be declared");
                TypeShape::Record {
                    name: &record.name,
                    fields: field_shapes(&record.fields),
                }
            }
            FlatDataType::Enum(name) => {
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
            FlatDataType::String
            | FlatDataType::Int
            | FlatDataType::Float
            | FlatDataType::Array(_)
            | FlatDataType::Function(_) => TypeShape::Unmatchable,
        }
    }
}

#[cfg(test)]
mod test_support {

    use crate::display::{write_lines, write_variant};
    use std::fmt;

    use super::FlatEnum;
    use super::FlatField;
    use super::FlatRecord;
    use super::FlatTypeDeclarations;
    use super::FlatVariant;

    impl fmt::Display for FlatTypeDeclarations {
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

    impl fmt::Display for FlatRecord {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write_lines(
                f,
                format_args!("record {} {{", self.name.as_str()),
                &self.fields,
                "}",
            )
        }
    }

    impl fmt::Display for FlatEnum {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write_lines(
                f,
                format_args!("enum {} {{", self.name.as_str()),
                &self.variants,
                "}",
            )
        }
    }

    impl fmt::Display for FlatVariant {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write_variant(f, self.name.as_str(), &self.fields)
        }
    }

    impl fmt::Display for FlatField {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}: {}", self.name.as_str(), self.typ)
        }
    }
}
