use crate::hop::typing::Ty;
use crate::ir::flat_type::{FlatDataType, FlatSignature, FlatType, IrModifier};
use crate::ir::flat_type_declarations::{
    FlatEnum, FlatField, FlatRecord, FlatTypeDeclarations, FlatVariant,
};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

fn type_name(name: &str) -> TypeName {
    TypeName::parse(name).unwrap_or_else(|e| panic!("invalid type name `{name}`: {e:?}"))
}

fn field_name(name: &str) -> FieldName {
    FieldName::parse(name).unwrap_or_else(|e| panic!("invalid field name `{name}`: {e:?}"))
}

type Fields = Vec<(String, Ty)>;

#[derive(Clone, Default)]
pub struct FlatTypeDeclarationsBuilder {
    records: Vec<(String, Fields)>,
    enums: Vec<(String, Vec<(String, Fields)>)>,
}

impl FlatTypeDeclarationsBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record<'a>(
        mut self,
        name: &str,
        fields: impl IntoIterator<Item = (&'a str, Ty)>,
    ) -> Self {
        let fields = fields
            .into_iter()
            .map(|(field, ty)| (field.to_string(), ty))
            .collect();
        self.records.push((name.to_string(), fields));
        self
    }

    pub fn enum_<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = (&'a str, Vec<(&'a str, Ty)>)>,
    ) -> Self {
        let variants = variants
            .into_iter()
            .map(|(variant, fields)| {
                let fields = fields
                    .into_iter()
                    .map(|(field, ty)| (field.to_string(), ty))
                    .collect();
                (variant.to_string(), fields)
            })
            .collect();
        self.enums.push((name.to_string(), variants));
        self
    }

    pub fn build(self) -> FlatTypeDeclarations {
        let mut names: Vec<&str> = self
            .records
            .iter()
            .map(|(name, _)| name.as_str())
            .chain(self.enums.iter().map(|(name, _)| name.as_str()))
            .collect();
        names.sort();
        if let Some(pair) = names.windows(2).find(|pair| pair[0] == pair[1]) {
            panic!("duplicate declaration of type `{}`", pair[0]);
        }

        // A field may name a type declared after it, so the fields are
        // typed once every name is known.
        let records: Vec<String> = self.records.iter().map(|(name, _)| name.clone()).collect();
        let enums: Vec<String> = self.enums.iter().map(|(name, _)| name.clone()).collect();
        let fields = |fields: Fields| {
            fields
                .into_iter()
                .map(|(name, ty)| FlatField {
                    name: field_name(&name),
                    typ: data_type(ty, &records, &enums),
                })
                .collect()
        };
        let mut declarations = FlatTypeDeclarations {
            records: self
                .records
                .into_iter()
                .map(|(name, declared)| FlatRecord {
                    name: type_name(&name),
                    fields: fields(declared),
                })
                .collect(),
            enums: self
                .enums
                .into_iter()
                .map(|(name, variants)| FlatEnum {
                    name: type_name(&name),
                    variants: variants
                        .into_iter()
                        .map(|(variant, declared)| FlatVariant {
                            name: type_name(&variant),
                            fields: fields(declared),
                        })
                        .collect(),
                })
                .collect(),
        };
        declarations
            .records
            .sort_by(|left, right| left.name.cmp(&right.name));
        declarations
            .enums
            .sort_by(|left, right| left.name.cmp(&right.name));
        declarations
    }
}

impl FlatTypeDeclarations {
    pub fn ty(&self, ty: Ty) -> FlatType {
        let records: Vec<String> = self
            .records
            .iter()
            .map(|record| record.name.as_str().to_string())
            .collect();
        let enums: Vec<String> = self
            .enums
            .iter()
            .map(|enumeration| enumeration.name.as_str().to_string())
            .collect();
        flat_type(ty, &records, &enums)
    }
}

fn flat_type(ty: Ty, records: &[String], enums: &[String]) -> FlatType {
    match ty {
        Ty::Html => FlatType::Html,
        ty => FlatType::Data(data_type(ty, records, enums)),
    }
}

fn data_type(ty: Ty, records: &[String], enums: &[String]) -> FlatDataType {
    let element = |ty: Box<Ty>| Box::new(data_type(*ty, records, enums));
    match ty {
        Ty::Int => FlatDataType::Int,
        Ty::String => FlatDataType::String,
        Ty::Bool => FlatDataType::Bool,
        Ty::Float => FlatDataType::Float,
        Ty::Html => panic!("Html is not a data type"),
        Ty::Function {
            modifier,
            parameters,
            result,
        } => FlatDataType::Function(FlatSignature {
            modifier: IrModifier::from(modifier),
            parameters: (parameters.into_iter())
                .map(|parameter| flat_type(parameter, records, enums))
                .collect(),
            result: Box::new(flat_type(*result, records, enums)),
        }),
        Ty::Array(ty) => FlatDataType::Array(element(ty)),
        Ty::Option(ty) => FlatDataType::Option(element(ty)),
        Ty::Tuple(elements) => FlatDataType::Tuple(
            elements
                .into_iter()
                .map(|ty| data_type(ty, records, enums))
                .collect(),
        ),
        Ty::Named(name) if records.contains(&name) => FlatDataType::Record(type_name(&name)),
        Ty::Named(name) if enums.contains(&name) => FlatDataType::Enum(type_name(&name)),
        Ty::Named(name) => panic!("type `{name}` is not declared"),
    }
}
