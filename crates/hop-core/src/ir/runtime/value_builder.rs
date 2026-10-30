use crate::ir::ir_type_declarations::IrTypeDeclarations;
use crate::ir::runtime::bounded_vec::BoundedVec;
use crate::ir::runtime::value::Value;
use crate::symbols::{field_name::FieldName, type_name::TypeName};
use std::rc::Rc;

pub struct ValueBuilder<'a> {
    types: &'a IrTypeDeclarations,
}

impl<'a> ValueBuilder<'a> {
    pub fn new(types: &'a IrTypeDeclarations) -> Self {
        ValueBuilder { types }
    }

    pub fn str(&self, s: &str) -> Value {
        Value::String(s.into())
    }

    pub fn int(&self, n: i32) -> Value {
        Value::Int(n)
    }

    pub fn bool(&self, b: bool) -> Value {
        Value::Bool(b)
    }

    #[allow(dead_code)]
    pub fn float(&self, f: f64) -> Value {
        Value::Float(f)
    }

    pub fn array(&self, elements: Vec<Value>) -> Value {
        Value::Array(Rc::new(BoundedVec::new(elements).unwrap()))
    }

    #[allow(dead_code)]
    pub fn some(&self, inner: Value) -> Value {
        Value::Option(Some(Rc::new(inner)))
    }

    #[allow(dead_code)]
    pub fn none(&self) -> Value {
        Value::Option(None)
    }

    #[allow(dead_code)]
    pub fn record(&self, record_name: &str, fields: Vec<(&str, Value)>) -> Value {
        let declared = self
            .types
            .records
            .iter()
            .find(|record| record.name.as_str() == record_name)
            .map(|record| {
                record
                    .fields
                    .iter()
                    .map(|f| f.name.as_str())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_else(|| panic!("no record `{record_name}` is declared"));
        Value::Record(checked_fields(&declared, fields, record_name))
    }

    #[allow(dead_code)]
    pub fn enum_variant(&self, enum_name: &str, variant_name: &str) -> Value {
        self.enum_variant_with_fields(enum_name, variant_name, vec![])
    }

    pub fn enum_variant_with_fields(
        &self,
        enum_name: &str,
        variant_name: &str,
        field_values: Vec<(&str, Value)>,
    ) -> Value {
        let variants = self
            .types
            .enums
            .iter()
            .find(|enumeration| enumeration.name.as_str() == enum_name)
            .map(|enumeration| &enumeration.variants)
            .unwrap_or_else(|| panic!("no enum `{enum_name}` is declared"));
        let declared = variants
            .iter()
            .find(|v| v.name.as_str() == variant_name)
            .map(|v| v.fields.iter().map(|f| f.name.as_str()).collect::<Vec<_>>())
            .unwrap_or_else(|| {
                let variant_names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
                panic!(
                    "Variant '{}' not found in enum '{}'. Available variants: {:?}",
                    variant_name, enum_name, variant_names
                )
            });
        Value::Enum(
            TypeName::parse(variant_name).unwrap(),
            checked_fields(
                &declared,
                field_values,
                &format!("{enum_name}::{variant_name}"),
            ),
        )
    }
}

fn checked_fields(
    declared: &[&str],
    fields: Vec<(&str, Value)>,
    owner: &str,
) -> Rc<[(FieldName, Value)]> {
    for (field_name, _) in &fields {
        assert!(
            declared.contains(field_name),
            "Field '{}' not found in '{}'",
            field_name,
            owner
        );
    }
    let missing_fields: Vec<&str> = declared
        .iter()
        .filter(|f| !fields.iter().any(|(name, _)| name == *f))
        .copied()
        .collect();
    assert!(
        missing_fields.is_empty(),
        "'{}' is missing fields: {:?}",
        owner,
        missing_fields
    );
    fields
        .into_iter()
        .map(|(name, value)| (FieldName::parse(name).unwrap(), value))
        .collect()
}
