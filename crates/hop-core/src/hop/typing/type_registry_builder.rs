use crate::hop::typing::r#type::{DataType, Type};
use crate::hop::typing::type_registry::{EnumVariant, RecordField, TypeDef, TypeRegistry};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// The module all test-declared types live in.
pub fn test_module() -> RootContainedFilePath {
    RootContainedFilePath::new("test.hop").unwrap()
}

fn type_name(name: &str) -> TypeName {
    TypeName::parse(name).unwrap_or_else(|e| panic!("invalid type name `{name}`: {e:?}"))
}

fn field_name(name: &str) -> FieldName {
    FieldName::parse(name).unwrap_or_else(|e| panic!("invalid field name `{name}`: {e:?}"))
}

/// A type as a test writes it. Named types live in the test module and
/// are checked against the declarations when the registry is built.
#[derive(Clone, Debug)]
pub enum Ty {
    Int,
    String,
    Bool,
    Float,
    Html,
    Named(String),
    Array(Box<Ty>),
    Option(Box<Ty>),
    Tuple(Vec<Ty>),
}

impl Ty {
    pub fn named(name: &str) -> Ty {
        Ty::Named(name.to_string())
    }

    pub fn array(element: Ty) -> Ty {
        Ty::Array(Box::new(element))
    }

    pub fn option(element: Ty) -> Ty {
        Ty::Option(Box::new(element))
    }

    pub fn tuple(elements: impl IntoIterator<Item = Ty>) -> Ty {
        Ty::Tuple(elements.into_iter().collect())
    }
}

impl From<Ty> for Type {
    fn from(ty: Ty) -> Type {
        match ty {
            Ty::Html => Type::Html,
            data => Type::Data(data.into()),
        }
    }
}

impl From<Ty> for DataType {
    fn from(ty: Ty) -> DataType {
        match ty {
            Ty::Int => DataType::Int,
            Ty::String => DataType::String,
            Ty::Bool => DataType::Bool,
            Ty::Float => DataType::Float,
            Ty::Html => panic!("Html is not a data type"),
            Ty::Named(name) => DataType::Named {
                module: test_module(),
                name: type_name(&name),
            },
            Ty::Array(element) => DataType::Array(Box::new((*element).into())),
            Ty::Option(element) => DataType::Option(Box::new((*element).into())),
            Ty::Tuple(elements) => {
                DataType::Tuple(elements.into_iter().map(DataType::from).collect())
            }
        }
    }
}

#[derive(Clone)]
enum Decl {
    Record {
        name: String,
        fields: Vec<(String, DataType)>,
    },
    EnumUnit {
        name: String,
        variants: Vec<String>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Vec<(String, DataType)>)>,
    },
}

#[derive(Clone, Default)]
pub struct TypeRegistryBuilder {
    decls: Vec<Decl>,
}

impl TypeRegistryBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record<'a>(
        mut self,
        name: &str,
        fields: impl IntoIterator<Item = (&'a str, Ty)>,
    ) -> Self {
        self.decls.push(Decl::Record {
            name: name.to_string(),
            fields: fields
                .into_iter()
                .map(|(f, t)| (f.to_string(), t.into()))
                .collect(),
        });
        self
    }

    pub fn enum_unit<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        self.decls.push(Decl::EnumUnit {
            name: name.to_string(),
            variants: variants.into_iter().map(str::to_string).collect(),
        });
        self
    }

    pub fn enum_<'a>(
        mut self,
        name: &str,
        variants: impl IntoIterator<Item = (&'a str, Vec<(&'a str, Ty)>)>,
    ) -> Self {
        self.decls.push(Decl::Enum {
            name: name.to_string(),
            variants: variants
                .into_iter()
                .map(|(v, fields)| {
                    (
                        v.to_string(),
                        fields
                            .into_iter()
                            .map(|(f, t)| (f.to_string(), t.into()))
                            .collect(),
                    )
                })
                .collect(),
        });
        self
    }

    pub fn build(self) -> TypeRegistry {
        let mut registry = TypeRegistry::default();
        for decl in self.decls {
            let (name, def) = match decl {
                Decl::Record { name, fields } => {
                    let fields = fields
                        .into_iter()
                        .map(|(f, typ)| RecordField {
                            name: field_name(&f),
                            typ,
                            examples: None,
                        })
                        .collect();
                    (name, TypeDef::Record { fields })
                }
                Decl::EnumUnit { name, variants } => {
                    let variants = variants
                        .iter()
                        .map(|v| EnumVariant {
                            name: type_name(v),
                            fields: vec![],
                        })
                        .collect();
                    (name, TypeDef::Enum { variants })
                }
                Decl::Enum { name, variants } => {
                    let variants = variants
                        .into_iter()
                        .map(|(v, fields)| EnumVariant {
                            name: type_name(&v),
                            fields: fields
                                .into_iter()
                                .map(|(f, typ)| RecordField {
                                    name: field_name(&f),
                                    typ,
                                    examples: None,
                                })
                                .collect(),
                        })
                        .collect();
                    (name, TypeDef::Enum { variants })
                }
            };
            if registry
                .iter()
                .any(|(_, declared, _)| declared.as_str() == name)
            {
                panic!("duplicate declaration of type `{name}`");
            }
            registry.insert(test_module(), type_name(&name), def);
        }

        // A field may name a type declared after it, so the names are
        // checked once every declaration is in.
        let fields = registry.iter().flat_map(|(_, _, def)| match def {
            TypeDef::Record { fields } => fields.iter().collect::<Vec<_>>(),
            TypeDef::Enum { variants } => variants.iter().flat_map(|v| v.fields.iter()).collect(),
        });
        for field in fields {
            assert_declared(&registry, &field.typ);
        }

        registry
    }
}

/// Panics if `typ` names a type the registry does not declare.
fn assert_declared(registry: &TypeRegistry, typ: &DataType) {
    match typ {
        DataType::String | DataType::Bool | DataType::Int | DataType::Float => {}
        DataType::Array(element) | DataType::Option(element) => assert_declared(registry, element),
        DataType::Tuple(elements) => {
            for element in elements {
                assert_declared(registry, element);
            }
        }
        DataType::Named { name, .. } => {
            if registry.resolve(typ).is_none() {
                panic!("type `{}` is not declared", name.as_str());
            }
        }
    }
}
