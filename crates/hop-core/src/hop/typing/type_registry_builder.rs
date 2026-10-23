use crate::document::DocumentCursor;
use crate::document_annotator::DocumentAnnotator;
use crate::hop::parsing::parse_type;
use crate::hop::typing::resolve_type::resolve_type;
use crate::hop::typing::r#type::{DataType, Type};
use crate::hop::typing::type_env::{Name, NameKind, TypeEnv};
use crate::hop::typing::type_registry::{EnumVariant, RecordField, TypeDef, TypeRegistry};
use crate::root_contained_file_path::RootContainedFilePath;
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;

/// The module all test-declared types live in.
fn test_module() -> RootContainedFilePath {
    RootContainedFilePath::new("test.hop").unwrap()
}

fn type_name(name: &str) -> TypeName {
    TypeName::parse(name).unwrap_or_else(|e| panic!("invalid type name `{name}`: {e:?}"))
}

fn field_name(name: &str) -> FieldName {
    FieldName::parse(name).unwrap_or_else(|e| panic!("invalid field name `{name}`: {e:?}"))
}

#[derive(Clone)]
enum Decl {
    Record {
        name: String,
        fields: Vec<(String, String)>,
    },
    EnumUnit {
        name: String,
        variants: Vec<String>,
    },
    Enum {
        name: String,
        variants: Vec<(String, Vec<(String, String)>)>,
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
        fields: impl IntoIterator<Item = (&'a str, &'a str)>,
    ) -> Self {
        self.decls.push(Decl::Record {
            name: name.to_string(),
            fields: fields
                .into_iter()
                .map(|(f, t)| (f.to_string(), t.to_string()))
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
        variants: impl IntoIterator<Item = (&'a str, Vec<(&'a str, &'a str)>)>,
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
                            .map(|(f, t)| (f.to_string(), t.to_string()))
                            .collect(),
                    )
                })
                .collect(),
        });
        self
    }

    pub fn build(self) -> TestTypes {
        let mut types = TestTypes {
            module: test_module(),
            registry: TypeRegistry::default(),
        };

        // Every name is registered with an empty definition before any
        // field is resolved, so a field may name a type declared later.
        for decl in &self.decls {
            let (name, def) = match decl {
                Decl::Record { name, .. } => (name, TypeDef::Record { fields: vec![] }),
                Decl::EnumUnit { name, .. } | Decl::Enum { name, .. } => {
                    (name, TypeDef::Enum { variants: vec![] })
                }
            };
            if types.declared().any(|declared| declared.as_str() == name) {
                panic!("duplicate declaration of type `{name}`");
            }
            types
                .registry
                .insert(types.module.clone(), type_name(name), def);
        }

        for decl in self.decls {
            match decl {
                Decl::Record { name, fields } => {
                    let fields = fields
                        .iter()
                        .map(|(f, t)| {
                            let Type::Data(typ) = types.resolve(t) else {
                                panic!("field `{f}` cannot have type Html")
                            };
                            RecordField {
                                name: field_name(f),
                                typ,
                                examples: None,
                            }
                        })
                        .collect();
                    types.registry.insert(
                        types.module.clone(),
                        type_name(&name),
                        TypeDef::Record { fields },
                    );
                }
                Decl::EnumUnit { name, variants } => {
                    let variants = variants
                        .iter()
                        .map(|v| EnumVariant {
                            name: type_name(v),
                            fields: vec![],
                        })
                        .collect();
                    types.registry.insert(
                        types.module.clone(),
                        type_name(&name),
                        TypeDef::Enum { variants },
                    );
                }
                Decl::Enum { name, variants } => {
                    let variants = variants
                        .iter()
                        .map(|(v, fields)| EnumVariant {
                            name: type_name(v),
                            fields: fields
                                .iter()
                                .map(|(f, t)| {
                                    let Type::Data(typ) = types.resolve(t) else {
                                        panic!("field `{f}` cannot have type Html")
                                    };
                                    RecordField {
                                        name: field_name(f),
                                        typ,
                                        examples: None,
                                    }
                                })
                                .collect(),
                        })
                        .collect();
                    types.registry.insert(
                        types.module.clone(),
                        type_name(&name),
                        TypeDef::Enum { variants },
                    );
                }
            }
        }

        types
    }
}

/// An immutable bundle of a TypeRegistry and handles to the types
/// registered in it. Every DataType::Named handed out by this value has a
/// matching TypeDef in the registry.
#[derive(Clone)]
pub struct TestTypes {
    module: RootContainedFilePath,
    registry: TypeRegistry,
}

impl TestTypes {
    pub fn registry(&self) -> &TypeRegistry {
        &self.registry
    }

    pub fn module(&self) -> &RootContainedFilePath {
        &self.module
    }

    /// The names of the declared types, sorted.
    fn declared(&self) -> impl Iterator<Item = &TypeName> {
        self.registry
            .iter()
            .filter(|(module, _, _)| *module == &self.module)
            .map(|(_, name, _)| name)
    }

    pub fn named(&self, name: &str) -> Type {
        let typ = DataType::Named {
            module: self.module.clone(),
            name: type_name(name),
        };
        if self.registry.resolve(&typ).is_none() {
            panic!(
                "type `{name}` is not declared, declared types: {:?}",
                self.declared().map(|k| k.as_str()).collect::<Vec<_>>()
            )
        }
        Type::Data(typ)
    }

    pub fn resolve(&self, type_str: &str) -> Type {
        let mut iter = DocumentCursor::new(self.module.clone(), type_str.to_string());
        let mut comments = Vec::new();
        let mut errors = Vec::new();
        let parsed = parse_type(&mut iter, &mut comments, &mut errors);
        let Ok(parsed) = parsed else {
            panic!("failed to parse type `{type_str}`: {errors:?}");
        };
        if !errors.is_empty() {
            panic!("failed to parse type `{type_str}`: {errors:?}");
        }
        if iter.peek().is_some() {
            panic!("trailing input after type `{type_str}`");
        }
        let type_env = self.type_env();
        let mut definition_links = Vec::new();
        let mut type_errors = Vec::new();
        resolve_type(
            &parsed,
            &type_env.names,
            &mut definition_links,
            &mut type_errors,
        )
        .unwrap_or_else(|| {
            let rendered = DocumentAnnotator::new()
                .with_severity_label()
                .without_location()
                .without_line_numbers()
                .annotate(type_errors.iter().map(|e| e.to_diagnostic()))
                .render();
            panic!("failed to resolve type `{type_str}`:\n{rendered}")
        })
    }

    /// The environment of a module that declares these types and nothing
    /// else.
    pub fn type_env(&self) -> TypeEnv {
        let decl_range = DocumentCursor::new(self.module.clone(), String::new()).eof_range();
        let names = self.declared().map(|name| {
            (
                name.to_cheap_string(),
                Name {
                    kind: NameKind::Type(DataType::Named {
                        module: self.module.clone(),
                        name: name.clone(),
                    }),
                    definition_range: decl_range.clone(),
                    import_range: None,
                },
            )
        });
        TypeEnv {
            names: names.collect(),
            functions: Default::default(),
        }
    }
}
