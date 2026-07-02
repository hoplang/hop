use std::collections::{BTreeMap, VecDeque};
use std::sync::Arc;

use crate::document::{DocumentCursor, DocumentRange};
use crate::document_annotator::DocumentAnnotator;
use crate::document_id::DocumentId;
use crate::expr::ExamplesAnnotation;
use crate::expr::parsing::parse_type::parse_type;
use crate::expr::typing::r#type::{EnumVariant, Type, TypeBinding};
use crate::expr::typing::type_checker::resolve_type;
use crate::expr::typing::type_registry::{ResolvedType, TypeDef, TypeRegistry};
use crate::symbols::field_name::FieldName;
use crate::symbols::type_name::TypeName;
use crate::variable_scope::VariableScope;

/// The module all test-declared types live in.
fn test_module() -> DocumentId {
    DocumentId::new("test.hop").unwrap()
}

fn type_name(name: &str) -> TypeName {
    TypeName::new(name).unwrap_or_else(|e| panic!("invalid type name `{name}`: {e:?}"))
}

fn field_name(name: &str) -> FieldName {
    FieldName::new(name).unwrap_or_else(|e| panic!("invalid field name `{name}`: {e:?}"))
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
        let module = test_module();
        let mut named = BTreeMap::new();

        for decl in &self.decls {
            let name = match decl {
                Decl::Record { name, .. }
                | Decl::EnumUnit { name, .. }
                | Decl::Enum { name, .. } => name,
            };
            let type_name = type_name(name);
            let typ = Arc::new(Type::Named {
                module: module.clone(),
                name: type_name.clone(),
            });
            if named.insert(type_name, typ).is_some() {
                panic!("duplicate declaration of type `{name}`");
            }
        }

        let mut types = TestTypes {
            module,
            registry: TypeRegistry::default(),
            named,
        };

        for decl in self.decls {
            match decl {
                Decl::Record { name, fields } => {
                    let fields = fields
                        .iter()
                        .map(|(f, t)| (field_name(f), types.resolve(t), None))
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
                                .map(|(f, t)| (field_name(f), types.resolve(t), None))
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
/// registered in it. Every Type::Named handed out by this value has a
/// matching TypeDef in the registry.
#[derive(Clone)]
pub struct TestTypes {
    module: DocumentId,
    registry: TypeRegistry,
    named: BTreeMap<TypeName, Arc<Type>>,
}

impl TestTypes {
    pub fn empty() -> Self {
        TypeRegistryBuilder::new().build()
    }

    pub fn registry(&self) -> &TypeRegistry {
        &self.registry
    }

    pub fn module(&self) -> &DocumentId {
        &self.module
    }

    pub fn named(&self, name: &str) -> Arc<Type> {
        self.named
            .get(&type_name(name))
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "type `{name}` is not declared, declared types: {:?}",
                    self.named.keys().map(|k| k.as_str()).collect::<Vec<_>>()
                )
            })
    }

    pub fn resolve(&self, type_str: &str) -> Arc<Type> {
        let cursor = DocumentCursor::new(self.module.clone(), type_str.to_string());
        let range = cursor.range();
        let mut iter = cursor.peekable();
        let mut comments = VecDeque::new();
        let mut errors = Vec::new();
        let parsed = parse_type(&mut iter, &mut comments, &mut errors, &range);
        let Some(parsed) = parsed else {
            panic!("failed to parse type `{type_str}`: {errors:?}");
        };
        if !errors.is_empty() {
            panic!("failed to parse type `{type_str}`: {errors:?}");
        }
        if iter.peek().is_some() {
            panic!("trailing input after type `{type_str}`");
        }
        let mut type_env = self.type_env();
        let mut definition_links = Vec::new();
        resolve_type(&parsed, &mut type_env, &mut definition_links).unwrap_or_else(|error| {
            let rendered = DocumentAnnotator::new()
                .with_label("error")
                .without_location()
                .without_line_numbers()
                .annotate(&self.module, [error])
                .render();
            panic!("failed to resolve type `{type_str}`:\n{rendered}")
        })
    }

    pub fn record_fields(
        &self,
        name: &str,
    ) -> &[(FieldName, Arc<Type>, Option<ExamplesAnnotation>)] {
        let typ = self.named.get(&type_name(name));
        match typ.map(|typ| self.registry.resolve(typ)) {
            Some(Some(ResolvedType::Record { fields, .. })) => fields,
            _ => panic!("no record `{name}` is declared"),
        }
    }

    pub fn enum_variants(&self, name: &str) -> &[EnumVariant] {
        let typ = self.named.get(&type_name(name));
        match typ.map(|typ| self.registry.resolve(typ)) {
            Some(Some(ResolvedType::Enum { variants, .. })) => variants,
            _ => panic!("no enum `{name}` is declared"),
        }
    }

    pub fn type_env(&self) -> VariableScope<TypeName, (TypeBinding, DocumentRange)> {
        let decl_range = DocumentCursor::new(self.module.clone(), String::new()).range();
        let mut env = VariableScope::new();
        for (name, typ) in &self.named {
            let _ = env.push(
                name.clone(),
                (TypeBinding::Value(typ.clone()), decl_range.clone()),
            );
        }
        env
    }
}
