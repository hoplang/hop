use std::collections::HashMap;

use crate::document::{CheapString, DocumentCursor};
use crate::hop::typing::r#type::{DataType, Type};
use crate::hop::typing::type_env::{
    FunctionSignature, Name, NameKind, ParamEntry, Row, Tail, TypeEnv,
};
use crate::hop::typing::type_registry::TypeRegistry;
use crate::hop::typing::type_registry_builder::{Ty, TypeRegistryBuilder, test_module};
use crate::symbols::var_name::VarName;

fn var_name(name: &str) -> VarName {
    VarName::parse(name).unwrap_or_else(|e| panic!("invalid var name `{name}`: {e:?}"))
}

#[derive(Clone)]
struct FunctionDecl {
    name: String,
    params: Vec<(String, Type)>,
    return_type: Type,
}

/// Declares the types of a TypeRegistryBuilder together with functions
/// whose signatures may name those types.
#[derive(Clone, Default)]
pub struct TestModuleBuilder {
    types: TypeRegistryBuilder,
    functions: Vec<FunctionDecl>,
}

impl From<TypeRegistryBuilder> for TestModuleBuilder {
    fn from(types: TypeRegistryBuilder) -> Self {
        Self {
            types,
            functions: Vec::new(),
        }
    }
}

impl TestModuleBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn function<'a>(
        mut self,
        name: &str,
        params: impl IntoIterator<Item = (&'a str, Ty)>,
        return_type: Ty,
    ) -> Self {
        self.functions.push(FunctionDecl {
            name: name.to_string(),
            params: params
                .into_iter()
                .map(|(p, t)| (p.to_string(), t.into()))
                .collect(),
            return_type: return_type.into(),
        });
        self
    }

    pub fn build(self) -> TestModule {
        let registry = self.types.build();
        let mut functions = HashMap::new();
        for decl in self.functions {
            let params: Vec<ParamEntry> = decl
                .params
                .into_iter()
                .map(|(p, typ)| ParamEntry {
                    name: var_name(&p),
                    typ,
                    fallback: None,
                })
                .collect();
            let signature = FunctionSignature {
                declared: params.len(),
                row: Row {
                    fields: params,
                    tail: Tail::Closed,
                },
                return_type: decl.return_type,
                rest_param: None,
            };
            if functions.insert(var_name(&decl.name), signature).is_some() {
                panic!("duplicate declaration of function `{}`", decl.name);
            }
        }
        TestModule {
            registry,
            functions,
        }
    }
}

/// An immutable bundle of a TypeRegistry and the settled signatures of the
/// functions declared beside its types.
#[derive(Clone)]
pub struct TestModule {
    registry: TypeRegistry,
    functions: HashMap<VarName, FunctionSignature>,
}

impl TestModule {
    pub fn registry(&self) -> &TypeRegistry {
        &self.registry
    }

    /// The environment of a module that declares these types and
    /// functions.
    pub fn type_env(&self) -> TypeEnv {
        let decl_range = DocumentCursor::new(test_module(), String::new()).eof_range();
        let mut type_env = TypeEnv {
            names: type_names(&self.registry),
            functions: Default::default(),
        };
        for (name, signature) in &self.functions {
            type_env.names.insert(
                name.to_cheap_string(),
                Name {
                    kind: NameKind::Function,
                    definition_range: decl_range.clone(),
                    import_range: None,
                },
            );
            type_env
                .functions
                .insert(name.to_cheap_string(), signature.clone());
        }
        type_env
    }
}

/// The names table of a module that declares the types in the registry.
fn type_names(registry: &TypeRegistry) -> HashMap<CheapString, Name> {
    let decl_range = DocumentCursor::new(test_module(), String::new()).eof_range();
    registry
        .iter()
        .map(|(module, name, _)| {
            (
                name.to_cheap_string(),
                Name {
                    kind: NameKind::Type(DataType::Named {
                        module: module.clone(),
                        name: name.clone(),
                    }),
                    definition_range: decl_range.clone(),
                    import_range: None,
                },
            )
        })
        .collect()
}
