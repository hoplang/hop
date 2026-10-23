use std::collections::HashMap;

use crate::document::DocumentCursor;
use crate::hop::typing::type_env::{
    FunctionSignature, Name, NameKind, ParamEntry, Row, Tail, TypeEnv,
};
use crate::hop::typing::type_registry_builder::{TestTypes, TypeRegistryBuilder};
use crate::symbols::var_name::VarName;

fn var_name(name: &str) -> VarName {
    VarName::parse(name).unwrap_or_else(|e| panic!("invalid var name `{name}`: {e:?}"))
}

#[derive(Clone)]
struct FunctionDecl {
    name: String,
    params: Vec<(String, String)>,
    return_type: String,
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
        params: impl IntoIterator<Item = (&'a str, &'a str)>,
        return_type: &str,
    ) -> Self {
        self.functions.push(FunctionDecl {
            name: name.to_string(),
            params: params
                .into_iter()
                .map(|(p, t)| (p.to_string(), t.to_string()))
                .collect(),
            return_type: return_type.to_string(),
        });
        self
    }

    pub fn build(self) -> TestModule {
        let types = self.types.build();
        let mut functions = HashMap::new();
        for decl in self.functions {
            let params: Vec<ParamEntry> = decl
                .params
                .iter()
                .map(|(p, t)| ParamEntry {
                    name: var_name(p),
                    typ: types.resolve(t),
                    fallback: None,
                })
                .collect();
            let signature = FunctionSignature {
                declared: params.len(),
                row: Row {
                    fields: params,
                    tail: Tail::Closed,
                },
                return_type: types.resolve(&decl.return_type),
                rest_param: None,
            };
            if functions.insert(var_name(&decl.name), signature).is_some() {
                panic!("duplicate declaration of function `{}`", decl.name);
            }
        }
        TestModule { types, functions }
    }
}

/// An immutable bundle of TestTypes and the settled signatures of the
/// functions declared beside them.
#[derive(Clone)]
pub struct TestModule {
    types: TestTypes,
    functions: HashMap<VarName, FunctionSignature>,
}

impl TestModule {
    pub fn types(&self) -> &TestTypes {
        &self.types
    }

    /// The environment of a module that declares these types and
    /// functions.
    pub fn type_env(&self) -> TypeEnv {
        let decl_range =
            DocumentCursor::new(self.types.module().clone(), String::new()).eof_range();
        let mut type_env = self.types.type_env();
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
