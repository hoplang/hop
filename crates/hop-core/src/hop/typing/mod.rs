mod compile_match;
mod export;
mod resolve_type;
mod rest_spread;
mod r#type;
mod type_env;
mod type_registry;
#[cfg(test)]
mod type_registry_builder;
mod typecheck;
mod typecheck_call;
mod typecheck_expr;
mod typecheck_match;
mod typecheck_node;
mod typecheck_pattern;
mod typed_ast;
#[cfg(test)]
mod typed_ast_builder;
mod typed_expr;
mod typed_match_pattern;
mod variable_scope;

pub use compile_match::CaseVar;
pub use compile_match::Decision;
pub use export::Export;
pub use r#type::{ComparableType, EquatableType, NumericType, Type};
pub use type_env::{FunctionSignature, ParamEntry, Tail};
pub use type_registry::{EnumVariant, ResolvedType, TypeRegistry};
#[cfg(test)]
pub use type_registry_builder::{TestTypes, TypeRegistryBuilder};
pub use typecheck::typecheck;
pub use typed_ast::{TypedAst, TypedFunctionDeclaration, TypedPageDeclaration, TypedParameter};
#[cfg(test)]
pub use typed_ast_builder::{build_page, build_page_no_params, build_page_with_types};
pub use typed_expr::{
    TypedAttribute, TypedAttrs, TypedExpr, TypedLoopSource, TypedRecordUpdateField,
};
pub use typed_match_pattern::TypedMatchPattern;
