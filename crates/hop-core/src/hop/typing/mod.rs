mod compile_match;
mod export;
mod resolve_type;
mod rest_spread;
mod r#type;
mod type_env;
mod type_error;
mod type_registry;
mod typecheck;
mod typecheck_call;
mod typecheck_expr;
mod typecheck_macro;
mod typecheck_match;
mod typecheck_node;
mod typecheck_pattern;
mod typed_ast;
mod typed_expr;
mod typed_pattern;
mod variable_scope;

pub use compile_match::CaseVar;
pub use compile_match::Decision;
pub use export::Export;
pub use r#type::{ComparableType, EquatableType, NumericType, Type};
pub use type_error::TypeError;
pub use type_registry::{EnumVariant, ResolvedType, TypeRegistry};
pub use typecheck::typecheck;
pub use typed_ast::{TypedAst, TypedFunctionDeclaration, TypedPageDeclaration, TypedParameter};
pub use typed_expr::{
    TypedAttribute, TypedAttrs, TypedExpr, TypedLoopSource, TypedRecordUpdateField,
};
pub use typed_pattern::TypedPattern;

#[cfg(test)]
mod type_registry_builder;
#[cfg(test)]
mod typed_ast_builder;

#[cfg(test)]
pub use type_registry_builder::{TestTypes, TypeRegistryBuilder};
#[cfg(test)]
pub use typed_ast_builder::{build_page, build_page_no_params, build_page_with_types};
