use crate::dop::{Expr, Type};
use crate::hop::syntax::ast::{Ast, Attribute, ComponentDefinition, Record};

pub type TypedAst = Ast<Expr, Type>;
pub type TypedAttribute = Attribute<Expr>;
pub type TypedRecord = Record<Type>;
pub type TypedComponentDefinition = ComponentDefinition<Expr, Type>;
