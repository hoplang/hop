pub mod semantics;
pub mod symbols;
pub mod syntax;

pub use semantics::{Expr, Type, resolve_type, typecheck_expr};
pub use symbols::VarName;
pub use syntax::{
    Declaration, ParseError, ParseTree, ParsedType, Parser, RecordDeclaration,
    RecordDeclarationField, Token, Tokenizer,
};
