#[cfg(test)]
pub mod builder;
pub mod inlined_ast;
pub mod inlined_node;
pub mod inliner;
pub mod transform;

pub use inlined_ast::{InlinedComponentDeclaration, InlinedViewDeclaration};
pub use inlined_node::InlinedNode;
pub use inliner::Inliner;
