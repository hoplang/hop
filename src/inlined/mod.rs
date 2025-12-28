#[cfg(test)]
pub mod builder;
pub mod inlined_ast;
pub mod inliner;
pub mod transform;

pub use inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
};
pub use inliner::Inliner;
pub use transform::{DoctypeInjector, HtmlStructureInjector, MetaInjector, TailwindInjector};
