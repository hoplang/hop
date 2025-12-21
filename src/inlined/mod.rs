pub mod inlined_ast;
#[cfg(test)]
pub mod inlined_test_builder;
pub mod inliner;
pub mod transform;

pub use inlined_ast::{
    InlinedAttribute, InlinedAttributeValue, InlinedComponentDeclaration, InlinedNode,
};
pub use inliner::Inliner;
pub use transform::{DoctypeInjector, HtmlStructureInjector, TailwindInjector};
