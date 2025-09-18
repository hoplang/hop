use crate::dop::expr::TypedExpr;
use crate::hop::ast::ComponentDefinition;

pub mod doctype_injector;

pub use doctype_injector::DoctypeInjector;

use super::ast::TypedComponentDefinition;

/// A transformation that can be applied to a component definition
pub trait ComponentTransform {
    /// Apply the transformation to the component definition
    fn transform(&mut self, component: &mut TypedComponentDefinition);
}

/// Pipeline for running component transformations
pub struct TransformPipeline {
    transforms: Vec<Box<dyn ComponentTransform>>,
}

impl TransformPipeline {
    /// Create a new transform pipeline with default transforms
    pub fn new() -> Self {
        Self {
            transforms: vec![Box::new(DoctypeInjector::new())],
        }
    }

    /// Run all transforms on the given component
    pub fn run(&mut self, component: &mut ComponentDefinition<TypedExpr>) {
        for transform in &mut self.transforms {
            transform.transform(component);
        }
    }
}
