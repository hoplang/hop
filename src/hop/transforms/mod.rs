use crate::dop::Type;
use crate::hop::ast::Ast;

pub mod doctype_injector;

pub use doctype_injector::DoctypeInjector;

/// A transformation that can be applied to a Hop AST
pub trait AstTransform {
    /// Apply the transformation to the AST
    fn transform(&mut self, ast: &mut Ast<Type>);
}

/// Pipeline for running AST transformations
pub struct TransformPipeline {
    transforms: Vec<Box<dyn AstTransform>>,
}

impl TransformPipeline {
    /// Create a new transform pipeline with default transforms
    pub fn new() -> Self {
        Self {
            transforms: vec![
                Box::new(DoctypeInjector::new()),
            ],
        }
    }

    /// Run all transforms on the given AST
    pub fn run(&mut self, ast: &mut Ast<Type>) {
        for transform in &mut self.transforms {
            transform.transform(ast);
        }
    }
}