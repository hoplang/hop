mod asset_reference;
mod asset_rewriter;
mod config;
mod css;
mod css_error;
mod definition_link;
mod dependency_graph;
mod diagnostic;
mod document;
mod document_annotator;
mod document_id;
#[cfg(test)]
mod end_to_end_tests;
mod examples_annotation;
#[cfg(test)]
mod extract_position;
mod hop;
mod hover_annotation;
mod html;
mod ir;
mod itertools;
mod orchestrator;
mod parse_error;
mod program;
mod project;
mod severity;
mod symbols;
mod type_error;

// Public API
pub use asset_reference::AssetReference;
pub use asset_rewriter::AssetRewriter;
pub use config::{Config, TargetLanguage};
pub use diagnostic::Diagnostic;
pub use document::{Document, DocumentPosition, DocumentRange, PositionEncoding};
pub use document_annotator::DocumentAnnotator;
pub use document_id::{DocumentId, DocumentIdError};
pub use program::{EvaluatePageError, FormatError, Program};
pub use project::{PathError, Project, ProjectError};
pub use severity::Severity;
