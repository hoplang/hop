pub mod document_annotator;
pub mod document_cursor;
pub mod document_info;
pub mod document_position;

#[cfg(test)]
pub mod extract_position;

pub use document_annotator::DocumentAnnotator;
pub use document_position::DocumentPosition;

#[cfg(test)]
pub use document_annotator::SimpleAnnotation;
