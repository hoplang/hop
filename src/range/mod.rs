pub mod range;
pub mod ranged_chars;
pub mod ranged_string;
pub mod source_annotator;

pub use range::{Position, Range, Ranged};
pub use ranged_chars::RangedChars;
pub use ranged_string::RangedString;
pub use source_annotator::{Annotated, SourceAnnotator};

#[cfg(test)]
pub use source_annotator::SimpleAnnotation;