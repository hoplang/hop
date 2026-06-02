use std::fmt::Display;

use crate::annotation::Annotation;
use crate::document::DocumentRange;

#[derive(Clone, Debug)]
pub struct SimpleAnnotation {
    pub range: DocumentRange,
    pub message: String,
}

impl Annotation for SimpleAnnotation {
    fn message(&self) -> String {
        self.message.clone()
    }
    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

impl Display for SimpleAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
