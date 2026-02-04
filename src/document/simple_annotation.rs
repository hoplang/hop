use std::cmp::Ordering;
use std::fmt::Display;

use super::{Annotation, DocumentRange};

/// Simple annotation implementation for basic use cases
#[derive(Clone, Debug)]
pub struct SimpleAnnotation {
    pub range: DocumentRange,
    pub message: String,
}

impl PartialEq for SimpleAnnotation {
    fn eq(&self, other: &Self) -> bool {
        self.range.start() == other.range.start()
            && self.range.end() == other.range.end()
            && self.message == other.message
    }
}

impl Eq for SimpleAnnotation {}

impl PartialOrd for SimpleAnnotation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SimpleAnnotation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.range
            .start()
            .cmp(&other.range.start())
            .then_with(|| self.range.end().cmp(&other.range.end()))
            .then_with(|| self.message.cmp(&other.message))
    }
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
