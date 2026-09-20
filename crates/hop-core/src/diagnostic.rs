use crate::document::DocumentRange;
use crate::severity::Severity;

/// A diagnostic is an error or warning that should be displayed for a
/// specific range in a document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    message: String,
    range: DocumentRange,
    severity: Severity,
}

impl Diagnostic {
    pub(crate) fn new(message: String, range: DocumentRange, severity: Severity) -> Self {
        Diagnostic {
            message,
            range,
            severity,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn range(&self) -> &DocumentRange {
        &self.range
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }
}
