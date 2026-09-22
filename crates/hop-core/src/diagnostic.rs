use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::DocumentRange;

/// A diagnostic is an error or warning that should be displayed for a
/// specific range in a document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    message: String,
    range: DocumentRange,
    severity: DiagnosticSeverity,
}

impl Diagnostic {
    pub(crate) fn new(message: String, range: DocumentRange, severity: DiagnosticSeverity) -> Self {
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

    pub fn severity(&self) -> DiagnosticSeverity {
        self.severity
    }
}
