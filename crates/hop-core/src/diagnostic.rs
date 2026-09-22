use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::DocumentRange;

/// An error or warning that should be displayed for a
/// specific [DocumentRange](crate::DocumentRange) in a [Document](crate::Document).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub(crate) message: String,
    pub(crate) range: DocumentRange,
    pub(crate) severity: DiagnosticSeverity,
}

impl Diagnostic {
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
