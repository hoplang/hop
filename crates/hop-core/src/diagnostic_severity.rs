/// The severity of a [Diagnostic](crate::Diagnostic).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}
