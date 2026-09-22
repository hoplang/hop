use crate::asset_path::AssetPath;
use crate::diagnostic::Diagnostic;
use crate::diagnostic_severity::DiagnosticSeverity;
use crate::document::DocumentRange;

/// An [AssetPath](crate::AssetPath) located at a
/// [DocumentRange](crate::DocumentRange) inside a [Document](crate::Document).
///
/// Produced via an `asset!(...)` macro (in hop) or an `--asset(...)` call (in CSS).
#[derive(Debug, Clone)]
pub struct AssetReference {
    /// The full range of the asset reference (including the macro/function call).
    pub(crate) range: DocumentRange,
    /// The resolved path of the asset (e.g. `img/logo.svg`).
    pub(crate) path: AssetPath,
}

impl AssetReference {
    /// The resolved path of the asset (e.g. `img/logo.svg`).
    pub fn path(&self) -> &AssetPath {
        &self.path
    }

    /// The diagnostic to report when the referenced asset does not exist.
    pub fn not_found(&self) -> Diagnostic {
        Diagnostic {
            message: format!("asset `{}` was not found", self.path),
            range: self.range.clone(),
            severity: DiagnosticSeverity::Error,
        }
    }
}
