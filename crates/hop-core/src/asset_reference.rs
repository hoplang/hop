use crate::diagnostic::Diagnostic;
use crate::document::DocumentRange;
use crate::document_id::DocumentId;
use crate::severity::Severity;

/// A reference to an asset via an `asset!(...)` macro (in hop) or an `--asset(...)` call (in CSS).
#[derive(Debug, Clone)]
pub struct AssetReference {
    /// The full range of the asset reference (including the macro/function call).
    pub(crate) range: DocumentRange,
    /// The document_id for the asset (e.g. `img/logo.svg`).
    pub(crate) document_id: DocumentId,
}

impl AssetReference {
    /// The document_id for the asset (e.g. `img/logo.svg`).
    pub fn document_id(&self) -> &DocumentId {
        &self.document_id
    }

    /// The diagnostic to report when the referenced asset does not exist.
    pub fn not_found(&self) -> Diagnostic {
        Diagnostic::new(
            format!("asset `{}` was not found", self.document_id),
            self.range.clone(),
            Severity::Error,
        )
    }
}
