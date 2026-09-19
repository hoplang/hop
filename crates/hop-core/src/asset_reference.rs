use crate::document::DocumentRange;
use crate::document_id::DocumentId;

/// A reference to an asset via an `asset!(...)` macro (in hop) or an `--asset(...)` call (in CSS).
#[derive(Debug, Clone)]
pub struct AssetReference {
    /// The full range of the asset reference (including the macro/function call).
    pub range: DocumentRange,
    /// The document_id for the asset (e.g. `img/logo.svg`).
    pub document_id: DocumentId,
}
