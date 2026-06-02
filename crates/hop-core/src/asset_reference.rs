use crate::asset_error::AssetError;
use crate::document::DocumentRange;
use crate::document_id::DocumentId;
use crate::project::Project;

/// A reference to an asset via an `asset!(...)` macro (in hop) or an `--asset(...)` call (in CSS).
#[derive(Debug, Clone)]
pub struct AssetReference {
    /// The full range of the asset reference (including the macro/function call).
    pub range: DocumentRange,
    /// The document_id for the asset (e.g. `img/logo.svg`).
    pub document_id: DocumentId,
}

/// Validate that asset references point to files that exist on disk
/// relative to the given project root.
pub fn validate_asset_existence(
    asset_references: &[AssetReference],
    project: &Project,
) -> Vec<AssetError> {
    let mut errors = Vec::new();
    for asset_ref in asset_references {
        let res = project.document_exists(&asset_ref.document_id);
        if !res.is_ok_and(|b| b) {
            errors.push(AssetError::MissingAsset {
                document_id: asset_ref.document_id.clone(),
                range: asset_ref.range.clone(),
            });
        }
    }
    errors
}
