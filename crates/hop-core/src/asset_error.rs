use crate::annotation::Annotation;
use crate::document::DocumentRange;
use crate::document_id::DocumentId;

#[derive(Debug, Clone)]
pub enum AssetError {
    /// An asset referenced via `asset!()` does not exist on disk.
    MissingAsset {
        document_id: DocumentId,
        range: DocumentRange,
    },
}

impl Annotation for AssetError {
    fn message(&self) -> String {
        match self {
            AssetError::MissingAsset { document_id, .. } => {
                format!("asset `{document_id}` does not exist on disk")
            }
        }
    }

    fn range(&self) -> &DocumentRange {
        match self {
            AssetError::MissingAsset { range, .. } => range,
        }
    }
}
