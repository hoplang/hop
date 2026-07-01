use crate::annotation::Annotation;
use crate::document::DocumentRange;
use crate::document_id::DocumentId;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct AssetError {
    kind: AssetErrorKind,
    range: DocumentRange,
}

impl AssetError {
    pub(crate) fn new(kind: AssetErrorKind, range: DocumentRange) -> Self {
        AssetError { kind, range }
    }
}

impl Annotation for AssetError {
    fn message(&self) -> String {
        self.kind.to_string()
    }

    fn range(&self) -> &DocumentRange {
        &self.range
    }
}

#[derive(Debug, Clone, Error)]
pub(crate) enum AssetErrorKind {
    /// An asset referenced via `asset!()` does not exist on disk.
    #[error("asset `{document_id}` does not exist on disk")]
    MissingAsset { document_id: DocumentId },
}
