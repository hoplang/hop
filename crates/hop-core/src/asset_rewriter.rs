use crate::document_id::DocumentId;

/// Maps an asset's document id to the URL emitted for it in compiled output.
pub trait AssetRewriter: Send + Sync {
    fn rewrite(&self, document_id: &DocumentId) -> String;
}

impl<F: Fn(&DocumentId) -> String + Send + Sync> AssetRewriter for F {
    fn rewrite(&self, document_id: &DocumentId) -> String {
        self(document_id)
    }
}
