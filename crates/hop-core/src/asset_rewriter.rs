use std::collections::HashMap;

use crate::document_id::DocumentId;

pub trait AssetRewriter: Send + Sync {
    fn rewrite(&self, document_id: &DocumentId) -> String;
}

pub struct ReplacingAssetRewriter {
    replacements: HashMap<DocumentId, String>,
}

impl ReplacingAssetRewriter {
    pub fn new(replacements: HashMap<DocumentId, String>) -> Self {
        Self { replacements }
    }
}

impl AssetRewriter for ReplacingAssetRewriter {
    fn rewrite(&self, document_id: &DocumentId) -> String {
        self.replacements.get(document_id).unwrap().clone()
    }
}

pub struct PrefixingAssetRewriter {
    prefix: String,
}

impl PrefixingAssetRewriter {
    pub fn new(prefix: String) -> Self {
        Self { prefix }
    }
}

impl AssetRewriter for PrefixingAssetRewriter {
    fn rewrite(&self, document_id: &DocumentId) -> String {
        format!("/{}/{}", self.prefix.trim_matches('/'), document_id)
    }
}
