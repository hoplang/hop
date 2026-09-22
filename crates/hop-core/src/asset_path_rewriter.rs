use crate::asset_path::AssetPath;

/// Maps the [AssetPath] of an asset to the URL emitted for it in compiled output.
pub trait AssetPathRewriter: Send + Sync {
    fn rewrite(&self, asset_path: &AssetPath) -> String;
}

impl<F: Fn(&AssetPath) -> String + Send + Sync> AssetPathRewriter for F {
    fn rewrite(&self, asset_path: &AssetPath) -> String {
        self(asset_path)
    }
}
