use crate::root_relative_file_path::RootRelativeFilePath;

/// Maps the [RootRelativeFilePath] of an asset to the URL emitted for it in compiled output.
pub trait AssetPathRewriter: Send + Sync {
    fn rewrite(&self, asset_path: &RootRelativeFilePath) -> String;
}

impl<F: Fn(&RootRelativeFilePath) -> String + Send + Sync> AssetPathRewriter for F {
    fn rewrite(&self, asset_path: &RootRelativeFilePath) -> String {
        self(asset_path)
    }
}
