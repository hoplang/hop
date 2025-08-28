use super::position_marker::extract_position;
use crate::common::Position;
use simple_txtar::{Archive, Builder, File};

#[derive(Debug, Clone, PartialEq)]
pub struct MarkerInfo {
    pub filename: String,
    pub position: Position,
}

/// Extracts all position markers from an archive and returns the cleaned archive
/// along with information about each marker found.
///
/// # Returns
/// - The cleaned archive (with all markers removed)
/// - A vector of MarkerInfo containing filenames and positions for each marker
pub fn extract_markers_from_archive(archive: &Archive) -> (Archive, Vec<MarkerInfo>) {
    let mut markers = Vec::new();
    let mut builder = Builder::new();
    
    for file in archive.iter() {
        if let Some((clean_content, pos)) = extract_position(&file.content) {
            markers.push(MarkerInfo {
                filename: file.name.clone(),
                position: pos,
            });
            builder.file(File::new(file.name.clone(), clean_content));
        } else {
            builder.file(file.clone());
        }
    }
    
    (builder.build(), markers)
}

