use crate::document::DocumentPosition;
use crate::document::extract_position::extract_position;
use simple_txtar::{Archive, Builder, File};
use std::{
    env, fs,
    path::{Path, PathBuf},
};

/// Creates a temporary directory with files from a txtar Archive.
///
/// The directory is created with a random name in the system temp directory
/// and populated with all files from the archive.
pub fn temp_dir_from_archive(archive: &Archive) -> std::io::Result<PathBuf> {
    let r = rand::random::<u64>();
    let temp_dir = env::temp_dir().join(format!("hop_test_{}", r));
    fs::create_dir_all(&temp_dir)?;
    for file in archive.iter() {
        let file_path = temp_dir.join(&file.name);
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&file_path, &file.content)?;
    }
    Ok(temp_dir)
}

/// Recursively walks a directory and creates an Archive from all files found.
///
/// Files are sorted by path to ensure deterministic output across different filesystems.
#[allow(dead_code)]
pub fn archive_from_dir(dir: &Path) -> std::io::Result<Archive> {
    let mut builder = Builder::new();
    archive_from_dir_recursive(dir, dir, &mut builder)?;
    Ok(builder.build())
}

#[allow(dead_code)]
fn archive_from_dir_recursive(
    base_dir: &Path,
    current_dir: &Path,
    builder: &mut Builder,
) -> std::io::Result<()> {
    let mut entries: Vec<_> = fs::read_dir(current_dir)?.collect::<Result<Vec<_>, _>>()?;
    // Sort entries to ensure deterministic output - fs::read_dir() order is not guaranteed
    // and can vary between filesystems and OS implementations
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();

        if path.is_dir() {
            archive_from_dir_recursive(base_dir, &path, builder)?;
        } else {
            let relative_path = path.strip_prefix(base_dir).map_err(std::io::Error::other)?;
            let content = fs::read_to_string(&path)?;
            builder.file((relative_path.to_string_lossy().to_string(), content));
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub struct MarkerInfo {
    pub filename: String,
    pub position: DocumentPosition,
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
