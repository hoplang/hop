pub use simple_txtar::{Archive, Builder, File};
use std::{fs, path::Path};

/// Writes files from a txtar Archive into the given directory.
pub fn write_archive_to_dir(archive: &Archive, dir: &Path) -> std::io::Result<()> {
    for file in archive.iter() {
        let file_path = dir.join(&file.name);
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&file_path, &file.content)?;
    }
    Ok(())
}

/// Recursively walks a directory and creates an Archive from all files found.
///
/// Files are sorted by path to ensure deterministic output across different filesystems.
pub fn read_archive_from_dir(path: &Path) -> std::io::Result<Archive> {
    let mut builder = Builder::new();
    let mut stack: Vec<std::path::PathBuf> = vec![path.to_path_buf()];

    while let Some(current) = stack.pop() {
        if current.is_dir() {
            let mut entries: Vec<_> = fs::read_dir(&current)?.collect::<Result<Vec<_>, _>>()?;
            entries.sort_by_key(|entry| entry.path());
            for entry in entries.into_iter().rev() {
                stack.push(entry.path());
            }
        } else {
            let relative_path = current.strip_prefix(path).map_err(std::io::Error::other)?;
            let content = fs::read_to_string(&current)?;
            builder.file((relative_path.to_string_lossy().to_string(), content));
        }
    }

    Ok(builder.build())
}
