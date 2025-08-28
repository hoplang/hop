use simple_txtar::{Archive, Builder};
use std::{env, fs, path::{Path, PathBuf}};

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
pub fn archive_from_dir(dir: &Path) -> std::io::Result<Archive> {
    let mut builder = Builder::new();
    archive_from_dir_recursive(dir, dir, &mut builder)?;
    Ok(builder.build())
}

fn archive_from_dir_recursive(
    base_dir: &Path,
    current_dir: &Path,
    builder: &mut Builder,
) -> std::io::Result<()> {
    for entry in fs::read_dir(current_dir)? {
        let entry = entry?;
        let path = entry.path();
        
        if path.is_dir() {
            archive_from_dir_recursive(base_dir, &path, builder)?;
        } else {
            let relative_path = path.strip_prefix(base_dir)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
            let content = fs::read_to_string(&path)?;
            builder.file((
                relative_path.to_string_lossy().to_string(),
                content,
            ));
        }
    }
    Ok(())
}