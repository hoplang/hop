use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum TailwindError {
    #[error("Failed to extract binary: {0}")]
    Extraction(#[source] std::io::Error),
    #[error("Failed to decompress binary: {0}")]
    Decompression(#[source] std::io::Error),
    #[error("Tailwind CSS failed: {stderr}")]
    ExecutionFailed { stderr: String },
    #[error("Failed to spawn process: {0}")]
    Spawn(#[source] std::io::Error),
}

pub struct TailwindRunner {
    binary_path: PathBuf,
}

pub struct TailwindConfig {
    pub input: PathBuf,
    pub output: PathBuf,
    pub working_dir: PathBuf,
}

impl TailwindRunner {
    pub async fn new(extraction_path: PathBuf) -> Result<Self, TailwindError> {
        let binary_path = extract_binary(extraction_path).await?;
        Ok(Self { binary_path })
    }

    pub async fn run_once(&self, config: &TailwindConfig) -> Result<String, TailwindError> {
        let output = tokio::process::Command::new(&self.binary_path)
            .arg("--input")
            .arg(&config.input)
            .arg("--output")
            .arg(&config.output)
            .arg("--minify")
            .current_dir(&config.working_dir)
            .output()
            .await
            .map_err(TailwindError::Spawn)?;

        if !output.status.success() {
            return Err(TailwindError::ExecutionFailed {
                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            });
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    pub fn watch(&self, config: &TailwindConfig) -> Result<tokio::process::Child, TailwindError> {
        let child = tokio::process::Command::new(&self.binary_path)
            .arg("--watch=always")
            .arg("--input")
            .arg(&config.input)
            .arg("--output")
            .arg(&config.output)
            .arg("--minify")
            .current_dir(&config.working_dir)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
            .map_err(TailwindError::Spawn)?;

        Ok(child)
    }
}

#[cfg(target_os = "linux")]
static TAILWIND_BINARY: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/binaries/tailwindcss-linux"));

#[cfg(target_os = "macos")]
static TAILWIND_BINARY: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/binaries/tailwindcss-macos"));

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
compile_error!("Only Linux and macOS are supported");

async fn extract_binary(extraction_path: PathBuf) -> Result<PathBuf, TailwindError> {
    use std::os::unix::fs::PermissionsExt;

    // Use the checksum from build.rs (first 8 chars for brevity)
    const CHECKSUM: &str = env!("TAILWIND_CHECKSUM");
    let hash_prefix = &CHECKSUM[..8.min(CHECKSUM.len())];

    let binary_name = if cfg!(target_os = "linux") {
        format!("tailwindcss-linux-{}", hash_prefix)
    } else {
        format!("tailwindcss-macos-{}", hash_prefix)
    };

    let binary_path = extraction_path.join(binary_name);

    if !binary_path.exists() {
        tokio::fs::create_dir_all(&extraction_path)
            .await
            .map_err(TailwindError::Extraction)?;

        // Decompress the binary
        let decompressed =
            zstd::decode_all(TAILWIND_BINARY).map_err(TailwindError::Decompression)?;
        tokio::fs::write(&binary_path, decompressed)
            .await
            .map_err(TailwindError::Extraction)?;

        let metadata = tokio::fs::metadata(&binary_path)
            .await
            .map_err(TailwindError::Extraction)?;
        let mut permissions = metadata.permissions();
        permissions.set_mode(0o755);
        tokio::fs::set_permissions(&binary_path, permissions)
            .await
            .map_err(TailwindError::Extraction)?;
    }

    Ok(binary_path)
}
