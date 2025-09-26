use anyhow::Result;
use std::path::PathBuf;
use tokio::process::{Child, Command};

pub struct TailwindRunner {
    binary_path: PathBuf,
}

pub struct TailwindConfig {
    pub input: PathBuf,
    pub output: PathBuf,
}

pub struct WatchHandle {
    child: Child,
}

impl WatchHandle {
    pub async fn stop(mut self) -> Result<()> {
        self.child.kill().await?;
        Ok(())
    }
}

impl Drop for WatchHandle {
    fn drop(&mut self) {
        // Kill the child process when the handle is dropped
        let _ = self.child.start_kill();
    }
}

impl TailwindRunner {
    pub async fn new(extraction_path: PathBuf) -> Result<Self> {
        let binary_path = extract_binary(extraction_path).await?;
        Ok(Self { binary_path })
    }

    pub async fn run_once(&self, config: &TailwindConfig) -> Result<String> {
        let output = Command::new(&self.binary_path)
            .arg("--input")
            .arg(&config.input)
            .arg("--output")
            .arg(&config.output)
            .arg("--minify")
            .output()
            .await?;

        if !output.status.success() {
            return Err(anyhow::anyhow!(
                "Tailwind CSS failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    }

    pub async fn watch(&self, config: &TailwindConfig) -> Result<WatchHandle> {
        let child = Command::new(&self.binary_path)
            .arg("--watch")
            .arg("--input")
            .arg(&config.input)
            .arg("--output")
            .arg(&config.output)
            .arg("--minify")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()?;

        Ok(WatchHandle { child })
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

async fn extract_binary(extraction_path: PathBuf) -> Result<PathBuf> {
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
        tokio::fs::create_dir_all(&extraction_path).await?;

        // Decompress the binary
        let decompressed = zstd::decode_all(TAILWIND_BINARY)?;
        tokio::fs::write(&binary_path, decompressed).await?;

        let metadata = tokio::fs::metadata(&binary_path).await?;
        let mut permissions = metadata.permissions();
        permissions.set_mode(0o755);
        tokio::fs::set_permissions(&binary_path, permissions).await?;
    }

    Ok(binary_path)
}
