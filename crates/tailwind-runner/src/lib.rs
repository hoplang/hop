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
    #[error("Failed to create temp directory: {0}")]
    TempDir(#[source] std::io::Error),
    #[error("Failed to write sources file: {0}")]
    WriteSources(#[source] std::io::Error),
    #[error("Failed to setup file watcher: {0}")]
    Watch(#[source] notify::Error),
    #[error("Channel closed")]
    ChannelClosed,
    #[error("Could not determine cache directory")]
    NoCacheDir,
}

pub struct TailwindRunner {
    binary_path: PathBuf,
    cache_dir: PathBuf,
}

pub struct TailwindConfig {
    /// Path to the input CSS file. If None, a default file with `@import "tailwindcss";` is used.
    pub input: Option<PathBuf>,
    pub output: PathBuf,
    pub working_dir: PathBuf,
}

/// Handle for interacting with a running Tailwind watcher
#[derive(Clone)]
pub struct TailwindWatcher {
    sources_tx: tokio::sync::mpsc::Sender<String>,
    css_rx: tokio::sync::watch::Receiver<String>,
    working_dir: PathBuf,
}

impl TailwindWatcher {
    /// Update the hop sources. Tailwind will regenerate CSS automatically.
    pub async fn update_sources(&self, sources: String) -> Result<(), TailwindError> {
        self.sources_tx
            .send(sources)
            .await
            .map_err(|_| TailwindError::ChannelClosed)
    }

    /// Subscribe to CSS changes (clones the receiver)
    pub fn subscribe(&self) -> tokio::sync::watch::Receiver<String> {
        self.css_rx.clone()
    }

    /// Returns the working directory used by the Tailwind watcher
    pub fn working_dir(&self) -> &std::path::Path {
        &self.working_dir
    }
}

impl TailwindRunner {
    pub async fn new() -> Result<Self, TailwindError> {
        let cache_dir = dirs::cache_dir()
            .ok_or(TailwindError::NoCacheDir)?
            .join("hop");
        let binary_path = extract_binary(cache_dir.clone()).await?;
        Ok(Self {
            binary_path,
            cache_dir,
        })
    }

    /// Returns the path to the default input CSS file, creating it if necessary.
    async fn get_default_input_path(&self) -> Result<PathBuf, TailwindError> {
        let default_input = self.cache_dir.join("default-input.css");
        if !default_input.exists() {
            tokio::fs::write(&default_input, r#"@import "tailwindcss";"#)
                .await
                .map_err(TailwindError::WriteSources)?;
        }
        Ok(default_input)
    }

    /// Compile sources to CSS in an isolated temp directory.
    ///
    /// This is the high-level API for one-shot compilation. It creates a temp
    /// directory, writes the sources, runs Tailwind, and returns the generated CSS.
    pub async fn compile_once(
        &self,
        input_css_path: Option<PathBuf>,
        sources: &str,
    ) -> Result<String, TailwindError> {
        // Create temp directory for isolation
        let tmp_dir = tempfile::tempdir().map_err(TailwindError::TempDir)?;

        // Write sources for Tailwind to scan
        tokio::fs::write(tmp_dir.path().join("sources.hop"), sources)
            .await
            .map_err(TailwindError::WriteSources)?;

        let output_path = tmp_dir.path().join("tailwind-output.css");
        let config = TailwindConfig {
            input: input_css_path,
            output: output_path.clone(),
            working_dir: tmp_dir.path().to_path_buf(),
        };

        self.run_once(&config).await?;

        tokio::fs::read_to_string(&output_path)
            .await
            .map_err(TailwindError::WriteSources)
    }

    async fn run_once(&self, config: &TailwindConfig) -> Result<(), TailwindError> {
        let input_path = match &config.input {
            Some(p) => p.clone(),
            None => self.get_default_input_path().await?,
        };

        let output = tokio::process::Command::new(&self.binary_path)
            .arg("--input")
            .arg(&input_path)
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

        Ok(())
    }

    async fn watch(&self, config: &TailwindConfig) -> Result<tokio::process::Child, TailwindError> {
        let input_path = match &config.input {
            Some(p) => p.clone(),
            None => self.get_default_input_path().await?,
        };

        let child = tokio::process::Command::new(&self.binary_path)
            .arg("--watch=always")
            .arg("--input")
            .arg(&input_path)
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

    /// Start a Tailwind watcher with a channel-based API.
    ///
    /// This method manages its own temp directory and file watching internally.
    /// Clients push source code strings in and receive CSS content notifications out.
    pub async fn start_watcher(
        &self,
        input_css_path: Option<PathBuf>,
        initial_sources: String,
    ) -> Result<TailwindWatcher, TailwindError> {
        use notify::Watcher;
        use tokio::sync::{mpsc, watch};

        // Create temp directory (owned by the background task)
        let tmp_dir = tempfile::tempdir().map_err(TailwindError::TempDir)?;
        let working_dir = tmp_dir.path().to_path_buf();

        // Write initial sources to sources.hop
        let sources_path = tmp_dir.path().join("sources.hop");
        tokio::fs::write(&sources_path, &initial_sources)
            .await
            .map_err(TailwindError::WriteSources)?;

        let output_path = tmp_dir.path().join("tailwind-output.css");
        // Create the output file so notify can watch it immediately
        tokio::fs::write(&output_path, "")
            .await
            .map_err(TailwindError::WriteSources)?;
        let config = TailwindConfig {
            input: input_css_path,
            output: output_path.clone(),
            working_dir: tmp_dir.path().to_path_buf(),
        };

        // Start Tailwind in watch mode (it compiles immediately on startup)
        let mut tailwind_child = self.watch(&config).await?;

        // Create channels for source updates and CSS notifications
        let (sources_tx, mut sources_rx) = mpsc::channel::<String>(16);
        let (css_tx, css_rx) = watch::channel(String::new());

        // Set up file watcher for CSS output
        let css_tx_for_watcher = css_tx.clone();
        let output_path_for_watcher = output_path.clone();
        let output_path_for_watch = output_path.clone();

        let mut css_watcher = notify::RecommendedWatcher::new(
            move |res: Result<notify::Event, notify::Error>| {
                if let Ok(event) = res {
                    if event.kind.is_modify() {
                        if let Ok(new_css) = std::fs::read_to_string(&output_path_for_watcher) {
                            let _ = css_tx_for_watcher.send(new_css);
                        }
                    }
                }
            },
            notify::Config::default(),
        )
        .map_err(TailwindError::Watch)?;

        css_watcher
            .watch(&output_path_for_watch, notify::RecursiveMode::NonRecursive)
            .map_err(TailwindError::Watch)?;

        // Spawn background task that manages everything
        let sources_path_clone = sources_path.clone();

        tokio::spawn(async move {
            // Keep tmp_dir and css_watcher alive for the lifetime of this task
            let _tmp_dir = tmp_dir;
            let _css_watcher = css_watcher;

            // Listen for source updates
            while let Some(new_sources) = sources_rx.recv().await {
                // Write updated sources to the file
                if std::fs::write(&sources_path_clone, &new_sources).is_err() {
                    continue;
                }
            }

            // Channel closed, clean up
            let _ = tailwind_child.kill().await;
        });

        Ok(TailwindWatcher {
            sources_tx,
            css_rx,
            working_dir,
        })
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
