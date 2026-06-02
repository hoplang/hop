use std::{fs, path::PathBuf};

use tracing::instrument;

#[derive(Debug, thiserror::Error)]
pub enum TailwindError {
    #[error("Tailwind CSS failed: {stderr}")]
    ExecutionFailed { stderr: String },
    #[error(
        "Failed to run tailwindcss. Is tailwindcss installed and on your PATH?\n\
         {source}"
    )]
    Spawn {
        #[source]
        source: std::io::Error,
    },
    #[error("Failed to create temp directory: {0}")]
    TempDir(#[source] std::io::Error),
    #[error("Failed to write sources file: {0}")]
    WriteSources(#[source] std::io::Error),
    #[error("Failed to setup file watcher: {0}")]
    Watch(#[source] notify::Error),
}

pub struct TailwindRunner {}

pub struct WatcherChannels {
    pub css_output: tokio::sync::watch::Sender<String>,
    pub css_input: tokio::sync::watch::Sender<String>,
    pub sources_input: tokio::sync::watch::Sender<String>,
}

pub struct TailwindConfig {
    pub input_path: PathBuf,
    pub output_path: PathBuf,
    pub working_dir: PathBuf,
}

impl Default for TailwindRunner {
    fn default() -> Self {
        Self::new()
    }
}

impl TailwindRunner {
    pub fn new() -> Self {
        Self {}
    }

    /// Compile sources to CSS in an isolated temp directory.
    ///
    /// This is the high-level API for one-shot compilation. It creates a temp
    /// directory, writes the sources, runs Tailwind, and returns the generated CSS.
    #[instrument(
        name = "TailwindRunner::compile_once",
        skip(self, css_input, sources_input)
    )]
    pub fn compile_once(
        &self,
        css_input: &str,
        sources_input: &str,
    ) -> Result<String, TailwindError> {
        // Create temp directory for isolation
        let tmp_dir = tempfile::tempdir().map_err(TailwindError::TempDir)?;

        let css_input_path = tmp_dir.path().join("input.css");
        let sources_input_path = tmp_dir.path().join("sources.hop");

        // Write input files
        fs::write(css_input_path.as_path(), css_input).map_err(TailwindError::WriteSources)?;
        fs::write(sources_input_path.as_path(), sources_input)
            .map_err(TailwindError::WriteSources)?;

        let css_output_path = tmp_dir.path().join("tailwind-output.css");
        let config = TailwindConfig {
            input_path: css_input_path,
            output_path: css_output_path.clone(),
            working_dir: tmp_dir.path().to_path_buf(),
        };

        self.run_once(&config)?;

        fs::read_to_string(&css_output_path).map_err(TailwindError::WriteSources)
    }

    fn run_once(&self, config: &TailwindConfig) -> Result<(), TailwindError> {
        let input_str = config.input_path.display().to_string();
        let output_str = config.output_path.display().to_string();
        let args = vec!["--input", &input_str, "--output", &output_str, "--minify"];

        tracing::info!(target: "exec", cmd = "tailwindcss", args = ?args, working_dir = %config.working_dir.display());

        let output = std::process::Command::new("tailwindcss")
            .args(&args)
            .current_dir(&config.working_dir)
            .output()
            .map_err(|e| TailwindError::Spawn { source: e })?;

        let stderr = String::from_utf8_lossy(&output.stderr);

        if !output.status.success() {
            return Err(TailwindError::ExecutionFailed {
                stderr: stderr.to_string(),
            });
        }

        if stderr.contains("warning") {
            eprintln!("{stderr}");
        }

        Ok(())
    }

    async fn watch(&self, config: &TailwindConfig) -> Result<tokio::process::Child, TailwindError> {
        let input_path_str = config.input_path.display().to_string();
        let output_path_str = config.output_path.display().to_string();

        let args = vec![
            // tailwindcss will keep watching files even when stdin is closed when specifying `--watch=always`
            "--watch=always",
            "--input",
            input_path_str.as_str(),
            "--output",
            output_path_str.as_str(),
        ];

        tracing::info!(target: "exec", cmd = "tailwindcss", args = ?args, working_dir = %config.working_dir.display());

        let child = tokio::process::Command::new("tailwindcss")
            .args(&args)
            .current_dir(&config.working_dir)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .spawn()
            .map_err(|e| TailwindError::Spawn { source: e })?;

        Ok(child)
    }

    /// Start a Tailwind watcher with a channel-based API.
    ///
    /// This method manages its own temp directory and file watching internally.
    /// Clients push source code strings in and receive CSS content notifications out.
    #[instrument(
        name = "TailwindRunner::start_watcher",
        skip(self, initial_css_input, initial_sources_input)
    )]
    pub async fn start_watcher(
        &self,
        initial_css_input: String,
        initial_sources_input: String,
    ) -> Result<WatcherChannels, TailwindError> {
        use notify::Watcher;
        use tokio::sync::watch;

        // Create temp directory (owned by the background task)
        let tmp_dir = tempfile::tempdir().map_err(TailwindError::TempDir)?;

        let sources_input_path = tmp_dir.path().join("sources.hop");
        let css_output_path = tmp_dir.path().join("tailwind-output.css");
        let css_input_path = tmp_dir.path().join("input.css");

        tokio::fs::write(&sources_input_path, &initial_sources_input)
            .await
            .map_err(TailwindError::WriteSources)?;
        tokio::fs::write(&css_input_path, &initial_css_input)
            .await
            .map_err(TailwindError::WriteSources)?;
        tokio::fs::write(&css_output_path, "")
            .await
            .map_err(TailwindError::WriteSources)?;

        let config = TailwindConfig {
            input_path: css_input_path.clone(),
            output_path: css_output_path.clone(),
            working_dir: tmp_dir.path().to_path_buf(),
        };

        let mut tailwind_process = self.watch(&config).await?;

        // Create channels for source updates and CSS notifications
        let (sources_input_tx, mut sources_input_rx) =
            watch::channel(initial_sources_input.clone());
        let (css_input_tx, mut css_input_rx) = watch::channel(initial_css_input.clone());
        let (css_output_tx, css_output_rx) = watch::channel(String::new());

        // Set up file watcher for CSS output
        let css_output_tx_for_watcher = css_output_tx.clone();
        let css_output_path_for_watcher = css_output_path.clone();
        let mut css_output_watcher = notify::RecommendedWatcher::new(
            move |res: Result<notify::Event, notify::Error>| {
                if let Ok(event) = res {
                    if event.kind.is_modify() {
                        if let Ok(new_css) = std::fs::read_to_string(&css_output_path_for_watcher) {
                            let _ = css_output_tx_for_watcher.send(new_css);
                        }
                    }
                }
            },
            notify::Config::default(),
        )
        .map_err(TailwindError::Watch)?;

        css_output_watcher
            .watch(&css_output_path, notify::RecursiveMode::NonRecursive)
            .map_err(TailwindError::Watch)?;

        tokio::spawn(async move {
            let _tmp_dir = tmp_dir;
            let _css_output_watcher = css_output_watcher;
            // Move css_output_rx into this task to avoid the channel being closed
            // due to no watchers.
            let _css_output_rx = css_output_rx;

            loop {
                tokio::select! {
                    res = sources_input_rx.changed() => {
                        if res.is_err() {
                            break;
                        }
                        let data = sources_input_rx.borrow().clone();
                        if std::fs::write(&sources_input_path, &data).is_err() {
                            break;
                        }
                    }
                    res = css_input_rx.changed() => {
                        if res.is_err() {
                            break;
                        }
                        let data = css_input_rx.borrow().clone();
                        if std::fs::write(&css_input_path, &data).is_err() {
                            break;
                        }
                    }
                }
            }

            // Channel closed, clean up
            let _ = tailwind_process.kill().await;
        });

        Ok(WatcherChannels {
            css_output: css_output_tx,
            css_input: css_input_tx,
            sources_input: sources_input_tx,
        })
    }
}
