mod server;

use crate::filesystem::adaptive_watcher::{AdaptiveWatcher, WatchEvent};
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::program::Program;
use crate::log_info;
use server::{AppState, create_router};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use tailwind_runner::TailwindRunner;
use tokio::process::Child;

pub struct DevServer {
    pub router: axum::Router,
    _hop_watcher: AdaptiveWatcher,
    _css_watcher: notify::RecommendedWatcher,
    // The Tailwind process that watches the source files and writes
    // to the output path.
    pub tailwind_process: Child,
    // Handle to the temporary directory, containing the tailwind output
    // file and the tailwind binary etc.
    _tmp_dir: tempfile::TempDir,
}

async fn start_tailwind_watcher(
    root: &ProjectRoot,
    tmp_dir: &tempfile::TempDir,
) -> anyhow::Result<(String, PathBuf, Child)> {
    let input_path = match root.get_tailwind_input_path().await? {
        Some(p) => p,
        None => {
            let temp_input = tmp_dir.path().join("default-input.css");
            let default_content = r#"@import "tailwindcss";"#;
            tokio::fs::write(&temp_input, default_content).await?;
            temp_input
        }
    };

    let cache_dir = tmp_dir.path().to_path_buf();
    let runner = TailwindRunner::new(cache_dir.clone()).await?;

    let output_path = cache_dir.join("tailwind-output.css");
    let tailwind_config = tailwind_runner::TailwindConfig {
        input: input_path.to_path_buf(),
        output: output_path.clone(),
    };

    // Run once initially to generate CSS - return error if it fails
    runner.run_once(&tailwind_config).await?;

    // Read the generated CSS file
    let css_content = tokio::fs::read_to_string(&tailwind_config.output).await?;

    // Start watcher
    let handle = runner.watch(&tailwind_config)?;

    Ok((css_content, output_path, handle))
}

async fn create_file_watcher(
    root: &ProjectRoot,
    css_output_path: PathBuf,
    state: AppState,
) -> anyhow::Result<(AdaptiveWatcher, notify::RecommendedWatcher)> {
    let local_root = root.clone();

    // Create adaptive watcher with ignored folders for the project directory
    let ignored_folders = vec![".git", ".direnv", "node_modules", "target"];

    let adaptive_watcher = AdaptiveWatcher::new(root.get_path(), ignored_folders).await?;

    // Spawn task to handle watch events from the adaptive watcher
    let mut rx = adaptive_watcher.subscribe();
    let state_clone = state.clone();
    tokio::spawn(async move {
        while let Ok(event) = rx.recv().await {
            match event {
                WatchEvent::Created(ref path) | WatchEvent::Modified(ref path) => {
                    // Check if it's a .hop file
                    let is_hop_file = path.extension().and_then(|e| e.to_str()) == Some("hop");

                    if is_hop_file {
                        let total_start = std::time::Instant::now();

                        // Incrementally update just the changed module
                        if let Ok((module_name, document)) = local_root.load_hop_module(path) {
                            let load_elapsed = total_start.elapsed();

                            let update_start = std::time::Instant::now();
                            if let Ok(mut program) = state_clone.program.write() {
                                program.update_module(module_name, document);
                            }
                            let update_elapsed = update_start.elapsed();

                            let event_type = if matches!(event, WatchEvent::Created(_)) {
                                "created"
                            } else {
                                "modified"
                            };
                            log_info!(
                                "watch_event",
                                kind = event_type,
                                path = path.display(),
                                load = format!("{:?}", load_elapsed),
                                update = format!("{:?}", update_elapsed),
                                total = format!("{:?}", total_start.elapsed()),
                            );
                        }
                        // Tell the client to hot reload
                        let _ = state_clone.reload_channel.send(());
                    }
                }
                WatchEvent::Deleted(path) => {
                    // Check if it's a .hop file
                    let is_hop_file = path.extension().and_then(|e| e.to_str()) == Some("hop");

                    if is_hop_file {
                        let total_start = std::time::Instant::now();

                        // For deleted files, do a full reload since we don't have remove_module
                        if let Ok(modules) = local_root.load_all_hop_modules() {
                            let load_elapsed = total_start.elapsed();

                            let program_start = std::time::Instant::now();
                            let new_program = Program::new(modules);
                            let program_elapsed = program_start.elapsed();

                            if let Ok(mut program) = state_clone.program.write() {
                                *program = new_program;
                            }

                            log_info!(
                                "watch_event",
                                kind = "deleted",
                                path = path.display(),
                                load = format!("{:?}", load_elapsed),
                                rebuild = format!("{:?}", program_elapsed),
                                total = format!("{:?}", total_start.elapsed()),
                            );
                        }
                        // Tell the client to hot reload
                        let _ = state_clone.reload_channel.send(());
                    }
                }
            }
        }
    });

    // Create a separate watcher for the CSS output file created by Tailwind
    use notify::Watcher;
    let state_for_css_watcher = state.clone();
    let local_path = css_output_path.clone();
    let mut css_watcher = notify::RecommendedWatcher::new(
        move |res: Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                if event.kind.is_modify() {
                    let start = std::time::Instant::now();
                    if let Ok(new_css) = std::fs::read_to_string(local_path.as_path()) {
                        if let Ok(mut css_guard) = state.tailwind_css.write() {
                            *css_guard = Some(new_css);
                        }
                    }
                    log_info!(
                        "watch_event",
                        kind = "css_modified",
                        total = format!("{:?}", start.elapsed()),
                    );
                    // Tell the client to hot reload
                    let _ = state_for_css_watcher.reload_channel.send(());
                }
            }
        },
        notify::Config::default(),
    )?;

    // Watch the CSS output file
    css_watcher.watch(&css_output_path, notify::RecursiveMode::NonRecursive)?;

    Ok((adaptive_watcher, css_watcher))
}

/// Create a router that responds to render requests.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/event_source` route.
pub async fn execute(root: &ProjectRoot) -> anyhow::Result<DevServer> {
    let total_start = std::time::Instant::now();

    let load_start = std::time::Instant::now();
    let modules = root.load_all_hop_modules()?;
    let module_count = modules.len();
    log_info!(
        "dev",
        step = "load_modules",
        modules = module_count,
        duration = format!("{:?}", load_start.elapsed()),
    );

    let tmp_dir = tempfile::tempdir()?;

    let tailwind_start = std::time::Instant::now();
    let (css_content, css_output_path, tailwind_handle) =
        start_tailwind_watcher(root, &tmp_dir).await?;
    log_info!(
        "dev",
        step = "tailwind",
        duration = format!("{:?}", tailwind_start.elapsed()),
    );

    let program_start = std::time::Instant::now();
    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);
    let app_state = AppState {
        program: Arc::new(RwLock::new(Program::new(modules))),
        reload_channel,
        tailwind_css: Arc::new(RwLock::new(Some(css_content))),
    };
    log_info!(
        "dev",
        step = "create_program",
        duration = format!("{:?}", program_start.elapsed()),
    );

    let watcher_start = std::time::Instant::now();
    let (adaptive_watcher, css_watcher) =
        create_file_watcher(root, css_output_path, app_state.clone()).await?;
    log_info!(
        "dev",
        step = "setup_watchers",
        duration = format!("{:?}", watcher_start.elapsed()),
    );

    let router = create_router().with_state(app_state);

    log_info!(
        "dev",
        step = "ready",
        total = format!("{:?}", total_start.elapsed()),
    );

    Ok(DevServer {
        router,
        _hop_watcher: adaptive_watcher,
        _css_watcher: css_watcher,
        tailwind_process: tailwind_handle,
        _tmp_dir: tmp_dir,
    })
}
