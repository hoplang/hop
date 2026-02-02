mod frontend;
mod server;

use crate::filesystem::adaptive_watcher::{AdaptiveWatcher, WatchEvent};
use crate::filesystem::project_root::ProjectRoot;
use crate::hop::program::Program;
use crate::log_info;
use crate::tui::timing;
use server::{AppState, create_router};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use tailwind_runner::{TailwindRunner, TailwindWatcher};
use tokio::time::{Duration, Instant, sleep_until};

pub struct DevServer {
    pub router: axum::Router,
    pub timer: timing::TimingCollector,
    _hop_watcher: AdaptiveWatcher,
    _tailwind_watcher: TailwindWatcher,
}

async fn create_file_watcher(
    root: &ProjectRoot,
    tailwind_watcher: TailwindWatcher,
    state: AppState,
) -> anyhow::Result<AdaptiveWatcher> {
    let local_root = root.clone();

    // Create adaptive watcher with ignored folders for the project directory
    let ignored_folders = vec![".git", ".direnv", "node_modules", "target"];

    let adaptive_watcher = AdaptiveWatcher::new(root.get_path(), ignored_folders).await?;

    // Spawn task to handle watch events from the adaptive watcher with debouncing
    let mut rx = adaptive_watcher.subscribe();
    let state_clone = state.clone();
    tokio::spawn(async move {
        enum ChangeKind {
            Modified,
            Deleted,
        }

        let debounce_duration = Duration::from_millis(50);
        let mut pending_changes: HashMap<PathBuf, ChangeKind> = HashMap::new();
        let mut deadline: Option<Instant> = None;

        loop {
            tokio::select! {
                biased;

                // Receive new events and accumulate them
                result = rx.recv() => {
                    match result {
                        Ok(event) => {
                            let (path, kind) = match &event {
                                WatchEvent::Created(p) => (p.clone(), "created"),
                                WatchEvent::Modified(p) => (p.clone(), "modified"),
                                WatchEvent::Deleted(p) => (p.clone(), "deleted"),
                            };

                            let is_hop_file = path.extension().and_then(|e| e.to_str()) == Some("hop");
                            if is_hop_file {
                                let change_kind = if matches!(event, WatchEvent::Deleted(_)) {
                                    ChangeKind::Deleted
                                } else {
                                    ChangeKind::Modified
                                };
                                pending_changes.insert(path.clone(), change_kind);
                                deadline = Some(Instant::now() + debounce_duration);
                                log_info!(
                                    "watch_event",
                                    kind = kind,
                                    path = path.display(),
                                    pending = pending_changes.len(),
                                );
                            }
                        }
                        Err(_) => break, // Channel closed or lagged
                    }
                }

                // Timer fires when debounce period elapses
                _ = async {
                    match deadline {
                        Some(d) => sleep_until(d).await,
                        None => std::future::pending().await,
                    }
                }, if deadline.is_some() => {
                    let change_count = pending_changes.len();
                    log_info!(
                        "debounce",
                        event = "triggered",
                        changes = change_count,
                    );

                    let total_start = std::time::Instant::now();

                    if let Ok(mut program) = state_clone.program.write() {
                        for (path, kind) in &pending_changes {
                            match kind {
                                ChangeKind::Deleted => {
                                    if let Ok(module_id) = local_root.path_to_module_id(path) {
                                        program.remove_module(&module_id);
                                    }
                                }
                                ChangeKind::Modified => {
                                    if let Ok((module_id, document)) = local_root.load_hop_module(path) {
                                        program.update_module(module_id, document);
                                    }
                                }
                            }
                        }
                    }

                    // Update Tailwind sources via the channel
                    let sources = state_clone.program.read().ok().map(|p| p.get_all_sources());
                    if let Some(sources) = sources {
                        let _ = tailwind_watcher.update_sources(sources).await;
                    }

                    log_info!(
                        "program",
                        event = "update_complete",
                        changes = change_count,
                        total = format!("{:?}", total_start.elapsed()),
                    );

                    // Send single reload signal for all accumulated changes
                    let _ = state_clone.reload_channel.send(());

                    // Reset state
                    pending_changes.clear();
                    deadline = None;
                }
            }
        }
    });

    Ok(adaptive_watcher)
}

/// Create a router that responds to render requests.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/event_source` route.
pub async fn execute(root: &ProjectRoot) -> anyhow::Result<DevServer> {
    let mut timer = timing::TimingCollector::new();

    timer.start_phase("load modules");
    let modules = root.load_all_hop_modules()?;
    let program = Program::new(modules);

    // Get tailwind input path if it exists
    let input_css_path = root.get_tailwind_input_path().await?;

    // Create the Tailwind runner and start the watcher
    timer.start_phase("tailwind");
    let cache_dir = PathBuf::from("/tmp/.hop-cache");
    let runner = TailwindRunner::new(cache_dir).await?;
    let tailwind_handle = runner
        .start_watcher(input_css_path, program.get_all_sources())
        .await?;

    timer.start_phase("setup watchers");
    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);
    let app_state = AppState {
        program: Arc::new(RwLock::new(program)),
        reload_channel,
        tailwind_css: Arc::new(RwLock::new(Some(tailwind_handle.initial_css))),
    };

    // Spawn a task that listens for CSS updates from the Tailwind watcher
    let state_for_css = app_state.clone();
    let mut css_rx = tailwind_handle.watcher.subscribe();
    tokio::spawn(async move {
        while css_rx.changed().await.is_ok() {
            let new_css = css_rx.borrow().clone();
            if let Ok(mut css_guard) = state_for_css.tailwind_css.write() {
                *css_guard = Some(new_css);
            }
            log_info!("watch_event", kind = "css_modified");
            let _ = state_for_css.reload_channel.send(());
        }
    });

    let tailwind_watcher = tailwind_handle.watcher;
    let adaptive_watcher =
        create_file_watcher(root, tailwind_watcher.clone(), app_state.clone()).await?;

    let router = create_router().with_state(app_state);

    Ok(DevServer {
        router,
        timer,
        _hop_watcher: adaptive_watcher,
        _tailwind_watcher: tailwind_watcher,
    })
}
