mod frontend;
mod server;

use crate::project::Project;
use crate::project::project_watcher::ProjectWatcher;
use crate::hop::program::Program;
use crate::log_info;
use crate::tui::timing;
use server::{AppState, create_router};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::Duration;
use tailwind_runner::{TailwindRunner, TailwindWatcher};

pub struct DevServer {
    pub router: axum::Router,
    pub timer: timing::TimingCollector,
    _project_watcher: ProjectWatcher,
    _tailwind_watcher: TailwindWatcher,
}

/// Create a router that responds to render requests.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/event_source` route.
pub async fn execute(project: &Project) -> anyhow::Result<DevServer> {
    let mut timer = timing::TimingCollector::new();

    timer.start_phase("load modules");
    let module_ids = project.find_modules()?;
    let mut modules = HashMap::new();
    for module_id in module_ids {
        let document = project.load_module(&module_id)?;
        modules.insert(module_id, document);
    }
    let program = Program::new(modules);

    // Get tailwind input path if it exists
    let input_css_path = project.get_tailwind_input_path().await?;

    // Create the Tailwind runner, compile initial CSS, and start the watcher
    timer.start_phase("tailwind");
    let runner = TailwindRunner::new().await?;
    let sources = program.get_all_sources();
    let initial_css = runner
        .compile_once(input_css_path.clone(), &sources)
        .await?;
    let tailwind_watcher = runner.start_watcher(input_css_path, sources).await?;

    timer.start_phase("setup watchers");
    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);
    let app_state = AppState {
        program: Arc::new(RwLock::new(program)),
        reload_channel,
        tailwind_css: Arc::new(RwLock::new(Some(initial_css))),
    };

    // Spawn a task that listens for CSS updates from the Tailwind watcher
    let state_for_css = app_state.clone();
    let mut css_rx = tailwind_watcher.subscribe();
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

    // Spawn a task that listens for .hop file changes
    let project_watcher = ProjectWatcher::new(project, Duration::from_millis(50)).await?;
    let mut hop_rx = project_watcher.subscribe();
    let state_copy = app_state.clone();
    let project_copy = project.clone();
    let tailwind_watcher_copy = tailwind_watcher.clone();
    tokio::spawn(async move {
        while let Ok(batch) = hop_rx.recv().await {
            log_info!(
                "watch_event",
                modified = batch.modified.len(),
                deleted = batch.deleted.len(),
            );

            let start = std::time::Instant::now();

            if let Ok(mut program) = state_copy.program.write() {
                for module_id in &batch.deleted {
                    program.remove_module(module_id);
                }
                for module_id in &batch.modified {
                    if let Ok(document) = project_copy.load_module(module_id) {
                        program.update_module(module_id, document);
                    }
                }
            }

            // Update Tailwind sources
            if let Some(sources) = state_copy.program.read().ok().map(|p| p.get_all_sources()) {
                let _ = tailwind_watcher_copy.update_sources(sources).await;
            }

            log_info!(
                "program",
                event = "update_complete",
                elapsed = format!("{:?}", start.elapsed()),
            );

            // Signal reload to connected browsers
            let _ = state_copy.reload_channel.send(());
        }
    });

    let router = create_router().with_state(app_state);

    Ok(DevServer {
        router,
        timer,
        _project_watcher: project_watcher,
        _tailwind_watcher: tailwind_watcher,
    })
}
