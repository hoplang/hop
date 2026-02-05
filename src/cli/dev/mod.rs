mod frontend;
mod server;

use crate::hop::program::Program;
use crate::log_info;
use crate::project::Project;
use crate::project::project_watcher::{ProjectWatcher, WatchBatch};
use crate::timing;
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

// Spawn a task that listens for file changes in the project
async fn spawn_project_watcher_rx_reader(
    mut project_change_rx: tokio::sync::broadcast::Receiver<WatchBatch>,
    reload_channel_tx: tokio::sync::broadcast::Sender<()>,
    tailwind_watcher_tx: tokio::sync::mpsc::Sender<String>,
    project: Project,
    program: Arc<RwLock<Program>>,
) {
    tokio::spawn(async move {
        while let Ok(batch) = project_change_rx.recv().await {
            log_info!(
                "watch_event",
                modified = batch.modified.len(),
                deleted = batch.deleted.len(),
            );

            let start = std::time::Instant::now();

            if let Ok(mut program) = program.write() {
                for module_id in &batch.deleted {
                    program.remove_module(module_id);
                }
                for module_id in &batch.modified {
                    if let Ok(document) = project.load_module(module_id) {
                        program.update_module(module_id, document);
                    }
                }
            }

            // Update Tailwind sources
            if let Some(sources) = program.read().ok().map(|p| p.get_all_sources()) {
                let _ = tailwind_watcher_tx.send(sources).await;
            }

            log_info!(
                "program",
                event = "update_complete",
                elapsed = format!("{:?}", start.elapsed()),
            );

            // Signal reload to connected browsers
            let _ = reload_channel_tx.send(());
        }
    });
}

// Spawn a task that listens for CSS updates from the Tailwind watcher
async fn spawn_tailwind_rx_reader(
    mut tailwind_css_rx: tokio::sync::watch::Receiver<String>,
    reload_channel_tx: tokio::sync::broadcast::Sender<()>,
    tailwind_css: Arc<RwLock<String>>,
) {
    tokio::spawn(async move {
        while tailwind_css_rx.changed().await.is_ok() {
            let new_css = tailwind_css_rx.borrow().clone();
            if let Ok(mut css_guard) = tailwind_css.write() {
                *css_guard = new_css;
            }
            log_info!("watch_event", kind = "css_modified");
            let _ = reload_channel_tx.send(());
        }
    });
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

    let program = Arc::new(RwLock::new(program));

    let css = Arc::new(RwLock::new(initial_css));

    timer.start_phase("setup watchers");
    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);
    let app_state = AppState {
        program: program.clone(),
        reload_channel: reload_channel.clone(),
        tailwind_css: css.clone(),
    };

    spawn_tailwind_rx_reader(
        tailwind_watcher.subscribe(),
        reload_channel.clone(),
        css.clone(),
    )
    .await;

    let project_watcher = ProjectWatcher::new(project, Duration::from_millis(50)).await?;

    spawn_project_watcher_rx_reader(
        project_watcher.subscribe(),
        reload_channel,
        tailwind_watcher.sources_tx.clone(),
        project.clone(),
        program,
    )
    .await;

    let router = create_router().with_state(app_state);

    Ok(DevServer {
        router,
        timer,
        _project_watcher: project_watcher,
        _tailwind_watcher: tailwind_watcher,
    })
}
