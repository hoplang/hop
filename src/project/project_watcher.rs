//! File system watcher for hop projects.
//!
//! This module provides [`ProjectWatcher`], which monitors a project directory for changes
//! to `.hop` files and emits debounced [`WatchBatch`] events containing [`ModuleId`]s.
//! It automatically:
//!
//! - Watches all directories recursively from the project root
//! - Dynamically registers newly created directories
//! - Ignores common non-source directories (node_modules, target, .git, etc.)
//! - Filters for `.hop` files and converts paths to module IDs
//! - Debounces rapid file changes into batches

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use anyhow::Result;
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::{broadcast, mpsc};
use tokio::time::{Instant, sleep_until};
use walkdir::WalkDir;

use super::project::{Project, should_skip_directory};
use crate::hop::symbols::module_id::ModuleId;

/// A batch of module changes emitted after the debounce period.
#[derive(Debug, Clone, Default)]
pub struct WatchBatch {
    /// Modules that were created or modified.
    pub modified: Vec<ModuleId>,
    /// Modules that were deleted.
    pub deleted: Vec<ModuleId>,
}

impl WatchBatch {
    fn is_empty(&self) -> bool {
        self.modified.is_empty() && self.deleted.is_empty()
    }
}

pub struct ProjectWatcher {
    batch_tx: broadcast::Sender<WatchBatch>,
    _watcher: Arc<Mutex<RecommendedWatcher>>,
    _task_handle: tokio::task::JoinHandle<()>,
}

impl ProjectWatcher {
    pub async fn new(project: &Project, debounce: Duration) -> anyhow::Result<Self> {
        let root = project.get_project_root();
        let project = project.clone();

        let (notify_tx, mut notify_rx) = mpsc::unbounded_channel();

        let state = Arc::new(WatcherState {
            watcher: Arc::new(Mutex::new(RecommendedWatcher::new(
                move |res: Result<Event, notify::Error>| {
                    if let Ok(event) = res {
                        let _ = notify_tx.send(event);
                    }
                },
                Config::default(),
            )?)),
            watched_paths: Arc::new(Mutex::new(HashSet::new())),
        });

        // Initial directory scanning and watching
        for entry in WalkDir::new(root)
            .into_iter()
            .filter_entry(|e| {
                e.file_name()
                    .to_str()
                    .map(|s| !should_skip_directory(s))
                    .unwrap_or(true)
            })
            .flatten()
        {
            if entry.path().is_dir() {
                let mut watcher = state.watcher.lock().unwrap();
                watcher.watch(entry.path(), RecursiveMode::NonRecursive)?;
                state
                    .watched_paths
                    .lock()
                    .unwrap()
                    .insert(entry.path().to_path_buf());
            }
        }

        let (batch_tx, _rx) = broadcast::channel(100);
        let batch_tx_clone = batch_tx.clone();
        let state_clone = state.clone();

        let task_handle = tokio::spawn(async move {
            // Track pending changes: true = modified/created, false = deleted
            let mut pending: HashMap<PathBuf, bool> = HashMap::new();
            let mut deadline: Option<Instant> = None;

            loop {
                tokio::select! {
                    biased;

                    // Receive raw notify events
                    result = notify_rx.recv() => {
                        match result {
                            Some(event) => {
                                for path in &event.paths {
                                    // Handle directory watching (not part of user-facing events)
                                    if matches!(event.kind, EventKind::Create(_)) && path.is_dir() {
                                        state_clone.add_watch(path);
                                    }
                                    if matches!(event.kind, EventKind::Remove(_)) {
                                        state_clone.remove_watch(path);
                                    }

                                    // Only track .hop files
                                    if path.extension().and_then(|e| e.to_str()) != Some("hop") {
                                        continue;
                                    }

                                    // Accumulate changes
                                    match event.kind {
                                        EventKind::Create(_) | EventKind::Modify(_) => {
                                            pending.insert(path.clone(), true);
                                        }
                                        EventKind::Remove(_) => {
                                            pending.insert(path.clone(), false);
                                        }
                                        _ => {}
                                    }
                                }

                                if !pending.is_empty() {
                                    deadline = Some(Instant::now() + debounce);
                                }
                            }
                            None => break, // Channel closed
                        }
                    }

                    // Debounce timer fires
                    _ = async {
                        match deadline {
                            Some(d) => sleep_until(d).await,
                            None => std::future::pending().await,
                        }
                    }, if deadline.is_some() => {
                        let mut batch = WatchBatch::default();

                        for (path, is_modified) in pending.drain() {
                            if let Ok(module_id) = project.path_to_module_id(&path) {
                                if is_modified {
                                    batch.modified.push(module_id);
                                } else {
                                    batch.deleted.push(module_id);
                                }
                            }
                        }

                        if !batch.is_empty() {
                            let _ = batch_tx_clone.send(batch);
                        }

                        deadline = None;
                    }
                }
            }
        });

        Ok(Self {
            batch_tx,
            _watcher: state.watcher.clone(),
            _task_handle: task_handle,
        })
    }

    pub fn subscribe(&self) -> broadcast::Receiver<WatchBatch> {
        self.batch_tx.subscribe()
    }
}

struct WatcherState {
    watcher: Arc<Mutex<RecommendedWatcher>>,
    watched_paths: Arc<Mutex<HashSet<PathBuf>>>,
}

impl WatcherState {
    fn add_watch(&self, path: &Path) {
        if Self::should_ignore(path) {
            return;
        }

        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| !Self::should_ignore(e.path()))
            .flatten()
        {
            if entry.path().is_dir() {
                let mut watched_paths = self.watched_paths.lock().unwrap();
                if !watched_paths.contains(entry.path()) {
                    if let Ok(mut watcher) = self.watcher.lock() {
                        if watcher
                            .watch(entry.path(), RecursiveMode::NonRecursive)
                            .is_ok()
                        {
                            watched_paths.insert(entry.path().to_path_buf());
                        }
                    }
                }
            }
        }
    }

    fn remove_watch(&self, path: &Path) {
        let mut watched_paths = self.watched_paths.lock().unwrap();
        if watched_paths.contains(path) {
            if let Ok(mut watcher) = self.watcher.lock() {
                let _ = watcher.unwatch(path);
            }
            watched_paths.remove(path);
        }
    }

    fn should_ignore(path: &Path) -> bool {
        path.components().any(|c| {
            c.as_os_str()
                .to_str()
                .map(should_skip_directory)
                .unwrap_or(false)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::archive::temp_dir_from_archive;
    use indoc::indoc;
    use simple_txtar::Archive;
    use tempfile::TempDir;

    const TEST_DEBOUNCE: Duration = Duration::from_millis(10);

    fn create_test_project() -> (TempDir, Project) {
        let archive = Archive::from(indoc! {r#"
            -- hop.toml --
            [build]
            target = "ts"
            output_path = "app.ts"
        "#});
        let temp_dir = temp_dir_from_archive(&archive).unwrap();
        let project = Project::from(temp_dir.path()).unwrap();
        (temp_dir, project)
    }

    #[tokio::test]
    async fn hop_file_creation() {
        let (temp_dir, project) = create_test_project();

        let watcher = ProjectWatcher::new(&project, TEST_DEBOUNCE)
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        let test_file = temp_dir.path().join("test.hop");
        std::fs::write(&test_file, "<test>hello</test>").expect("Failed to write test file");

        let batch = rx.recv().await.expect("Failed to receive batch");
        let expected_id = ModuleId::new("test").unwrap();
        assert!(batch.modified.contains(&expected_id));
        assert!(batch.deleted.is_empty());
    }

    #[tokio::test]
    async fn hop_file_modification() {
        let (temp_dir, project) = create_test_project();

        let test_file = temp_dir.path().join("test.hop");
        std::fs::write(&test_file, "<test>initial</test>").expect("Failed to write test file");

        let watcher = ProjectWatcher::new(&project, TEST_DEBOUNCE)
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        std::fs::write(&test_file, "<test>modified</test>").expect("Failed to modify test file");

        let batch = rx.recv().await.expect("Failed to receive batch");
        let expected_id = ModuleId::new("test").unwrap();
        assert!(batch.modified.contains(&expected_id));
    }

    #[tokio::test]
    async fn multiple_hop_files_batched() {
        let (temp_dir, project) = create_test_project();

        let watcher = ProjectWatcher::new(&project, TEST_DEBOUNCE)
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        // Create multiple .hop files rapidly
        let file1 = temp_dir.path().join("file1.hop");
        let file2 = temp_dir.path().join("file2.hop");
        std::fs::write(&file1, "<file1>one</file1>").expect("Failed to write file1");
        std::fs::write(&file2, "<file2>two</file2>").expect("Failed to write file2");

        let batch = rx.recv().await.expect("Failed to receive batch");
        assert!(batch.modified.contains(&ModuleId::new("file1").unwrap()));
        assert!(batch.modified.contains(&ModuleId::new("file2").unwrap()));
    }

    #[tokio::test]
    async fn non_hop_files_ignored() {
        let (temp_dir, project) = create_test_project();

        let watcher = ProjectWatcher::new(&project, TEST_DEBOUNCE)
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        // Create a non-.hop file (should be ignored)
        let txt_file = temp_dir.path().join("test.txt");
        std::fs::write(&txt_file, "hello").expect("Failed to write txt file");

        // Create a .hop file
        let hop_file = temp_dir.path().join("test.hop");
        std::fs::write(&hop_file, "<test>hello</test>").expect("Failed to write hop file");

        let batch = rx.recv().await.expect("Failed to receive batch");
        // Should only contain the .hop file
        assert_eq!(batch.modified.len(), 1);
        assert!(batch.modified.contains(&ModuleId::new("test").unwrap()));
    }

    #[tokio::test]
    async fn nested_hop_file() {
        let (temp_dir, project) = create_test_project();

        // Create nested directory structure
        let nested_dir = temp_dir.path().join("src").join("components");
        std::fs::create_dir_all(&nested_dir).expect("Failed to create nested dirs");

        let watcher = ProjectWatcher::new(&project, TEST_DEBOUNCE)
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        let hop_file = nested_dir.join("button.hop");
        std::fs::write(&hop_file, "<button>click</button>").expect("Failed to write hop file");

        let batch = rx.recv().await.expect("Failed to receive batch");
        let expected_id = ModuleId::new("src::components::button").unwrap();
        assert!(batch.modified.contains(&expected_id));
    }

    #[tokio::test]
    async fn dynamic_directory_watching() {
        let (temp_dir, project) = create_test_project();

        let watcher = ProjectWatcher::new(&project, TEST_DEBOUNCE)
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        // Create a new directory
        let new_dir = temp_dir.path().join("new_dir");
        std::fs::create_dir(&new_dir).expect("Failed to create directory");

        // Wait for the directory watch to be established
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Now create a .hop file in the new directory
        let test_file = new_dir.join("test.hop");
        std::fs::write(&test_file, "<test>hello</test>").expect("Failed to write test file");

        // Should receive the .hop file
        let batch = rx.recv().await.expect("Failed to receive batch");
        let expected_id = ModuleId::new("new_dir::test").unwrap();
        assert!(batch.modified.contains(&expected_id));
    }
}
