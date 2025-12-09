use anyhow::{Result, bail};
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tokio::sync::{broadcast, mpsc};
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub enum WatchEvent {
    Created(PathBuf),
    Modified(PathBuf),
    Deleted(PathBuf),
}

struct WatcherState {
    watcher: Arc<Mutex<RecommendedWatcher>>,
    watched_paths: Arc<Mutex<HashSet<PathBuf>>>,
    ignored_folder_names: HashSet<String>,
}

impl WatcherState {
    fn handle_notify_event(&self, event: Event, tx: &broadcast::Sender<WatchEvent>) {
        for path in &event.paths {
            // Handle new directory creation
            if matches!(event.kind, EventKind::Create(_)) && path.is_dir() {
                self.add_watch(path);
            }

            // Handle directory removal
            if matches!(event.kind, EventKind::Remove(_)) {
                let mut watched_paths = self.watched_paths.lock().unwrap();
                if watched_paths.contains(path) {
                    if let Ok(mut watcher) = self.watcher.lock() {
                        let _ = watcher.unwatch(path);
                    }
                    watched_paths.remove(path);
                }
            }

            // Send user-facing events
            let watch_event = match event.kind {
                EventKind::Create(_) => Some(WatchEvent::Created(path.clone())),
                EventKind::Modify(_) => Some(WatchEvent::Modified(path.clone())),
                EventKind::Remove(_) => Some(WatchEvent::Deleted(path.clone())),
                _ => None,
            };

            if let Some(event) = watch_event {
                let _ = tx.send(event);
            }
        }
    }

    fn add_watch(&self, path: &Path) {
        if self.should_ignore(path) {
            return;
        }

        // Use walkdir to handle the new directory and all its subdirectories
        for entry in WalkDir::new(path)
            .into_iter()
            .filter_entry(|e| !self.should_ignore(e.path()))
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

    fn should_ignore(&self, path: &Path) -> bool {
        path.components().any(|c| {
            c.as_os_str()
                .to_str()
                .map(|s| self.ignored_folder_names.contains(s))
                .unwrap_or(false)
        })
    }
}

pub struct AdaptiveWatcher {
    event_tx: broadcast::Sender<WatchEvent>,
    _watcher: Arc<Mutex<RecommendedWatcher>>,
    _task_handle: tokio::task::JoinHandle<()>,
}

impl AdaptiveWatcher {
    pub async fn new(root: impl AsRef<Path>, ignored_folder_names: Vec<&str>) -> Result<Self> {
        let root = root.as_ref().to_path_buf();

        if !root.exists() {
            bail!("Root path does not exist: {:?}", root);
        }

        if !root.is_dir() {
            bail!("Root path must be a directory: {:?}", root);
        }

        let ignored_folder_names: HashSet<String> = ignored_folder_names
            .into_iter()
            .map(|s| s.to_string())
            .collect();
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
            ignored_folder_names: ignored_folder_names.clone(),
        });

        // Initial directory scanning and watching
        for entry in WalkDir::new(&root).into_iter().filter_entry(|e| {
            e.file_name()
                .to_str()
                .map(|s| !ignored_folder_names.contains(s))
                .unwrap_or(true)
        }) {
            let entry = entry?;
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

        let (event_tx, _rx) = broadcast::channel(1000);
        let event_tx_clone = event_tx.clone();
        let state_clone = state.clone();
        let task_handle = tokio::spawn(async move {
            while let Some(event) = notify_rx.recv().await {
                state_clone.handle_notify_event(event, &event_tx_clone);
            }
        });

        Ok(Self {
            event_tx,
            _watcher: state.watcher.clone(),
            _task_handle: task_handle,
        })
    }

    pub fn subscribe(&self) -> broadcast::Receiver<WatchEvent> {
        self.event_tx.subscribe()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn file_creation_event() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        let watcher = AdaptiveWatcher::new(root_path, vec![])
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        let test_file = root_path.join("test.txt");
        std::fs::write(&test_file, "hello").expect("Failed to write test file");

        match rx.recv().await {
            Ok(WatchEvent::Created(path)) => {
                assert_eq!(path, test_file);
            }
            Ok(event) => panic!("Expected Created event, got {:?}", event),
            Err(e) => panic!("Failed to receive event: {:?}", e),
        }
    }

    #[tokio::test]
    async fn file_modification_event() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        // Create file first
        let test_file = root_path.join("test.txt");
        std::fs::write(&test_file, "initial").expect("Failed to write test file");

        let watcher = AdaptiveWatcher::new(root_path, vec![])
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        // Modify the file
        std::fs::write(&test_file, "modified").expect("Failed to modify test file");

        match rx.recv().await {
            Ok(WatchEvent::Modified(path)) => {
                assert_eq!(path, test_file);
            }
            Ok(event) => panic!("Expected Modified event, got {:?}", event),
            Err(e) => panic!("Failed to receive event: {:?}", e),
        }
    }

    #[tokio::test]
    async fn dynamic_directory_watching() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        let watcher = AdaptiveWatcher::new(root_path, vec![])
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        // Create a new directory
        let new_dir = root_path.join("new_dir");
        std::fs::create_dir(&new_dir).expect("Failed to create directory");

        // Wait for the directory creation event
        match rx.recv().await {
            Ok(WatchEvent::Created(path)) => {
                assert_eq!(path, new_dir);
            }
            Ok(event) => panic!("Expected Created event for directory, got {:?}", event),
            Err(e) => panic!("Failed to receive directory creation event: {:?}", e),
        }

        // Now create a file in the new directory
        let test_file = new_dir.join("test.txt");
        std::fs::write(&test_file, "hello").expect("Failed to write test file");

        // Wait for the file creation event
        match rx.recv().await {
            Ok(WatchEvent::Created(path)) => {
                assert_eq!(path, test_file);
            }
            Ok(event) => panic!("Expected Created event for file, got {:?}", event),
            Err(e) => panic!("Failed to receive file creation event: {:?}", e),
        }
    }

    #[tokio::test]
    async fn ignored_directory_not_watched() {
        let temp_dir = TempDir::new().unwrap();
        let root_path = temp_dir.path();

        let watcher = AdaptiveWatcher::new(root_path, vec!["node_modules"])
            .await
            .expect("Failed to create watcher");

        let mut rx = watcher.subscribe();

        // Create an ignored directory
        let ignored_dir = root_path.join("node_modules");
        std::fs::create_dir(&ignored_dir).expect("Failed to create ignored directory");

        match rx.recv().await {
            Ok(WatchEvent::Created(path)) => {
                assert_eq!(path, ignored_dir);
            }
            _ => panic!(),
        }

        // Create a file in the ignored directory
        let test_file = ignored_dir.join("test.txt");
        std::fs::write(&test_file, "hello").expect("Failed to write test file");

        let valid_file = root_path.join("valid.txt");
        std::fs::write(&valid_file, "test").expect("Failed to write valid file");

        match rx.recv().await {
            Ok(WatchEvent::Created(path)) => {
                assert_eq!(path, valid_file);
            }
            Ok(event) => panic!("Expected Created event for valid file, got {:?}", event),
            Err(e) => panic!("Failed to receive event: {:?}", e),
        }
    }
}
