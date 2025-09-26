use crate::filesystem::files::ProjectRoot;
use crate::hop::program::Program;
use axum::body::Body;
use axum::extract::{Query, State};
use axum::http::StatusCode;
use axum::response::Response;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use tailwind_runner::{TailwindRunner, WatchHandle};

#[derive(Clone)]
struct AppState {
    program: Arc<RwLock<Program>>,
    reload_channel: tokio::sync::broadcast::Sender<()>,
    tailwind_css: Arc<RwLock<Option<String>>>,
}

async fn handle_development_mode_js() -> Response<Body> {
    Response::builder()
        .header("Content-Type", "application/javascript")
        .header("Access-Control-Allow-Origin", "*")
        .header("Cache-Control", "public, max-age=31536000, immutable")
        .body(Body::from(include_str!("./js/development_mode.js")))
        .unwrap()
}

async fn handle_event_source(
    State(state): State<AppState>,
) -> axum::response::sse::Sse<
    impl tokio_stream::Stream<Item = Result<axum::response::sse::Event, axum::Error>>,
> {
    use axum::response::sse::{Event, Sse};
    use tokio_stream::StreamExt;

    Sse::new(
        tokio_stream::wrappers::BroadcastStream::new(state.reload_channel.subscribe())
            .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload"))),
    )
}

#[derive(serde::Deserialize)]
struct RenderParams {
    entrypoint: String,
    params: String,
}

async fn handle_render(
    State(state): State<AppState>,
    Query(query): Query<RenderParams>,
) -> Response<Body> {
    let program = state.program.read().unwrap();

    // Parse the JSON params
    let params: HashMap<String, serde_json::Value> = match serde_json::from_str(&query.params) {
        Ok(p) => p,
        Err(e) => {
            return Response::builder()
                .status(StatusCode::BAD_REQUEST)
                .header("Access-Control-Allow-Origin", "*")
                .header("Content-Type", "text/html")
                .body(Body::from(format!("Invalid JSON parameters: {}", e)))
                .unwrap();
        }
    };

    // Get the CSS content from state
    let css = state.tailwind_css.read().unwrap();
    let css_content = css.as_deref();

    // Render the component using IR evaluation
    match program.evaluate_ir_entrypoint(&query.entrypoint, params, "dev", css_content) {
        Ok(html) => Response::builder()
            .status(StatusCode::OK)
            .header("Access-Control-Allow-Origin", "*")
            .header("Content-Type", "text/html")
            .body(Body::from(html))
            .unwrap(),
        Err(e) => Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .header("Access-Control-Allow-Origin", "*")
            .header("Content-Type", "text/html")
            .body(Body::from(format!("Error rendering component: {}", e)))
            .unwrap(),
    }
}

async fn create_default_tailwind_input() -> anyhow::Result<PathBuf> {
    let cache_dir = PathBuf::from("/tmp/.hop-cache");
    tokio::fs::create_dir_all(&cache_dir).await?;

    let temp_input = cache_dir.join("default-input.css");
    let default_content = r#"@import "tailwindcss";"#;

    tokio::fs::write(&temp_input, default_content).await?;
    Ok(temp_input)
}

async fn start_tailwind_watcher(input_path: &PathBuf) -> anyhow::Result<(String, WatchHandle)> {
    let cache_dir = PathBuf::from("/tmp/.hop-cache");
    let runner = TailwindRunner::new(cache_dir).await?;

    let tailwind_config = tailwind_runner::TailwindConfig {
        input: input_path.clone(),
        output: PathBuf::from("/tmp/.hop-cache/tailwind-output.css"),
    };

    // Run once initially to generate CSS - return error if it fails
    runner.run_once(&tailwind_config).await?;

    // Read the generated CSS file
    let css_content = tokio::fs::read_to_string(&tailwind_config.output).await?;

    // Start watcher
    let handle = runner.watch(&tailwind_config).await?;

    // TODO: Monitor the output file for changes and update state

    Ok((css_content, handle))
}

fn create_file_watcher(
    root: &ProjectRoot,
    css_output_path: PathBuf,
    state: AppState,
) -> anyhow::Result<notify::RecommendedWatcher> {
    use notify::Watcher;
    let local_root = root.clone();

    // TODO: We should ignore folders such as .git, .direnv

    let css_path_clone = css_output_path.clone();
    let mut watcher = notify::RecommendedWatcher::new(
        move |res: Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() || event.kind.is_remove() {
                    // Check if it's a .hop file
                    let is_hop_file = event
                        .paths
                        .iter()
                        .any(|p| p.extension().and_then(|e| e.to_str()) == Some("hop"));

                    // Check if it's the CSS output file
                    let is_css_file = event.paths.iter().any(|p| p == &css_path_clone);

                    if is_hop_file {
                        // Reload all modules from scratch
                        if let Ok(modules) = local_root.load_all_hop_modules() {
                            let new_program = Program::new(modules);
                            if let Ok(mut program) = state.program.write() {
                                *program = new_program;
                            }
                        }
                    }

                    if is_css_file {
                        // Update CSS content in AppState
                        if let Ok(new_css) = std::fs::read_to_string(&css_path_clone) {
                            if let Ok(mut css_guard) = state.tailwind_css.write() {
                                *css_guard = Some(new_css);
                            }
                        }
                    }

                    let _ = state.reload_channel.send(());
                }
            }
        },
        notify::Config::default(),
    )?;

    watcher.watch(root.get_path(), notify::RecursiveMode::Recursive)?;

    // Also watch the CSS output file
    watcher.watch(&css_output_path, notify::RecursiveMode::NonRecursive)?;

    Ok(watcher)
}

/// Create a router that responds to render requests.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/_hop/event_source` route.
pub async fn execute(
    root: &ProjectRoot,
) -> anyhow::Result<(axum::Router, notify::RecommendedWatcher, WatchHandle)> {
    use axum::routing::get;

    let modules = root.load_all_hop_modules()?;

    // Load config to check for Tailwind configuration
    let config = root.load_config()?;

    // Always start Tailwind first - either with user config or default
    let tailwind_input_path = match config.css.tailwind {
        Some(ref tailwind_config) => PathBuf::from(&tailwind_config.input),
        None => create_default_tailwind_input().await?,
    };

    let (css_content, tailwind_handle) = start_tailwind_watcher(&tailwind_input_path).await?;

    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);

    let app_state = AppState {
        program: Arc::new(RwLock::new(Program::new(modules))),
        reload_channel,
        tailwind_css: Arc::new(RwLock::new(Some(css_content))),
    };

    let css_output_path = PathBuf::from("/tmp/.hop-cache/tailwind-output.css");
    let watcher = create_file_watcher(root, css_output_path, app_state.clone())?;

    let router = axum::Router::new()
        .route("/development_mode.js", get(handle_development_mode_js))
        .route("/_hop/event_source", get(handle_event_source))
        .route("/render", get(handle_render));

    Ok((router.with_state(app_state), watcher, tailwind_handle))
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::to_bytes;
    use axum::http::Request;
    use expect_test::expect;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_handle_render_with_ir_entrypoint() {
        // Create a test program with an entrypoint
        let mut modules = HashMap::new();
        modules.insert(
            crate::hop::module_name::ModuleName::new("test".to_string()).unwrap(),
            r#"
            <greeting-comp entrypoint {name: string, title: string}><h1>{title}</h1><p>Hello, {name}!</p></greeting-comp>

            <simple-comp entrypoint><div>Simple content</div></simple-comp>
            "#
            .to_string(),
        );

        let program = Program::new(modules);
        let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);

        let app_state = AppState {
            program: Arc::new(RwLock::new(program)),
            reload_channel,
            tailwind_css: Arc::new(RwLock::new(None)),
        };

        let app = axum::Router::new()
            .route("/render", axum::routing::get(handle_render))
            .with_state(app_state);

        // Test rendering with parameters
        let params = serde_json::json!({
            "name": "Alice",
            "title": "Welcome"
        });

        let encoded_params = params
            .to_string()
            .replace('"', "%22")
            .replace(' ', "%20")
            .replace('{', "%7B")
            .replace('}', "%7D")
            .replace(':', "%3A")
            .replace(',', "%2C");
        let request = Request::builder()
            .uri(format!(
                "/render?entrypoint=greeting-comp&params={}",
                encoded_params
            ))
            .body(Body::empty())
            .unwrap();

        let response = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);

        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let html = String::from_utf8(body.to_vec()).unwrap();

        expect!["<!DOCTYPE html><html><head></head><body><h1>Welcome</h1><p>Hello, Alice!</p></body></html>"].assert_eq(&html);

        // Test rendering without parameters
        let request = Request::builder()
            .uri("/render?entrypoint=simple-comp&params={}")
            .body(Body::empty())
            .unwrap();

        let response = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);

        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let html = String::from_utf8(body.to_vec()).unwrap();

        expect!["<!DOCTYPE html><html><head></head><body><div>Simple content</div></body></html>"]
            .assert_eq(&html);

        // Test non-existent entrypoint
        let request = Request::builder()
            .uri("/render?entrypoint=nonexistent&params={}")
            .body(Body::empty())
            .unwrap();

        let response = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);

        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let error_msg = String::from_utf8(body.to_vec()).unwrap();

        expect![[r#"Error rendering component: Entrypoint 'nonexistent' not found"#]]
            .assert_eq(&error_msg);

        // Test invalid JSON parameters
        let request = Request::builder()
            .uri("/render?entrypoint=simple-comp&params=invalid-json")
            .body(Body::empty())
            .unwrap();

        let response = app.oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::BAD_REQUEST);

        let body = to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let error_msg = String::from_utf8(body.to_vec()).unwrap();

        // The error message will include details about the JSON parse error
        // so we just check that it starts with the expected prefix
        assert!(error_msg.starts_with("Invalid JSON parameters:"));
    }
}
