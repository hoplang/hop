use crate::filesystem::files::ProjectRoot;
use crate::hop::evaluator::HopMode;
use crate::hop::program::Program;
use axum::body::Body;
use axum::extract::{Query, State};
use axum::http::StatusCode;
use axum::response::Response;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
struct AppState {
    program: Arc<RwLock<Program>>,
    reload_channel: tokio::sync::broadcast::Sender<()>,
}

async fn handle_idiomorph() -> Response<Body> {
    Response::builder()
        .header("Content-Type", "application/javascript")
        .header("Cache-Control", "public, max-age=31536000, immutable")
        .body(Body::from(include_str!("_hop/idiomorph.js")))
        .unwrap()
}

async fn handle_bootstrap() -> Response<Body> {
    Response::builder()
        .header("Content-Type", "application/javascript")
        .header("Access-Control-Allow-Origin", "*")
        .header("Cache-Control", "public, max-age=31536000, immutable")
        .body(Body::from(include_str!("_hop/bootstrap.js")))
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

    // Find the module containing the entrypoint
    // The entrypoint name is just the component name, we need to find which module it's in
    let mut found_module = None;
    for (module_name, ast) in program.get_modules() {
        for component in ast.get_component_definitions() {
            if component.is_entrypoint && component.tag_name.as_str() == query.entrypoint {
                found_module = Some(module_name.clone());
                break;
            }
        }
        if found_module.is_some() {
            break;
        }
    }

    let module_name = match found_module {
        Some(m) => m,
        None => {
            return Response::builder()
                .status(StatusCode::NOT_FOUND)
                .header("Access-Control-Allow-Origin", "*")
                .header("Content-Type", "text/html")
                .body(Body::from(format!(
                    "Entrypoint '{}' not found",
                    query.entrypoint
                )))
                .unwrap();
        }
    };

    // Render the component
    let mut html = String::new();
    match program.evaluate_component(
        &module_name,
        &query.entrypoint,
        params,
        HopMode::Dev,
        &mut html,
    ) {
        Ok(()) => Response::builder()
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

fn create_file_watcher(
    root: &ProjectRoot,
    state: AppState,
) -> anyhow::Result<notify::RecommendedWatcher> {
    use notify::Watcher;
    let local_root = root.clone();

    // TODO: We should ignore folders such as .git, .direnv

    let mut watcher = notify::RecommendedWatcher::new(
        move |res: Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() || event.kind.is_remove() {
                    // Check if it's a .hop file
                    let is_hop_file = event
                        .paths
                        .iter()
                        .any(|p| p.extension().and_then(|e| e.to_str()) == Some("hop"));

                    if is_hop_file {
                        // Reload all modules from scratch
                        if let Ok(modules) = local_root.load_all_hop_modules() {
                            let new_program = Program::new(modules);
                            if let Ok(mut program) = state.program.write() {
                                *program = new_program;
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

    Ok(watcher)
}

/// Create a router that responds to requests for the output files specified in the build.hop file.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/_hop/event_source` route. There is also an injected
/// script on in all `html` files that listens to SSE-events on that route and performs
/// hot-reloading when an event is emitted.
///
/// The client may change the build.hop file while the server is running as the server will reread the
/// build file whenever a new request comes in.
pub async fn execute(
    root: &ProjectRoot,
) -> anyhow::Result<(axum::Router, notify::RecommendedWatcher)> {
    use axum::routing::get;

    let modules = root.load_all_hop_modules()?;

    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);

    let app_state = AppState {
        program: Arc::new(RwLock::new(Program::new(modules))),
        reload_channel,
    };

    let watcher = create_file_watcher(root, app_state.clone())?;

    let router = axum::Router::new()
        .route("/_hop/idiomorph.js", get(handle_idiomorph))
        .route("/_hop/bootstrap.js", get(handle_bootstrap))
        .route("/_hop/event_source", get(handle_event_source))
        .route("/render", get(handle_render));

    Ok((router.with_state(app_state), watcher))
}
