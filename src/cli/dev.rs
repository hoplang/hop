use crate::filesystem::files::ProjectRoot;
use crate::hop::evaluator::HopMode;
use crate::hop::module_name::ModuleName;
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, OnceLock, RwLock};

use crate::filesystem::files;
use crate::hop::program::Program;
use axum::body::Body;
use axum::extract::{Request, State};
use axum::http::StatusCode;
use axum::response::{Html, Response};

#[derive(Clone)]
struct AppState {
    program: Arc<RwLock<Program>>,
    reload_channel: tokio::sync::broadcast::Sender<()>,
}

const ERROR_TEMPLATES: &str = include_str!("../../hop/error_pages.hop");
const UI_TEMPLATES: &str = include_str!("../../hop/ui.hop");
const ICONS_TEMPLATES: &str = include_str!("../../hop/icons.hop");

static CACHED_UI_SERVER: OnceLock<Program> = OnceLock::new();

fn get_ui_program() -> &'static Program {
    CACHED_UI_SERVER.get_or_init(|| {
        let mut program = Program::default();

        program.update_module(
            ModuleName::new("hop/error_pages".to_string()).unwrap(),
            ERROR_TEMPLATES.to_string(),
        );
        program.update_module(ModuleName::new("hop/ui".to_string()).unwrap(), UI_TEMPLATES.to_string());
        program.update_module(ModuleName::new("hop/icons".to_string()).unwrap(), ICONS_TEMPLATES.to_string());

        // Check for any errors in the UI templates
        let parse_errors = program.get_parse_errors();
        let type_errors = program.get_type_errors();

        if parse_errors.values().any(|errors| !errors.is_empty()) {
            panic!("Parse errors in UI templates: {:#?}", parse_errors);
        }
        if type_errors.values().any(|errors| !errors.is_empty()) {
            panic!("Type errors in UI templates: {:#?}", type_errors);
        }

        program
    })
}

fn inject_hot_reload_script(html: &str) -> String {
    const HOT_RELOAD_SCRIPT: &str = r#"<script type="module" src="/_hop/hmr.js"></script>"#;

    // Try to inject before closing body tag, fallback to end of document
    if let Some(body_end_pos) = html.rfind("</body>") {
        let mut result = String::with_capacity(html.len() + HOT_RELOAD_SCRIPT.len());
        result.push_str(&html[..body_end_pos]);
        result.push_str(HOT_RELOAD_SCRIPT);
        result.push_str(&html[body_end_pos..]);
        result
    } else if let Some(html_end_pos) = html.rfind("</html>") {
        let mut result = String::with_capacity(html.len() + HOT_RELOAD_SCRIPT.len());
        result.push_str(&html[..html_end_pos]);
        result.push_str(HOT_RELOAD_SCRIPT);
        result.push_str(&html[html_end_pos..]);
        result
    } else {
        // If no body or html tags found, append to end
        let mut result = String::with_capacity(html.len() + HOT_RELOAD_SCRIPT.len() + 1);
        result.push_str(html);
        result.push_str(HOT_RELOAD_SCRIPT);
        result.push('\n');
        result
    }
}

async fn handle_idiomorph() -> Response<Body> {
    Response::builder()
        .header("Content-Type", "application/javascript")
        .body(Body::from(include_str!("_hop/idiomorph.js")))
        .unwrap()
}

async fn handle_hmr() -> Response<Body> {
    Response::builder()
        .header("Content-Type", "application/javascript")
        .body(Body::from(include_str!("_hop/hmr.js")))
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

async fn handle_script(State(state): State<AppState>) -> Response<Body> {
    let program = state.program.read().unwrap();
    Response::builder()
        .header("Content-Type", "application/javascript")
        .body(Body::from(program.get_scripts().to_string()))
        .unwrap()
}

async fn handle_request(
    State(state): State<AppState>,
    req: Request,
) -> Result<Html<String>, (StatusCode, Html<String>)> {
    // Get the program from shared state
    let program = state.program.read().unwrap();

    let path = req.uri().path();

    // Convert request path to file path format
    let mut file_path = match path {
        "/" => "index.html".to_string(),
        path if path.starts_with('/') => path[1..].to_string(),
        path => path.to_string(),
    };

    if !file_path.ends_with(".html") {
        file_path += ".html";
    }

    // Try to render the requested file
    let mut content = String::new();
    match program.render_file(&file_path, HopMode::Dev, &mut content) {
        Ok(()) => Ok(Html(inject_hot_reload_script(&content))),
        Err(_) => {
            let available_paths: Vec<String> = program
                .get_renderable_file_paths()
                .iter()
                .map(|file_path| {
                    if file_path == "index.html" {
                        "/".to_string()
                    } else if file_path.ends_with(".html") {
                        format!("/{}", file_path.strip_suffix(".html").unwrap())
                    } else {
                        format!("/{}", file_path)
                    }
                })
                .collect();

            Err((
                StatusCode::NOT_FOUND,
                Html(create_not_found_page(req.uri().path(), &available_paths)),
            ))
        }
    }
}

#[allow(dead_code)]
fn create_error_page(error: &anyhow::Error) -> String {
    let program = get_ui_program();

    let mut args = HashMap::new();
    args.insert(
        "error".to_string(),
        serde_json::json!({
            "message": format!("{:#}", error).to_string()
        }),
    );
    let mut html = String::new();
    match program.evaluate_component(
        &ModuleName::new("hop/error_pages".to_string()).unwrap(),
        "generic-error",
        args,
        HopMode::Dev,
        &mut html,
    ) {
        Ok(()) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_not_found_page(path: &str, available_routes: &[String]) -> String {
    let program = get_ui_program();

    let mut args = HashMap::new();
    args.insert("path".to_string(), serde_json::json!(path));
    args.insert(
        "available_routes".to_string(),
        serde_json::json!(available_routes),
    );
    let mut html = String::new();
    match program.evaluate_component(
        &ModuleName::new("hop/error_pages".to_string()).unwrap(),
        "not-found-error",
        args,
        HopMode::Dev,
        &mut html,
    ) {
        Ok(()) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
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
                        if let Ok(modules) = files::load_all_hop_modules(&local_root) {
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
/// If `static_dir` is specified the server serves static files from the given directory as well.
///
/// The client may change the build.hop file while the server is running as the server will reread the
/// build file whenever a new request comes in.
pub async fn execute(
    root: &ProjectRoot,
    static_dir: Option<&Path>,
    script_file: Option<&str>,
) -> anyhow::Result<(axum::Router, notify::RecommendedWatcher)> {
    use axum::routing::get;
    use tower_http::services::ServeDir;

    let modules = files::load_all_hop_modules(root)?;

    let (reload_channel, _) = tokio::sync::broadcast::channel::<()>(100);

    let app_state = AppState {
        program: Arc::new(RwLock::new(Program::new(modules))),
        reload_channel,
    };

    let watcher = create_file_watcher(root, app_state.clone())?;

    let mut router = axum::Router::new()
        .route("/_hop/idiomorph.js", get(handle_idiomorph))
        .route("/_hop/hmr.js", get(handle_hmr))
        .route("/_hop/event_source", get(handle_event_source));

    if let Some(script_filename) = script_file {
        router = router.route(&format!("/{}", script_filename), get(handle_script));
    }

    if let Some(static_dir) = static_dir {
        if !static_dir.is_dir() {
            anyhow::bail!("servedir '{}' is not a directory", static_dir.display());
        }
        router = router.fallback_service(
            ServeDir::new(static_dir).fallback(get(handle_request).with_state(app_state.clone())),
        );
    } else {
        router = router.fallback(handle_request);
    }

    Ok((router.with_state(app_state), watcher))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::archive::temp_dir_from_archive;
    use axum::http;
    use axum_test::{TestResponse, TestServer};
    use expect_test::{Expect, expect};
    use indoc::indoc;
    use simple_txtar::Archive;
    use std::fs;

    fn check_response(response: TestResponse, status: http::StatusCode, expected: &Expect) {
        response.assert_status(status);
        expected.assert_eq(&response.text());
    }

    fn check_response_contains(response: TestResponse, status: http::StatusCode, contains: &str) {
        response.assert_status(status);
        let body = response.text();
        assert!(
            body.contains(contains),
            "Response body does not contain: {}",
            contains
        );
    }

    /// When the user calls `hop dev` and has a entry for `index.html` in the manifest file, the
    /// index.html entry should be rendered when the user issues a GET to /.
    #[tokio::test]
    async fn test_dev_and_get_index() {
        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
              Hello from build.hop!
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router.into_make_service()).unwrap();

        check_response(
            server.get("/").await,
            http::StatusCode::OK,
            &expect![[r#"

                  Hello from build.hop!
                <script type="module" src="/_hop/hmr.js"></script>
            "#]],
        );
    }

    /// When the user changes the contents of the build.hop file after running `hop dev` the
    /// changes should be reflected when the user sends a request to the server.
    #[tokio::test]
    async fn test_dev_from_hop_dynamic_update() {
        let archive = Archive::from(indoc! {r#"
            -- src/test.hop --
            <foo-comp>
              message is foo
            </foo-comp>
            <bar-comp>
              message is bar
            </bar-comp>
            -- build.hop --
            <import component="foo-comp" from="@/src/test" />

            <render file="index.html">
              <foo-comp />
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router.into_make_service()).unwrap();

        check_response(
            server.get("/").await,
            http::StatusCode::OK,
            &expect![[r#"

                  <div data-hop-id="src/test/foo-comp">
                  message is foo
                </div>
                <script type="module" src="/_hop/hmr.js"></script>
            "#]],
        );

        fs::write(
            dir.join("build.hop"),
            indoc! {r#"
                <import component="bar-comp" from="@/src/test" />

                <render file="index.html">
                  <bar-comp />
                </render>
            "#},
        )
        .unwrap();

        // TODO: This is flaky, the correct thing to do would be to
        // read the event source.
        tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;

        check_response(
            server.get("/").await,
            http::StatusCode::OK,
            &expect![[r#"

                  <div data-hop-id="src/test/bar-comp">
                  message is bar
                </div>
                <script type="module" src="/_hop/hmr.js"></script>
            "#]],
        );
    }

    /// When the user calls `hop dev` with a servedir parameter, static files should be served
    /// from the given directory.
    #[tokio::test]
    async fn test_dev_from_hop_static_files() {
        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
              hello world!
            </render>
            -- static/css/style.css --
            body { background: blue; }
            -- static/js/script.js --
            console.log("Hello from static file");
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, Some(&dir.join("static")), None)
            .await
            .unwrap();

        let server = TestServer::new(router.into_make_service()).unwrap();

        let response = server.get("/").await;
        check_response(
            response,
            http::StatusCode::OK,
            &expect![[r#"

                  hello world!
                <script type="module" src="/_hop/hmr.js"></script>
            "#]],
        );

        check_response(
            server.get("/css/style.css").await,
            http::StatusCode::OK,
            &expect![[r#"
                body { background: blue; }
            "#]],
        );

        check_response(
            server.get("/js/script.js").await,
            http::StatusCode::OK,
            &expect![[r#"
                console.log("Hello from static file");
            "#]],
        );
        check_response_contains(
            server.get("/_hop/hmr.js").await,
            http::StatusCode::OK,
            "new EventSource('/_hop/event_source')",
        );
    }

    /// When the user calls `hop dev` the global HOP_MODE variable should
    /// be set to 'dev'.
    #[tokio::test]
    async fn test_dev_has_hop_mode_dev() {
        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
              mode: {HOP_MODE}
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router.into_make_service()).unwrap();

        check_response(
            server.get("/").await,
            http::StatusCode::OK,
            &expect![[r#"

                  mode: dev
                <script type="module" src="/_hop/hmr.js"></script>
            "#]],
        );
    }

    /// When the user calls `hop dev` and requests a path that doesn't exist in the manifest,
    /// a 404 page should be returned with available routes.
    #[tokio::test]
    async fn test_dev_404_with_helpful_message() {
        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
              Hello world!
            </render>
            <render file="about.html">
              The about page
            </render>
            <render file="foo/bar.html">
              The nested page
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router.into_make_service()).unwrap();

        // Test non-existent path
        let response = server.get("/nonexistent").await;
        check_response(
            response,
            http::StatusCode::NOT_FOUND,
            &expect![[r#"

                	<!DOCTYPE html>
                	<html>
                	<head>
                		<title>404 Not Found</title>
                		<script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
                		<link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
                        <style>
                          body { font-family: "JetBrains Mono"; }
                        </style>
                	</head>
                	<body>
                		<div data-hop-id="hop/ui/page-container" class="max-w-6xl px-4 my-12 mx-auto">
	
                			<div data-hop-id="hop/error_pages/error-not-found-error" class="flex flex-col gap-4">
                	<div data-hop-id="hop/ui/heading-box" class="border border-2 shadow-[4px_4px_rgba(0,0,0,0.1)]">
                	<div class="p-3 py-2 flex justify-between border-b-2">
                		<div>
                			<span class="font-medium uppercase">
                			  Error - Route not found
                			</span>
                		</div>
                	</div>
                	<div class="p-5 gap-5 flex flex-col" data-id="body">
		
                		<div>
                		The requested route <span data-hop-id="hop/error_pages/code-text">
                	<code class="italic">/nonexistent</code>
                </span> was not found in the build file.
                		</div>
                		Available routes:<br>
                		<ul class="ml-6" style="list-style-type: square;">
			
                				<li><a href="/">/</a></li>
			
                				<li><a href="/about">/about</a></li>
			
                				<li><a href="/foo/bar">/foo/bar</a></li>
			
                		</ul>
                		<p>
                			To add this route, update your <span data-hop-id="hop/error_pages/code-text">
                	<code class="italic">build.hop</code>
                </span> file with an entry for this path.
                		</p>
	
                	</div>
                </div>
                </div>
		
                </div>
                	<script type="module" src="/_hop/hmr.js"></script></body>
                	</html>
            "#]],
        );
    }

    /// When the user calls `hop dev` and the program has parse errors, it should still run
    /// but not find any routes (since parsing failed).
    #[tokio::test]
    async fn test_dev_parse_error_shows_no_routes() {
        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
                <div>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router.into_make_service()).unwrap();

        // With parse errors, the program still exists but has no render nodes,
        // so we get a 404 with no available routes
        let response = server.get("/").await;
        response.assert_status(axum::http::StatusCode::NOT_FOUND);
    }
}
