use crate::filesystem::files::ProjectRoot;
use std::path::Path;
use std::sync::OnceLock;

use crate::filesystem::files;
use crate::hop;
use crate::hop::compiler::compile;
use crate::hop::runtime::HopMode;

const ERROR_TEMPLATES: &str = include_str!("../../hop/error_pages.hop");
const UI_TEMPLATES: &str = include_str!("../../hop/ui.hop");
const ICONS_TEMPLATES: &str = include_str!("../../hop/icons.hop");

static CACHED_UI_PROGRAM: OnceLock<hop::runtime::Program> = OnceLock::new();

fn get_ui_program() -> &'static hop::runtime::Program {
    CACHED_UI_PROGRAM.get_or_init(|| {
        let modules = vec![
            ("hop/error_pages".to_string(), ERROR_TEMPLATES.to_string()),
            ("hop/ui".to_string(), UI_TEMPLATES.to_string()),
            ("hop/icons".to_string(), ICONS_TEMPLATES.to_string()),
        ];

        compile(modules, HopMode::Dev).expect("Failed to compile UI templates")
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

fn create_error_page(error: &anyhow::Error) -> String {
    let program = get_ui_program();

    match program.evaluate_component(
        "hop/error_pages",
        "generic-error",
        vec![serde_json::json!(format!("{:#}", error).to_string())],
        None,
        None,
    ) {
        Ok(html) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_not_found_page(path: &str, available_routes: &[String]) -> String {
    let program = get_ui_program();

    match program.evaluate_component(
        "hop/error_pages",
        "not-found-error",
        vec![serde_json::json!(path), serde_json::json!(available_routes)],
        None,
        None,
    ) {
        Ok(html) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_file_watcher(
    root: &ProjectRoot,
) -> anyhow::Result<(
    notify::RecommendedWatcher,
    tokio::sync::broadcast::Sender<()>,
)> {
    use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};

    let (channel, _) = tokio::sync::broadcast::channel::<()>(100);

    let sender = channel.clone();

    // TODO: We should ignore folders such as .git, .direnv

    let mut watcher = RecommendedWatcher::new(
        move |res: Result<notify::Event, notify::Error>| {
            if let Ok(event) = res {
                if event.kind.is_modify() || event.kind.is_create() || event.kind.is_remove() {
                    let _ = sender.send(());
                }
            }
        },
        Config::default(),
    )?;

    watcher.watch(root.get_path(), RecursiveMode::Recursive)?;

    Ok((watcher, channel))
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
    use axum::body::Body;
    use axum::extract::Request;
    use axum::http::StatusCode;
    use axum::response::Html;
    use axum::response::sse::{Event, Sse};
    use axum::routing::get;
    use tokio_stream::StreamExt;
    use tokio_stream::wrappers::BroadcastStream;

    // Set up file watcher for hot reloading
    let mut router = axum::Router::new();

    // Add SSE endpoint for hot reload events
    let (watcher, channel) = create_file_watcher(root)?;
    router = router.route(
        "/_hop/event_source",
        get(async move || {
            Sse::new(
                BroadcastStream::new(channel.subscribe())
                    .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload"))),
            )
        }),
    );

    // Add idiomorph.js serving endpoint
    router = router.route(
        "/_hop/idiomorph.js",
        get(async move || {
            axum::response::Response::builder()
                .header("Content-Type", "application/javascript")
                .body(Body::from(include_str!("_hop/idiomorph.js")))
                .unwrap()
        }),
    );

    // Add hot reload script serving endpoint
    router = router.route(
        "/_hop/hmr.js",
        get(async move || {
            axum::response::Response::builder()
                .header("Content-Type", "application/javascript")
                .body(Body::from(include_str!("_hop/hmr.js")))
                .unwrap()
        }),
    );

    // Add script serving endpoint if script_file is specified
    if let Some(script_filename) = script_file {
        let script_path = format!("/{}", script_filename);
        let local_root = root.clone();
        router = router.route(
            &script_path,
            get(async move || {
                match files::load_all_hop_modules(&local_root)
                    .and_then(|modules| compile(modules, HopMode::Dev))
                {
                    Ok(program) => Ok(axum::response::Response::builder()
                        .header("Content-Type", "application/javascript")
                        .body(Body::from(program.get_scripts().to_string()))
                        .unwrap()),
                    Err(e) => Err((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        format!("Failed to compile hop program: {}", e),
                    )),
                }
            }),
        );
    }

    let local_root = root.clone();
    let request_handler = async move |req: Request| {
        // Compile the program
        let program = match (|| -> anyhow::Result<hop::runtime::Program> {
            let modules = files::load_all_hop_modules(&local_root)?;
            compile(modules, HopMode::Dev)
        })() {
            Ok(program) => program,
            Err(e) => {
                return Err((
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Html(create_error_page(&e)),
                ));
            }
        };

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
        match program.render_file(&file_path) {
            Ok(content) => Ok(Html(inject_hot_reload_script(&content))),
            Err(_) => {
                let available_paths: Vec<String> = program
                    .get_render_file_paths()
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
    };

    // Set up static file serving if specified
    if let Some(static_dir) = static_dir {
        use axum::routing::get;
        if !static_dir.is_dir() {
            anyhow::bail!("servedir '{}' is not a directory", static_dir.display());
        }
        router = router.fallback_service(
            tower_http::services::ServeDir::new(static_dir).fallback(get(request_handler)),
        );
    } else {
        router = router.fallback(request_handler);
    }

    Ok((router, watcher))
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

        let server = TestServer::new(router).unwrap();

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
            <import component="foo-comp" from="src/test" />

            <render file="index.html">
              <foo-comp />
            </render>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router).unwrap();

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
                <import component="bar-comp" from="src/test" />

                <render file="index.html">
                  <bar-comp />
                </render>
            "#},
        )
        .unwrap();

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

        let server = TestServer::new(router).unwrap();

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

        let server = TestServer::new(router).unwrap();

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

        let server = TestServer::new(router).unwrap();

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

    /// When the user calls `hop dev` and the program doesn't compile an error should be returned.
    #[tokio::test]
    async fn test_dev_compile_error() {
        let archive = Archive::from(indoc! {r#"
            -- build.hop --
            <render file="index.html">
                <div>
        "#});
        let dir = temp_dir_from_archive(&archive).unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router).unwrap();

        // Test non-existent path
        let response = server.get("/").await;
        response.assert_status(axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    }
}
