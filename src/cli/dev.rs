use crate::filesystem::files::ProjectRoot;
use std::path::Path;
use std::sync::OnceLock;

use crate::common::escape_html;
use crate::filesystem::files;
use crate::hop;
use crate::hop::compiler::compile;
use crate::hop::runtime::HopMode;

const ERROR_TEMPLATES: &str = include_str!("../../hop/error_pages.hop");
const INSPECT_TEMPLATES: &str = include_str!("../../hop/inspector.hop");
const UI_TEMPLATES: &str = include_str!("../../hop/ui.hop");
const ICONS_TEMPLATES: &str = include_str!("../../hop/icons.hop");

static CACHED_UI_PROGRAM: OnceLock<hop::runtime::Program> = OnceLock::new();

fn get_ui_program() -> &'static hop::runtime::Program {
    CACHED_UI_PROGRAM.get_or_init(|| {
        let modules = vec![
            ("hop/error_pages".to_string(), ERROR_TEMPLATES.to_string()),
            ("hop/inspector".to_string(), INSPECT_TEMPLATES.to_string()),
            ("hop/ui".to_string(), UI_TEMPLATES.to_string()),
            ("hop/icons".to_string(), ICONS_TEMPLATES.to_string()),
        ];

        compile(modules, HopMode::Dev).expect("Failed to compile inspect program templates")
    })
}

fn inject_hot_reload_script(html: &str) -> String {
    const HOT_RELOAD_SCRIPT: &str = r#"
<script type="module">
import Idiomorph from '/idiomorph.js';

const eventSource = new EventSource('/__hop_hot_reload');
eventSource.onmessage = function(event) {
    if (event.data === 'reload') {
        fetch(window.location.href)
            .then(response => response.text())
            .then(html => {
                const parser = new DOMParser();
                const doc = parser.parseFromString(html, 'text/html');
                // Morph the entire document to enable head merging
                Idiomorph.morph(document.documentElement, doc.documentElement, {
                    head: {
                        shouldPreserve: function(elt) {
                            // Preserve elements with im-preserve attribute (default behavior)
                            if (elt.getAttribute('im-preserve') === 'true') {
                                return true;
                            }
                            // Preserve Tailwind CSS style tags
                            if (elt.tagName === 'STYLE' && elt.textContent && 
                                elt.textContent.includes('/*! tailwindcss')) {
                                return true;
                            }
                            return false;
                        }
                    }
                });
            })
            .catch(error => {
                console.error('Hot reload fetch error:', error);
            });
    }
};
eventSource.onerror = function(event) {
    console.log('Hot reload connection error:', event);
    setTimeout(() => {
        eventSource.close();
        location.reload();
    }, 1000);
};
window.addEventListener("beforeunload", function() {
    // This is important on chrome, not closing the event source will leave it open even when the
    // user navigates away.
    eventSource.close();
});
</script>
"#;

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
        let mut result = String::with_capacity(html.len() + HOT_RELOAD_SCRIPT.len());
        result.push_str(html);
        result.push_str(HOT_RELOAD_SCRIPT);
        result
    }
}

fn create_error_page(error: &anyhow::Error) -> String {
    let program = get_ui_program();

    let mut params = std::collections::BTreeMap::new();
    params.insert(
        "error".to_string(),
        serde_json::json!(format!("{:#}", error).to_string()),
    );
    match program.execute_simple("hop/error_pages", "generic-error", params) {
        Ok(html) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_not_found_page(path: &str, available_routes: &[String]) -> String {
    let program = get_ui_program();

    let mut params = std::collections::BTreeMap::new();
    params.insert("path".to_string(), serde_json::json!(path));
    params.insert(
        "available_routes".to_string(),
        serde_json::json!(available_routes),
    );
    match program.execute_simple("hop/error_pages", "not-found-error", params) {
        Ok(html) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_inspect_page(program: &hop::runtime::Program) -> String {
    let inspect_program = get_ui_program();

    let component_maps = program.get_component_maps();
    let parameter_types = program.get_parameter_types();

    let mut modules_data = Vec::new();

    let mut sorted_modules: Vec<_> = component_maps.iter().collect();
    sorted_modules.sort_by_key(|(name, _)| *name);

    for (module_name, components) in sorted_modules {
        let mut components_data = Vec::new();

        // Sort components for consistent output
        let mut sorted_components: Vec<_> = components.iter().collect();
        sorted_components.sort_by_key(|(name, _)| *name);

        for (component_name, component_def) in sorted_components {
            let has_preview = component_def.preview.is_some();
            let encoded_module = module_name.replace("/", "%2F");
            let encoded_component = component_name.replace("/", "%2F");

            let parameter_type = if let Some(module_types) = parameter_types.get(module_name) {
                if let Some(param_map) = module_types.get(component_name) {
                    if param_map.is_empty() {
                        Some("()".to_string())
                    } else {
                        let params: Vec<String> = param_map
                            .iter()
                            .map(|(name, typ)| format!("{}: {}", name, typ))
                            .collect();
                        Some(format!("({})", params.join(", ")))
                    }
                } else {
                    None
                }
            } else {
                None
            };

            let slots_text = if component_def.slots.is_empty() {
                None
            } else {
                Some(component_def.slots.join(", "))
            };

            components_data.push(serde_json::json!({
                "name": component_name,
                "has_preview": has_preview,
                "link": format!("/_inspect/{encoded_module}/{encoded_component}"),
                "preview_url": format!("/_preview/{encoded_module}/{encoded_component}"),
                "parameter_type": parameter_type,
                "is_entrypoint": component_def.entrypoint,
                "slots": component_def.slots,
                "slots_text": slots_text
            }));
        }

        modules_data.push(serde_json::json!({
            "name": module_name,
            "components": components_data
        }));
    }

    let inspect_data = serde_json::json!({
        "modules": modules_data
    });

    let combined_script = inspect_program.get_scripts();

    let mut params = std::collections::BTreeMap::new();
    params.insert("data".to_string(), inspect_data);
    match inspect_program.execute_simple("hop/inspector", "inspect-page", params) {
        Ok(html) => {
            let hot_reload_script = r#"
<script type="module">
import Idiomorph from './idiomorph.js';

const eventSource = new EventSource('/__hop_hot_reload');
eventSource.onmessage = async function(event) {
    if (event.data === 'reload') {
        // Reload all iframes
        const iframes = document.querySelectorAll('iframe');
        for (const iframe of iframes) {
            try {
                const previewUrl = iframe.src;
                const response = await fetch(previewUrl);
                const html = await response.text();
                if (iframe.contentDocument) {
                    // Parse the HTML and morph the entire iframe document
                    const parser = new DOMParser();
                    const doc = parser.parseFromString(html, 'text/html');
                    if (iframe.contentDocument && iframe.contentDocument.documentElement) {
                        Idiomorph.morph(iframe.contentDocument.documentElement, doc.documentElement, {
                            head: {
                                shouldPreserve: function(elt) {
                                    // Preserve elements with im-preserve attribute (default behavior)
                                    if (elt.getAttribute('im-preserve') === 'true') {
                                        return true;
                                    }
                                    // Preserve Tailwind CSS style tags
                                    if (elt.tagName === 'STYLE' && elt.textContent && 
                                        elt.textContent.includes('/*! tailwindcss')) {
                                        return true;
                                    }
                                    return false;
                                }
                            }
                        });
                    }
                }
            } catch (error) {
                console.error('Error reloading iframe:', error);
                // Fallback to full iframe reload
                iframe.src = iframe.src;
            }
        }
    }
};
eventSource.onerror = function(event) {
    console.log('Hot reload connection error:', event);
    setTimeout(() => {
        eventSource.close();
        location.reload();
    }, 1000);
};
window.addEventListener("beforeunload", function() {
    eventSource.close();
});
</script>
"#;

            html.replace(
                "</body>",
                format!(
                    "{}<script>{}</script></body>",
                    hot_reload_script, combined_script
                )
                .as_str(),
            )
        }
        Err(e) => format!("Error rendering inspect template: {}", e),
    }
}

fn create_component_preview(
    program: &hop::runtime::Program,
    module_name: &str,
    component_name: &str,
) -> Result<String, String> {
    let component_maps = program.get_component_maps();
    let component_map = component_maps
        .get(module_name)
        .ok_or_else(|| format!("Module '{}' not found", module_name))?;
    let component_def = component_map.get(component_name).ok_or_else(|| {
        format!(
            "Component '{}' not found in module '{}'",
            component_name, module_name
        )
    })?;
    if component_def.preview.is_none() {
        return Err("Component does not have preview content defined".to_string());
    }

    // Render the component using preview content if available
    match program.execute_preview(module_name, component_name) {
        Ok(rendered_content) => {
            let combined_script = program.get_scripts();

            // Create a simple HTML document with proper structure
            let html = format!(
                r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{}/{} Preview</title>
    <script src="https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4"></script>
    <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:ital,wght@0,100..800;1,100..800&display=swap" rel="stylesheet">
    <style>
      body {{ font-family: "JetBrains Mono"; }}
    </style>
</head>
<body>
{}
<script>{}</script>
</body>
</html>"#,
                escape_html(module_name),
                escape_html(component_name),
                rendered_content,
                combined_script
            );

            Ok(html)
        }
        Err(e) => Err(format!("Failed to render component: {}", e)),
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
/// The watcher emits SSE-events on the `/__hop_hot_reload` route. There is also an injected
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
    use axum::extract::Path;
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
        "/__hop_hot_reload",
        get(async move || {
            Sse::new(
                BroadcastStream::new(channel.subscribe())
                    .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload"))),
            )
        }),
    );

    // Add inspect endpoint for listing modules and components
    let inspect_root = root.clone();
    router = router.route(
        "/_inspect",
        get(async move || {
            match files::load_all_hop_modules(&inspect_root)
                .and_then(|modules| compile(modules, HopMode::Dev))
            {
                Ok(program) => {
                    let html = create_inspect_page(&program);
                    Ok(Html(html))
                }
                Err(e) => Err((
                    StatusCode::INTERNAL_SERVER_ERROR,
                    Html(create_error_page(&e)),
                )),
            }
        }),
    );

    // Add simple preview endpoint for rendering components without inspect UI
    let preview_root = root.clone();
    router = router.route(
        "/_preview/{module}/{component}",
        get(
            async move |Path((module_name, component_name)): Path<(String, String)>| {
                // decode the parameters
                let decoded_module = module_name.replace("%2F", "/");
                let decoded_component = component_name.replace("%2F", "/");

                match files::load_all_hop_modules(&preview_root)
                    .and_then(|modules| compile(modules, HopMode::Dev))
                {
                    Ok(program) => {
                        match create_component_preview(
                            &program,
                            &decoded_module,
                            &decoded_component,
                        ) {
                            Ok(html) => Ok(Html(html)),
                            Err(e) => Err((
                                StatusCode::NOT_FOUND,
                                Html(create_error_page(&anyhow::anyhow!(e))),
                            )),
                        }
                    }
                    Err(e) => Err((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        Html(create_error_page(&e)),
                    )),
                }
            },
        ),
    );

    // Add idiomorph.js serving endpoint
    router = router.route(
        "/idiomorph.js",
        get(async move || {
            axum::response::Response::builder()
                .header("Content-Type", "application/javascript")
                .body(Body::from(include_str!("../../idiomorph.js")))
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
    use axum_test::TestServer;
    use simple_txtar::Archive;
    use std::{env, fs};

    fn temp_dir_from_txtar(archive: &str) -> std::io::Result<std::path::PathBuf> {
        let r = rand::random::<u64>();
        let temp_dir = env::temp_dir().join(format!("hop_test_{}", r));
        fs::create_dir_all(&temp_dir)?;
        for file in Archive::from(archive).iter() {
            let file_path = temp_dir.join(&file.name);
            if let Some(parent) = file_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&file_path, &file.content)?;
        }
        Ok(temp_dir)
    }

    /// When the user calls `hop dev` and has a entry for `index.html` in the manifest file, the
    /// index.html entry should be rendered when the user issues a GET to /.
    #[tokio::test]
    async fn test_dev_and_get_index() {
        let dir = temp_dir_from_txtar(
            r#"
-- build.hop --
<render file="index.html">
  Hello from build.hop!
</render>
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();

        let body = response.text();
        assert!(body.contains("Hello from build.hop!"));
    }

    /// When the user changes the contents of the build.hop file after running `hop dev` the
    /// changes should be reflected when the user sends a request to the server.
    #[tokio::test]
    async fn test_dev_from_hop_dynamic_update() -> anyhow::Result<()> {
        let dir = temp_dir_from_txtar(
            r#"
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
"#,
        )?;
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await?;

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("message is foo"));

        fs::write(
            dir.join("build.hop"),
            r#"<import component="bar-comp" from="src/test" />

<render file="index.html">
  <bar-comp />
</render>"#,
        )?;

        let response = server.get("/").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("message is bar"));
        Ok(())
    }

    /// When the user calls `hop dev` with a servedir parameter, static files should be served
    /// from the given directory.
    #[tokio::test]
    async fn test_dev_from_hop_static_files() {
        let dir = temp_dir_from_txtar(
            r#"
-- build.hop --
<render file="index.html">
  hello world!
</render>
-- static/css/style.css --
body { background: blue; }
-- static/js/script.js --
console.log("Hello from static file");
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, Some(&dir.join("static")), None)
            .await
            .unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("hello world!"));

        let response = server.get("/css/style.css").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("body { background: blue; }"));

        let response = server.get("/js/script.js").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("console.log(\"Hello from static file\");"));
    }

    /// When the user calls `hop dev` the global HOP_MODE variable should
    /// be set to 'build'.
    #[tokio::test]
    async fn test_dev_has_hop_mode_dev() {
        let dir = temp_dir_from_txtar(
            r#"
-- build.hop --
<render file="index.html">
  mode: {HOP_MODE}
</render>
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("mode: dev"));
    }

    /// When the user calls `hop dev` and requests a path that doesn't exist in the manifest,
    /// a 404 page should be returned with available routes.
    #[tokio::test]
    async fn test_dev_404_with_helpful_message() {
        let dir = temp_dir_from_txtar(
            r#"
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
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router).unwrap();

        // Test non-existent path
        let response = server.get("/nonexistent").await;
        response.assert_status(axum::http::StatusCode::NOT_FOUND);

        let body = response.text();
        assert!(body.contains("404 Not Found"));
        assert!(body.contains("/nonexistent"));
        assert!(body.contains("Available routes"));
        assert!(body.contains("href=\"/\""));
        assert!(body.contains("href=\"/about\""));
        assert!(body.contains("href=\"/foo/bar\""));
        assert!(body.contains(">/</a>"));
        assert!(body.contains(">/about</a>"));
        assert!(body.contains(">/foo/bar</a>"));
        assert!(body.contains("build.hop"));

        // Verify hot reload script is included
        assert!(body.contains("/__hop_hot_reload"));
    }

    /// When the user calls `hop dev` and the program doesn't compile an error should be returned.
    #[tokio::test]
    async fn test_dev_compile_error() {
        let dir = temp_dir_from_txtar(
            r#"
-- build.hop --
<render file="index.html">
    <div>
"#,
        )
        .unwrap();
        let root = ProjectRoot::find_upwards(&dir).unwrap();

        let (router, _watcher) = execute(&root, None, None).await.unwrap();

        let server = TestServer::new(router).unwrap();

        // Test non-existent path
        let response = server.get("/").await;
        response.assert_status(axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    }
}
