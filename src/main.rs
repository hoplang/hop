mod common;
mod compiler;
mod error_formatter;
mod expression_parser;
mod files;
mod lsp;
mod parser;
mod runtime;
mod scriptcollector;
mod server;
mod tokenizer;
mod toposorter;
mod typechecker;
mod unifier;

use clap::{CommandFactory, Parser, Subcommand};
use compiler::Compiler;
use std::path::Path;

pub fn compile_hop_program(build_file: Option<&Path>) -> anyhow::Result<runtime::Program> {
    use compiler::Compiler;

    let mut compiler = Compiler::new();

    // Determine the base directory for finding source files
    let base_dir = if let Some(build_path) = build_file {
        build_path.parent().unwrap_or_else(|| Path::new("."))
    } else {
        Path::new(".")
    };

    // Load all hop modules from the base directory
    let modules = files::load_all_hop_modules(base_dir)?;

    for (module_name, content) in modules {
        compiler.add_module(module_name, content);
    }

    compiler
        .compile()
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))
}

#[derive(Parser)]
#[command(name = "hop")]
#[command(about = "A HTML-like templating language with built-in type checking")]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run the Language Server Protocol (LSP) server
    Lsp,
    /// Build hop templates from a manifest to files
    Build {
        /// Path to build.hop file
        #[arg(short, long, default_value = "build.hop")]
        build_file: String,
        /// Output directory
        #[arg(long)]
        outdir: String,
        /// Optional script file name to output collected scripts
        #[arg(long)]
        script_file: Option<String>,
    },
    /// Start an HTTP server for serving hop templates from a manifest
    Serve {
        /// Path to build.hop file
        #[arg(short, long, default_value = "build.hop")]
        build_file: String,
        /// Port to serve on
        #[arg(short, long, default_value = "3000")]
        port: u16,
        /// Host to bind to
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        /// Directory to serve static files from
        #[arg(long)]
        servedir: Option<String>,
        /// Optional script file name to make scripts available over HTTP
        #[arg(long)]
        script_file: Option<String>,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    fn print_header(action: &str, elapsed: u128) {
        use colored::*;
        println!();
        println!("  {} | {} in {} ms", "hop".bold(), action, elapsed);
        println!();
    }

    /// Format file size.
    fn format_file_size(bytes: usize) -> String {
        if bytes < 1024 {
            format!("{} B", bytes)
        } else if bytes < 1024 * 1024 {
            format!("{:.2} kB", bytes as f64 / 1024.0)
        } else {
            format!("{:.2} MB", bytes as f64 / (1024.0 * 1024.0))
        }
    }

    match &cli.command {
        Some(Commands::Lsp) => {
            lsp::run_lsp().await;
        }
        Some(Commands::Build {
            build_file,
            outdir,
            script_file,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();
            let mut outputs = build_from_hop(
                Path::new(build_file),
                Path::new(outdir),
                script_file.as_deref(),
            )?;
            let elapsed = start_time.elapsed();

            print_header("built", elapsed.as_millis());
            outputs.sort_by(|a, b| a.0.cmp(&b.0));
            let mut total_size = 0;
            for (file_path, size) in outputs {
                println!("  {:<50} {}", file_path, format_file_size(size));
                total_size += size;
            }
            println!();
            println!("  {:<50} {}", "total", format_file_size(total_size));
            println!();
        }
        Some(Commands::Serve {
            build_file,
            port,
            host,
            servedir,
            script_file,
        }) => {
            use colored::*;
            use std::time::Instant;
            let start_time = Instant::now();
            let (router, _watcher) = serve_from_hop(
                Path::new(build_file),
                servedir.as_deref().map(Path::new),
                script_file.as_deref(),
            )
            .await?;
            let elapsed = start_time.elapsed();
            let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;

            print_header("ready", elapsed.as_millis());
            println!("  {} http://{}:{}/", "➜".green(), host, port);
            println!();
            axum::serve(listener, router).await?;
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }

    Ok(())
}

fn build_from_hop(
    build_file: &Path,
    output_dir: &Path,
    script_file: Option<&str>,
) -> anyhow::Result<Vec<(String, usize)>> {
    use anyhow::Context;
    use std::fs;

    // Check that build file exists
    if !build_file.exists() {
        anyhow::bail!("Failed to read build file");
    }

    // Create output directory if it doesn't exist
    fs::create_dir_all(output_dir)?;

    // Use render_build_files to get all rendered content
    let rendered_files = render_build_files(build_file)?;
    let mut file_outputs = Vec::new();

    // Write each rendered file to the output directory
    for (file_path, content) in rendered_files {
        let output_path = output_dir.join(&file_path);
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        fs::write(&output_path, &content)
            .with_context(|| format!("Failed to write file {}", output_path.display()))?;

        file_outputs.push((file_path, content.len()));
    }

    // Handle script collection if requested
    if let Some(script_file_name) = script_file {
        let program = compile_hop_program(Some(build_file))?;
        let combined_script = program.get_scripts();
        let script_path = output_dir.join(script_file_name);
        fs::write(&script_path, combined_script)
            .with_context(|| format!("Failed to write script file {}", script_path.display()))?;
    }

    Ok(file_outputs)
}

fn render_build_files(
    build_file: &Path,
) -> anyhow::Result<std::collections::HashMap<String, String>> {
    use crate::common::Environment;
    use std::collections::HashMap;

    // Compile the hop program including the build file
    let program = compile_hop_program(Some(build_file))?;
    let mut rendered_files = HashMap::new();
    let mut env = Environment::new();

    for render_nodes in program.get_render_nodes().values() {
        for node in render_nodes {
            // Evaluate the children to get the rendered content
            let mut content = String::new();

            for child in &node.children {
                let rendered = program
                    .evaluate_node_entrypoint(child, &mut env, "build")
                    .map_err(|e| anyhow::anyhow!("Failed to evaluate render content: {}", e))?;
                content.push_str(&rendered);
            }

            rendered_files.insert(node.file_attr.value.clone(), content);
        }
    }

    Ok(rendered_files)
}

// Function to inject hot reload script into HTML
fn inject_hot_reload_script(html: &str) -> String {
    const HOT_RELOAD_SCRIPT: &str = r#"
<script type="module">
import morphdom from 'https://unpkg.com/morphdom@2.7.0/dist/morphdom-esm.js';

const eventSource = new EventSource('/__hop_hot_reload');
eventSource.onmessage = function(event) {
    if (event.data === 'reload') {
        fetch(window.location.href)
            .then(response => response.text())
            .then(html => {
                const parser = new DOMParser();
                const doc = parser.parseFromString(html, 'text/html');
                const newContent = doc.body;
                if (newContent) {
                    morphdom(document.body, newContent);
                }
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

// Function to build hop modules and execute a specific entrypoint

const ERROR_TEMPLATES: &str = include_str!("../hop/error_pages.hop");

fn create_error_page(error: &anyhow::Error) -> String {
    let mut compiler = Compiler::new();
    compiler.add_module("error_pages".to_string(), ERROR_TEMPLATES.to_string());

    let program = match compiler.compile() {
        Ok(program) => program,
        Err(e) => return format!("Template compilation error: {}", e),
    };

    let error_data = serde_json::json!({
        "message": format!("{:#}", error)
    });

    match program.execute_simple("error_pages", "generic-error", error_data) {
        Ok(html) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_not_found_page(path: &str, available_routes: &[String]) -> String {
    let mut compiler = Compiler::new();
    compiler.add_module("error_pages".to_string(), ERROR_TEMPLATES.to_string());

    let program = match compiler.compile() {
        Ok(program) => program,
        Err(e) => return format!("Template compilation error: {}", e),
    };

    let not_found_data = serde_json::json!({
        "path": path,
        "available_routes": available_routes
    });

    match program.execute_simple("error_pages", "not-found-error", not_found_data) {
        Ok(html) => inject_hot_reload_script(&html),
        Err(e) => format!("Error rendering template: {}", e),
    }
}

fn create_file_watcher(
    build_file: &Path,
) -> anyhow::Result<(
    notify::RecommendedWatcher,
    tokio::sync::broadcast::Sender<()>,
)> {
    use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};

    let (channel, _) = tokio::sync::broadcast::channel::<()>(100);

    let sender = channel.clone();

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

    watcher.watch(build_file, RecursiveMode::NonRecursive)?;

    Ok((watcher, channel))
}

/// Create a router that responds to requests for the output files specified in the build.hop file.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/__hop_hot_reload` route. There is also an injected
/// script on in all `html` files that listens to SSE-events on that route and performs
/// hot-reloading when an event is emitted.
///
/// If `servedir` is specified the server serves static files from the given directory as well.
///
/// The client may change the build.hop file while the server is running as the server will reread the
/// build file whenever a new request comes in.
async fn serve_from_hop(
    build_file: &Path,
    serve_dir: Option<&Path>,
    script_file: Option<&str>,
) -> anyhow::Result<(axum::Router, notify::RecommendedWatcher)> {
    use axum::http::StatusCode;
    use axum::response::sse::{Event, Sse};
    use axum::routing::get;
    use tokio_stream::StreamExt;
    use tokio_stream::wrappers::BroadcastStream;

    // Set up file watcher for hot reloading
    let mut router = axum::Router::new();

    // Add SSE endpoint for hot reload events
    let (watcher, channel) = create_file_watcher(build_file)?;
    router = router.route(
        "/__hop_hot_reload",
        get(async move || {
            Sse::new(
                BroadcastStream::new(channel.subscribe())
                    .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload"))),
            )
        }),
    );

    // Add script serving endpoint if script_file is specified
    if let Some(script_filename) = script_file {
        let script_path = format!("/{}", script_filename);
        let build_file_for_script = build_file.to_path_buf();
        router = router.route(
            &script_path,
            get(
                async move || match compile_hop_program(Some(&build_file_for_script)) {
                    Ok(program) => {
                        use axum::body::Body;
                        Ok(axum::response::Response::builder()
                            .header("Content-Type", "application/javascript")
                            .body(Body::from(program.get_scripts().to_string()))
                            .unwrap())
                    }
                    Err(e) => Err((
                        StatusCode::INTERNAL_SERVER_ERROR,
                        format!("Failed to compile hop program: {}", e),
                    )),
                },
            ),
        );
    }

    let build_file_path = build_file.to_path_buf();
    let request_handler = async move |req: axum::extract::Request| -> Result<
        axum::response::Html<String>,
        (StatusCode, axum::response::Html<String>),
    > {
        // Render all build files into a HashMap
        let rendered_files = match render_build_files(&build_file_path) {
            Ok(files) => files,
            Err(e) => {
                return Err((
                    StatusCode::INTERNAL_SERVER_ERROR,
                    axum::response::Html(create_error_page(&e)),
                ));
            }
        };

        let path = req.uri().path();

        // Convert request path to file path format
        let file_path = match path {
            "/" => "index.html",
            path if path.starts_with('/') => &path[1..],
            path => path,
        };

        // Look up the file in our rendered files
        if let Some(content) = rendered_files.get(file_path) {
            Ok(axum::response::Html(inject_hot_reload_script(content)))
        } else {
            // Try without .html extension for paths that might have it
            let html_file_path = format!("{}.html", file_path);
            if let Some(content) = rendered_files.get(&html_file_path) {
                Ok(axum::response::Html(inject_hot_reload_script(content)))
            } else {
                // Create available paths list for 404 page
                let available_paths: Vec<String> = rendered_files
                    .keys()
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
                    axum::response::Html(create_not_found_page(req.uri().path(), &available_paths)),
                ))
            }
        }
    };

    // Set up static file serving if specified
    if let Some(serve_dir) = serve_dir {
        use axum::routing::get;
        let servedir_path = serve_dir;
        if !servedir_path.is_dir() {
            anyhow::bail!("servedir '{}' is not a directory", servedir_path.display());
        }
        router = router.fallback_service(
            tower_http::services::ServeDir::new(servedir_path).fallback(get(request_handler)),
        );
    } else {
        router = router.fallback(request_handler);
    }

    Ok((router, watcher))
}

#[cfg(test)]
mod tests {
    use axum_test::TestServer;
    use simple_txtar::Archive;
    use std::{env, fs};

    use super::*;

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

    /// When the user calls `hop build` and the build file does not exist, an error should be
    /// returned stating the the build file could not be read.
    #[test]
    fn test_build_nonexistent_build_file() {
        let dir = temp_dir_from_txtar("").unwrap();
        let result = build_from_hop(&dir.join("build.hop"), &dir.join("out"), None);
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Failed to read build file")
        );
    }

    /// When the user calls `hop serve` and has a entry for `index.html` in the manifest file, the
    /// index.html entry should be rendered when the user issues a GET to /.
    #[tokio::test]
    async fn test_serve_and_get_index() {
        let dir = temp_dir_from_txtar(
            r#"
-- build.hop --
<render file="index.html">
  Hello from build.hop!
</render>
"#,
        )
        .unwrap();

        let (router, _watcher) = serve_from_hop(&dir.join("build.hop"), None, None)
            .await
            .unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();

        let body = response.text();
        assert!(body.contains("Hello from build.hop!"));
    }

    /// When the user changes the contents of the build.hop file after running `hop serve` the
    /// changes should be reflected when the user sends a request to the server.
    #[tokio::test]
    async fn test_serve_from_hop_dynamic_update() -> anyhow::Result<()> {
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

        let (router, _watcher) = serve_from_hop(&dir.join("build.hop"), None, None).await?;

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

    /// When the user calls `hop serve` with a servedir parameter, static files should be served
    /// from the given directory.
    #[tokio::test]
    async fn test_serve_from_hop_static_files() {
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

        let (router, _watcher) =
            serve_from_hop(&dir.join("build.hop"), Some(&dir.join("static")), None)
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

    /// When the user calls `hop build` with a build.hop file, the rendered content
    /// should be written to the specified output files.
    #[test]
    fn test_build_with_hop_file() {
        let dir = temp_dir_from_txtar(
            r#"
-- src/components.hop --
<hello-world>
  <h1>Build.hop Test</h1>
  <p>This content comes from a build.hop file.</p>
</hello-world>
-- build.hop --
<import component="hello-world" from="src/components" />

<render file="index.html">
  <hello-world />
</render>
"#,
        )
        .unwrap();

        let result = build_from_hop(&dir.join("build.hop"), &dir.join("out"), None);

        assert!(result.is_ok());
        let outputs = result.unwrap();
        assert_eq!(outputs.len(), 1);

        // Check that the output file was created with the correct content
        let output_path = dir.join("out").join("index.html");
        let content = std::fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("Build.hop Test"));
        assert!(content.contains("This content comes from a build.hop file"));
    }

    /// When the user calls `hop serve` and requests a path that doesn't exist in the manifest,
    /// a 404 page should be returned with available routes.
    #[tokio::test]
    async fn test_serve_404_with_helpful_message() {
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

        let (router, _watcher) = serve_from_hop(&dir.join("build.hop"), None, None)
            .await
            .unwrap();

        let server = TestServer::new(router).unwrap();

        // Test non-existent path
        let response = server.get("/nonexistent").await;
        response.assert_status(axum::http::StatusCode::NOT_FOUND);

        let body = response.text();
        assert!(body.contains("404 Not Found"));
        assert!(body.contains("/nonexistent"));
        assert!(body.contains("Available Routes"));
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
}
