mod common;
mod compiler;
mod error_formatter;
mod expression_parser;
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
use serde::{Deserialize, Serialize};
use std::path::Path;

pub fn compile_hop_program(hop_dir: &Path) -> anyhow::Result<runtime::Program> {
    use anyhow::Context;
    use compiler::Compiler;
    use std::fs;

    let dir = fs::read_dir(hop_dir).context("Failed to read hop directory")?;

    let mut compiler = Compiler::new();
    for entry in dir {
        let path = entry.context("Failed to read directory entry")?.path();
        if path.extension().and_then(|s| s.to_str()) == Some("hop") {
            let module_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .context("Invalid file name")?
                .to_string();
            let content = fs::read_to_string(&path)
                .with_context(|| format!("Failed to read file {:?}", path))?;
            compiler.add_module(module_name, content);
        }
    }
    compiler
        .compile()
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum DataSource {
    /// Path to a JSON file
    File(String),
    /// Inline JSON object
    Inline(serde_json::Value),
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ManifestEntry {
    /// The output file path
    pub path: String,
    /// The hop module to use
    pub module: String,
    /// The function to call
    pub entrypoint: String,
    /// Optional data file path or inline JSON object to pass as parameters
    pub data: Option<DataSource>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Manifest {
    /// Array of files to build
    pub files: Vec<ManifestEntry>,
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
        /// Path to manifest.json file
        #[arg(short, long, default_value = "manifest.json")]
        manifest: String,
        /// Output directory
        #[arg(long)]
        outdir: String,
        /// Directory containing hop files
        #[arg(long, default_value = "./hop")]
        hopdir: String,
        /// Directory containing data files
        #[arg(long, default_value = "./data")]
        datadir: String,
    },
    /// Start an HTTP server for serving hop templates from a manifest
    Serve {
        /// Path to manifest.json file
        #[arg(short, long, default_value = "manifest.json")]
        manifest: String,
        /// Port to serve on
        #[arg(short, long, default_value = "3000")]
        port: u16,
        /// Host to bind to
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        /// Directory to serve static files from
        #[arg(long)]
        servedir: Option<String>,
        /// Directory containing hop files
        #[arg(long, default_value = "./hop")]
        hopdir: String,
        /// Directory containing data files
        #[arg(long, default_value = "./data")]
        datadir: String,
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
            manifest,
            outdir,
            hopdir,
            datadir,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();
            let mut outputs = build_from_manifest(
                Path::new(manifest),
                Path::new(outdir),
                Path::new(hopdir),
                Path::new(datadir),
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
            manifest,
            port,
            host,
            servedir,
            hopdir,
            datadir,
        }) => {
            use colored::*;
            use std::time::Instant;
            let start_time = Instant::now();
            let (router, _watcher) = serve_from_manifest(
                Path::new(manifest),
                servedir.as_deref().map(Path::new),
                Path::new(hopdir),
                Path::new(datadir),
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

fn build_from_manifest(
    manifest_file: &Path,
    output_dir: &Path,
    hop_dir: &Path,
    data_dir: &Path,
) -> anyhow::Result<Vec<(String, usize)>> {
    use anyhow::Context;
    use std::fs;

    let manifest = load_manifest(manifest_file)?;

    let program = compile_hop_program(hop_dir)?;

    let mut file_outputs = Vec::new();

    for entry in &manifest.files {
        // Parse the json data used to render the output file
        let data = match &entry.data {
            Some(DataSource::File(data_file_path)) => {
                // Resolve data file path relative to data directory
                let data_path = data_dir.join(data_file_path);
                let json_str = fs::read_to_string(&data_path)
                    .with_context(|| format!("Failed to read data file {}", data_path.display()))?;
                let data = serde_json::from_str(&json_str).with_context(|| {
                    format!("Failed to parse JSON from file {}", data_path.display())
                })?;
                program
                    .validate(&entry.module, &entry.entrypoint, &data)
                    .with_context(|| format!("Validation error in {:?}", data_path))?;
                data
            }
            Some(DataSource::Inline(data)) => {
                program
                    .validate(&entry.module, &entry.entrypoint, data)
                    .with_context(|| "Validation error with inline data")?;
                data.clone()
            }
            None => serde_json::Value::Null,
        };

        // Render output file
        let html = program
            .execute_simple(&entry.module, &entry.entrypoint, data)
            .map_err(|e| {
                anyhow::anyhow!(
                    "Failed to execute {}::{}: {}",
                    entry.module,
                    entry.entrypoint,
                    e
                )
            })?;

        // Create parent directory for output file
        let output_file_path = output_dir.join(&entry.path);
        if let Some(parent) = output_file_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory {:?}", parent))?;
        }

        // Write output file
        fs::write(&output_file_path, &html)
            .with_context(|| format!("Failed to write to file {:?}", output_file_path))?;

        file_outputs.push((output_file_path.display().to_string(), html.len()));
    }

    Ok(file_outputs)
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
fn build_and_execute(
    module_name: &str,
    entrypoint: &str,
    data_source: Option<&DataSource>,
    hop_dir: &Path,
    data_dir: &Path,
) -> anyhow::Result<String> {
    use anyhow::Context;
    use std::fs;

    // Load and compile all hop modules for this request
    let program = compile_hop_program(hop_dir)?;

    // Load data from file or use inline data if specified
    let data = match data_source {
        Some(DataSource::File(data_file_path)) => {
            // Resolve data file path relative to data directory
            let data_path = data_dir.join(data_file_path);
            let json_str = fs::read_to_string(&data_path)
                .with_context(|| format!("Failed to read data file {}", data_path.display()))?;
            serde_json::from_str(&json_str).with_context(|| {
                format!("Failed to parse JSON from file {}", data_path.display())
            })?
        }
        Some(DataSource::Inline(data)) => data.clone(),
        None => serde_json::Value::Null,
    };

    program.validate(module_name, entrypoint, &data)?;

    // Execute the entrypoint
    program
        .execute_simple(module_name, entrypoint, data)
        .map_err(|e| anyhow::anyhow!("Failed to execute {}::{}: {}", module_name, entrypoint, e))
}

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

fn load_manifest(manifest_path: &Path) -> anyhow::Result<Manifest> {
    use anyhow::Context;
    use std::fs;

    let manifest_content = fs::read_to_string(manifest_path)
        .with_context(|| format!("Failed to read manifest file {}", manifest_path.display()))?;
    let manifest = serde_json::from_str::<Manifest>(&manifest_content)
        .with_context(|| format!("Failed to parse manifest file {}", manifest_path.display()))?;
    Ok(manifest)
}

fn create_file_watcher(
    hop_dir: &Path,
    data_dir: &Path,
    manifest_file: &Path,
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

    watcher.watch(hop_dir, RecursiveMode::Recursive)?;

    if data_dir.exists() {
        watcher.watch(data_dir, RecursiveMode::Recursive)?;
    }

    watcher.watch(manifest_file, RecursiveMode::NonRecursive)?;

    Ok((watcher, channel))
}

/// Create a router that responds to requests for the output files specified in the manifest.
///
/// Also sets up a watcher that watches all source files used to construct the output files.
/// The watcher emits SSE-events on the `/__hop_hot_reload` route. There is also an injected
/// script on in all `html` files that listens to SSE-events on that route and performs
/// hot-reloading when an event is emitted.
///
/// If `servedir` is specified the server serves static files from the given directory as well.
///
/// The client may change the manifest while the server is running as the server will reread the
/// manifest whenever a new request comes in.
async fn serve_from_manifest(
    manifest_file: &Path,
    serve_dir: Option<&Path>,
    hop_dir: &Path,
    data_dir: &Path,
) -> anyhow::Result<(axum::Router, notify::RecommendedWatcher)> {
    use axum::http::StatusCode;
    use axum::response::sse::{Event, Sse};
    use axum::routing::get;
    use tokio_stream::StreamExt;
    use tokio_stream::wrappers::BroadcastStream;

    // Set up file watcher for hot reloading
    let mut router = axum::Router::new();

    // Add SSE endpoint for hot reload events
    let (watcher, channel) = create_file_watcher(hop_dir, data_dir, manifest_file)?;
    router = router.route(
        "/__hop_hot_reload",
        get(async move || {
            Sse::new(
                BroadcastStream::new(channel.subscribe())
                    .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload"))),
            )
        }),
    );

    let mp = manifest_file.to_path_buf();
    let hopdir_for_handler = hop_dir.to_path_buf();
    let datadir_for_handler = data_dir.to_path_buf();
    let request_handler = async move |req: axum::extract::Request| {
        // Read and parse manifest
        let manifest = match load_manifest(&mp) {
            Ok(manifest) => manifest,
            Err(e) => {
                return Ok(axum::response::Html(create_error_page(&e)));
            }
        };
        let path = req.uri().path();

        // Convert request path to file path format
        let file_path = match path {
            "/" => "index.html",
            path if path.starts_with('/') => &path[1..],
            path => path,
        };

        // Find matching manifest entry
        let entry = manifest.files.iter().find(|entry| {
            entry.path == file_path
                || (entry.path.ends_with(".html")
                    && entry.path.strip_suffix(".html").unwrap() == file_path)
        });

        if let Some(entry) = entry {
            match build_and_execute(
                &entry.module,
                &entry.entrypoint,
                entry.data.as_ref(),
                &hopdir_for_handler,
                &datadir_for_handler,
            ) {
                Ok(html) => Ok(axum::response::Html(inject_hot_reload_script(&html))),
                Err(e) => Ok(axum::response::Html(create_error_page(&e))),
            }
        } else {
            // Collect available paths from manifest for helpful 404 page
            let available_paths: Vec<String> = manifest
                .files
                .iter()
                .map(|f| {
                    if f.path == "index.html" {
                        "/".to_string()
                    } else if f.path.ends_with(".html") {
                        format!("/{}", f.path.strip_suffix(".html").unwrap())
                    } else {
                        format!("/{}", f.path)
                    }
                })
                .collect();

            Err((
                StatusCode::NOT_FOUND,
                axum::response::Html(create_not_found_page(path, &available_paths)),
            ))
        }
    };

    // Add static file serving if servedir is provided
    if let Some(servedir_path) = serve_dir {
        if !servedir_path.exists() {
            anyhow::bail!("servedir '{}' does not exist", servedir_path.display());
        }
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

    /// When the user calls `hop build` and the manifest file does not exist, an error should be
    /// returned stating the the manifest file could not be read.
    #[test]
    fn test_build_nonexistent_manifest() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<hello-world>
  <p set-inner-text="p.name"></p>
</hello-world>
-- data/data.json --
{"name": "foo bar"}
"#,
        )
        .unwrap();
        let result = build_from_manifest(
            &dir.join("manifest.json"),
            &dir.join("out"),
            &dir.join("hop"),
            &dir.join("data"),
        );
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Failed to read manifest file")
        );
    }

    /// When the user calls `hop serve` and has a entry for `index.html` in the manifest file, the
    /// index.html entry should be rendered when the user issues a GET to /.
    #[tokio::test]
    async fn test_serve_from_manifest() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<hello-world params-as="p">
  <p set-inner-text="p.name"></p>
</hello-world>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "hello-world",
      "data": "data.json"
    }
  ]
}
-- data/data.json --
{"name": "foo bar"}
"#,
        )
        .unwrap();

        let (router, _watcher) = serve_from_manifest(
            &dir.join("manifest.json"),
            None,
            &dir.join("hop"),
            &dir.join("data"),
        )
        .await
        .unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();

        let body = response.text();
        assert!(body.contains("foo bar"));
    }

    /// When the user changes the contents of the manifest file after running `hop serve` the
    /// changes should be reflected when the user sends a request to the server.
    #[tokio::test]
    async fn test_serve_from_manifest_dynamic_update() -> anyhow::Result<()> {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<foo-comp>
  message is foo
</foo-comp>
<bar-comp>
  message is bar
</bar-comp>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "foo-comp"
    }
  ]
}
"#,
        )?;

        let (router, _watcher) = serve_from_manifest(
            &dir.join("manifest.json"),
            None,
            &dir.join("hop"),
            &dir.join("data"),
        )
        .await?;

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("message is foo"));
        fs::write(
            dir.join("manifest.json"),
            r#"{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "bar-comp"
    }
  ]
}"#,
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
    async fn test_serve_from_manifest_static_files() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<hello-world>
  hello world!
</hello-world>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "hello-world"
    }
  ]
}
-- static/style.css --
body { background: blue; }
-- static/script.js --
console.log("Hello from static file");
"#,
        )
        .unwrap();

        let (router, _watcher) = serve_from_manifest(
            &dir.join("manifest.json"),
            Some(&dir.join("static")),
            &dir.join("hop"),
            &dir.join("data"),
        )
        .await
        .unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("hello world!"));

        let response = server.get("/style.css").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("body { background: blue; }"));

        let response = server.get("/script.js").await;
        response.assert_status_ok();
        let body = response.text();
        assert!(body.contains("console.log(\"Hello from static file\");"));
    }

    /// When the user specifies inline data in the manifest, it should be used directly without
    /// reading from a file.
    #[tokio::test]
    async fn test_serve_with_inline_data() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<hello-world params-as="p">
  <p set-inner-text="p.name"></p>
  <p set-inner-text="p.message"></p>
</hello-world>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "hello-world",
      "data": {
        "name": "inline user",
        "message": "This data was specified inline!"
      }
    }
  ]
}
"#,
        )
        .unwrap();

        let (router, _watcher) = serve_from_manifest(
            &dir.join("manifest.json"),
            None,
            &dir.join("hop"),
            &dir.join("data"),
        )
        .await
        .unwrap();

        let server = TestServer::new(router).unwrap();

        let response = server.get("/").await;
        response.assert_status_ok();

        let body = response.text();
        assert!(body.contains("inline user"));
        assert!(body.contains("This data was specified inline!"));
    }

    /// When the user calls `hop build` with inline data in the manifest, the inline data
    /// should be used to render the output files.
    #[test]
    fn test_build_with_inline_data() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<hello-world params-as="p">
  <h1 set-inner-text="p.title"></h1>
  <p set-inner-text="p.description"></p>
</hello-world>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "hello-world",
      "data": {
        "title": "Inline Data Test",
        "description": "This content comes from inline JSON data in the manifest."
      }
    }
  ]
}
"#,
        )
        .unwrap();

        let result = build_from_manifest(
            &dir.join("manifest.json"),
            &dir.join("out"),
            &dir.join("hop"),
            &dir.join("data"),
        );

        assert!(result.is_ok());
        let outputs = result.unwrap();
        assert_eq!(outputs.len(), 1);

        // Check that the output file was created with the correct content
        let output_path = dir.join("out").join("index.html");
        let content = std::fs::read_to_string(&output_path).unwrap();
        assert!(content.contains("Inline Data Test"));
        assert!(content.contains("This content comes from inline JSON data"));
    }

    /// When the user calls `hop serve` and requests a path that doesn't exist in the manifest,
    /// a 404 page should be returned with available routes.
    #[tokio::test]
    async fn test_serve_404_with_helpful_message() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<hello-world>
  hello world!
</hello-world>
<about-page>
  about page
</about-page>
<nested-page>
  nested page content
</nested-page>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "hello-world"
    },
    {
      "path": "about.html",
      "module": "test",
      "entrypoint": "about-page"
    },
    {
      "path": "foo/bar.html",
      "module": "test",
      "entrypoint": "nested-page"
    }
  ]
}
"#,
        )
        .unwrap();

        let (router, _watcher) = serve_from_manifest(
            &dir.join("manifest.json"),
            None,
            &dir.join("hop"),
            &dir.join("data"),
        )
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
        assert!(body.contains("manifest.json"));

        // Verify hot reload script is included
        assert!(body.contains("/__hop_hot_reload"));
    }
}
