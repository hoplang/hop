mod common;
mod compiler;
mod formatter;
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
use common::escape_html;
use serde::{Deserialize, Serialize};
use std::path::Path;

pub fn compile_hop_program(hop_dir: &Path) -> anyhow::Result<runtime::Program> {
    use anyhow::Context;
    use compiler::Compiler;
    use std::fs;
    anyhow::ensure!(hop_dir.exists(), "hop directory does not exist");

    let mut compiler = Compiler::new();
    for entry in fs::read_dir(hop_dir).context("Failed to read hop directory")? {
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

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ManifestEntry {
    /// The output file path
    pub path: String,
    /// The hop module to use
    pub module: String,
    /// The function to call
    pub entrypoint: String,
    /// Optional data file to pass as parameters
    pub data: Option<String>,
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
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Lsp) => {
            lsp::run_lsp().await;
        }
        Some(Commands::Build { manifest, outdir }) => {
            build_from_manifest(manifest, outdir)?;
        }
        Some(Commands::Serve {
            manifest,
            port,
            host,
            servedir,
            hopdir,
        }) => {
            let router =
                serve_from_manifest(manifest, host, *port, servedir.as_deref(), hopdir).await?;
            let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;
            axum::serve(listener, router).await?;
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }

    Ok(())
}

fn build_from_manifest(manifest_path: &str, output_dir_str: &str) -> anyhow::Result<()> {
    use anyhow::Context;
    use colored::*;
    use std::fs;
    use std::path::Path;
    use std::time::Instant;

    let start_time = Instant::now();

    let manifest_content = fs::read_to_string(manifest_path)
        .with_context(|| format!("Failed to read manifest file {}", manifest_path))?;

    let manifest: Manifest = serde_json::from_str(&manifest_content)
        .with_context(|| format!("Failed to parse manifest file {}", manifest_path))?;

    let program = compile_hop_program(Path::new("./hop"))?;

    let mut total_size = 0;
    let mut file_outputs = Vec::new();

    for entry in &manifest.files {
        // Parse the json data used to render the output file
        let data = match &entry.data {
            Some(data_file_path) => {
                // Resolve data file path relative to manifest file
                let manifest_dir = Path::new(manifest_path).parent().unwrap_or(Path::new("."));
                let data_path = manifest_dir.join(data_file_path);
                let json_str = fs::read_to_string(&data_path)
                    .with_context(|| format!("Failed to read data file {}", data_path.display()))?;
                serde_json::from_str(&json_str).with_context(|| {
                    format!("Failed to parse JSON from file {}", data_path.display())
                })?
            }
            None => serde_json::Value::Null,
        };

        // Render output file
        let html = program
            .execute(&entry.module, &entry.entrypoint, data)
            .map_err(|e| {
                anyhow::anyhow!(
                    "Failed to execute {}::{}: {}",
                    entry.module,
                    entry.entrypoint,
                    e
                )
            })?;

        // Create parent directory for output file
        let output_file_path = Path::new(output_dir_str).join(&entry.path);
        if let Some(parent) = output_file_path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("Failed to create directory {:?}", parent))?;
        }

        // Write output file
        fs::write(&output_file_path, &html)
            .with_context(|| format!("Failed to write to file {:?}", output_file_path))?;

        total_size += html.len();
        file_outputs.push((output_file_path.display().to_string(), html.len()));
    }

    let elapsed = start_time.elapsed();

    println!();
    println!("  {} | built in {} ms", "hop".bold(), elapsed.as_millis());
    println!();
    file_outputs.sort_by(|a, b| a.0.cmp(&b.0));
    for (file_path, size) in file_outputs {
        println!("  {:<50} {}", file_path, format_file_size(size));
    }
    println!();
    println!("  {:<50} {}", "total", format_file_size(total_size));
    println!();
    Ok(())
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
                // Parse the HTML document properly
                const parser = new DOMParser();
                const doc = parser.parseFromString(html, 'text/html');
                
                // Extract the body content
                const newContent = doc.body;
                
                // Use morphdom to update the current page
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
    // Attempt to reconnect after a delay
    setTimeout(() => {
        eventSource.close();
        location.reload();
    }, 1000);
};
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
    data_file: Option<&str>,
    hopdir: &str,
    manifest_path: &str,
) -> anyhow::Result<String> {
    use anyhow::Context;
    use std::fs;
    use std::path::Path;

    // Load and compile all hop modules for this request
    let program = compile_hop_program(Path::new(hopdir))?;

    // Load data from file if specified
    let data = match data_file {
        Some(data_file_path) => {
            // Resolve data file path relative to manifest file
            let manifest_dir = Path::new(manifest_path).parent().unwrap_or(Path::new("."));
            let data_path = manifest_dir.join(data_file_path);
            let json_str = fs::read_to_string(&data_path)
                .with_context(|| format!("Failed to read data file {}", data_path.display()))?;
            serde_json::from_str(&json_str).with_context(|| {
                format!("Failed to parse JSON from file {}", data_path.display())
            })?
        }
        None => serde_json::Value::Null,
    };

    // Execute the entrypoint
    program
        .execute(module_name, entrypoint, data)
        .map_err(|e| anyhow::anyhow!("Failed to execute {}::{}: {}", module_name, entrypoint, e))
}

async fn serve_from_manifest(
    manifest_path: &str,
    host: &str,
    port: u16,
    servedir: Option<&str>,
    hopdir: &str,
) -> anyhow::Result<axum::Router> {
    use axum::http::StatusCode;
    use axum::response::sse::{Event, Sse};
    use axum::routing::get;
    use colored::*;
    use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
    use std::fs;
    use std::sync::Arc;
    use std::time::Instant;
    use tokio::sync::broadcast;
    use tokio_stream::StreamExt;
    use tokio_stream::wrappers::BroadcastStream;

    let start_time = Instant::now();

    // Set up broadcast channel for hot reload events
    let (reload_tx, _) = broadcast::channel::<()>(100);
    let reload_tx = Arc::new(reload_tx);

    // Set up file watcher for hot reloading
    let watcher_tx = reload_tx.clone();
    let hop_dir_path = std::path::Path::new(hopdir).to_path_buf();
    let mp = manifest_path.to_string();
    tokio::spawn(async move {
        let (tx, mut rx) = tokio::sync::mpsc::channel(100);

        let mut watcher = RecommendedWatcher::new(
            move |res: Result<notify::Event, notify::Error>| {
                if let Ok(event) = res {
                    if event.kind.is_modify() || event.kind.is_create() {
                        let _ = tx.try_send(());
                    }
                }
            },
            Config::default(),
        )
        .expect("Failed to create file watcher");

        // Watch hop directory for .hop files
        watcher
            .watch(&hop_dir_path, RecursiveMode::Recursive)
            .expect("Failed to watch hop directory");

        // Read and parse manifest
        let manifest_content = fs::read_to_string(mp).unwrap();
        let manifest: Manifest = serde_json::from_str(&manifest_content).unwrap();
        let mut json_files = std::collections::HashSet::new();
        for entry in &manifest.files {
            if let Some(data_file) = &entry.data {
                json_files.insert(data_file.clone());
            }
        }
        for json_file in &json_files {
            let json_path = std::path::Path::new(json_file);
            if json_path.exists() {
                watcher
                    .watch(json_path, RecursiveMode::NonRecursive)
                    .unwrap_or_else(|_| panic!("Failed to watch JSON file: {}", json_file));
            }
        }

        while (rx.recv().await).is_some() {
            // Send reload event to all connected clients
            let _ = watcher_tx.send(());
        }
    });

    let mut router = axum::Router::new();

    // Add SSE endpoint for hot reload events
    router = router.route(
        "/__hop_hot_reload",
        get(async move || {
            Sse::new(
                BroadcastStream::new(reload_tx.subscribe())
                    .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload"))),
            )
        }),
    );

    let mp = manifest_path.to_string();
    let hopdir_for_handler = hopdir.to_string();
    let request_handler = {
        let hopdir_clone = hopdir_for_handler.clone();
        let mp_clone = mp.clone();
        async move |req: axum::extract::Request| {
            // Read and parse manifest
            let manifest_content = fs::read_to_string(&mp_clone).unwrap();
            let manifest: Manifest = serde_json::from_str(&manifest_content).unwrap();
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
                    entry.data.as_deref(),
                    &hopdir_clone,
                    &mp_clone,
                ) {
                    Ok(html) => {
                        let html_with_hot_reload = inject_hot_reload_script(&html);
                        Ok(axum::response::Html(html_with_hot_reload))
                    }
                    Err(e) => Ok(axum::response::Html(format!(
                        r#"<!DOCTYPE html>
<html>
<head>
    <title>Error</title>
</head>
<body style="background: black; color: white; max-width: 1200px; padding: 32px;">
    <div>
        <div>Error</div>
        <div>{}</div>
    </div>
</body>
</html>"#,
                        escape_html(format!("{:#}", e).as_str()),
                    ))),
                }
            } else {
                Err(StatusCode::NOT_FOUND)
            }
        }
    };

    // Add static file serving if servedir is provided
    if let Some(servedir_path) = servedir {
        let servedir = std::path::Path::new(servedir_path);
        if !servedir.exists() {
            anyhow::bail!("serve directory '{}' does not exist", servedir_path);
        }
        if !servedir.is_dir() {
            anyhow::bail!("serve path '{}' is not a directory", servedir_path);
        }
        router = router.fallback_service(
            tower_http::services::ServeDir::new(servedir_path).fallback(get(request_handler)),
        );
    } else {
        router = router.fallback(request_handler);
    }

    let elapsed = start_time.elapsed();

    println!();
    println!("  {} | ready in {} ms", "hop".bold(), elapsed.as_millis());
    println!();
    println!("  {} http://{}:{}/", "➜".green(), host, port);
    println!();

    Ok(router)
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
    /// returned.
    #[test]
    fn test_build_nonexistent_manifest() {
        let dir = temp_dir_from_txtar(
            r#"
-- hop/test.hop --
<component name="hello" params-as="p">
  <p inner-text="p.name"></p>
</component>
-- data.json --
{"name": "foo bar"}
"#,
        )
        .unwrap();
        let result = build_from_manifest(
            dir.join("manifest.json").to_str().unwrap(),
            dir.join("out").to_str().unwrap(),
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
<component name="hello" params-as="p">
  <p inner-text="p.name"></p>
</component>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "hello",
      "data": "data.json"
    }
  ]
}
-- data.json --
{"name": "foo bar"}
"#,
        )
        .unwrap();

        let router = serve_from_manifest(
            dir.join("manifest.json").to_str().unwrap(),
            "127.0.0.1",
            3000,
            None,
            dir.join("hop").to_str().unwrap(),
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
<component name="foo">
  message is foo
</component>
<component name="bar">
  message is bar
</component>
-- manifest.json --
{
  "files": [
    {
      "path": "index.html",
      "module": "test",
      "entrypoint": "foo"
    }
  ]
}
"#,
        )?;

        let router = serve_from_manifest(
            dir.join("manifest.json").to_str().unwrap(),
            "127.0.0.1",
            3000,
            None,
            dir.join("hop").to_str().unwrap(),
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
      "entrypoint": "bar"
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
}
