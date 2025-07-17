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
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ManifestEntry {
    /// The hop module to use
    pub module: String,
    /// The function to call
    pub entrypoint: String,
    /// Optional data file to pass as parameters
    pub data: Option<String>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Manifest {
    /// Map of file paths to their configuration
    pub files: std::collections::HashMap<String, ManifestEntry>,
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
    /// Render hop templates from a manifest to files
    Render {
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
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Lsp) => {
            lsp::run_lsp().await;
        }
        Some(Commands::Render { manifest, outdir }) => {
            if let Err(e) = render_from_manifest(manifest, outdir) {
                eprintln!("Error: {:#}", e);
                std::process::exit(1);
            }
        }
        Some(Commands::Serve {
            manifest,
            port,
            host,
            servedir,
        }) => {
            if let Err(e) = serve_from_manifest(manifest, host, *port, servedir.as_deref()).await {
                eprintln!("Error: {:#}", e);
                std::process::exit(1);
            }
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }
}

fn render_from_manifest(manifest_path: &str, output_dir: &str) -> anyhow::Result<()> {
    use anyhow::Context;
    use compiler::Compiler;
    use std::fs;
    use std::path::Path;

    let manifest_content = fs::read_to_string(manifest_path)
        .with_context(|| format!("Failed to read manifest file {}", manifest_path))?;

    let manifest: Manifest = serde_json::from_str(&manifest_content)
        .with_context(|| format!("Failed to parse manifest file {}", manifest_path))?;

    let hop_dir = Path::new("./hop");
    if !hop_dir.exists() {
        anyhow::bail!("./hop directory does not exist");
    }

    let output_path = Path::new(output_dir);
    if !output_path.exists() {
        fs::create_dir_all(output_path)
            .with_context(|| format!("Failed to create output directory {}", output_dir))?;
    }

    let mut compiler = Compiler::new();
    let entries = fs::read_dir(hop_dir).context("Failed to read ./hop directory")?;

    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("hop") {
            let module_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap()
                .to_string();

            let content = fs::read_to_string(&path)
                .with_context(|| format!("Failed to read file {:?}", path))?;

            compiler.add_module(module_name, content);
        }
    }

    let program = compiler
        .compile()
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))?;

    println!("Rendering from manifest: {}", manifest_path);
    for (file_path, entry) in &manifest.files {
        let data = match &entry.data {
            Some(data_file_path) => {
                let json_str = fs::read_to_string(data_file_path)
                    .with_context(|| format!("Failed to read data file {}", data_file_path))?;
                serde_json::from_str(&json_str)
                    .with_context(|| format!("Failed to parse JSON from file {}", data_file_path))?
            }
            None => serde_json::Value::Null,
        };

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

        let output_file_path = output_path.join(file_path);
        if let Some(parent) = output_file_path.parent() {
            if !parent.exists() {
                fs::create_dir_all(parent)
                    .with_context(|| format!("Failed to create directory {:?}", parent))?;
            }
        }

        fs::write(&output_file_path, html)
            .with_context(|| format!("Failed to write to file {:?}", output_file_path))?;

        println!("Generated {} -> {:?}", file_path, output_file_path);
    }

    println!("Rendered {} files to {}", manifest.files.len(), output_dir);
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

// Function to compile hop modules and execute a specific entrypoint
fn compile_and_execute(
    module_name: &str,
    entrypoint: &str,
    data_file: Option<&str>,
) -> Result<String, String> {
    use compiler::Compiler;
    use std::fs;

    let hop_dir = std::path::Path::new("./hop");
    if !hop_dir.exists() {
        return Err("./hop directory does not exist".to_string());
    }

    // Load and compile all hop modules for this request
    let mut compiler = Compiler::new();
    match fs::read_dir(hop_dir) {
        Ok(entries) => {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|s| s.to_str()) == Some("hop") {
                    let module_name = path
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or("unknown")
                        .to_string();

                    match fs::read_to_string(&path) {
                        Ok(content) => {
                            compiler.add_module(module_name, content);
                        }
                        Err(e) => {
                            return Err(format!("Error reading file {:?}: {}", path, e));
                        }
                    }
                }
            }
        }
        Err(e) => {
            return Err(format!("Error reading ./hop directory: {}", e));
        }
    }

    let program = compiler.compile()?;

    // Load data from file if specified
    let data = match data_file {
        Some(data_file_path) => {
            let json_str = fs::read_to_string(data_file_path)
                .map_err(|e| format!("Error reading data file {}: {}", data_file_path, e))?;
            serde_json::from_str(&json_str)
                .map_err(|e| format!("Error parsing JSON from file {}: {}", data_file_path, e))?
        }
        None => serde_json::Value::Null,
    };

    // Execute the entrypoint
    program.execute(module_name, entrypoint, data)
}

async fn serve_from_manifest(
    manifest_path: &str,
    host: &str,
    port: u16,
    servedir: Option<&str>,
) -> anyhow::Result<()> {
    use anyhow::Context;
    use axum::http::StatusCode;
    use axum::response::{
        Html,
        sse::{Event, KeepAlive, Sse},
    };
    use axum::routing::get;
    use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
    use std::fs;
    use std::sync::Arc;
    use tokio::sync::broadcast;
    use tokio_stream::StreamExt;

    // Read and parse manifest
    let manifest_content = fs::read_to_string(manifest_path)
        .with_context(|| format!("Failed to read manifest file {}", manifest_path))?;

    let manifest: Manifest = serde_json::from_str(&manifest_content)
        .with_context(|| format!("Failed to parse manifest file {}", manifest_path))?;

    let hop_dir = std::path::Path::new("./hop");
    if !hop_dir.exists() {
        anyhow::bail!("./hop directory does not exist");
    }

    // Note: Compilation now happens on each request for hot reloading

    // Set up broadcast channel for hot reload events
    let (reload_tx, _) = broadcast::channel::<()>(100);
    let reload_tx = Arc::new(reload_tx);

    // Collect all JSON data files referenced in manifest
    let mut json_files = std::collections::HashSet::new();
    for entry in manifest.files.values() {
        if let Some(data_file) = &entry.data {
            json_files.insert(data_file.clone());
        }
    }

    // Set up file watcher for hot reloading
    let watcher_tx = reload_tx.clone();
    let hop_dir_path = hop_dir.to_path_buf();
    tokio::spawn(async move {
        let (tx, mut rx) = tokio::sync::mpsc::channel(100);

        let mut watcher = RecommendedWatcher::new(
            move |res: Result<notify::Event, notify::Error>| {
                if let Ok(event) = res {
                    let should_reload = (event.kind.is_modify() || event.kind.is_create())
                        && event.paths.iter().any(|p| {
                            matches!(p.extension().and_then(|s| s.to_str()), Some("hop" | "json"))
                        });

                    if should_reload {
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

        // Watch each JSON data file individually
        for json_file in &json_files {
            let json_path = std::path::Path::new(json_file);
            if json_path.exists() {
                watcher
                    .watch(json_path, RecursiveMode::NonRecursive)
                    .unwrap_or_else(|_| panic!("Failed to watch JSON file: {}", json_file));
            }
        }

        while (rx.recv().await).is_some() {
            // Debounce rapid file changes
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;

            // Send reload event to all connected clients
            let _ = watcher_tx.send(());
        }
    });

    // Create router
    let mut router = axum::Router::new();
    let manifest_files = manifest.files.clone();

    // Add SSE endpoint for hot reload events
    let sse_reload_tx = reload_tx.clone();
    router = router.route(
        "/__hop_hot_reload",
        get({
            let tx = sse_reload_tx.clone();
            move || {
                let tx = tx.clone();
                async move {
                    let rx = tx.subscribe();
                    let stream = tokio_stream::wrappers::BroadcastStream::new(rx)
                        .map(|_| Ok::<Event, axum::Error>(Event::default().data("reload")));

                    Sse::new(stream).keep_alive(
                        KeepAlive::new()
                            .interval(std::time::Duration::from_secs(30))
                            .text("keep-alive"),
                    )
                }
            }
        }),
    );

    // Add manifest routes first (these take precedence)
    for (file_path, entry) in manifest.files {
        let route_path = match file_path.as_str() {
            "index.html" => "/".to_string(),
            path if path.ends_with(".html") => format!("/{}", path.strip_suffix(".html").unwrap()),
            path => format!("/{}", path),
        };

        let module = entry.module.clone();
        let entrypoint = entry.entrypoint.clone();
        let data_file = entry.data.clone();

        router = router.route(
            &route_path,
            get(move || {
                let module = module.clone();
                let entrypoint = entrypoint.clone();
                let data_file = data_file.clone();

                async move {
                    match compile_and_execute(&module, &entrypoint, data_file.as_deref()) {
                        Ok(html) => {
                            let html_with_hot_reload = inject_hot_reload_script(&html);
                            Ok(Html(html_with_hot_reload))
                        }
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            Err(StatusCode::INTERNAL_SERVER_ERROR)
                        }
                    }
                }
            }),
        );
    }

    // Add static file serving as fallback if servedir is provided
    if let Some(servedir_path) = servedir {
        let servedir = std::path::Path::new(servedir_path);
        if !servedir.exists() {
            anyhow::bail!("serve directory '{}' does not exist", servedir_path);
        }
        if !servedir.is_dir() {
            anyhow::bail!("serve path '{}' is not a directory", servedir_path);
        }
        router = router.fallback_service(tower_http::services::ServeDir::new(servedir_path));
    }

    let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port))
        .await
        .with_context(|| format!("Failed to bind to {}:{}", host, port))?;

    println!("Hop server running on http://{}:{}", host, port);
    println!("Serving from manifest: {}", manifest_path);
    if let Some(static_dir) = servedir {
        println!("Static files from: {}", static_dir);
    }
    println!("Available routes:");
    for (file_path, entry) in &manifest_files {
        let route_path = match file_path.as_str() {
            "index.html" => "/".to_string(),
            path if path.ends_with(".html") => format!("/{}", path.strip_suffix(".html").unwrap()),
            path => format!("/{}", path),
        };
        println!("  {} -> {}::{}", route_path, entry.module, entry.entrypoint);
    }

    axum::serve(listener, router)
        .await
        .context("Server error")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// When the user calls `hop render` and the manifest file does not exist, an error should be
    /// returned.
    #[test]
    fn test_render_from_manifest_nonexistent_manifest() {
        let result = render_from_manifest("/tmp/non-existent-manifest.json", "/tmp/output");
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Failed to read manifest file")
        );
    }
}
