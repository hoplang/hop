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

#[derive(Debug, Deserialize, Serialize)]
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
        }) => {
            serve_from_manifest(manifest, host, *port, servedir.as_deref()).await?;
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
    use compiler::Compiler;
    use std::fs;
    use std::path::Path;
    use std::time::Instant;

    let start_time = Instant::now();

    let hop_dir = Path::new("./hop");
    anyhow::ensure!(hop_dir.exists(), "hop directory does not exist");

    let manifest_content = fs::read_to_string(manifest_path)
        .with_context(|| format!("Failed to read manifest file {}", manifest_path))?;

    let manifest: Manifest = serde_json::from_str(&manifest_content)
        .with_context(|| format!("Failed to parse manifest file {}", manifest_path))?;

    // Compile hop program
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

            compiler.add_module(module_name, content)
        }
    }
    let program = compiler
        .compile()
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))?;

    let mut total_size = 0;
    let mut file_outputs = Vec::new();

    for entry in &manifest.files {
        // Parse the json data used to render the output file
        let data = match &entry.data {
            Some(data_file_path) => {
                let json_str = fs::read_to_string(data_file_path)
                    .with_context(|| format!("Failed to read data file {}", data_file_path))?;
                serde_json::from_str(&json_str)
                    .with_context(|| format!("Failed to parse JSON from file {}", data_file_path))?
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
) -> anyhow::Result<String> {
    use anyhow::Context;
    use compiler::Compiler;
    use std::fs;

    let hop_dir = std::path::Path::new("./hop");
    anyhow::ensure!(hop_dir.exists(), "./hop directory does not exist");

    // Load and compile all hop modules for this request
    let mut compiler = Compiler::new();
    for entry in fs::read_dir(hop_dir).context("Failed to read ./hop directory")? {
        let entry = entry.context("Failed to read directory entry")?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("hop") {
            let module_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .context("Invalid hop file name")?
                .to_string();

            let content = fs::read_to_string(&path)
                .with_context(|| format!("Failed to read file {:?}", path))?;

            compiler.add_module(module_name, content);
        }
    }

    let program = compiler
        .compile()
        .map_err(|e| anyhow::anyhow!("Compilation failed: {}", e))?;

    // Load data from file if specified
    let data = match data_file {
        Some(data_file_path) => {
            let json_str = fs::read_to_string(data_file_path)
                .with_context(|| format!("Failed to read data file {}", data_file_path))?;
            serde_json::from_str(&json_str)
                .with_context(|| format!("Failed to parse JSON from file {}", data_file_path))?
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
) -> anyhow::Result<()> {
    use anyhow::Context;
    use axum::http::StatusCode;
    use axum::response::{
        Html,
        sse::{Event, Sse},
    };
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

    // Read and parse manifest
    let manifest_content = fs::read_to_string(manifest_path)
        .with_context(|| format!("Failed to read manifest file {}", manifest_path))?;

    let manifest: Manifest = serde_json::from_str(&manifest_content)
        .with_context(|| format!("Failed to parse manifest file {}", manifest_path))?;

    let hop_dir = std::path::Path::new("./hop");
    if !hop_dir.exists() {
        anyhow::bail!("./hop directory does not exist");
    }

    // Set up broadcast channel for hot reload events
    let (reload_tx, _) = broadcast::channel::<()>(100);
    let reload_tx = Arc::new(reload_tx);

    // Collect all JSON data files referenced in manifest
    let mut json_files = std::collections::HashSet::new();
    for entry in &manifest.files {
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

    // Add manifest routes first (these take precedence)
    for entry in manifest.files {
        let route_path = match entry.path.as_str() {
            "index.html" => "/".to_string(),
            path if path.ends_with(".html") => format!("/{}", path.strip_suffix(".html").unwrap()),
            path => format!("/{}", path),
        };

        router = router.route(
            &route_path,
            get(move || async move {
                match build_and_execute(&entry.module, &entry.entrypoint, entry.data.as_deref()) {
                    Ok(html) => {
                        let html_with_hot_reload = inject_hot_reload_script(&html);
                        Ok(Html(html_with_hot_reload))
                    }
                    Err(e) => {
                        eprintln!("Error: {:#}", e);
                        Err(StatusCode::INTERNAL_SERVER_ERROR)
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

    let elapsed = start_time.elapsed();

    println!();
    println!("  {} | ready in {} ms", "hop".bold(), elapsed.as_millis());
    println!();
    println!("  {} http://{}:{}/", "➜".green(), host, port);
    println!();

    axum::serve(listener, router)
        .await
        .context("Server error")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{env, fs};

    use super::*;

    /// When the user calls `hop build` and the manifest file does not exist, an error should be
    /// returned.
    #[test]
    fn test_build_nonexistent_manifest() {
        let dir = env::temp_dir();
        let manifest_json = dir.join("non-existent-manifest.json");
        let output_dir = dir.join("output");
        let hop_dir = dir.join("hop");
        assert!(env::set_current_dir(&dir).is_ok());
        assert!(fs::create_dir_all(&hop_dir).is_ok());
        let result = build_from_manifest(
            manifest_json.to_str().unwrap(),
            output_dir.to_str().unwrap(),
        );
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Failed to read manifest file")
        );
    }
}
