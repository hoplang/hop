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
    pub function: String,
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
            render_from_manifest(manifest, outdir);
        }
        Some(Commands::Serve {
            manifest,
            port,
            host,
            servedir,
        }) => {
            serve_from_manifest(manifest, host, *port, servedir.as_deref()).await;
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }
}

fn render_from_manifest(manifest_path: &str, output_dir: &str) {
    use compiler::Compiler;
    use std::fs;
    use std::path::Path;

    // Read and parse manifest
    let manifest_content = match fs::read_to_string(manifest_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading manifest file {}: {}", manifest_path, e);
            std::process::exit(1);
        }
    };

    let manifest: Manifest = match serde_json::from_str(&manifest_content) {
        Ok(manifest) => manifest,
        Err(e) => {
            eprintln!("Error parsing manifest file {}: {}", manifest_path, e);
            std::process::exit(1);
        }
    };

    let hop_dir = Path::new("./hop");
    if !hop_dir.exists() {
        eprintln!("Error: ./hop directory does not exist");
        std::process::exit(1);
    }

    // Create output directory if it doesn't exist
    let output_path = Path::new(output_dir);
    if !output_path.exists() {
        if let Err(e) = fs::create_dir_all(output_path) {
            eprintln!("Error creating output directory {}: {}", output_dir, e);
            std::process::exit(1);
        }
    }

    // Load and compile all hop modules
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
                            eprintln!("Error reading file {:?}: {}", path, e);
                            std::process::exit(1);
                        }
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Error reading ./hop directory: {}", e);
            std::process::exit(1);
        }
    }

    let program = match compiler.compile() {
        Ok(program) => program,
        Err(errors) => {
            eprintln!("Compilation errors: {}", errors);
            std::process::exit(1);
        }
    };

    // Render each file from the manifest
    println!("Rendering from manifest: {}", manifest_path);
    for (file_path, entry) in &manifest.files {
        // Load data for this file
        let data = if let Some(data_file_path) = &entry.data {
            match fs::read_to_string(data_file_path) {
                Ok(json_str) => match serde_json::from_str(&json_str) {
                    Ok(value) => value,
                    Err(e) => {
                        eprintln!("Error parsing JSON from file {}: {}", data_file_path, e);
                        std::process::exit(1);
                    }
                },
                Err(e) => {
                    eprintln!("Error reading data file {}: {}", data_file_path, e);
                    std::process::exit(1);
                }
            }
        } else {
            serde_json::Value::Null
        };

        // Execute the function
        let html = match program.execute(&entry.module, &entry.function, data) {
            Ok(html) => html,
            Err(e) => {
                eprintln!(
                    "Error executing {}::{}: {}",
                    entry.module, entry.function, e
                );
                std::process::exit(1);
            }
        };

        // Write to output file
        let output_file_path = output_path.join(file_path);
        if let Some(parent) = output_file_path.parent() {
            if !parent.exists() {
                if let Err(e) = fs::create_dir_all(parent) {
                    eprintln!("Error creating directory {:?}: {}", parent, e);
                    std::process::exit(1);
                }
            }
        }

        match fs::write(&output_file_path, html) {
            Ok(_) => {
                println!("Generated {} -> {:?}", file_path, output_file_path);
            }
            Err(e) => {
                eprintln!("Error writing to file {:?}: {}", output_file_path, e);
                std::process::exit(1);
            }
        }
    }

    println!("Rendered {} files to {}", manifest.files.len(), output_dir);
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
    function_name: &str,
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

    let program = match compiler.compile() {
        Ok(program) => program,
        Err(errors) => {
            return Err(format!("Compilation errors: {}", errors));
        }
    };

    // Load data from file if specified
    let data = if let Some(data_file_path) = data_file {
        match fs::read_to_string(data_file_path) {
            Ok(json_str) => match serde_json::from_str(&json_str) {
                Ok(value) => value,
                Err(e) => {
                    return Err(format!(
                        "Error parsing JSON from file {}: {}",
                        data_file_path, e
                    ));
                }
            },
            Err(e) => {
                return Err(format!("Error reading data file {}: {}", data_file_path, e));
            }
        }
    } else {
        serde_json::Value::Null
    };

    // Execute the entrypoint
    program.execute(module_name, function_name, data)
}

async fn serve_from_manifest(manifest_path: &str, host: &str, port: u16, servedir: Option<&str>) {
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
    let manifest_content = match fs::read_to_string(manifest_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading manifest file {}: {}", manifest_path, e);
            std::process::exit(1);
        }
    };

    let manifest: Manifest = match serde_json::from_str(&manifest_content) {
        Ok(manifest) => manifest,
        Err(e) => {
            eprintln!("Error parsing manifest file {}: {}", manifest_path, e);
            std::process::exit(1);
        }
    };

    let hop_dir = std::path::Path::new("./hop");
    if !hop_dir.exists() {
        eprintln!("Error: ./hop directory does not exist");
        std::process::exit(1);
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
                    if event.kind.is_modify() || event.kind.is_create() {
                        if event.paths.iter().any(|p| {
                            let ext = p.extension().and_then(|s| s.to_str());
                            ext == Some("hop") || ext == Some("json")
                        }) {
                            let _ = tx.try_send(());
                        }
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
                    .expect(&format!("Failed to watch JSON file: {}", json_file));
            }
        }

        while let Some(_) = rx.recv().await {
            // Debounce rapid file changes
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;

            // Send reload event to all connected clients
            let _ = watcher_tx.send(());
        }
    });

    // Note: Data loading now happens on each request for hot reloading

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
        let route_path = if file_path == "index.html" {
            "/".to_string()
        } else if file_path.ends_with(".html") {
            format!("/{}", file_path.strip_suffix(".html").unwrap())
        } else {
            format!("/{}", file_path)
        };

        let module = entry.module.clone();
        let function = entry.function.clone();
        let data_file = entry.data.clone();

        router = router.route(
            &route_path,
            get(move || {
                let module = module.clone();
                let function = function.clone();
                let data_file = data_file.clone();

                async move {
                    match compile_and_execute(&module, &function, data_file.as_deref()) {
                        Ok(html) => {
                            // Inject hot reload script for development
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
    if let Some(static_dir) = servedir {
        use tower_http::services::ServeDir;
        router = router.fallback_service(ServeDir::new(static_dir));
    }

    let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port))
        .await
        .unwrap();

    println!("Hop server running on http://{}:{}", host, port);
    println!("Serving from manifest: {}", manifest_path);
    if let Some(static_dir) = servedir {
        println!("Static files from: {}", static_dir);
    }
    println!("Available routes:");
    for (file_path, entry) in &manifest_files {
        let route_path = if file_path == "index.html" {
            "/".to_string()
        } else if file_path.ends_with(".html") {
            format!("/{}", file_path.strip_suffix(".html").unwrap())
        } else {
            format!("/{}", file_path)
        };
        println!("  {} -> {}::{}", route_path, entry.module, entry.function);
    }

    axum::serve(listener, router).await.unwrap();
}
