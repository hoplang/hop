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
        /// Output directory (defaults to current directory)
        #[arg(short, long, default_value = ".")]
        output: String,
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
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Lsp) => {
            lsp::run_lsp().await;
        }
        Some(Commands::Render {
            manifest,
            output,
        }) => {
            render_from_manifest(manifest, output);
        }
        Some(Commands::Serve {
            manifest,
            port,
            host,
        }) => {
            serve_from_manifest(manifest, host, *port).await;
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
                eprintln!("Error executing {}::{}: {}", entry.module, entry.function, e);
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

async fn serve_from_manifest(manifest_path: &str, host: &str, port: u16) {
    use axum::http::StatusCode;
    use axum::response::Html;
    use axum::routing::get;
    use compiler::Compiler;
    use std::fs;

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

    // Load and compile all hop modules once at startup
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

    // Load data for each manifest entry
    let mut file_data = std::collections::HashMap::new();
    for (file_path, entry) in &manifest.files {
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
        file_data.insert(file_path.clone(), data);
    }

    // Create router with routes for each file in manifest
    let mut router = axum::Router::new();
    let manifest_files = manifest.files.clone();

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
        let data = file_data.get(&file_path).unwrap().clone();
        let program_clone = program.clone();

        router = router.route(
            &route_path,
            get(move || {
                let module = module.clone();
                let function = function.clone();
                let data = data.clone();
                let program = program_clone.clone();

                async move {
                    match program.execute(&module, &function, data) {
                        Ok(html) => Ok(Html(html)),
                        Err(e) => {
                            eprintln!("Error executing {}::{}: {}", module, function, e);
                            Err(StatusCode::INTERNAL_SERVER_ERROR)
                        }
                    }
                }
            }),
        );
    }

    let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port))
        .await
        .unwrap();

    println!("Hop server running on http://{}:{}", host, port);
    println!("Serving from manifest: {}", manifest_path);
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
