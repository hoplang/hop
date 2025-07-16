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
    /// Render a hop module function to HTML
    Render {
        /// The hop module to render from
        module: String,
        /// The function to render
        function: String,
        /// Output file (defaults to stdout)
        #[arg(short, long)]
        output: Option<String>,
        /// JSON data file to pass as parameters
        #[arg(short, long)]
        data: Option<String>,
    },
    /// Start an HTTP server for serving hop templates
    Serve {
        /// The hop module to serve from
        module: String,
        /// The function to serve
        function: String,
        /// Port to serve on
        #[arg(short, long, default_value = "3000")]
        port: u16,
        /// Host to bind to
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        /// JSON data file to pass as parameters
        #[arg(short, long)]
        data: Option<String>,
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
            module,
            function,
            output,
            data,
        }) => {
            render_function(module, function, output.as_deref(), data.as_deref());
        }
        Some(Commands::Serve {
            module,
            function,
            port,
            host,
            data,
        }) => {
            serve_function(module, function, host, *port, data.as_deref()).await;
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }
}

fn render_function(module: &str, function: &str, output: Option<&str>, data: Option<&str>) {
    use compiler::Compiler;
    use std::fs;

    // Read all .hop files from ./hop directory
    let hop_dir = std::path::Path::new("./hop");
    if !hop_dir.exists() {
        eprintln!("Error: ./hop directory does not exist");
        std::process::exit(1);
    }

    let mut compiler = Compiler::new();

    // Read all .hop files
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

    // Compile (parse and typecheck) all modules
    let program = match compiler.compile() {
        Ok(program) => program,
        Err(errors) => {
            eprintln!("{}", errors);
            std::process::exit(1);
        }
    };

    // Read JSON data if provided
    let params = match data {
        Some(data_file) => match fs::read_to_string(data_file) {
            Ok(json_str) => match serde_json::from_str(&json_str) {
                Ok(value) => value,
                Err(e) => {
                    eprintln!("Error parsing JSON from {}: {}", data_file, e);
                    std::process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("Error reading data file {}: {}", data_file, e);
                std::process::exit(1);
            }
        },
        None => serde_json::Value::Null,
    };

    // Execute the specified function from the module
    let result = match program.execute(module, function, params) {
        Ok(html) => html,
        Err(e) => {
            eprintln!("Error executing {}.{}: {}", module, function, e);
            std::process::exit(1);
        }
    };

    // Write output
    match output {
        Some(file_path) => match fs::write(file_path, result) {
            Ok(_) => println!("Output written to {}", file_path),
            Err(e) => {
                eprintln!("Error writing to file {}: {}", file_path, e);
                std::process::exit(1);
            }
        },
        None => {
            print!("{}", result);
        }
    }
}

async fn serve_function(
    module: &str,
    function: &str,
    host: &str,
    port: u16,
    data_file: Option<&str>,
) {
    use axum::http::StatusCode;
    use axum::response::Html;
    use compiler::Compiler;
    use std::fs;

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

    // Load data once at startup
    let data = if let Some(data_file_path) = data_file {
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

    let module_owned = module.to_string();
    let function_owned = function.to_string();

    let app = axum::Router::new().route(
        "/",
        axum::routing::get(move || {
            let module = module_owned.clone();
            let function = function_owned.clone();
            let data = data.clone();
            let program = program.clone();

            async move {
                match program.execute(&module, &function, data) {
                    Ok(html) => Ok(Html(html)),
                    Err(e) => {
                        eprintln!("Error executing {}.{}: {}", module, function, e);
                        Err(StatusCode::INTERNAL_SERVER_ERROR)
                    }
                }
            }
        }),
    );

    let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port))
        .await
        .unwrap();

    println!("Hop server running on http://{}:{}", host, port);
    println!("Serving {}.{}", module, function);
    if let Some(data_file) = data_file {
        println!("Using data from file: {}", data_file);
    }

    axum::serve(listener, app).await.unwrap();
}
