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
