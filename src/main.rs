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
    /// Compile a hop file
    Compile {
        /// The hop file to compile
        file: String,
    },
    /// Run a hop file
    Run {
        /// The hop file to run
        file: String,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Lsp) => {
            lsp::run_lsp().await;
        }
        Some(Commands::Compile { file }) => {
            compile_file(file);
        }
        Some(Commands::Run { file }) => {
            run_file(file);
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }
}

fn compile_file(file_path: &str) {
    eprintln!("Compile command not yet implemented for: {}", file_path);
    std::process::exit(1);
}

fn run_file(file_path: &str) {
    eprintln!("Run command not yet implemented for: {}", file_path);
    std::process::exit(1);
}
