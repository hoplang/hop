mod commands;
mod common;
mod compiler;
mod error_formatter;
mod expression_parser;
mod files;
mod parser;
mod runtime;
mod scriptcollector;
mod server;
mod timing;
mod tokenizer;
mod toposorter;
mod typechecker;
mod unifier;

use clap::{CommandFactory, Parser, Subcommand};
use files::ProjectRoot;
use std::path::Path;

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
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Output directory
        #[arg(long)]
        outdir: String,
        /// Optional script file name to output collected scripts
        #[arg(long)]
        scriptfile: Option<String>,
        /// Directory to copy static files from
        #[arg(long)]
        staticdir: Option<String>,
    },
    /// Start development server for serving hop templates from a manifest
    Dev {
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Port to serve on
        #[arg(short, long, default_value = "3000")]
        port: u16,
        /// Host to bind to
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        /// Directory to serve static files from
        #[arg(long)]
        staticdir: Option<String>,
        /// Optional script file name to make scripts available over HTTP
        #[arg(long)]
        scriptfile: Option<String>,
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
            commands::lsp::execute().await;
        }
        Some(Commands::Build {
            projectdir,
            outdir,
            scriptfile,
            staticdir,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();
            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };
            let mut outputs = commands::build::execute(
                &root,
                Path::new(outdir),
                scriptfile.as_deref(),
                staticdir.as_deref(),
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
        Some(Commands::Dev {
            projectdir,
            port,
            host,
            staticdir,
            scriptfile,
        }) => {
            use colored::*;
            use std::time::Instant;
            let start_time = Instant::now();
            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };
            let (router, _watcher) = commands::dev::execute(
                &root,
                staticdir.as_deref().map(Path::new),
                scriptfile.as_deref(),
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
