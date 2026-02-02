mod cli;
mod common;
mod config;
mod document;
mod dop;
mod environment;
mod error_collector;
mod filesystem;
mod hop;
mod inlined;
mod ir;
mod orchestrator;
mod parse_error;
mod test_utils;
mod toposorter;
mod tui;
mod type_error;

use clap::{CommandFactory, Parser, Subcommand};
use filesystem::project_root::ProjectRoot;
use std::path::Path;

#[derive(Parser)]
#[command(name = "hop")]
#[command(about = "The hop compiler - https://hoplang.com")]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a hop project
    Build {
        /// Path to project root
        #[arg(long)]
        project: Option<String>,
        /// Skip optimization passes
        #[arg(long)]
        no_optimize: bool,
        /// Show timing breakdown for each compilation phase
        #[arg(long, hide = true)]
        timing: bool,
    },
    /// Start development server
    Dev {
        /// Path to project root
        #[arg(long)]
        project: Option<String>,
        /// Port to serve on
        #[arg(long, default_value = "3000")]
        port: u16,
        /// Host to bind to
        #[arg(long, default_value = "localhost")]
        host: String,
        /// Show timing information for startup
        #[arg(long, hide = true)]
        timing: bool,
    },
    /// Format source code
    Fmt {
        /// Path to project root
        #[arg(long)]
        project: Option<String>,
        /// Specific .hop file to format (formats all files if not specified)
        file: Option<String>,
        /// Show timing information
        #[arg(long, hide = true)]
        timing: bool,
    },
    /// Run the LSP server
    #[command(hide = true)]
    Lsp,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
        Some(Commands::Lsp) => {
            cli::lsp::execute().await;
        }
        Some(Commands::Fmt {
            project,
            file,
            timing,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match project {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            let mut result = cli::fmt::execute(&root, file.as_deref())?;
            let elapsed = start_time.elapsed();

            tui::print_header(&format!("formatted in {} ms", elapsed.as_millis()));
            println!(
                "    {} file(s) formatted, {} unchanged",
                result.files_formatted, result.files_unchanged
            );

            if *timing {
                result.timer.print();
            }
            println!();
        }
        Some(Commands::Build {
            project,
            no_optimize,
            timing,
        }) => {
            use colored::*;
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match project {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            let mut result = cli::build::execute(&root, *no_optimize).await?;
            let elapsed = start_time.elapsed();

            tui::print_header(&format!("built in {} ms", elapsed.as_millis()));

            println!("  {}", "output".bold());
            println!();
            println!("    {}", result.output_path.display());

            if *timing {
                result.timer.print();
            }
            println!();
        }
        Some(Commands::Dev {
            project,
            port,
            host,
            timing,
        }) => {
            use colored::*;
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match project {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            // Try binding to the specified port, then up to 5 additional ports
            let mut listener = None;
            let mut bound_port = *port;
            for attempt in 0..=5 {
                let try_port = port + attempt;
                let addr = format!("{}:{}", host, try_port);
                if let Ok(l) = tokio::net::TcpListener::bind(&addr).await {
                    listener = Some(l);
                    bound_port = try_port;
                    break;
                }
            }
            let listener = listener.ok_or_else(|| {
                anyhow::anyhow!("failed to bind to ports {}-{} on {}", port, port + 5, host)
            })?;

            let mut res = cli::dev::execute(&root).await?;
            let elapsed = start_time.elapsed();
            let dev_server = axum::serve(listener, res.router);

            tui::print_header(&format!("started in {} ms", elapsed.as_millis()));
            println!("    {} http://{}:{}", "⟶".green().bold(), host, bound_port);

            if *timing {
                res.timer.print();
            }
            println!();

            dev_server
                .await
                .map_err(|e| anyhow::anyhow!("Dev server error: {}", e))?;
        }
    }

    Ok(())
}
