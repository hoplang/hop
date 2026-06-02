mod build;
mod fmt;
mod lsp;

use clap::{CommandFactory, Parser, Subcommand};
use hop_core::project::Project;
use std::path::Path;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt::format::FmtSpan;

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
        #[arg(long, hide = true)]
        no_optimize: bool,
        /// Suppress output (errors are still shown)
        #[arg(long)]
        silent: bool,
    },
    /// Format source code
    Fmt {
        /// Path to project root
        #[arg(long)]
        project: Option<String>,
        /// Specific .hop file to format (formats all files if not specified)
        file: Option<String>,
    },
    /// Run the LSP server
    #[command(hide = true)]
    Lsp,
}

fn print_header(message: &str) {
    println!();
    println!("  hop | {}", message);
    println!();
}

fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_span_events(FmtSpan::CLOSE)
        .init();

    let cli = Cli::parse();

    match &cli.command {
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
        Some(Commands::Lsp) => {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()?
                .block_on(lsp::execute());
        }
        Some(Commands::Fmt { project, file }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let proj = match project {
                Some(d) => Project::from(Path::new(d))?,
                None => Project::find_traversing_superdirectories(Path::new("."))?,
            };

            let result = fmt::execute(&proj, file.as_deref())?;
            let elapsed = start_time.elapsed();

            print_header(&format!("formatted in {} ms", elapsed.as_millis()));
            println!(
                "    {} file(s) formatted, {} unchanged",
                result.files_formatted, result.files_unchanged
            );

            println!();
        }
        Some(Commands::Build {
            project,
            no_optimize,
            silent,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let proj = match project {
                Some(d) => Project::from(Path::new(d))?,
                None => Project::find_traversing_superdirectories(Path::new("."))?,
            };

            let result = build::execute(&proj, *no_optimize)?;
            let elapsed = start_time.elapsed();

            if !silent {
                print_header(&format!("built in {} ms", elapsed.as_millis()));

                println!("  output");
                println!();
                println!("    {}", result.output_path.display());

                println!();
            }
        }
    }

    Ok(())
}
