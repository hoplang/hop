mod cli;
mod common;
mod document;
mod dop;
mod environment;
mod error_collector;
mod filesystem;
mod hop;
mod inlined;
mod ir;
mod orchestrator;
mod test_utils;
mod toposorter;
mod tui;

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
    /// Compile a hop project to native code
    Compile {
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Skip optimization passes
        #[arg(long)]
        no_optimize: bool,
    },
    /// Start development server for serving a hop project
    Dev {
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Port to serve on
        #[arg(short, long, default_value = "33861")]
        port: u16,
        /// Host to bind to
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
    },
    /// Format all .hop files in a project
    Fmt {
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Specific .hop file to format (formats all files if not specified)
        file: Option<String>,
    },
    /// Run the LSP server
    Lsp,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::Lsp) => {
            cli::lsp::execute().await;
        }
        Some(Commands::Fmt { projectdir, file }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            let result = cli::fmt::execute(&root, file.as_deref())?;
            let elapsed = start_time.elapsed();

            tui::print_header("formatted", elapsed.as_millis());
            println!(
                "    {} file(s) formatted, {} unchanged",
                result.files_formatted, result.files_unchanged
            );
            println!();
        }
        Some(Commands::Compile {
            projectdir,
            no_optimize,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            let mut result = cli::compile::execute(&root, *no_optimize).await?;
            let elapsed = start_time.elapsed();

            tui::print_header("compiled", elapsed.as_millis());

            // Display output file path
            {
                use colored::*;
                println!("  {}", "output".bold());
                println!();
                println!("    {}", result.output_path.display());
            }

            result.timer.print();
            println!();
        }
        Some(Commands::Dev {
            projectdir,
            port,
            host,
        }) => {
            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            async fn run_dev_server(
                root: &ProjectRoot,
                host: &str,
                port: u16,
            ) -> anyhow::Result<()> {
                use colored::*;

                // Set up Ctrl-C handler
                let (sigint_tx, mut sigint_rx) = tokio::sync::oneshot::channel();
                tokio::spawn(async move {
                    tokio::signal::ctrl_c().await.ok();
                    sigint_tx.send(()).ok();
                });

                let addr = format!("{}:{}", host, port);
                let listener = tokio::net::TcpListener::bind(&addr)
                    .await
                    .map_err(|e| anyhow::anyhow!("failed to bind to {}: {}", addr, e))?;

                // Start the dev server
                let mut res = cli::dev::execute(root).await?;
                let dev_server = axum::serve(listener, res.router);

                println!();
                println!("  {} | served at http://{}:{}", "hop".bold(), host, port);
                println!();

                // Block until Ctrl-C or server exit
                let result = tokio::select! {
                    _ = &mut sigint_rx => {
                        Ok(())
                    }
                    result = dev_server => {
                        result.map_err(|e| anyhow::anyhow!("Dev server error: {}", e))
                    }
                    tailwind_status = res.tailwind_process.wait() => {
                        match tailwind_status {
                            Ok(exit_status) => {
                                Err(anyhow::anyhow!("Tailwind watcher exited with status: {}", exit_status))
                            }
                            Err(e) => {
                                Err(anyhow::anyhow!("Failed to wait for tailwind watcher: {}", e))
                            }
                        }
                    }
                };

                let _ = res.tailwind_process.kill().await;

                result
            }

            // Run the dev server and ensure cleanup always happens
            run_dev_server(&root, host, *port).await?
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }

    Ok(())
}
