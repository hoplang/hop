mod cli;
mod common;
mod decl;
mod document;
mod dop;
mod error_collector;
mod filesystem;
mod hop;
mod ir;
mod test_utils;
mod tui;

use clap::{CommandFactory, Parser, Subcommand};
use filesystem::{config::TargetConfig, project_root::ProjectRoot};
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
    /// Run the LSP server
    Lsp,
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

    match &cli.command {
        Some(Commands::Lsp) => {
            cli::lsp::execute().await;
        }
        Some(Commands::Compile { projectdir }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            let mut result = cli::compile::execute(&root).await?;
            let elapsed = start_time.elapsed();

            print_header("compiled", elapsed.as_millis());

            // Display output file path
            {
                use colored::*;
                println!();
                println!("  {}", "output".bold());
                println!();
                println!("    {}", result.output_path.display());
            }

            if !result.entry_points.is_empty() {
                println!();
                use colored::*;
                println!("  {}", "exported".bold());
                println!();
                for func_name in &result.entry_points {
                    println!("    {}()", func_name);
                }
            } else {
                use colored::*;
                println!();
                println!("  {} No entrypoint components found", "⚠".yellow());
                println!("  Add 'entrypoint' attribute to components you want to export");
            }
            result.timer.print();
            println!();
        }
        Some(Commands::Dev {
            projectdir,
            port,
            host,
        }) => {
            use std::time::Instant;

            let start_time = Instant::now();
            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            async fn run_dev_server(
                root: &ProjectRoot,
                host: &str,
                port: u16,
                start_time: Instant,
            ) -> anyhow::Result<()> {
                use colored::*;

                // Set up Ctrl-C handler
                //
                // The channel is necessary here since the listener is not
                // registered until the signal is actually awaited, and we want
                // to register it right here.
                let (sigint_tx, mut sigint_rx) = tokio::sync::oneshot::channel();
                tokio::spawn(async move {
                    tokio::signal::ctrl_c().await.ok();
                    sigint_tx.send(()).ok();
                });

                let config = root.load_config().await?;
                let target_config = config.get_target();

                let elapsed = start_time.elapsed();
                let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;

                // Print header inline since print_header is defined outside
                println!();
                println!("  {} | ready in {} ms", "hop".bold(), elapsed.as_millis());
                println!();

                // The logic for the dev server works like this:
                //
                // (1) We read the array of `compile_and_run` commands from the users config.
                //
                // (2) We compile the users project. This creates a file containing code that
                //     checks the HOP_DEV_MODE environment variable at runtime to decide whether
                //     to render dev mode stubs (with bootstrap script for hot-reloading) or
                //     production HTML.
                //
                // (3) We execute all the commands of the `compile_and_run` block except
                //     the last one synchronously (these are the commands that should compile the
                //     software and get it ready for execution, e.g. typechecking, linting, compilation
                //     etc).
                //
                // (4) We execute the last command of the `compile_and_run` command in a
                //     separate thread with HOP_DEV_MODE=enabled set.
                //     This command is expected to start the users backend server which will
                //     render dev mode stubs due to the environment variable.
                //
                // (5) We start the dev server, which is the server that will respond to the
                //     requests made from the stubs rendered in step (4).
                //
                // (6) We block until a subprocess exits or we receive Ctrl-C.

                // Step (1) - Read `compile_and_run`
                let commands = match &target_config {
                    TargetConfig::Javascript(config) => &config.compile_and_run,
                    TargetConfig::Typescript(config) => &config.compile_and_run,
                    TargetConfig::Python(config) => &config.compile_and_run,
                    TargetConfig::Go(config) => &config.compile_and_run,
                };

                // Step (2) - Compile project (generates both dev and prod code)
                cli::compile::execute(root).await?;

                // Store the last command
                let last_command = commands
                    .last()
                    .ok_or_else(|| anyhow::anyhow!("No commands found in compile_and_run"))?;

                // Step (3) - Compile the users backend server
                for command in &commands[..commands.len() - 1] {
                    println!("  {} {}", "→".green(), command.dimmed());
                    let output = if cfg!(target_os = "windows") {
                        tokio::process::Command::new("cmd")
                            .args(["/C", command])
                            .output()
                            .await?
                    } else {
                        tokio::process::Command::new("sh")
                            .args(["-c", command])
                            .output()
                            .await?
                    };

                    if !output.stdout.is_empty() {
                        println!();
                        print!("{}", String::from_utf8_lossy(&output.stdout));
                        println!();
                    }
                    if !output.stderr.is_empty() {
                        eprintln!();
                        eprint!("{}", String::from_utf8_lossy(&output.stderr));
                        eprintln!();
                    }
                    if !output.status.success() {
                        return Err(anyhow::anyhow!("Command failed: {}", command));
                    }
                }

                println!("  {} {}", "→".green(), last_command.dimmed());
                println!();

                // Step (4) - Start the users backend server
                let mut backend_server = if cfg!(target_os = "windows") {
                    tokio::process::Command::new("cmd")
                        .args(["/C", last_command])
                        .env("HOP_DEV_MODE", "enabled")
                        .spawn()
                } else {
                    tokio::process::Command::new("sh")
                        .args(["-c", last_command])
                        .env("HOP_DEV_MODE", "enabled")
                        .spawn()
                }?;

                // Step (5) - Start our server (this takes ~200ms since we need to spawn a tailwind watcher)
                let mut res = cli::dev::execute(root).await?;
                let dev_server = axum::serve(listener, res.router);

                // Step (6) - Block
                let result = tokio::select! {
                    _ = &mut sigint_rx => {
                        Ok(())
                    }
                    result = dev_server => {
                        // Dev server exited (shouldn't normally happen)
                        result.map_err(|e| anyhow::anyhow!("Dev server error: {}", e))
                    }
                    status = backend_server.wait() => {
                        // Users backend server exited
                        match status {
                            Ok(exit_status) => {
                                Err(anyhow::anyhow!("Server exited with status: {}", exit_status))
                            }
                            Err(e) => {
                                Err(anyhow::anyhow!("Failed to wait for server: {}", e))
                            }
                        }
                    }
                    tailwind_status = res.tailwind_process.wait() => {
                        // Tailwind watcher exited
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

                // Kill all child processes that are still running.
                let _ = backend_server.kill().await;
                let _ = res.tailwind_process.kill().await;

                result
            }

            // Run the dev server and ensure cleanup always happens
            run_dev_server(&root, host, *port, start_time).await?
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }

    Ok(())
}
