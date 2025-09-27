mod cased_string;
mod cli;
mod common;
mod document;
mod dop;
mod error_collector;
mod filesystem;
mod hop;
mod ir;
mod test_utils;
mod tui;

use clap::{CommandFactory, Parser, Subcommand, ValueEnum};
use filesystem::{config::TargetLanguage, files::ProjectRoot};
use std::path::Path;

#[derive(Clone, Debug, ValueEnum)]
enum CompileLanguage {
    /// JavaScript
    Js,
    /// TypeScript
    Ts,
    /// Go
    Go,
    /// Python
    Py,
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
    /// Initialize a new hop project
    #[command(arg_required_else_help = true)]
    Init {
        /// Target language for the project
        #[arg(value_enum)]
        target: TargetLanguage,
    },
    /// Run the Language Server Protocol (LSP) server
    Lsp,
    /// Compile hop templates using target from hop.toml
    Compile {
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Compile for development mode (generates bootstrap HTML)
        #[arg(long, default_value = "false")]
        development: bool,
    },
    /// Start development server for serving hop templates from a manifest
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
    /// Format a hop file
    Fmt {
        /// Path to the file to format
        filename: String,
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
        Some(Commands::Init { target }) => {
            cli::init::execute(target)?;
        }
        Some(Commands::Lsp) => {
            cli::lsp::execute().await;
        }
        Some(Commands::Compile {
            projectdir,
            development,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            let mut result = cli::compile::execute(&root, *development).await?;
            let elapsed = start_time.elapsed();

            print_header("compiled", elapsed.as_millis());
            println!(
                "  {:<50} {}",
                result.output_path,
                format_file_size(result.file_size)
            );

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
            use colored::*;
            use std::time::Instant;
            use tokio::process::Command;

            let start_time = Instant::now();
            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            // Load config to get compile_and_run commands
            let config = root.load_config()?;
            let (_target_language, target_config) = config.get_target();

            let (router, _watcher, _tailwind_handle) = cli::dev::execute(&root).await?;
            let elapsed = start_time.elapsed();
            let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;

            print_header("ready", elapsed.as_millis());

            // The logic for the dev server works like this:
            //
            // (1) We read the array of `compile_and_run` commands from the users config.
            //
            // (2) We compile the users project in development mode. This creates
            //     a file containing stubs for each entrypoint of the project, the stubs
            //     contain a bootstrap script that makes the backend able to do
            //     hot-reloading.
            //
            // (3) We execute all the commands of the `compile_and_run` block except
            //     the last one synchronously (these are the commands that should compile the
            //     software and get it ready for execution, e.g. typechecking, linting, compilation
            //     etc).
            //
            // (4) We execute the last command of the `compile_and_run` command in a
            //     separate thread.
            //     This command is expected to start the users backend server which should
            //     now have the loaded stubs that were created in step (2).
            //
            // (5) We compile the project in production mode (replacing the stubs we created in
            //     step (2) with the real code). This is necessary since the user might have
            //     the output file checked in to version control, and we don't want the VCS state
            //     to be dirty just because the dev server is running.
            //
            // (6) We start the dev server, which is the server that will respond to the
            //     requests made from the stubs created in step (2).

            // Step (1) - Read `compile_and_run`
            let commands = &target_config.compile_and_run;

            // Set up SIGINT handler (cross-platform)
            #[cfg(unix)]
            let mut sigint =
                tokio::signal::unix::signal(tokio::signal::unix::SignalKind::interrupt())?;
            #[cfg(windows)]
            let mut sigint = tokio::signal::windows::ctrl_c()?;

            // Step (2) - Create stubs
            let _ = cli::compile::execute(&root, true).await;

            // Store the last command
            let last_command = commands
                .last()
                .ok_or_else(|| anyhow::anyhow!("No commands found in compile_and_run"))?;

            // Step (3) - Compile the users backend server
            for command in &commands[..commands.len() - 1] {
                println!("  > {}", command.dimmed());
                let output = if cfg!(target_os = "windows") {
                    Command::new("cmd").args(["/C", command]).output().await
                } else {
                    Command::new("sh").args(["-c", command]).output().await
                };

                match output {
                    Ok(output) => {
                        if !output.stdout.is_empty() {
                            print!("{}", String::from_utf8_lossy(&output.stdout));
                        }
                        if !output.stderr.is_empty() {
                            eprint!("{}", String::from_utf8_lossy(&output.stderr));
                        }
                        if !output.status.success() {
                            eprintln!(
                                "  {} Command failed with exit code: {:?}",
                                "✗".red(),
                                output.status.code()
                            );
                            return Err(anyhow::anyhow!("Command failed: {}", command));
                        }
                    }
                    Err(e) => {
                        eprintln!("  {} Failed to execute command: {}", "✗".red(), e);
                        return Err(anyhow::anyhow!("Failed to execute command: {}", e));
                    }
                }
            }

            println!("  > {}", last_command.dimmed());

            // Step (4) - Start the users backend server
            let mut child = if cfg!(target_os = "windows") {
                Command::new("cmd").args(["/C", last_command]).spawn()
            } else {
                Command::new("sh").args(["-c", last_command]).spawn()
            }
            .map_err(|e| anyhow::anyhow!("Failed to start background command: {}", e))?;

            let local_root = root.clone();
            tokio::spawn(async move {
                // Wait for background server to start before replacing the stubs.
                //
                // This is necessary in dynamic languages like TypeScript and Python
                // since the stubs are not loaded until the language runtime has
                // performed import resolution and read the files from disk.
                tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
                let _ = cli::compile::execute(&local_root, false).await;
            });

            let result: anyhow::Result<()> = tokio::select! {
                // Step (6) - Start the dev server
                result = axum::serve(listener, router) => {
                    // Dev server exited (shouldn't normally happen)
                    result.map_err(|e| anyhow::anyhow!("Dev server error: {}", e))
                }
                status = child.wait() => {
                    // Users backend server exited
                    match status {
                        Ok(exit_status) => {
                            Err(anyhow::anyhow!("Background command exited with status: {}", exit_status))
                        }
                        Err(e) => {
                            Err(anyhow::anyhow!("Failed to wait for background command: {}", e))
                        }
                    }
                }
                _ = sigint.recv() => {
                    // SIGINT received - perform cleanup
                    eprintln!("\n  Shutting down gracefully...");
                    Ok(())
                }
            };

            // Always restore production code on exit
            eprintln!("  Restoring production code...");
            if let Err(e) = cli::compile::execute(&root, false).await {
                eprintln!("  {} Failed to restore production code: {}", "✗".red(), e);
            } else {
                eprintln!("  {} Production code restored", "✓".green());
            }

            // Kill the child process if it's still running
            if let Err(e) = child.kill().await {
                // Ignore error - process may have already exited
                eprintln!(
                    "  {} Backend process may have already stopped: {}",
                    "⚠".yellow(),
                    e
                );
            }

            result?;
        }
        Some(Commands::Fmt { filename }) => {
            use hop::pretty_print::pretty_print_from_source;
            use std::fs;

            // Read the file
            let content = fs::read_to_string(filename)?;

            // Format the content
            match pretty_print_from_source(&content, 80) {
                Ok(formatted) => {
                    // Write the formatted content back to the file
                    fs::write(filename, formatted)?;
                    use colored::*;
                    println!("  {} {}", "✓".green(), filename);
                }
                Err(errors) => {
                    use colored::*;
                    eprintln!("  {} Failed to format {}", "✗".red(), filename);
                    for error in errors {
                        eprintln!("    {}", error);
                    }
                    std::process::exit(1);
                }
            }
        }
        None => {
            let mut cmd = Cli::command();
            cmd.print_help().unwrap();
        }
    }

    Ok(())
}
