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
use filesystem::config::TargetLanguage;
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
    /// Run tailwindcss
    Tailwind {
        /// Input CSS file path
        #[arg(long, default_value = "input.css")]
        input: String,
        /// Output CSS file path
        #[arg(long, default_value = "output.css")]
        output: String,
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

            let mut result = cli::compile::execute(projectdir.as_deref(), *development)?;
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
            use filesystem::files::ProjectRoot;
            use std::time::Instant;
            let start_time = Instant::now();
            let root = match projectdir {
                Some(d) => ProjectRoot::from(Path::new(d))?,
                None => ProjectRoot::find_upwards(Path::new("."))?,
            };

            // Load config to get compile_and_run commands
            let config = root.load_config()?;
            let (_target_language, target_config) = config.get_target();

            // First compilation with development: true for stubs
            let _ = cli::compile::execute(projectdir.as_deref(), true)?;

            // Set up Ctrl-C handler to ensure we compile production output before exit
            let projectdir_clone = projectdir.clone();
            tokio::spawn(async move {
                tokio::signal::ctrl_c().await.ok();
                // Compile with production mode to clean up stubs
                let _ = cli::compile::execute(projectdir_clone.as_deref(), false);
                std::process::exit(130); // Standard exit code for SIGINT
            });

            let (router, _watcher, _tailwind_handle) = cli::dev::execute(&root).await?;
            let elapsed = start_time.elapsed();
            let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;

            print_header("ready", elapsed.as_millis());

            // Execute compile_and_run commands
            if target_config.compile_and_run.is_empty() {
                eprintln!(
                    "  {} No compile_and_run commands configured in hop.toml",
                    "✗".red()
                );
                eprintln!("  Add commands to start your application server");
                return Err(anyhow::anyhow!("No compile_and_run commands configured"));
            }

            let commands = &target_config.compile_and_run;

            // Execute all commands except the last one synchronously
            for command in &commands[..commands.len().saturating_sub(1)] {
                println!("  > {}", command.dimmed());
                use std::process::Command;
                let output = if cfg!(target_os = "windows") {
                    Command::new("cmd").args(["/C", command]).output()
                } else {
                    Command::new("sh").args(["-c", command]).output()
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

            // Execute the last command asynchronously (non-blocking)
            let last_command = commands
                .last()
                .ok_or_else(|| anyhow::anyhow!("No commands found in compile_and_run"))?;

            println!("  > {}", last_command.dimmed());
            use std::process::Command;
            use tokio::sync::oneshot;

            let command_clone = last_command.clone();

            // Try to start the command and check if it fails immediately
            let child = if cfg!(target_os = "windows") {
                Command::new("cmd").args(["/C", &command_clone]).spawn()
            } else {
                Command::new("sh").args(["-c", &command_clone]).spawn()
            };

            match child {
                Ok(mut child) => {
                    // Create a channel to signal when the background process exits
                    let (tx, rx) = oneshot::channel::<std::process::ExitStatus>();

                    // Spawn a task to monitor the background process
                    tokio::spawn(async move {
                        if let Ok(status) = child.wait() {
                            // Send the exit status through the channel (ignore if receiver dropped)
                            let _ = tx.send(status);
                        }
                    });

                    // Wait for background server to start and load the development stubs
                    tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;

                    // Second compilation with development: false for clean production output
                    let _ = cli::compile::execute(projectdir.as_deref(), false)?;

                    // Run the server and monitor for background process exit
                    tokio::select! {
                        result = axum::serve(listener, router) => {
                            // Server ended (shouldn't normally happen)
                            result?;
                        }
                        status = rx => {
                            // Background process exited
                            match status {
                                Ok(exit_status) => {
                                    eprintln!();
                                    eprintln!("  {} Background command exited with status: {}",
                                             "✗".red(),
                                             exit_status);
                                    return Err(anyhow::anyhow!("Background command exited"));
                                }
                                Err(_) => {
                                    // Channel was dropped, which shouldn't happen
                                    eprintln!("  {} Lost connection to background command", "✗".red());
                                    return Err(anyhow::anyhow!("Lost connection to background command"));
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!(
                        "  {} Failed to execute background command: {}",
                        "✗".red(),
                        e
                    );
                    return Err(anyhow::anyhow!("Failed to start background command: {}", e));
                }
            }
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
        Some(Commands::Tailwind { input, output }) => {
            use std::path::PathBuf;
            use tailwind_runner::{TailwindConfig, TailwindRunner};

            // NOTE: We need to think about where to store this file.
            // If it is stored in the current directory it is being
            // scanned by tailwind.
            //
            // See https://github.com/tailwindlabs/tailwindcss/tree/664f2e36da9f3a18e418b207179fe5cbc7481824/crates/oxide/src/scanner/fixtures
            let cache_dir = PathBuf::from("/tmp/.hop-cache");

            println!("Initializing Tailwind CSS...");
            let runner = TailwindRunner::new(cache_dir).await?;

            let config = TailwindConfig {
                input: PathBuf::from(input),
                output: PathBuf::from(output),
            };

            println!("Running Tailwind CSS...");
            match runner.run_once(&config).await {
                Ok(output) => {
                    use colored::*;
                    println!("  {} Generated {}", "✓".green(), config.output.display());
                    if !output.is_empty() {
                        println!("{}", output);
                    }
                }
                Err(e) => {
                    use colored::*;
                    eprintln!("  {} Tailwind CSS failed: {}", "✗".red(), e);
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
