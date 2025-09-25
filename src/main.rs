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

            let result = cli::compile::execute(projectdir.as_deref(), *development)?;
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

            let (router, _watcher) = cli::dev::execute(&root).await?;
            let elapsed = start_time.elapsed();
            let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;

            print_header("ready", elapsed.as_millis());
            println!("  {} http://{}:{}/", "➜".green(), host, port);
            println!();
            println!(
                "  Development server running on port {}",
                port.to_string().green()
            );
            println!("  /render endpoint available for component rendering");

            // Execute compile_and_run commands
            if !target_config.compile_and_run.is_empty() {
                println!();
                println!("  {} Executing compile_and_run commands:", "🔄".cyan());

                let commands = &target_config.compile_and_run;

                // Execute all commands except the last one synchronously
                for command in &commands[..commands.len().saturating_sub(1)] {
                    println!("    > {}", command.dimmed());
                    use std::process::Command;
                    let output = if cfg!(target_os = "windows") {
                        Command::new("cmd")
                            .args(["/C", command])
                            .output()
                    } else {
                        Command::new("sh")
                            .args(["-c", command])
                            .output()
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
                                eprintln!("    {} Command failed with exit code: {:?}", "✗".red(), output.status.code());
                            } else {
                                println!("    {} Command completed successfully", "✓".green());
                            }
                        }
                        Err(e) => {
                            eprintln!("    {} Failed to execute command: {}", "✗".red(), e);
                        }
                    }
                }

                // Execute the last command asynchronously (non-blocking)
                if let Some(last_command) = commands.last() {
                    println!("    > {} (running in background)", last_command.dimmed());
                    use std::process::Command;
                    let command_clone = last_command.clone();
                    tokio::spawn(async move {
                        let mut child = if cfg!(target_os = "windows") {
                            Command::new("cmd")
                                .args(["/C", &command_clone])
                                .spawn()
                        } else {
                            Command::new("sh")
                                .args(["-c", &command_clone])
                                .spawn()
                        };

                        match child {
                            Ok(ref mut child) => {
                                let _ = child.wait();
                            }
                            Err(e) => {
                                eprintln!("    {} Failed to execute background command: {}", "✗".red(), e);
                            }
                        }
                    });
                }
            }

            println!();
            axum::serve(listener, router).await?;
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
