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
use std::path::Path;

#[derive(Clone, Debug, ValueEnum)]
enum CompileLanguage {
    /// JavaScript
    Js,
    /// TypeScript
    Ts,
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
    /// Run the Language Server Protocol (LSP) server
    Lsp,
    /// Initialize a new hop project with a build.hop file
    Init {
        /// Path to initialize the project in (defaults to current directory)
        #[arg(long)]
        path: Option<String>,
    },
    /// Compile hop templates to the specified language
    Compile {
        /// Target language (currently only 'js' is supported)
        #[arg(value_enum)]
        language: CompileLanguage,
        /// Path to project root
        #[arg(long)]
        projectdir: Option<String>,
        /// Output file (defaults to output.{ext})
        #[arg(short, long)]
        output: Option<String>,
    },
    ///// Start development server for serving hop templates from a manifest
    //Dev {
    //    /// Path to project root
    //    #[arg(long)]
    //    projectdir: Option<String>,
    //    /// Port to serve on
    //    #[arg(short, long, default_value = "3000")]
    //    port: u16,
    //    /// Host to bind to
    //    #[arg(long, default_value = "127.0.0.1")]
    //    host: String,
    //    /// Directory to serve static files from
    //    #[arg(long)]
    //    staticdir: Option<String>,
    //    /// Optional script file name to make scripts available over HTTP
    //    #[arg(long)]
    //    scriptfile: Option<String>,
    //},
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
        Some(Commands::Lsp) => {
            cli::lsp::execute().await;
        }
        Some(Commands::Init { path }) => {
            let init_path = match path {
                Some(p) => Path::new(p),
                None => Path::new("."),
            };
            cli::init::execute(init_path)?;
            use colored::*;
            println!();
            println!("  {} | Initialized new hop project", "hop".bold());
            println!();
            println!("  Created build.hop with a basic template");
            println!();
            println!(
                "  Run {} to start the development server",
                "hop dev".green()
            );
            println!();
        }
        //Some(Commands::Build {
        //    projectdir,
        //    outdir,
        //    scriptfile,
        //    staticdir,
        //}) => {
        //    use std::time::Instant;
        //    let start_time = Instant::now();
        //    let root = match projectdir {
        //        Some(d) => ProjectRoot::from(Path::new(d))?,
        //        None => ProjectRoot::find_upwards(Path::new("."))?,
        //    };
        //    let mut outputs = cli::build::execute(
        //        &root,
        //        Path::new(outdir),
        //        scriptfile.as_deref(),
        //        staticdir.as_deref(),
        //    )?;
        //    let elapsed = start_time.elapsed();
        //
        //    print_header("built", elapsed.as_millis());
        //    outputs.sort_by(|a, b| a.0.cmp(&b.0));
        //    let mut total_size = 0;
        //    for (file_path, size) in outputs {
        //        println!("  {:<50} {}", file_path, format_file_size(size));
        //        total_size += size;
        //    }
        //    println!();
        //    println!("  {:<50} {}", "total", format_file_size(total_size));
        //    println!();
        //}
        Some(Commands::Compile {
            language,
            projectdir,
            output,
        }) => {
            use std::time::Instant;
            let start_time = Instant::now();

            // Determine output filename based on language if not specified
            let output_path = output.as_deref().unwrap_or(match language {
                CompileLanguage::Js => "output.js",
                CompileLanguage::Ts => "output.ts",
            });

            let result = cli::compile::execute(projectdir.as_deref(), output_path, language)?;
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
        //Some(Commands::Dev {
        //    projectdir,
        //    port,
        //    host,
        //    staticdir,
        //    scriptfile,
        //}) => {
        //    use colored::*;
        //    use std::time::Instant;
        //    let start_time = Instant::now();
        //    let root = match projectdir {
        //        Some(d) => ProjectRoot::from(Path::new(d))?,
        //        None => ProjectRoot::find_upwards(Path::new("."))?,
        //    };
        //    let (router, _watcher) = cli::dev::execute(
        //        &root,
        //        staticdir.as_deref().map(Path::new),
        //        scriptfile.as_deref(),
        //    )
        //    .await?;
        //    let elapsed = start_time.elapsed();
        //    let listener = tokio::net::TcpListener::bind(&format!("{}:{}", host, port)).await?;
        //
        //    print_header("ready", elapsed.as_millis());
        //    println!("  {} http://{}:{}/", "➜".green(), host, port);
        //    println!();
        //    axum::serve(listener, router).await?;
        //}
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
