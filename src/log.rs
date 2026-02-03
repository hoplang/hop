//! Structured logging utilities for the cli.
//!
//! Provides macros for logging messages in logfmt format with automatic timestamps
//! and colored output. Available macros: [`log_info!`], [`log_debug!`], [`log_warn!`],
//! and [`log_error!`].
//!
//! # Example
//!
//! ```ignore
//! log_info!("server", port = 8080, status = "running");
//! // Output: time=12:34:56.789 level=info tag=server port=8080 status=running
//! ```

use colored::Colorize;
use std::time::SystemTime;

pub fn format_timestamp() -> String {
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    let secs = now.as_secs() % 86400;
    let hours = (secs / 3600) % 24;
    let minutes = (secs % 3600) / 60;
    let seconds = secs % 60;
    let millis = now.subsec_millis();
    format!("{:02}:{:02}:{:02}.{:03}", hours, minutes, seconds, millis)
}

pub fn format_logfmt(level: &str, tag: &str, fields: &[(&str, String)]) -> String {
    let mut parts = vec![
        format!("{}={}", "time".dimmed(), format_timestamp()),
        format!("{}={}", "level".dimmed(), level),
        format!("{}={}", "tag".dimmed(), tag),
    ];
    for (key, value) in fields {
        parts.push(format!("{}={}", key.dimmed(), value));
    }
    parts.join(" ")
}

#[macro_export]
macro_rules! log_info {
    ($tag:expr, $($key:ident = $value:expr),* $(,)?) => {{
        use colored::Colorize;
        eprintln!("{}", $crate::log::format_logfmt(
            &"info".cyan().to_string(),
            $tag,
            &[$(( stringify!($key), format!("{}", $value) )),*]
        ))
    }};
}

#[macro_export]
macro_rules! log_debug {
    ($tag:expr, $($key:ident = $value:expr),* $(,)?) => {{
        use colored::Colorize;
        eprintln!("{}", $crate::log::format_logfmt(
            &"debug".dimmed().to_string(),
            $tag,
            &[$(( stringify!($key), format!("{}", $value) )),*]
        ))
    }};
}

#[macro_export]
macro_rules! log_warn {
    ($tag:expr, $($key:ident = $value:expr),* $(,)?) => {{
        use colored::Colorize;
        eprintln!("{}", $crate::log::format_logfmt(
            &"warn".yellow().to_string(),
            $tag,
            &[$(( stringify!($key), format!("{}", $value) )),*]
        ))
    }};
}

#[macro_export]
macro_rules! log_error {
    ($tag:expr, $($key:ident = $value:expr),* $(,)?) => {{
        use colored::Colorize;
        eprintln!("{}", $crate::log::format_logfmt(
            &"error".red().to_string(),
            $tag,
            &[$(( stringify!($key), format!("{}", $value) )),*]
        ))
    }};
}
