pub mod timing;

use colored::Colorize;

pub fn print_header(message: &str) {
    println!();
    println!("  {} | {}", "hop".bold(), message);
    println!();
}
