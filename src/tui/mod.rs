pub mod log;
pub mod timing;

use colored::Colorize;

pub fn print_header(action: &str, elapsed: u128) {
    println!();
    println!("  {} | {} in {} ms", "hop".bold(), action, elapsed);
    println!();
}
