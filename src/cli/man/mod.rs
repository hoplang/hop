use std::io::{self, IsTerminal, Write};
use std::process::{Command, Stdio};

const MANUAL: &str = include_str!("man.md");

pub fn execute() {
    let output = crate::markdown::render(MANUAL);

    if io::stdout().is_terminal() {
        if let Ok(mut child) = Command::new("less")
            .arg("-R")
            .stdin(Stdio::piped())
            .spawn()
        {
            if let Some(mut stdin) = child.stdin.take() {
                let _ = stdin.write_all(output.as_bytes());
            }
            let _ = child.wait();
            return;
        }
    }

    print!("{output}");
}
