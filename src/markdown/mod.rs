use colored::Colorize;

const MAX_WIDTH: usize = 80;

pub fn render(markdown: &str) -> String {
    let mut out = String::new();
    let mut in_code_block = false;

    for line in markdown.lines() {
        if line.starts_with("```") {
            in_code_block = !in_code_block;
            if in_code_block {
                out.push('\n');
            }
            continue;
        }

        if in_code_block {
            out.push_str(&format!("    {}\n", line.dimmed()));
            continue;
        }

        if let Some(heading) = line.strip_prefix("### ") {
            out.push_str(&format!("\n  {}\n", heading.bold()));
            continue;
        }

        if let Some(heading) = line.strip_prefix("## ") {
            out.push_str(&format!("\n  {}\n", heading.bold().underline()));
            continue;
        }

        if let Some(heading) = line.strip_prefix("# ") {
            out.push_str(&format!("\n  {}\n", heading.bold().underline()));
            continue;
        }

        if line.is_empty() {
            out.push('\n');
            continue;
        }

        if let Some(content) = line.strip_prefix("- ") {
            let wrapped = wrap_text(content, MAX_WIDTH - 4);
            for (i, wline) in wrapped.iter().enumerate() {
                let formatted = render_inline(wline);
                if i == 0 {
                    out.push_str(&format!("  - {formatted}\n"));
                } else {
                    out.push_str(&format!("    {formatted}\n"));
                }
            }
            continue;
        }

        let wrapped = wrap_text(line, MAX_WIDTH - 2);
        for wline in &wrapped {
            let formatted = render_inline(wline);
            out.push_str(&format!("  {formatted}\n"));
        }
    }
    out.push('\n');
    out
}

fn wrap_text(text: &str, max_width: usize) -> Vec<String> {
    let words: Vec<&str> = text.split_whitespace().collect();
    if words.is_empty() {
        return vec![String::new()];
    }

    let mut lines = Vec::new();
    let mut current = String::new();
    let mut current_width: usize = 0;

    for word in &words {
        let w = visible_width(word);
        if current_width > 0 && current_width + 1 + w > max_width {
            lines.push(current);
            current = word.to_string();
            current_width = w;
        } else {
            if current_width > 0 {
                current.push(' ');
                current_width += 1;
            }
            current.push_str(word);
            current_width += w;
        }
    }
    if !current.is_empty() {
        lines.push(current);
    }

    lines
}

fn visible_width(text: &str) -> usize {
    let mut width = 0;
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '`' {
            for c in chars.by_ref() {
                if c == '`' {
                    break;
                }
                width += 1;
            }
        } else if ch == '*' && chars.peek() == Some(&'*') {
            chars.next();
        } else {
            width += 1;
        }
    }
    width
}

fn render_inline(line: &str) -> String {
    let mut out = String::new();
    let mut chars = line.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '`' {
            let mut code = String::new();
            for c in chars.by_ref() {
                if c == '`' {
                    break;
                }
                code.push(c);
            }
            out.push_str(&format!("{}", code.dimmed()));
        } else if ch == '*' && chars.peek() == Some(&'*') {
            chars.next();
            let mut bold = String::new();
            loop {
                match chars.next() {
                    Some('*') if chars.peek() == Some(&'*') => {
                        chars.next();
                        break;
                    }
                    Some(c) => bold.push(c),
                    None => break,
                }
            }
            out.push_str(&format!("{}", bold.bold()));
        } else {
            out.push(ch);
        }
    }

    out
}

