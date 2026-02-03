/// Escape HTML special characters to prevent XSS
/// Converts &, <, >, ", and ' to their HTML entity equivalents
pub fn write_escaped_html(text: &str, output: &mut String) {
    for c in text.chars() {
        match c {
            '&' => output.push_str("&amp;"),
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            '"' => output.push_str("&quot;"),
            '\'' => output.push_str("&#39;"),
            _ => output.push(c),
        }
    }
}

/// Return true if the string represents a void element
/// The void elements are `area`, `base`, `br`, `col`, `embed`, `hr`,
/// `img`, `input`, `link`, `meta`, `param`, `source`, `track` and `wbr`.
pub fn is_void_element(el: &str) -> bool {
    matches!(
        el,
        "area"
            | "base"
            | "br"
            | "col"
            | "embed"
            | "hr"
            | "img"
            | "input"
            | "link"
            | "meta"
            | "param"
            | "source"
            | "track"
            | "wbr"
    )
}
