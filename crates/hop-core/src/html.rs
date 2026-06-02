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

/// Return true if the string represents a void element.
/// See https://developer.mozilla.org/en-US/docs/Glossary/Void_element
pub fn is_void_element(tag_name: &str) -> bool {
    matches!(
        tag_name,
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
