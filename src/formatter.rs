use crate::common::{Range, RangeError};
use unicode_width::UnicodeWidthStr;

pub struct ErrorFormatter {
    lines: Vec<String>,
    filename: String,
}

impl ErrorFormatter {
    pub fn new(source_code: String, filename: String) -> Self {
        let lines = source_code.lines().map(|s| s.to_string()).collect();
        Self { lines, filename }
    }

    pub fn format_error(&self, error: &RangeError) -> String {
        let mut output = String::new();

        // Error header
        output.push_str(&format!("error: {}\n", error.message));

        let range = error.range;

        // Location info
        output.push_str(&format!(
            "  --> {} (line {}, col {})\n",
            self.filename, range.start.line, range.start.column
        ));

        // Source code context with surrounding lines
        self.format_source_context(&mut output, range);

        output
    }

    fn format_source_context(&self, output: &mut String, range: Range) {
        let error_line = range.start.line;
        let max_line_width = self.lines.len().to_string().len();

        // Show one line before (if exists)
        if error_line > 1 {
            if let Some(prev_line) = self.lines.get(error_line - 2) {
                output.push_str(&format!(
                    "{:width$} | {}\n",
                    error_line - 1,
                    prev_line,
                    width = max_line_width
                ));
            }
        }

        // Show the error line
        if let Some(line_content) = self.lines.get(error_line - 1) {
            output.push_str(&format!(
                "{:width$} | {}\n",
                error_line,
                line_content,
                width = max_line_width
            ));

            // Underline the error
            let mut underline = String::new();
            underline.push_str(&" ".repeat(max_line_width));
            underline.push_str(" | ");

            // Convert byte positions to display positions for proper alignment
            let display_start = self.byte_to_display_position(line_content, range.start.column);
            let display_end = if range.start.line == range.end.line {
                self.byte_to_display_position(line_content, range.end.column)
            } else {
                line_content.width()
            };

            // Add spaces to align with the error position
            underline.push_str(&" ".repeat(display_start - 1));

            // Add carets for the error span
            let span_length = display_end - display_start;
            underline.push_str(&"^".repeat(span_length.max(1)));

            output.push_str(&underline);
            output.push('\n');
        }
    }

    fn byte_to_display_position(&self, line: &str, byte_column: usize) -> usize {
        // byte_column is 1-based, convert to 0-based byte offset
        let target_byte_offset = byte_column - 1;

        // If byte offset is beyond the line, return display width + 1
        if target_byte_offset >= line.len() {
            return line.width() + 1;
        }

        // Calculate display width of the substring up to the byte offset
        let substring = &line[..target_byte_offset];
        substring.width() + 1 // +1 for 1-based indexing
    }
}
