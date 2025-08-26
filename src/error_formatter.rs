use crate::common::{Range, RangeError};
use unicode_width::UnicodeWidthStr;

#[derive(Debug)]
struct ModuleInfo {
    module_name: String,
    lines: Vec<String>,
    filename: String,
    errors: Vec<RangeError>,
}

pub struct ErrorFormatter {
    modules: Vec<ModuleInfo>,
    show_location: bool,
}

impl ErrorFormatter {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            show_location: true,
        }
    }

    pub fn add_errors(
        &mut self,
        module_name: String,
        source_code: String,
        errors: Vec<RangeError>,
    ) {
        if errors.is_empty() {
            panic!("add_errors called with empty errors array");
        }

        let lines = source_code.lines().map(|s| s.to_string()).collect();
        let filename = format!("{}.hop", module_name);

        if let Some(module_info) = self
            .modules
            .iter_mut()
            .find(|m| m.module_name == module_name)
        {
            module_info.errors.extend(errors);
        } else {
            self.modules.push(ModuleInfo {
                module_name,
                lines,
                filename,
                errors,
            });
        }
    }

    pub fn without_location_info(mut self) -> Self {
        self.show_location = false;
        self
    }

    pub fn has_errors(&self) -> bool {
        !self.modules.is_empty()
    }

    pub fn format_all_errors(&self) -> String {
        let mut formatted_errors = String::new();
        let mut first_module = true;

        for module_info in &self.modules {
            if module_info.errors.is_empty() {
                continue;
            }

            if !first_module {
                formatted_errors.push('\n');
            }
            first_module = false;

            for (i, error) in module_info.errors.iter().enumerate() {
                formatted_errors.push_str(&self.format_error(module_info, error));
                // Only add newline if there are more errors after this one
                if i < module_info.errors.len() - 1 {
                    formatted_errors.push('\n');
                }
            }
        }
        formatted_errors
    }

    fn format_error(&self, module_info: &ModuleInfo, error: &RangeError) -> String {
        let mut output = String::new();

        // Error header
        output.push_str(&format!("error: {}\n", error.message));

        let range = error.range;

        // Location info
        if self.show_location {
            output.push_str(&format!(
                "  --> {} (line {}, col {})\n",
                module_info.filename, range.start.line, range.start.column
            ));
        }

        // Source code context with surrounding lines
        self.format_source_context(&mut output, module_info, range);

        output
    }

    fn format_source_context(&self, output: &mut String, module_info: &ModuleInfo, range: Range) {
        let error_line = range.start.line;
        let max_line_width = module_info.lines.len().to_string().len();

        // Show one line before (if exists)
        if error_line > 1 {
            if let Some(prev_line) = module_info.lines.get(error_line - 2) {
                output.push_str(&format!(
                    "{:width$} | {}\n",
                    error_line - 1,
                    self.expand_tabs(prev_line),
                    width = max_line_width
                ));
            }
        }

        // Show the error line
        if let Some(line_content) = module_info.lines.get(error_line - 1) {
            output.push_str(&format!(
                "{:width$} | {}\n",
                error_line,
                self.expand_tabs(line_content),
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
                self.display_width_with_tabs(line_content)
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
            return self.display_width_with_tabs(line) + 1;
        }

        // Calculate display width of the substring up to the byte offset
        let substring = &line[..target_byte_offset];
        self.display_width_with_tabs(substring) + 1 // +1 for 1-based indexing
    }

    fn display_width_with_tabs(&self, text: &str) -> usize {
        let mut width = 0;
        for ch in text.chars() {
            if ch == '\t' {
                // Tab width is 4 spaces
                width += 4;
            } else {
                width += UnicodeWidthStr::width(ch.to_string().as_str());
            }
        }
        width
    }

    fn expand_tabs(&self, text: &str) -> String {
        text.replace('\t', "    ")
    }
}
