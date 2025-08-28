use crate::common::Range;
use unicode_width::UnicodeWidthStr;

/// Trait for any annotation that can be displayed on source code
pub trait Annotation {
    fn range(&self) -> Range;
    fn message(&self) -> String;
}

/// Simple annotation implementation for basic use cases
pub struct SimpleAnnotation {
    pub range: Range,
    pub message: String,
}

impl Annotation for SimpleAnnotation {
    fn range(&self) -> Range {
        self.range
    }

    fn message(&self) -> String {
        self.message.clone()
    }
}

/// Annotator that can display source code with annotations
pub struct SourceAnnotator {
    // Display options
    pub show_line_numbers: bool,
    pub show_location: bool,
    pub lines_before: usize,
    pub lines_after: usize,

    // Style options
    pub underline_char: char,
    pub tab_width: usize,
    pub label: Option<String>,
    pub filename: Option<String>,
}

impl SourceAnnotator {
    pub fn new() -> Self {
        Self {
            show_line_numbers: true,
            show_location: false,
            lines_before: 0,
            lines_after: 0,
            underline_char: '^',
            tab_width: 4,
            label: None,
            filename: None,
        }
    }

    pub fn with_underline(mut self, ch: char) -> Self {
        self.underline_char = ch;
        self
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    pub fn with_lines_before(mut self, n: usize) -> Self {
        self.lines_before = n;
        self
    }

    pub fn with_lines_after(mut self, n: usize) -> Self {
        self.lines_after = n;
        self
    }

    pub fn with_filename(mut self, filename: impl Into<String>) -> Self {
        self.filename = Some(filename.into());
        self
    }

    pub fn with_location(mut self) -> Self {
        self.show_location = true;
        self
    }

    pub fn without_location(mut self) -> Self {
        self.show_location = false;
        self
    }

    pub fn without_line_numbers(mut self) -> Self {
        self.show_line_numbers = false;
        self
    }

    pub fn annotate<A: Annotation>(&self, source: &str, annotations: &[A]) -> String {
        let mut output = String::new();
        let lines: Vec<&str> = source.lines().collect();

        for (i, annotation) in annotations.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }

            let range = annotation.range();

            // Add label and message
            if let Some(ref label) = self.label {
                output.push_str(&format!("{}: {}\n", label, annotation.message()));
            } else {
                output.push_str(&format!("{}\n", annotation.message()));
            }

            // Add location info
            if self.show_location {
                if let Some(ref filename) = self.filename {
                    output.push_str(&format!(
                        "  --> {} (line {}, col {})\n",
                        filename, range.start.line, range.start.column
                    ));
                } else {
                    output.push_str(&format!(
                        "  --> (line {}, col {})\n",
                        range.start.line, range.start.column
                    ));
                }
            }

            // Add source context
            self.add_source_context(&mut output, &lines, range);
        }

        output
    }

    fn add_source_context(&self, output: &mut String, lines: &[&str], range: Range) {
        let start_line = range.start.line;
        let max_line_num_width = if self.show_line_numbers {
            lines.len().to_string().len()
        } else {
            0
        };

        // Calculate which lines to show before
        let first_line = if start_line > self.lines_before {
            start_line - self.lines_before
        } else {
            1
        };

        // Show lines before
        for line_num in first_line..start_line {
            if let Some(line) = lines.get(line_num - 1) {
                self.format_line(output, line_num, line, max_line_num_width, false);
            }
        }

        // Show the annotated line(s)
        if let Some(line) = lines.get(start_line - 1) {
            self.format_line(output, start_line, line, max_line_num_width, false);

            // Add the underline
            if self.show_line_numbers {
                output.push_str(&" ".repeat(max_line_num_width));
                output.push_str(" | ");
            }

            let display_start = self.byte_to_display_position(line, range.start.column);
            let display_end = if range.start.line == range.end.line {
                self.byte_to_display_position(line, range.end.column)
            } else {
                self.display_width(line) + 1
            };

            output.push_str(&" ".repeat(display_start.saturating_sub(1)));
            let underline_length = display_end.saturating_sub(display_start).max(1);
            output.push_str(&self.underline_char.to_string().repeat(underline_length));
            output.push('\n');
        }

        // Show lines after
        let last_line = (start_line + self.lines_after).min(lines.len());
        for line_num in (start_line + 1)..=last_line {
            if let Some(line) = lines.get(line_num - 1) {
                self.format_line(output, line_num, line, max_line_num_width, false);
            }
        }
    }

    fn format_line(
        &self,
        output: &mut String,
        line_num: usize,
        content: &str,
        width: usize,
        _highlight: bool,
    ) {
        if self.show_line_numbers {
            output.push_str(&format!("{:width$} | ", line_num, width = width));
        }
        output.push_str(&self.expand_tabs(content));
        output.push('\n');
    }

    fn byte_to_display_position(&self, line: &str, byte_column: usize) -> usize {
        // byte_column is 1-based, convert to 0-based byte offset
        let target_byte_offset = byte_column.saturating_sub(1);

        // If byte offset is beyond the line, return display width + 1
        if target_byte_offset >= line.len() {
            return self.display_width(line) + 1;
        }

        // Calculate display width of the substring up to the byte offset
        let substring = &line[..target_byte_offset];
        self.display_width(substring) + 1 // +1 for 1-based indexing
    }

    fn display_width(&self, text: &str) -> usize {
        let mut width = 0;
        for ch in text.chars() {
            if ch == '\t' {
                width += self.tab_width;
            } else {
                width += UnicodeWidthStr::width(ch.to_string().as_str());
            }
        }
        width
    }

    fn expand_tabs(&self, text: &str) -> String {
        let tab_replacement = " ".repeat(self.tab_width);
        text.replace('\t', &tab_replacement)
    }
}

impl Default for SourceAnnotator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::Position;

    #[test]
    fn test_simple_annotation() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(2, 15), Position::new(2, 19)),
            message: "Test error".to_string(),
        };

        let output = SourceAnnotator::new().annotate("there is\n    some code here", &[annotation]);

        let expected = r#"
Test error
2 |     some code here
  |               ^^^^
"#
        .trim_start();
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_label() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(1, 1), Position::new(1, 5)),
            message: "Missing semicolon".to_string(),
        };

        let output = SourceAnnotator::new()
            .with_label("error")
            .annotate("code", &[annotation]);

        let expected = r#"
error: Missing semicolon
1 | code
  | ^^^^
"#
        .trim_start();
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_custom_underline() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(1, 1), Position::new(1, 5)),
            message: "Warning".to_string(),
        };

        let output = SourceAnnotator::new()
            .with_underline('~')
            .annotate("code", &[annotation]);

        let expected = r#"
Warning
1 | code
  | ~~~~
"#
        .trim_start();
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_location_info() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(2, 8), Position::new(2, 12)),
            message: "Type error".to_string(),
        };

        let output = SourceAnnotator::new()
            .with_location()
            .with_filename("main.rs")
            .annotate("first line\nsecond line", &[annotation]);

        let expected = r#"
Type error
  --> main.rs (line 2, col 8)
2 | second line
  |        ^^^^
"#
        .trim_start();
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_lines_before() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(3, 6), Position::new(3, 11)),
            message: "Error here".to_string(),
        };

        let output = SourceAnnotator::new()
            .with_lines_before(2)
            .annotate("line one\nline two\nline three\nline four", &[annotation]);

        let expected = r#"
Error here
1 | line one
2 | line two
3 | line three
  |      ^^^^^
"#
        .trim_start();
        assert_eq!(output, expected);
    }

    #[test]
    fn test_with_lines_after() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(2, 6), Position::new(2, 9)),
            message: "Error here".to_string(),
        };

        let source = "line one\nline two\nline three\nline four";
        let actual = SourceAnnotator::new()
            .with_lines_after(2)
            .annotate(source, &[annotation]);

        let expected = r#"
Error here
2 | line two
  |      ^^^
3 | line three
4 | line four
"#
        .trim_start();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_tab_expansion() {
        let actual = SourceAnnotator::new().with_lines_before(1).annotate(
            "code\n\tcode",
            &[SimpleAnnotation {
                range: Range::new(Position::new(2, 2), Position::new(2, 6)),
                message: "Tab test".to_string(),
            }],
        );

        // Tab should be expanded to 4 spaces
        let expected = r#"
Tab test
1 | code
2 |     code
  |     ^^^^
"#
        .trim_start();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_multiple_annotations() {
        let annotations = vec![
            SimpleAnnotation {
                range: Range::new(Position::new(1, 6), Position::new(1, 9)),
                message: "First error".to_string(),
            },
            SimpleAnnotation {
                range: Range::new(Position::new(3, 6), Position::new(3, 11)),
                message: "Second error".to_string(),
            },
        ];

        let source = "line one\nline two\nline three";
        let actual = SourceAnnotator::new().annotate(source, &annotations);

        let expected = r#"
First error
1 | line one
  |      ^^^

Second error
3 | line three
  |      ^^^^^
"#
        .trim_start();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_unicode_emoji_width() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(1, 6), Position::new(1, 10)),
            message: "After emoji".to_string(),
        };

        // Emoji is 4 bytes (positions 1-4), space is 1 byte (position 5)
        // "code" starts at byte position 6
        let actual = SourceAnnotator::new().annotate("😀 code", &[annotation]);

        // The underline should align correctly considering emoji takes 2 display columns
        let expected = r#"
After emoji
1 | 😀 code
  |    ^^^^
"#
        .trim_start();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_location_without_filename() {
        let annotation = SimpleAnnotation {
            range: Range::new(Position::new(1, 6), Position::new(1, 10)),
            message: "Error without filename".to_string(),
        };

        let actual = SourceAnnotator::new()
            .with_location()
            .annotate("some code", &[annotation]);

        let expected = r#"
Error without filename
  --> (line 1, col 6)
1 | some code
  |      ^^^^
"#
        .trim_start();
        assert_eq!(actual, expected);
    }
}
