use std::cmp;
use std::fmt::Display;

use itertools::Itertools as _;

use super::document_cursor::{DocumentCursor, DocumentRange, Ranged};

/// Annotator that can display source code with annotations
pub struct DocumentAnnotator {
    // Display options
    pub show_line_numbers: bool,
    pub show_location: bool,
    pub lines_before: usize,
    pub lines_after: usize,

    // Style options
    pub underline_char: char,
    pub tab_width: usize,
    pub label: Option<String>,
}

impl DocumentAnnotator {
    pub fn new() -> Self {
        Self {
            show_line_numbers: true,
            show_location: false,
            lines_before: 0,
            lines_after: 0,
            underline_char: '^',
            tab_width: 4,
            label: None,
        }
    }

    pub fn with_label(mut self, label: impl Into<String>) -> Self {
        self.label = Some(label.into());
        self
    }

    pub fn with_lines_before(mut self, n: usize) -> Self {
        self.lines_before = n;
        self
    }

    #[cfg(test)]
    pub fn with_lines_after(mut self, n: usize) -> Self {
        self.lines_after = n;
        self
    }

    pub fn with_location(mut self) -> Self {
        self.show_location = true;
        self
    }

    #[cfg(test)]
    pub fn without_location(mut self) -> Self {
        self.show_location = false;
        self
    }

    #[cfg(test)]
    pub fn without_line_numbers(mut self) -> Self {
        self.show_line_numbers = false;
        self
    }

    pub fn annotate<A>(
        &self,
        filename: Option<&str>,
        annotations: impl IntoIterator<Item = A>,
    ) -> String
    where
        A: Display + Ranged,
    {
        let annotations: Vec<A> = annotations.into_iter().collect();

        // Extract source from the first annotation's range
        let source = match annotations.first() {
            Some(first) => first.range().full_source(),
            None => return String::new(),
        };

        // Debug assert that all annotations are from the same source
        for annotation in &annotations {
            debug_assert!(
                std::ptr::eq(annotation.range().full_source(), source),
                "All annotations must be from the same source document"
            );
        }

        let mut output = String::new();
        let lines: Vec<Option<DocumentRange>> = DocumentCursor::new(source.to_string())
            .chunk_by(|range| range.start_utf32().line())
            .into_iter()
            .map(|(_, group)| group.filter(|s| s.ch() != '\n').collect())
            .collect();

        for (i, annotation) in annotations.into_iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }

            if let Some(ref label) = self.label {
                output.push_str(&format!("{}: {}\n", label, annotation));
            } else {
                output.push_str(&format!("{}\n", annotation));
            }

            if self.show_location {
                if let Some(filename) = filename {
                    output.push_str(&format!(
                        "  --> {} (line {}, col {})\n",
                        filename,
                        annotation.range().start_utf32().line() + 1,
                        annotation.range().start_utf32().column() + 1
                    ));
                } else {
                    output.push_str(&format!(
                        "  --> (line {}, col {})\n",
                        annotation.range().start_utf32().line() + 1,
                        annotation.range().start_utf32().column() + 1
                    ));
                }
            }

            self.format_annotation(&mut output, &lines, annotation.range());
        }

        output
    }

    fn format_annotation(
        &self,
        output: &mut String,
        lines: &[Option<DocumentRange>],
        range: &DocumentRange,
    ) {
        let max_line_col_width = lines.len().to_string().len();

        let first_line = range.start_utf32().line().saturating_sub(self.lines_before);
        let last_line = cmp::min(lines.len() - 1, range.end_utf32().line() + self.lines_after);

        for (i, line) in lines.iter().enumerate() {
            if i < first_line || i > last_line {
                continue;
            }

            // Write line content
            if self.show_line_numbers {
                output.push_str(&format!("{:width$} | ", i + 1, width = max_line_col_width));
            }
            if let Some(line) = line {
                output.push_str(&self.expand_tabs(line.as_str()));
            }
            output.push('\n');
            // Write annotation line
            if let Some(line) = line {
                if let Some(intersection) = line.intersection(range) {
                    let mut has_written_annotation = false;
                    if self.show_line_numbers {
                        output.push_str(&format!("{:width$} | ", "", width = max_line_col_width));
                    }
                    for range in line.cursor() {
                        if intersection.contains(&range) {
                            has_written_annotation = true;
                            output.push_str(
                                &self
                                    .underline_char
                                    .to_string()
                                    .repeat(self.char_display_width(range.ch())),
                            );
                        } else if !has_written_annotation {
                            output.push_str(&" ".repeat(self.char_display_width(range.ch())));
                        }
                    }
                    output.push('\n');
                }
            }
        }
    }

    fn char_display_width(&self, ch: char) -> usize {
        use unicode_width::UnicodeWidthChar;
        if ch == '\t' {
            self.tab_width
        } else {
            ch.width().unwrap_or(0)
        }
    }

    fn expand_tabs(&self, text: &str) -> String {
        let tab_replacement = " ".repeat(self.tab_width);
        text.replace('\t', &tab_replacement)
    }
}

impl Default for DocumentAnnotator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::expect;

    fn create_annotations_from_chunks(
        source: &str,
        predicate: impl Fn(char) -> bool,
    ) -> Vec<DocumentRange> {
        DocumentCursor::new(source.to_string())
            .chunk_by(|range| predicate(range.ch()))
            .into_iter()
            .filter_map(
                |(is_separator, group)| {
                    if !is_separator { group.collect() } else { None }
                },
            )
            .collect()
    }

    #[test]
    fn with_label() {
        let source = "line one\nline two\nline three\nline four";

        let annotations = create_annotations_from_chunks(source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_label("error")
            .annotate(None, annotations);

        expect![[r#"
            error: line one
            1 | line one
              | ^^^^^^^^

            error: line two
            2 | line two
              | ^^^^^^^^

            error: line three
            3 | line three
              | ^^^^^^^^^^

            error: line four
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn with_location_info() {
        let source = "line one\nline two\nline three\nline four";

        let annotations = create_annotations_from_chunks(source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(Some("main.rs"), annotations);

        expect![[r#"
            line one
              --> main.rs (line 1, col 1)
            1 | line one
              | ^^^^^^^^

            line two
              --> main.rs (line 2, col 1)
            2 | line two
              | ^^^^^^^^

            line three
              --> main.rs (line 3, col 1)
            3 | line three
              | ^^^^^^^^^^

            line four
              --> main.rs (line 4, col 1)
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn with_lines_before() {
        let source = "line one\nline two\nline three\nline four";

        let annotations = create_annotations_from_chunks(source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_lines_before(2)
            .annotate(None, annotations);

        expect![[r#"
            line one
            1 | line one
              | ^^^^^^^^

            line two
            1 | line one
            2 | line two
              | ^^^^^^^^

            line three
            1 | line one
            2 | line two
            3 | line three
              | ^^^^^^^^^^

            line four
            2 | line two
            3 | line three
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn with_lines_after() {
        let source = "line one\nline two\nline three\nline four";

        let annotations = create_annotations_from_chunks(source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_lines_after(2)
            .annotate(None, annotations);

        expect![[r#"
            line one
            1 | line one
              | ^^^^^^^^
            2 | line two
            3 | line three

            line two
            2 | line two
              | ^^^^^^^^
            3 | line three
            4 | line four

            line three
            3 | line three
              | ^^^^^^^^^^
            4 | line four

            line four
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn tab_expansion() {
        let source = "code\n\t\tcode\n\tcode";

        let annotations = create_annotations_from_chunks(source, |ch| ch.is_whitespace());

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(None, annotations);

        expect![[r#"
            code
              --> (line 1, col 1)
            1 | code
              | ^^^^

            code
              --> (line 2, col 3)
            2 |         code
              |         ^^^^

            code
              --> (line 3, col 2)
            3 |     code
              |     ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn unicode_emoji_width() {
        let source = "😀 code";

        let annotations = create_annotations_from_chunks(source, |ch| ch.is_whitespace());

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(None, annotations);

        expect![[r#"
            😀
              --> (line 1, col 1)
            1 | 😀 code
              | ^^

            code
              --> (line 1, col 3)
            1 | 😀 code
              |    ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn location_without_filename() {
        let source = "some code";

        let annotations = create_annotations_from_chunks(source, |ch| ch.is_whitespace());

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(None, annotations);

        expect![[r#"
            some
              --> (line 1, col 1)
            1 | some code
              | ^^^^

            code
              --> (line 1, col 6)
            1 | some code
              |      ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn lines_before_exceeds_start() {
        let source = "line one\nline two\nline three\nline four\nline five\nline six";

        let annotations = create_annotations_from_chunks(source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_lines_before(1000)
            .annotate(None, annotations);

        expect![[r#"
            line one
            1 | line one
              | ^^^^^^^^

            line two
            1 | line one
            2 | line two
              | ^^^^^^^^

            line three
            1 | line one
            2 | line two
            3 | line three
              | ^^^^^^^^^^

            line four
            1 | line one
            2 | line two
            3 | line three
            4 | line four
              | ^^^^^^^^^

            line five
            1 | line one
            2 | line two
            3 | line three
            4 | line four
            5 | line five
              | ^^^^^^^^^

            line six
            1 | line one
            2 | line two
            3 | line three
            4 | line four
            5 | line five
            6 | line six
              | ^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn multi_line_annotation() {
        let source = "line one\nline two\nline three\nline four\nline five";

        let annotations = create_annotations_from_chunks(source, |ch| ch == 'n');

        let actual = DocumentAnnotator::new().annotate(None, annotations);

        expect![[r#"
            li
            1 | line one
              | ^^

            e o
            1 | line one
              |    ^^^

            e
            li
            1 | line one
              |        ^
            2 | line two
              | ^^

            e two
            li
            2 | line two
              |    ^^^^^
            3 | line three
              | ^^

            e three
            li
            3 | line three
              |    ^^^^^^^
            4 | line four
              | ^^

            e four
            li
            4 | line four
              |    ^^^^^^
            5 | line five
              | ^^

            e five
            5 | line five
              |    ^^^^^^
        "#]]
        .assert_eq(&actual);
    }
}
