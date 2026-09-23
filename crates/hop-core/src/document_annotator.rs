use std::cmp;
use std::collections::BTreeMap;

use crate::itertools::ChunkByExt as _;

use crate::{
    diagnostic::Diagnostic,
    diagnostic_severity::DiagnosticSeverity,
    document::{DocumentCursor, DocumentRange},
    root_contained_file_path::RootContainedFilePath,
};

/// A printer that displays the source code of a [Document](crate::Document)
/// annotated with [Diagnostics](crate::Diagnostic).
pub struct DocumentAnnotator {
    // Display options
    show_line_numbers: bool,
    show_location: bool,
    show_severity_label: bool,
    lines_before: usize,
    lines_after: usize,

    // Style options
    underline_char: char,
    tab_width: usize,

    diagnostics: BTreeMap<RootContainedFilePath, Vec<Diagnostic>>,
}

impl DocumentAnnotator {
    pub fn new() -> Self {
        Self {
            show_line_numbers: true,
            show_location: false,
            show_severity_label: false,
            lines_before: 0,
            lines_after: 0,
            underline_char: '^',
            tab_width: 4,
            diagnostics: BTreeMap::new(),
        }
    }

    /// Prefix every message with its severity, as in `error: ...`.
    pub fn with_severity_label(mut self) -> Self {
        self.show_severity_label = true;
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

    pub fn annotate(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) -> &mut Self {
        for diagnostic in diagnostics {
            self.diagnostics
                .entry(diagnostic.range().document_id().clone())
                .or_default()
                .push(diagnostic);
        }
        self
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn render(&self) -> String {
        let mut output = String::new();
        let mut first_annotation = true;

        for (document_id, diagnostics) in &self.diagnostics {
            // Build lines vector for this document using the first diagnostic's source
            let first_range = match diagnostics.first() {
                Some(d) => d.range(),
                None => continue,
            };

            let range_doc_id = first_range.document_id().clone();
            let source = first_range.full_source().to_string();
            let lines: Vec<Option<DocumentRange>> = DocumentCursor::new(range_doc_id, source)
                .chunk_by(|range| range.start_position().line())
                .into_iter()
                .map(|(_, group)| group.filter(|s| s.ch() != '\n').collect())
                .collect();

            // Sort diagnostics by (start, end) within this document
            let mut sorted: Vec<&Diagnostic> = diagnostics.iter().collect();
            sorted.sort_by_key(|d| (d.range().start(), d.range().end()));

            for diagnostic in sorted {
                if !first_annotation {
                    output.push('\n');
                }
                first_annotation = false;

                if self.show_severity_label {
                    let label = match diagnostic.severity() {
                        DiagnosticSeverity::Error => "error",
                        DiagnosticSeverity::Warning => "warning",
                    };
                    output.push_str(&format!("{}: {}\n", label, diagnostic.message()));
                } else {
                    output.push_str(&format!("{}\n", diagnostic.message()));
                }

                if self.show_location {
                    output.push_str(&format!(
                        "  --> {} (line {}, col {})\n",
                        document_id.as_str(),
                        diagnostic.range().start_position().line() + 1,
                        diagnostic.range().start_position().utf32_column() + 1
                    ));
                }

                self.format_annotation(&mut output, &lines, diagnostic.range());
            }
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

        // An empty range marks a position between two characters rather than a
        // span, and is drawn as a single caret at that column.
        let anchor_line = cmp::min(range.start_position().line(), lines.len() - 1);
        let (first_line, last_line) = if range.is_empty() {
            (
                anchor_line.saturating_sub(self.lines_before),
                cmp::min(lines.len() - 1, anchor_line + self.lines_after),
            )
        } else {
            (
                range
                    .start_position()
                    .line()
                    .saturating_sub(self.lines_before),
                cmp::min(
                    lines.len() - 1,
                    range.end_position().line() + self.lines_after,
                ),
            )
        };

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
            if range.is_empty() {
                if i == anchor_line {
                    if self.show_line_numbers {
                        output.push_str(&format!("{:width$} | ", "", width = max_line_col_width));
                    }
                    let indent: usize = line
                        .iter()
                        .flat_map(|line| line.cursor())
                        .take_while(|ch_range| ch_range.start() < range.start())
                        .map(|ch_range| self.char_display_width(ch_range.ch()))
                        .sum();
                    output.push_str(&" ".repeat(indent));
                    output.push(self.underline_char);
                    output.push('\n');
                }
            } else if let Some(line) = line {
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
    use crate::root_contained_file_path::RootContainedFilePath;
    use expect_test::expect;

    fn create_annotations_from_chunks(
        doc_id: RootContainedFilePath,
        source: &str,
        predicate: impl Fn(char) -> bool,
    ) -> Vec<Diagnostic> {
        DocumentCursor::new(doc_id, source.to_string())
            .chunk_by(|range| predicate(range.ch()))
            .into_iter()
            .filter_map(|(is_separator, group)| {
                if !is_separator {
                    let range: Option<DocumentRange> = group.collect();
                    range.map(|range| Diagnostic {
                        message: range.as_str().to_string(),
                        range,
                        severity: DiagnosticSeverity::Error,
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    #[test]
    fn with_severity_label() {
        let source = "line one\nline two\nline three\nline four";

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch == '\n',
        );

        let actual = DocumentAnnotator::new()
            .with_severity_label()
            .annotate(annotations)
            .render();

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
    fn end_of_input_range_is_a_single_caret() {
        let doc_id = RootContainedFilePath::new("test.hop").unwrap();
        let annotation = Diagnostic {
            message: "unexpected end of file".to_string(),
            range: DocumentCursor::new(doc_id, "fn main(".to_string()).eof_range(),
            severity: DiagnosticSeverity::Error,
        };

        let actual = DocumentAnnotator::new().annotate([annotation]).render();

        expect![[r#"
            unexpected end of file
            1 | fn main(
              |         ^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn end_of_input_range_after_trailing_newline_clamps_to_the_last_line() {
        let doc_id = RootContainedFilePath::new("test.hop").unwrap();
        let annotation = Diagnostic {
            message: "unexpected end of file".to_string(),
            range: DocumentCursor::new(doc_id, "fn main() {\n".to_string()).eof_range(),
            severity: DiagnosticSeverity::Error,
        };

        let actual = DocumentAnnotator::new().annotate([annotation]).render();

        expect![[r#"
            unexpected end of file
            1 | fn main() {
              |            ^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn with_location_info() {
        let source = "line one\nline two\nline three\nline four";
        let doc_id = RootContainedFilePath::new("main.rs").unwrap();

        let annotations = create_annotations_from_chunks(doc_id, source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(annotations)
            .render();

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

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch == '\n',
        );

        let actual = DocumentAnnotator::new()
            .with_lines_before(2)
            .annotate(annotations)
            .render();

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

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch == '\n',
        );

        let actual = DocumentAnnotator::new()
            .with_lines_after(2)
            .annotate(annotations)
            .render();

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

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch.is_whitespace(),
        );

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(annotations)
            .render();

        expect![[r#"
            code
              --> test.hop (line 1, col 1)
            1 | code
              | ^^^^

            code
              --> test.hop (line 2, col 3)
            2 |         code
              |         ^^^^

            code
              --> test.hop (line 3, col 2)
            3 |     code
              |     ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn unicode_emoji_width() {
        let source = "😀 code";

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch.is_whitespace(),
        );

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(annotations)
            .render();

        expect![[r#"
            😀
              --> test.hop (line 1, col 1)
            1 | 😀 code
              | ^^

            code
              --> test.hop (line 1, col 3)
            1 | 😀 code
              |    ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn location_with_document_id() {
        let source = "some code";

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch.is_whitespace(),
        );

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(annotations)
            .render();

        expect![[r#"
            some
              --> test.hop (line 1, col 1)
            1 | some code
              | ^^^^

            code
              --> test.hop (line 1, col 6)
            1 | some code
              |      ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn lines_before_exceeds_start() {
        let source = "line one\nline two\nline three\nline four\nline five\nline six";

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch == '\n',
        );

        let actual = DocumentAnnotator::new()
            .with_lines_before(1000)
            .annotate(annotations)
            .render();

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

        let annotations = create_annotations_from_chunks(
            RootContainedFilePath::new("test.hop").unwrap(),
            source,
            |ch| ch == 'n',
        );

        let actual = DocumentAnnotator::new().annotate(annotations).render();

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
