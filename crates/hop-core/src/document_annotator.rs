use std::cmp;
use std::collections::BTreeMap;

use crate::itertools::ChunkByExt as _;

use crate::{
    annotation::Annotation,
    document::{DocumentCursor, DocumentRange},
    document_id::DocumentId,
};

struct StoredAnnotation {
    message: String,
    range: DocumentRange,
}

/// Annotator that can display source code with annotations
pub struct DocumentAnnotator {
    // Display options
    show_line_numbers: bool,
    show_location: bool,
    lines_before: usize,
    lines_after: usize,

    // Style options
    underline_char: char,
    tab_width: usize,
    label: Option<String>,

    annotations: BTreeMap<DocumentId, Vec<StoredAnnotation>>,
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
            annotations: BTreeMap::new(),
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
        &mut self,
        document_id: &DocumentId,
        annotations: impl IntoIterator<Item = A>,
    ) -> &mut Self
    where
        A: Annotation,
    {
        let stored: Vec<StoredAnnotation> = annotations
            .into_iter()
            .map(|a| {
                debug_assert!(
                    a.range().document_id() == document_id,
                    "Annotation's document_id must match the bucket document_id"
                );
                StoredAnnotation {
                    message: a.message(),
                    range: a.range().clone(),
                }
            })
            .collect();

        if stored.is_empty() {
            return self;
        }

        self.annotations
            .entry(document_id.clone())
            .or_default()
            .extend(stored);

        self
    }

    pub fn is_empty(&self) -> bool {
        self.annotations.is_empty()
    }

    pub fn render(&self) -> String {
        let mut output = String::new();
        let mut first_annotation = true;

        for (document_id, stored) in &self.annotations {
            // Build lines vector for this document using the first annotation's source
            let first_range = match stored.first() {
                Some(s) => &s.range,
                None => continue,
            };

            let range_doc_id = first_range.document_id().clone();
            let source = first_range.full_source().to_string();
            let lines: Vec<Option<DocumentRange>> = DocumentCursor::new(range_doc_id, source)
                .chunk_by(|range| range.start_utf32().line())
                .into_iter()
                .map(|(_, group)| group.filter(|s| s.ch() != '\n').collect())
                .collect();

            // Sort annotations by (start, end) within this document
            let mut sorted: Vec<&StoredAnnotation> = stored.iter().collect();
            sorted.sort_by_key(|a| (a.range.start(), a.range.end()));

            for annotation in sorted {
                if !first_annotation {
                    output.push('\n');
                }
                first_annotation = false;

                if let Some(ref label) = self.label {
                    output.push_str(&format!("{}: {}\n", label, annotation.message));
                } else {
                    output.push_str(&format!("{}\n", annotation.message));
                }

                if self.show_location {
                    output.push_str(&format!(
                        "  --> {} (line {}, col {})\n",
                        document_id,
                        annotation.range.start_utf32().line() + 1,
                        annotation.range.start_utf32().column() + 1
                    ));
                }

                self.format_annotation(&mut output, &lines, &annotation.range);
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
    use crate::{document_id::DocumentId, simple_annotation::SimpleAnnotation};
    use expect_test::expect;

    fn create_annotations_from_chunks(
        doc_id: DocumentId,
        source: &str,
        predicate: impl Fn(char) -> bool,
    ) -> Vec<SimpleAnnotation> {
        DocumentCursor::new(doc_id, source.to_string())
            .chunk_by(|range| predicate(range.ch()))
            .into_iter()
            .filter_map(|(is_separator, group)| {
                if !is_separator {
                    let range: Option<DocumentRange> = group.collect();
                    range.map(|range| SimpleAnnotation {
                        message: range.as_str().to_string(),
                        range,
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    #[test]
    fn with_label() {
        let source = "line one\nline two\nline three\nline four";

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch == '\n'
            });

        let actual = DocumentAnnotator::new()
            .with_label("error")
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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
    fn with_location_info() {
        let source = "line one\nline two\nline three\nline four";
        let doc_id = DocumentId::new("main.rs").unwrap();

        let annotations = create_annotations_from_chunks(doc_id.clone(), source, |ch| ch == '\n');

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(&doc_id, annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch == '\n'
            });

        let actual = DocumentAnnotator::new()
            .with_lines_before(2)
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch == '\n'
            });

        let actual = DocumentAnnotator::new()
            .with_lines_after(2)
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch.is_whitespace()
            });

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch.is_whitespace()
            });

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch.is_whitespace()
            });

        let actual = DocumentAnnotator::new()
            .with_location()
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch == '\n'
            });

        let actual = DocumentAnnotator::new()
            .with_lines_before(1000)
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
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

        let annotations =
            create_annotations_from_chunks(DocumentId::new("test.hop").unwrap(), source, |ch| {
                ch == 'n'
            });

        let actual = DocumentAnnotator::new()
            .annotate(&DocumentId::new("test.hop").unwrap(), annotations)
            .render();

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
