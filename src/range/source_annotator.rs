use std::{cmp, collections::HashMap};

use super::{
    RangedChars, RangedString,
    range::{Position, Range, Ranged},
};

/// Trait for any annotation that can be displayed on source code
pub trait Annotated: Ranged {
    fn message(&self) -> String;
}

impl<T: Annotated> Annotated for &T {
    fn message(&self) -> String {
        (*self).message()
    }
}

/// Simple annotation implementation for basic use cases
pub struct SimpleAnnotation {
    pub range: Range,
    pub message: String,
}

impl Ranged for SimpleAnnotation {
    fn range(&self) -> Range {
        self.range
    }
}

impl Annotated for SimpleAnnotation {
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

    pub fn annotate(
        &self,
        filename: Option<&str>,
        source: &str,
        annotations: impl IntoIterator<Item = impl Annotated>,
    ) -> String {
        let mut output = String::new();
        let lines = RangedChars::from(source).lines().collect::<HashMap<_, _>>();

        let line_count = source.lines().count();

        for (i, annotation) in annotations.into_iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }

            let range = annotation.range();

            if let Some(ref label) = self.label {
                output.push_str(&format!("{}: {}\n", label, annotation.message()));
            } else {
                output.push_str(&format!("{}\n", annotation.message()));
            }

            if self.show_location {
                if let Some(filename) = filename {
                    output.push_str(&format!(
                        "  --> {} (line {}, col {})\n",
                        filename,
                        range.start().line(),
                        range.start().column()
                    ));
                } else {
                    output.push_str(&format!(
                        "  --> (line {}, col {})\n",
                        range.start().line(),
                        range.start().column()
                    ));
                }
            }

            self.format_annotation(&mut output, &lines, range, line_count);
        }

        output
    }

    fn format_annotation(
        &self,
        output: &mut String,
        lines: &HashMap<usize, RangedString>,
        range: Range,
        line_count: usize,
    ) {
        let max_line_col_width = lines.len().to_string().len();

        let first_line = cmp::max(1, range.start().line().saturating_sub(self.lines_before));
        let last_line = cmp::min(line_count, range.end().line() + self.lines_after);

        for line_num in first_line..=last_line {
            let line_str = lines.get(&line_num).map(|s| s.value()).unwrap_or_default();
            self.format_line(output, line_num, line_str, max_line_col_width);
            if line_str.is_empty() {
                continue;
            }
            if let Some(line) = lines.get(&line_num) {
                let mut written = false;
                if let Some(intersection) = line.range().intersection(&range) {
                    if self.show_line_numbers {
                        output.push_str(&format!("{:width$} | ", "", width = max_line_col_width));
                    }
                    for (ch, ch_range) in line.chars() {
                        if intersection.contains_range(ch_range) {
                            written = true;
                            output.push_str(
                                &self
                                    .underline_char
                                    .to_string()
                                    .repeat(self.char_display_width(ch)),
                            );
                        } else if !written {
                            output.push_str(&" ".repeat(self.char_display_width(ch)));
                        }
                    }
                    output.push('\n');
                }
            }
        }
    }

    fn format_line(&self, output: &mut String, line_num: usize, content: &str, width: usize) {
        if self.show_line_numbers {
            output.push_str(&format!("{:width$} | ", line_num, width = width));
        }
        output.push_str(&self.expand_tabs(content));
        output.push('\n');
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

impl Default for SourceAnnotator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::range::RangedChars;

    use super::*;
    use expect_test::expect;

    #[test]
    fn test_with_label() {
        let source = "line one\nline two\nline three\nline four";

        let actual = SourceAnnotator::new().with_label("error").annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch == '\n'),
        );

        expect![[r#"
            error: "line one"
            1 | line one
              | ^^^^^^^^

            error: "line two"
            2 | line two
              | ^^^^^^^^

            error: "line three"
            3 | line three
              | ^^^^^^^^^^

            error: "line four"
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_with_location_info() {
        let source = "line one\nline two\nline three\nline four";

        let actual = SourceAnnotator::new().with_location().annotate(
            Some("main.rs"),
            source,
            RangedChars::from(source).split_by(|ch| ch == '\n'),
        );

        expect![[r#"
            "line one"
              --> main.rs (line 1, col 1)
            1 | line one
              | ^^^^^^^^

            "line two"
              --> main.rs (line 2, col 1)
            2 | line two
              | ^^^^^^^^

            "line three"
              --> main.rs (line 3, col 1)
            3 | line three
              | ^^^^^^^^^^

            "line four"
              --> main.rs (line 4, col 1)
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_with_lines_before() {
        let source = "line one\nline two\nline three\nline four";

        let actual = SourceAnnotator::new().with_lines_before(2).annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch == '\n'),
        );

        expect![[r#"
            "line one"
            1 | line one
              | ^^^^^^^^

            "line two"
            1 | line one
            2 | line two
              | ^^^^^^^^

            "line three"
            1 | line one
            2 | line two
            3 | line three
              | ^^^^^^^^^^

            "line four"
            2 | line two
            3 | line three
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_with_lines_after() {
        let source = "line one\nline two\nline three\nline four";

        let actual = SourceAnnotator::new().with_lines_after(2).annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch == '\n'),
        );

        expect![[r#"
            "line one"
            1 | line one
              | ^^^^^^^^
            2 | line two
            3 | line three

            "line two"
            2 | line two
              | ^^^^^^^^
            3 | line three
            4 | line four

            "line three"
            3 | line three
              | ^^^^^^^^^^
            4 | line four

            "line four"
            4 | line four
              | ^^^^^^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_tab_expansion() {
        let source = "code\n\t\tcode\n\tcode";

        let actual = SourceAnnotator::new().with_location().annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch.is_whitespace()),
        );

        expect![[r#"
            "code"
              --> (line 1, col 1)
            1 | code
              | ^^^^

            "code"
              --> (line 2, col 3)
            2 |         code
              |         ^^^^

            "code"
              --> (line 3, col 2)
            3 |     code
              |     ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_unicode_emoji_width() {
        let source = "😀 code";

        let actual = SourceAnnotator::new().with_location().annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch.is_whitespace()),
        );

        expect![[r#"
            "😀"
              --> (line 1, col 1)
            1 | 😀 code
              | ^^

            "code"
              --> (line 1, col 6)
            1 | 😀 code
              |    ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_location_without_filename() {
        let source = "some code";

        let actual = SourceAnnotator::new().with_location().annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch.is_whitespace()),
        );

        expect![[r#"
            "some"
              --> (line 1, col 1)
            1 | some code
              | ^^^^

            "code"
              --> (line 1, col 6)
            1 | some code
              |      ^^^^
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_lines_before_exceeds_start() {
        let source = "line one\nline two\nline three\nline four\nline five\nline six";

        let actual = SourceAnnotator::new().with_lines_before(1000).annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch == '\n'),
        );

        expect![[r#"
            "line one"
            1 | line one
              | ^^^^^^^^

            "line two"
            1 | line one
            2 | line two
              | ^^^^^^^^

            "line three"
            1 | line one
            2 | line two
            3 | line three
              | ^^^^^^^^^^

            "line four"
            1 | line one
            2 | line two
            3 | line three
            4 | line four
              | ^^^^^^^^^

            "line five"
            1 | line one
            2 | line two
            3 | line three
            4 | line four
            5 | line five
              | ^^^^^^^^^

            "line six"
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
    fn test_multi_line_annotation() {
        let source = "line one\nline two\nline three\nline four\nline five";

        let actual = SourceAnnotator::new().annotate(
            None,
            source,
            RangedChars::from(source).split_by(|ch| ch == 'n'),
        );

        expect![[r#"
            "li"
            1 | line one
              | ^^

            "e o"
            1 | line one
              |    ^^^

            "e\nli"
            1 | line one
              |        ^
            2 | line two
              | ^^

            "e two\nli"
            2 | line two
              |    ^^^^^
            3 | line three
              | ^^

            "e three\nli"
            3 | line three
              |    ^^^^^^^
            4 | line four
              | ^^

            "e four\nli"
            4 | line four
              |    ^^^^^^
            5 | line five
              | ^^

            "e five"
            5 | line five
              |    ^^^^^^
        "#]]
        .assert_eq(&actual);
    }
}
