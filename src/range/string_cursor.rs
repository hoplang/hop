use std::{fmt, iter::FromIterator, sync::Arc};

use super::{Position, Range, range::Ranged};

/// Holds source text and precomputed line start offsets for
/// efficient position lookups.
#[derive(Debug, Clone)]
struct SourceInfo {
    /// The source text.
    text: String,
    /// Byte offsets where each line starts.
    /// First line always starts at 0.
    line_starts: Vec<usize>,
}

#[derive(Clone)]
pub struct StringCursor {
    /// The source info containing text and line starts.
    source: Arc<SourceInfo>,
    /// The current byte offset in the source string.
    offset: usize,
    /// The byte offset where iteration should stop (exclusive).
    end: usize,
}

impl StringCursor {
    pub fn new(source: &str) -> Self {
        Self {
            offset: 0,
            end: source.len(),
            source: Arc::new(SourceInfo::new(source.to_string())),
        }
    }
}

impl Iterator for StringCursor {
    type Item = StringSpan;
    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.end {
            return None;
        }
        let start_offset = self.offset;
        self.source.text[self.offset..self.end]
            .chars()
            .next()
            .map(|ch| {
                self.offset += ch.len_utf8();
                StringSpan {
                    source: self.source.clone(),
                    start: start_offset,
                    end: self.offset,
                }
            })
    }
}

#[derive(Debug, Clone)]
pub struct StringSpan {
    /// The source info containing text and line starts.
    source: Arc<SourceInfo>,
    /// the start offset for this string span in the document (in bytes).
    start: usize,
    /// the end offset for this string span in the document (in bytes).
    end: usize,
}

impl StringSpan {
    pub fn ch(&self) -> char {
        self.source.text[self.start..].chars().next().unwrap()
    }

    pub fn to(self, other: StringSpan) -> Self {
        StringSpan {
            source: self.source,
            start: self.start,
            end: other.end,
        }
    }
    pub fn extend<I>(self, iter: I) -> Self
    where
        I: IntoIterator<Item = StringSpan>,
    {
        iter.into_iter().fold(self, |acc, span| acc.to(span))
    }
    pub fn as_str(&self) -> &str {
        &self.source.text[self.start..self.end]
    }
    pub fn cursor(&self) -> StringCursor {
        StringCursor {
            source: self.source.clone(),
            offset: self.start,
            end: self.end,
        }
    }
    pub fn start(&self) -> Position {
        self.source.offset_to_position(self.start)
    }
    pub fn end(&self) -> Position {
        self.source.offset_to_position(self.end)
    }

    pub fn range(&self) -> Range {
        Range::new(
            self.source.offset_to_position(self.start),
            self.source.offset_to_position(self.end),
        )
    }
}

impl fmt::Display for StringSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl FromIterator<StringSpan> for Option<StringSpan> {
    fn from_iter<I: IntoIterator<Item = StringSpan>>(iter: I) -> Self {
        iter.into_iter().reduce(|acc, span| acc.to(span))
    }
}

/// Trait for types that have a StringSpan representing their location in source code.
pub trait Spanned {
    /// Returns a reference to the StringSpan for this item.
    fn span(&self) -> &StringSpan;
}

impl Spanned for StringSpan {
    fn span(&self) -> &StringSpan {
        self
    }
}

impl SourceInfo {
    fn new(text: String) -> Self {
        let mut line_starts = vec![0];
        for (i, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(i + ch.len_utf8());
            }
        }
        Self { text, line_starts }
    }

    /// Convert a byte offset to a Position (line, column) using binary search.
    fn offset_to_position(&self, offset: usize) -> Position {
        // Binary search to find the line containing this offset
        let line_idx = match self.line_starts.binary_search(&offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };

        let line = line_idx + 1; // Lines are 1-indexed
        let line_start = self.line_starts[line_idx];

        // Calculate column by counting UTF-8 characters from line start to offset
        let column = self.text[line_start..offset]
            .chars()
            .map(|ch| ch.len_utf8())
            .sum::<usize>()
            + 1; // Columns are 1-indexed

        Position::new(line, column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_cursor_new() {
        let cursor = StringCursor::new("hello");
        assert_eq!(cursor.offset, 0);
        assert_eq!(cursor.end, 5);
    }

    #[test]
    fn test_string_cursor_single_line() {
        let mut cursor = StringCursor::new("abc");

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.start(), Position::new(1, 1));
        assert_eq!(span1.end(), Position::new(1, 2));

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), 'b');
        assert_eq!(span2.start(), Position::new(1, 2));
        assert_eq!(span2.end(), Position::new(1, 3));

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'c');
        assert_eq!(span3.start(), Position::new(1, 3));
        assert_eq!(span3.end(), Position::new(1, 4));

        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_multiline() {
        let mut cursor = StringCursor::new("a\nb\nc");

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.start(), Position::new(1, 1));
        assert_eq!(span1.end(), Position::new(1, 2));

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '\n');
        assert_eq!(span2.start(), Position::new(1, 2));
        assert_eq!(span2.end(), Position::new(2, 1));

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'b');
        assert_eq!(span3.start(), Position::new(2, 1));
        assert_eq!(span3.end(), Position::new(2, 2));

        let span4 = cursor.next().unwrap();
        assert_eq!(span4.ch(), '\n');
        assert_eq!(span4.start(), Position::new(2, 2));
        assert_eq!(span4.end(), Position::new(3, 1));

        let span5 = cursor.next().unwrap();
        assert_eq!(span5.ch(), 'c');
        assert_eq!(span5.start(), Position::new(3, 1));
        assert_eq!(span5.end(), Position::new(3, 2));

        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_utf8() {
        let mut cursor = StringCursor::new("a€b");

        let span1 = cursor.next().unwrap();
        assert_eq!(span1.ch(), 'a');
        assert_eq!(span1.end().column(), 2);

        let span2 = cursor.next().unwrap();
        assert_eq!(span2.ch(), '€');
        assert_eq!(span2.start().column(), 2);
        assert_eq!(span2.end().column(), 5); // € is 3 bytes in UTF-8

        let span3 = cursor.next().unwrap();
        assert_eq!(span3.ch(), 'b');
        assert_eq!(span3.start().column(), 5);
        assert_eq!(span3.end().column(), 6);
    }

    #[test]
    fn test_string_span_extend() {
        let mut cursor = StringCursor::new("abc");
        let span1 = cursor.next().unwrap();
        let _span2 = cursor.next().unwrap();
        let span3 = cursor.next().unwrap();

        let extended = span1.clone().to(span3);
        assert_eq!(extended.ch(), 'a');
        assert_eq!(extended.to_string(), "abc");
        assert_eq!(extended.start(), Position::new(1, 1));
        assert_eq!(extended.end(), Position::new(1, 4));
    }

    #[test]
    fn test_string_span_to_string() {
        let mut cursor = StringCursor::new("hello world");
        let spans: Vec<_> = cursor.by_ref().take(5).collect();

        assert_eq!(spans[0].to_string(), "h");
        assert_eq!(spans[1].to_string(), "e");
        assert_eq!(spans[2].to_string(), "l");
        assert_eq!(spans[3].to_string(), "l");
        assert_eq!(spans[4].to_string(), "o");

        let extended = spans[0].clone().to(spans[4].clone());
        assert_eq!(extended.to_string(), "hello");
    }

    #[test]
    fn test_empty_string_cursor() {
        let mut cursor = StringCursor::new("");
        assert!(cursor.next().is_none());
    }

    #[test]
    fn test_string_cursor_clone() {
        let cursor1 = StringCursor::new("test");
        let mut cursor2 = cursor1.clone();

        let span = cursor2.next().unwrap();
        assert_eq!(span.ch(), 't');
    }

    #[test]
    fn test_collect_string_spans() {
        let result: Option<StringSpan> = StringCursor::new("   hello")
            .take_while(|s| s.ch() == ' ')
            .collect();

        let span = result.unwrap();
        assert_eq!(span.as_str(), "   ");
        assert_eq!(span.start(), Position::new(1, 1));
        assert_eq!(span.end(), Position::new(1, 4));
    }

    #[test]
    fn test_collect_empty_spans() {
        let result: Option<StringSpan> = StringCursor::new("hello")
            .take_while(|s| s.ch() == ' ')
            .collect();

        assert!(result.is_none());
    }

    #[test]
    fn test_collect_multiline_spans() {
        let result: Option<StringSpan> = StringCursor::new("aaa\nbbb")
            .take_while(|s| s.ch() == 'a')
            .collect();

        let span = result.unwrap();
        assert_eq!(span.as_str(), "aaa");
        assert_eq!(span.start(), Position::new(1, 1));
        assert_eq!(span.end(), Position::new(1, 4));
    }

    #[test]
    fn test_collect_with_skip() {
        let result: Option<StringSpan> = StringCursor::new("   hello   ")
            .skip(3)
            .take_while(|s| s.ch().is_alphabetic())
            .collect();

        let span = result.unwrap();
        assert_eq!(span.as_str(), "hello");
        assert_eq!(span.start(), Position::new(1, 4));
        assert_eq!(span.end(), Position::new(1, 9));
    }
}
